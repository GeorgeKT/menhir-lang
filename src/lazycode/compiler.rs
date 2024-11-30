use std::collections::BTreeMap;
use std::ops::Deref;
use std::rc::Rc;

use super::consteval::expr_to_const;
use super::function::ByteCodeFunction;
use super::instruction::{branch_if_instr, branch_instr, ret_instr, store_instr, Instruction, Label};
use super::operand::{call_arg, ByteCodeProperty, CallArg, Operand};
use super::patterns::{pattern_to_bc, RESULT_OK_PATTERN_MATCH_IDX};
use super::scope::Scope;
use super::stack::Stack;
use super::{ByteCodeModule, Global, OPTIONAL_DATA_IDX};
use crate::ast::*;
use crate::build::Package;
use crate::compileerror::{code_gen_error, code_gen_result, type_error_result, CompileError, CompileResult};
use crate::target::Target;

fn literal_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    lit: &Literal,
    target: &Target,
) -> CompileResult<Operand> {
    Ok(match lit {
        Literal::Int(_, v, is) => Operand::const_int(*v, *is),
        Literal::UInt(_, v, is) => Operand::const_uint(*v, *is),
        Literal::Bool(_, v) => Operand::const_bool(*v),
        Literal::Char(_, c) => Operand::const_char(*c),
        Literal::Float(_, f, fs) => Operand::const_float(
            f.parse()
                .map_err(|_| code_gen_error("Invalid float constant"))?,
            *fs,
        ),
        Literal::String(_, s) => Operand::const_string(s.clone()),
        Literal::Array(a) => {
            let members = a
                .elements
                .iter()
                .map(|e| expr_to_bc(bc_mod, scope, e, target))
                .collect::<Result<Vec<_>, _>>()?;
            Operand::Array {
                members,
                typ: a.array_type.clone(),
            }
        }
        Literal::NullPtr(_, typ) => Operand::Null { typ: typ.clone() },
    })
}

fn name_ref_to_bc(scope: &Scope, nr: &NameRef) -> CompileResult<Operand> {
    let add_name_ref = |nr: &NameRef| {
        if let Some(var) = scope.get_var(&nr.name)? {
            Ok(var)
        } else {
            code_gen_result(format!("ICE: Unknown name {}", nr.name))
        }
    };

    match &nr.typ {
        Type::Sum(st) => {
            if let Some(idx) = st.index_of(&nr.name) {
                Ok(Operand::Sum {
                    variant: idx,
                    inner: None,
                    typ: nr.typ.clone(),
                })
            } else {
                add_name_ref(nr)
            }
        }

        Type::Enum(et) => {
            if let Some(idx) = et.index_of(&nr.name) {
                // enums are integers
                Ok(Operand::Enum {
                    variant: idx,
                    typ: nr.typ.clone(),
                })
            } else {
                add_name_ref(nr)
            }
        }

        Type::Func(_) => Ok(Operand::Func {
            name: nr.name.clone(),
            typ: nr.typ.clone(),
        }),

        _ => add_name_ref(nr),
    }
}

fn call_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, call: &Call, target: &Target) -> CompileResult<Operand> {
    let Type::Func(ft) = call.callee_type() else {
        panic!("ICE: Callee must have a function type");
    };

    let callee = name_ref_to_bc(scope, &call.callee)?;
    let mut args: Vec<CallArg> = call
        .args
        .iter()
        .zip(ft.args.iter())
        .map(|(a, d)| {
            let arg = expr_to_bc(bc_mod, scope, a, target)?;
            Ok::<CallArg, CompileError>(call_arg(arg, d.mutable, d.typ.clone()))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let rvo = ft.return_type.should_do_rvo();
    if rvo {
        // rvo, so alloc the destination and pass it to the function
        let rvo_arg = scope.declare(RVO_RETURN_ARG, None, ft.return_type.clone())?;
        args.push(call_arg(rvo_arg, true, ptr_type(ft.return_type.clone())));
    }

    Ok(Operand::Call {
        callee: Box::new(callee),
        args,
        typ: if rvo {
            ptr_type(call.return_type.clone())
        } else {
            call.return_type.clone()
        },
        rvo,
    })
}

fn block_expr_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    e: &Expression,
    target: &Target,
    is_last: bool,
) -> CompileResult<Operand> {
    let op = expr_to_bc(bc_mod, scope, e, target)?;
    if is_last {
        return Ok(op);
    }
    if op.is_call() {
        scope.add(Instruction::Exec { operand: op });
    }

    Ok(Operand::Void)
}

fn block_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, b: &Block, target: &Target) -> CompileResult<Operand> {
    for e in &b.deferred_expressions {
        scope.add_unwind_call(e.clone());
    }

    for df in &b.drop_flags {
        scope.declare(&df.0, Some(Operand::const_bool(false)), Type::Bool)?;
    }

    let mut ret = Operand::Void;
    for (idx, e) in b.expressions.iter().enumerate() {
        if idx == b.expressions.len() - 1 {
            let op = block_expr_to_bc(bc_mod, scope, e, target, true)?;
            if b.typ != Type::Void {
                ret = op;
            } else if op.is_call() {
                scope.add(Instruction::Exec { operand: op });
            }
        } else {
            block_expr_to_bc(bc_mod, scope, e, target, false)?;
        }
    }

    Ok(ret)
}

fn add_binding(bc_mod: &mut ByteCodeModule, scope: &mut Scope, b: &Binding, target: &Target) -> CompileResult<()> {
    match &b.binding_type {
        BindingType::Name(name) => {
            let init = expr_to_bc(bc_mod, scope, &b.init, target)?;
            scope.declare(name, Some(init), b.typ.clone())?;
        }
        BindingType::Struct(sp) => {
            // Declare init so it is only executed once
            let init = expr_to_bc(bc_mod, scope, &b.init, target)?;
            let init_typ = b.init.get_type();
            let init_var = scope.declare("$init", Some(init), init_typ)?;
            for (idx, b) in sp.bindings.iter().enumerate() {
                scope.alias(
                    &b.name,
                    if b.typ.pass_by_value() && b.mode == StructPatternBindingMode::Value {
                        Operand::member(
                            init_var.safe_clone()?,
                            Operand::const_uint(idx as u64, target.int_size),
                            b.typ.clone(),
                        )
                    } else {
                        Operand::member_ptr(
                            init_var.safe_clone()?,
                            Operand::const_uint(idx as u64, target.int_size),
                            ptr_type(b.typ.clone()),
                        )
                    },
                )?;
            }
        }
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum FieldAccessMode {
    Value,
    Pointer,
}

fn member_access_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    ma: &MemberAccess,
    target: &Target,
    field_access_mode: FieldAccessMode,
) -> CompileResult<Operand> {
    let obj = expr_to_bc(bc_mod, scope, &ma.left, target)?;
    match &ma.right {
        MemberAccessType::Call(call) => {
            let callee = name_ref_to_bc(scope, &call.callee)?;
            let Type::Func(ft) = call.callee_type() else {
                panic!("ICE: Callee must have a function type");
            };
            let mut args: Vec<CallArg> = call
                .args
                .iter()
                .zip(ft.args.iter())
                .map(|(e, d)| {
                    Ok::<CallArg, CompileError>(call_arg(
                        expr_to_bc(bc_mod, scope, e, target)?,
                        d.mutable,
                        d.typ.clone(),
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;
            if call.return_type.should_do_rvo() {
                // rvo, so alloc the destination and pass it to the function
                let rvo_arg = scope.declare(RVO_RETURN_ARG, None, call.return_type.clone())?;
                args.push(call_arg(rvo_arg, true, ptr_type(call.return_type.clone())));
            }

            Ok(Operand::Call {
                callee: Box::new(callee),
                args,
                typ: ma.typ.clone(),
                rvo: call.return_type.should_do_rvo(),
            })
        }
        MemberAccessType::Name(field) => {
            let idx = Operand::const_uint(field.index as u64, target.int_size);
            Ok(
                if field.typ.pass_by_value() && field_access_mode != FieldAccessMode::Pointer {
                    Operand::member(obj, idx, field.typ.clone())
                } else {
                    Operand::member_ptr(obj, idx, ptr_type(field.typ.clone()))
                },
            )
        }
        MemberAccessType::Property(prop) => Ok(Operand::Property {
            operand: Box::new(obj),
            property: match prop {
                Property::Len => ByteCodeProperty::Len,
                Property::Data => ByteCodeProperty::Data,
            },
            typ: ma.typ.clone(),
        }),
    }
}

fn assign_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, assign: &Assign, target: &Target) -> CompileResult<()> {
    // During type checking, other assigns, will be converted in a regular assign
    assert!(assign.operator == AssignOperator::Assign);

    let value = expr_to_bc(bc_mod, scope, &assign.right, target)?;
    match &assign.left {
        AssignTarget::Var(nr) => {
            scope.add(store_instr(
                Operand::VarPtr {
                    name: nr.name.clone(),
                    typ: nr.typ.ptr_of(),
                },
                value,
            ));
        }

        AssignTarget::MemberAccess(ma) => {
            let dst = member_access_to_bc(bc_mod, scope, ma, target, FieldAccessMode::Pointer)?;
            scope.add(store_instr(dst, value));
        }

        AssignTarget::Dereference(d) => {
            let inner = expr_to_bc(bc_mod, scope, &d.inner, target)?;
            scope.add(store_instr(inner, value));
        }

        AssignTarget::IndexOperation(iop) => {
            let tgt = expr_to_bc(bc_mod, scope, &iop.target, target)?;
            let idx = expr_to_bc(bc_mod, scope, &iop.index_expr, target)?;
            scope.add(store_instr(
                Operand::member_ptr(tgt, idx, ptr_type(iop.typ.clone())),
                value,
            ));
        }
    }
    Ok(())
}

fn unary_op_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    uop: &UnaryOp,
    target: &Target,
) -> CompileResult<Operand> {
    let i = expr_to_bc(bc_mod, scope, &uop.expression, target)?;
    Ok(Operand::Unary {
        inner: Box::new(i),
        op: uop.operator,
        typ: uop.typ.clone(),
    })
}

fn optional_or_to_bc(scope: &mut Scope, l: Operand, r: Operand, target: &Target) -> CompileResult<Operand> {
    let dst_type = r.get_type();
    let dst = scope.declare("$dst", None, dst_type.clone())?;
    let l_typ = l.get_type();
    let l_var = scope.declare("$l", Some(l), l_typ)?;

    let l_sti = Operand::sti(l_var.safe_clone()?, target.int_size);
    let l_is_valid = Operand::binary(
        BinaryOperator::Equals,
        l_sti,
        Operand::const_uint(0, target.int_size),
        Type::Bool,
    );
    let store_r_bb = scope.label();
    let store_l_bb = scope.label();
    let end_bb = scope.label();

    scope.add(branch_if_instr(l_is_valid, store_l_bb, store_r_bb));

    scope.start_label(store_r_bb);
    scope.add(store_instr(dst.safe_clone()?, r));
    scope.add(branch_instr(end_bb));

    scope.start_label(store_l_bb);
    scope.add(store_instr(
        dst.safe_clone()?,
        Operand::member(l_var, Operand::const_uint(OPTIONAL_DATA_IDX, target.int_size), dst_type),
    ));
    scope.add(branch_instr(end_bb));
    scope.start_label(end_bb);
    dst.make_var()
}

fn result_or_to_bc(scope: &mut Scope, l: Operand, r: Operand, target: &Target) -> CompileResult<Operand> {
    let dst_type = r.get_type();
    let dst = scope.declare("$dst", None, dst_type.clone())?;
    let l_typ = l.get_type();
    let l_var = scope.declare("$l", Some(l), l_typ)?;
    let sti = Operand::sti(l_var.safe_clone()?, target.int_size);

    let ok_branch = scope.label();
    let error_branch = scope.label();
    let end_bb = scope.label();

    let l_is_ok = Operand::binary(
        BinaryOperator::Equals,
        sti,
        Operand::const_uint(RESULT_OK_PATTERN_MATCH_IDX, target.int_size),
        Type::Bool,
    );
    scope.add(branch_if_instr(l_is_ok, ok_branch, error_branch));
    scope.start_label(ok_branch);
    scope.add(store_instr(
        dst.safe_clone()?,
        Operand::member(l_var.safe_clone()?, Operand::const_uint(0, target.int_size), dst_type),
    ));
    scope.add(branch_instr(end_bb));

    scope.start_label(error_branch);
    scope.add(store_instr(dst.safe_clone()?, r));
    scope.add(branch_instr(end_bb));

    scope.start_label(end_bb);
    dst.make_var()
}

fn binary_op_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    bop: &BinaryOp,
    target: &Target,
) -> CompileResult<Operand> {
    let left = expr_to_bc(bc_mod, scope, &bop.left, target)?;
    let right = expr_to_bc(bc_mod, scope, &bop.right, target)?;
    match (&left.get_type(), &bop.operator) {
        (Type::Optional(_), BinaryOperator::Or) => optional_or_to_bc(scope, left, right, target),
        (Type::Result(_), BinaryOperator::Or) => result_or_to_bc(scope, left, right, target),
        _ => Ok(Operand::Binary {
            op: bop.operator,
            left: Box::new(left),
            right: Box::new(right),
            typ: bop.typ.clone(),
        }),
    }
}

fn struct_initializer_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    si: &StructInitializer,
    target: &Target,
) -> CompileResult<Operand> {
    match &si.typ {
        Type::Struct(_) => {
            let members = si
                .member_initializers
                .iter()
                .map(|mi| expr_to_bc(bc_mod, scope, &mi.initializer, target))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Operand::Struct {
                members,
                typ: si.typ.clone(),
            })
        }
        Type::Sum(st) => {
            let idx = st
                .index_of(
                    si.struct_name
                        .as_ref()
                        .ok_or_else(|| code_gen_error("Case variant with struct type must have a name"))?,
                )
                .ok_or_else(|| code_gen_error("Cannot determine sum type case"))?;
            let members = si
                .member_initializers
                .iter()
                .map(|mi| expr_to_bc(bc_mod, scope, &mi.initializer, target))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Operand::Sum {
                variant: idx,
                inner: Some(Box::new(Operand::Struct {
                    members,
                    typ: st.cases[idx]
                        .typ
                        .clone()
                        .ok_or_else(|| code_gen_error("Sum type case has no data"))?,
                })),
                typ: si.typ.clone(),
            })
        }
        _ => code_gen_result("Expecting sum type or struct type in a StructInitializer"),
    }
}

fn while_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, w: &WhileLoop, target: &Target) -> CompileResult<()> {
    let cond_bb = scope.label();
    let body_bb = scope.label();
    let post_while_bb = scope.label();

    scope.add(branch_instr(cond_bb));
    scope.start_label(cond_bb);

    let cond = expr_to_bc(bc_mod, scope, &w.cond, target)?;
    scope.add(branch_if_instr(cond, body_bb, post_while_bb));
    scope.scope(|scope| {
        scope.start_label(body_bb);
        expr_to_bc(bc_mod, scope, &w.body, target)?;
        let keep_looping = !scope.last_instruction_is_return();
        if keep_looping {
            scope.exit(bc_mod, target, branch_instr(cond_bb))?;
        }
        Ok(())
    })?;
    scope.start_label(post_while_bb);
    Ok(())
}

fn for_range_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, f: &ForLoop, target: &Target) -> CompileResult<()> {
    let end_bb = scope.label();
    scope.scope(|scope| {
        let iterable = expr_to_bc(bc_mod, scope, &f.iterable, target)?;
        let iterable = scope.alias("$iterable", iterable)?;
        let Type::Range(it) = iterable.get_type() else {
            return code_gen_result("Expecting range type");
        };

        let index_ptr = scope.declare(
            &f.loop_variable,
            Some(Operand::member(
                iterable.safe_clone()?,
                Operand::const_uint(0, target.int_size),
                it.deref().clone(),
            )),
            it.deref().clone(),
        )?;
        let index = index_ptr.make_var()?;

        let len = Operand::member(
            iterable.safe_clone()?,
            Operand::const_uint(1, target.int_size),
            it.deref().clone(),
        );

        let cond_bb = scope.label();
        let body_bb = scope.label();
        let post_for_bb = scope.label();
        scope.add(branch_instr(cond_bb));
        scope.start_label(cond_bb);

        let cmp = Operand::binary(BinaryOperator::LessThan, index.safe_clone()?, len, Type::Bool);
        scope.add(branch_if_instr(cmp, body_bb, post_for_bb));

        scope.scope(|scope| {
            scope.start_label(body_bb);
            expr_to_bc(bc_mod, scope, &f.body, target)?;
            let keep_looping = !scope.last_instruction_is_return();
            if keep_looping {
                scope.add(store_instr(
                    index_ptr.safe_clone()?,
                    Operand::binary(
                        BinaryOperator::Add,
                        index.safe_clone()?,
                        match it.deref() {
                            Type::Int(is) => Operand::const_int(1, *is),
                            Type::UInt(is) => Operand::const_uint(1, *is),
                            _ => panic!("ICE: Expecting int type for loop iterations"),
                        },
                        it.deref().clone(),
                    ),
                ));
                scope.exit(bc_mod, target, branch_instr(cond_bb))?;
            }
            Ok(())
        })?;

        scope.start_label(post_for_bb);
        scope.exit(bc_mod, target, branch_instr(end_bb))
    })?;
    scope.start_label(end_bb);
    Ok(())
}

fn for_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, f: &ForLoop, target: &Target) -> CompileResult<()> {
    if matches!(f.iterable.get_type(), Type::Range(_)) {
        return for_range_to_bc(bc_mod, scope, f, target);
    }

    let end_bb = scope.label();
    scope.scope(|scope| {
        let iterable = expr_to_bc(bc_mod, scope, &f.iterable, target)?;
        let iterable = scope.alias("$iterable", iterable)?;
        let index_ptr = scope.declare(
            "$index",
            Some(Operand::const_uint(0, target.int_size)),
            Type::UInt(target.int_size),
        )?;
        let index = index_ptr.make_var()?;

        let len = if let Type::Array(at) = iterable.get_type() {
            Operand::const_uint(at.len as u64, target.int_size)
        } else {
            Operand::Property {
                operand: Box::new(iterable.safe_clone()?),
                property: ByteCodeProperty::Len,
                typ: target.native_uint_type.clone(),
            }
        };

        let cond_bb = scope.label();
        let body_bb = scope.label();
        let post_for_bb = scope.label();
        scope.add(branch_instr(cond_bb));
        scope.start_label(cond_bb);

        let cmp = Operand::binary(BinaryOperator::LessThan, index.safe_clone()?, len, Type::Bool);
        scope.add(branch_if_instr(cmp, body_bb, post_for_bb));

        scope.scope(|scope| {
            scope.start_label(body_bb);
            if f.loop_variable_type.pass_by_value() {
                scope.alias(
                    &f.loop_variable,
                    Operand::member(iterable, index.safe_clone()?, f.loop_variable_type.clone()),
                )?;
            } else {
                scope.alias(
                    &f.loop_variable,
                    Operand::member_ptr(iterable, index.safe_clone()?, f.loop_variable_type.ptr_of()),
                )?;
            }

            expr_to_bc(bc_mod, scope, &f.body, target)?;
            let keep_looping = !scope.last_instruction_is_return();
            if keep_looping {
                scope.add(store_instr(
                    index_ptr.safe_clone()?,
                    Operand::binary(
                        BinaryOperator::Add,
                        index.safe_clone()?,
                        Operand::const_uint(1, target.int_size),
                        Type::UInt(target.int_size),
                    ),
                ));
                scope.exit(bc_mod, target, branch_instr(cond_bb))?;
            }
            Ok(())
        })?;

        scope.start_label(post_for_bb);
        scope.exit(bc_mod, target, branch_instr(end_bb))
    })?;
    scope.start_label(end_bb);
    Ok(())
}

fn if_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    if_expr: &IfExpression,
    target: &Target,
) -> CompileResult<Operand> {
    let dst = scope.declare("$dst", None, if_expr.typ.clone())?;
    let true_bb = scope.label();
    let end_bb = scope.label();

    let cond = expr_to_bc(bc_mod, scope, &if_expr.condition, target)?;

    if let Some(on_false) = &if_expr.on_false {
        let false_bb = scope.label();
        scope.add(branch_if_instr(cond, true_bb, false_bb));
        scope.scope(|scope| {
            scope.start_label(false_bb);
            let val = expr_to_bc(bc_mod, scope, on_false, target)?;
            let add_branch = !scope.last_instruction_is_return();
            if add_branch {
                scope.add(store_instr(dst.safe_clone()?, val));
                scope.exit(bc_mod, target, branch_instr(end_bb))?;
            }
            Ok(())
        })?;
    } else {
        scope.add(branch_if_instr(cond, true_bb, end_bb));
    }

    scope.scope(|scope| {
        scope.start_label(true_bb);
        let val = expr_to_bc(bc_mod, scope, &if_expr.on_true, target)?;
        let add_branch = !scope.last_instruction_is_return();
        if add_branch {
            scope.add(store_instr(dst.safe_clone()?, val));
            scope.exit(bc_mod, target, branch_instr(end_bb))?;
        }
        Ok(())
    })?;

    scope.start_label(end_bb);
    dst.make_var()
}

fn lambda_to_bc(bc_mod: &mut ByteCodeModule, l: &Lambda, target: &Target) -> CompileResult<Operand> {
    let lambda = func_to_bc(&l.sig, bc_mod, &l.expr, target)?;
    bc_mod.functions.insert(l.sig.name.clone(), lambda);
    Ok(Operand::Func {
        name: l.sig.name.clone(),
        typ: l.sig.typ.clone(),
    })
}

fn return_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, r: &Return, target: &Target) -> CompileResult<Operand> {
    let ret = expr_to_bc(bc_mod, scope, &r.expression, target)?;
    if scope.rvo() {
        scope.add(store_instr(scope.rvo_var(), ret));
        scope.exit(bc_mod, target, ret_instr(Operand::Void))?;
    } else {
        scope.exit(bc_mod, target, ret_instr(ret))?;
    }
    Ok(Operand::Void)
}

fn compiler_call_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    c: &CompilerCall,
    target: &Target,
) -> CompileResult<Operand> {
    Ok(match c {
        CompilerCall::SizeOf { typ, int_size, .. } => Operand::SizeOf {
            typ: typ.clone(),
            int_size: *int_size,
        },
        CompilerCall::Drop {
            obj,
            destructor_call,
            drop_flag,
            ..
        } => {
            let Some(ds) = destructor_call else {
                return Ok(Operand::Void);
            };

            let end = scope.label();
            scope.scope(|scope| {
                let obj = expr_to_bc(bc_mod, scope, obj, target)?;
                let obj = scope.make_var("$tbd", obj)?;
                if let Some(df) = drop_flag {
                    let dropped = Operand::VarPtr {
                        name: df.0.clone(),
                        typ: Type::Bool.ptr_of(),
                    };
                    let not_dropped = Operand::Unary {
                        op: UnaryOperator::Not,
                        inner: Box::new(dropped.make_var()?),
                        typ: Type::Bool,
                    };
                    let drop_obj = scope.label();
                    scope.add(branch_if_instr(not_dropped, drop_obj, end));
                    scope.start_label(drop_obj);
                    scope.add(store_instr(dropped, Operand::const_bool(true)));
                }

                let call = Operand::Call {
                    callee: Box::new(name_ref_to_bc(scope, &ds.callee)?),
                    args: vec![call_arg(obj.safe_clone()?, true, obj.get_type())],
                    typ: Type::Void,
                    rvo: false,
                };
                scope.add(Instruction::Exec { operand: call });
                scope.exit(bc_mod, target, branch_instr(end))
            })?;

            scope.start_label(end);
            Operand::Void
        }
    })
}

fn array_to_slice_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    ats: &ArrayToSlice,
    target: &Target,
) -> CompileResult<Operand> {
    let array = expr_to_bc(bc_mod, scope, &ats.inner, target)?;
    let array = scope.make_var("$array", array)?;
    let range = Operand::Range {
        start: Box::new(Operand::const_uint(0, target.int_size)),
        end: Box::new(Operand::Property {
            operand: Box::new(array.safe_clone()?),
            property: ByteCodeProperty::Len,
            typ: target.native_int_type.clone(),
        }),
        typ: Type::Range(Rc::new(Type::UInt(target.int_size))),
    };
    Ok(Operand::slice(array, range, ats.slice_type.clone()))
}

fn bindings_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    b: &BindingList,
    target: &Target,
) -> CompileResult<Operand> {
    for b in &b.bindings {
        add_binding(bc_mod, scope, b, target)?;
    }
    Ok(Operand::Void)
}

fn match_case_body_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    body: &Expression,
    dst: Operand,
    match_case_bb: Label,
    match_end_bb: Label,
    target: &Target,
) -> CompileResult<()> {
    scope.start_label(match_case_bb);
    let val = expr_to_bc(bc_mod, scope, body, target)?;
    let add_branch = !scope.last_instruction_is_return();
    if add_branch {
        scope.add(store_instr(dst, val));
        scope.exit(bc_mod, target, branch_instr(match_end_bb))?;
    }
    Ok(())
}

fn match_case_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    pattern: &Pattern,
    body: &Expression,
    match_target: Operand,
    dst: Operand,
    match_end_bb: Label,
    target_machine: &Target,
) -> CompileResult<()> {
    let match_case_bb = scope.label();
    let next_bb = scope.label();

    scope.scope(|scope| {
        let non_match_bb = scope.label();
        pattern_to_bc(
            bc_mod,
            scope,
            pattern,
            match_target,
            match_case_bb,
            match_end_bb,
            non_match_bb,
            target_machine,
        )?;
        match_case_body_to_bc(bc_mod, scope, body, dst, match_case_bb, match_end_bb, target_machine)?;
        scope.start_label(non_match_bb);
        scope.exit(bc_mod, target_machine, branch_instr(next_bb))
    })?;

    scope.start_label(next_bb);
    Ok(())
}

fn match_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    m: &MatchExpression,
    target: &Target,
) -> CompileResult<Operand> {
    let dst = scope.declare("$dst", None, m.typ.clone())?;
    let mt_expr = expr_to_bc(bc_mod, scope, &m.target, target)?;
    let mt = scope.make_var("$mt", mt_expr)?;
    let match_end_bb = scope.label();

    scope.scope(|scope| {
        for mc in &m.cases {
            match_case_to_bc(
                bc_mod,
                scope,
                &mc.pattern,
                &mc.to_execute,
                mt.safe_clone()?,
                dst.safe_clone()?,
                match_end_bb,
                target,
            )?;
        }

        scope.exit(bc_mod, target, branch_instr(match_end_bb))
    })?;

    scope.start_label(match_end_bb);
    dst.make_var()
}

fn delete_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    d: &DeleteExpression,
    target: &Target,
) -> CompileResult<()> {
    match d {
        DeleteExpression::Delete { inner, .. } => {
            let object = expr_to_bc(bc_mod, scope, inner, target)?;
            scope.add(Instruction::Delete { object });
        }
        DeleteExpression::BlockWithDestructor { block, .. } => {
            let end = scope.label();
            scope.scope(|scope| {
                block_to_bc(bc_mod, scope, block, target)?;
                scope.exit(bc_mod, target, branch_instr(end))?;
                Ok(())
            })?;
            scope.start_label(end);
        }
    }
    Ok(())
}

fn address_of_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    a: &AddressOfExpression,
    target: &Target,
) -> CompileResult<Operand> {
    let inner = expr_to_bc(bc_mod, scope, &a.inner, target)?;
    Ok(match inner {
        Operand::Member { obj, idx, typ } => Operand::MemberPtr {
            obj,
            idx,
            typ: ptr_type(typ),
        },
        Operand::VarPtr { .. } => inner,
        Operand::Var { .. } | _ => Operand::AddressOf { obj: Box::new(inner) },
    })
}

fn dereference_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    d: &DereferenceExpression,
    target: &Target,
) -> CompileResult<Operand> {
    let inner = expr_to_bc(bc_mod, scope, &d.inner, target)?;
    let inner_typ = inner.get_type();
    if let Type::Pointer(it) = &inner_typ {
        Ok(if it.pass_by_value() {
            Operand::Dereference {
                inner: inner.into(),
                typ: d.typ.clone(),
            }
        } else {
            inner // Don't dereference structs or other complex types
        })
    } else {
        code_gen_result(format!("Cannot dereference a {}", inner_typ))
    }
}

fn range_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    r: &Range,
    indexee: Option<Operand>,
    target: &Target,
) -> CompileResult<Operand> {
    let start = if let Some(start) = &r.start {
        expr_to_bc(bc_mod, scope, start, target)?
    } else {
        Operand::const_uint(0, target.int_size)
    };

    let end = if let Some(end) = &r.end {
        expr_to_bc(bc_mod, scope, end, target)?
    } else if let Some(indexee) = indexee {
        Operand::Property {
            operand: indexee.into(),
            property: ByteCodeProperty::Len,
            typ: target.native_uint_type.clone(),
        }
    } else {
        return code_gen_result("Missing indexee in range"); // Should have been caught in the typechecker
    };

    Ok(Operand::Range {
        start: start.into(),
        end: end.into(),
        typ: r.typ.clone(),
    })
}

fn index_op_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    iop: &IndexOperation,
    target: &Target,
) -> CompileResult<Operand> {
    let t = expr_to_bc(bc_mod, scope, &iop.target, target)?;
    Ok(match &iop.index_expr {
        Expression::Range(r) => {
            let t = scope.make_var("$indexee", t)?;
            let r = range_to_bc(bc_mod, scope, r, Some(t.safe_clone()?), target)?;
            Operand::slice(t, r, iop.typ.clone())
        }
        i => {
            let idx = expr_to_bc(bc_mod, scope, i, target)?;
            Operand::member(t, idx, iop.typ.clone())
        }
    })
}

pub fn expr_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    expr: &Expression,
    target: &Target,
) -> CompileResult<Operand> {
    match expr {
        Expression::Literal(l) => literal_to_bc(bc_mod, scope, l, target),
        Expression::UnaryOp(uop) => unary_op_to_bc(bc_mod, scope, uop, target),
        Expression::BinaryOp(bop) => binary_op_to_bc(bc_mod, scope, bop, target),
        Expression::Block(b) => block_to_bc(bc_mod, scope, b, target),
        Expression::Call(c) => call_to_bc(bc_mod, scope, c, target),
        Expression::NameRef(nr) => name_ref_to_bc(scope, nr),
        Expression::Match(m) => match_to_bc(bc_mod, scope, m, target),
        Expression::If(i) => if_to_bc(bc_mod, scope, i, target),
        Expression::Lambda(l) => lambda_to_bc(bc_mod, l, target),
        Expression::Bindings(b) => bindings_to_bc(bc_mod, scope, b, target),
        Expression::StructInitializer(si) => struct_initializer_to_bc(bc_mod, scope, si, target),
        Expression::MemberAccess(ma) => member_access_to_bc(bc_mod, scope, ma, target, FieldAccessMode::Value),
        Expression::New(n) => Ok(Operand::New {
            inner: Box::new(expr_to_bc(bc_mod, scope, &n.inner, target)?),
            typ: n.typ.clone(),
        }),
        Expression::Delete(d) => {
            delete_to_bc(bc_mod, scope, d, target)?;
            Ok(Operand::Void)
        }
        Expression::ArrayToSlice(ats) => array_to_slice_to_bc(bc_mod, scope, ats, target),
        Expression::AddressOf(a) => address_of_to_bc(bc_mod, scope, a, target),
        Expression::Dereference(d) => dereference_to_bc(bc_mod, scope, d, target),
        Expression::Assign(a) => {
            assign_to_bc(bc_mod, scope, a, target)?;
            Ok(Operand::Void)
        }
        Expression::While(w) => {
            while_to_bc(bc_mod, scope, w, target)?;
            Ok(Operand::Void)
        }
        Expression::For(f) => {
            for_to_bc(bc_mod, scope, f, target)?;
            Ok(Operand::Void)
        }
        Expression::Nil(n) => Ok(Operand::Optional {
            inner: None,
            typ: n.typ.clone(),
        }),
        Expression::OptionalToBool(inner) => {
            let opt = expr_to_bc(bc_mod, scope, inner, target)?;
            Ok(Operand::Binary {
                op: BinaryOperator::Equals,
                left: Box::new(Operand::const_uint(0, target.int_size)),
                right: Box::new(Operand::sti(opt, target.int_size)),
                typ: Type::Bool,
            })
        }
        Expression::ResultToBool(inner) => {
            let res = expr_to_bc(bc_mod, scope, inner, target)?;
            Ok(Operand::Binary {
                op: BinaryOperator::Equals,
                left: Box::new(Operand::const_uint(0, target.int_size)),
                right: Box::new(Operand::sti(res, target.int_size)),
                typ: Type::Bool,
            })
        }
        Expression::ToOptional(inner) => {
            let i = expr_to_bc(bc_mod, scope, &inner.inner, target)?;
            Ok(Operand::Optional {
                inner: Some(Box::new(i)),
                typ: inner.optional_type.clone(),
            })
        }
        Expression::ToOkResult(r) => {
            let i = expr_to_bc(bc_mod, scope, &r.inner, target)?;
            Ok(Operand::Result {
                ok: true,
                inner: Box::new(i),
                typ: r.result_type.clone(),
            })
        }
        Expression::ToErrResult(r) => {
            let i = expr_to_bc(bc_mod, scope, &r.inner, target)?;
            Ok(Operand::Result {
                ok: false,
                inner: Box::new(i),
                typ: r.result_type.clone(),
            })
        }
        Expression::Cast(c) => {
            let inner = expr_to_bc(bc_mod, scope, &c.inner, target)?;
            Ok(Operand::Cast {
                inner: Box::new(inner),
                typ: c.destination_type.clone(),
            })
        }
        Expression::CompilerCall(c) => compiler_call_to_bc(bc_mod, scope, c, target),
        Expression::IndexOperation(i) => index_op_to_bc(bc_mod, scope, i, target),
        Expression::Return(r) => return_to_bc(bc_mod, scope, r, target),
        Expression::Void => Ok(Operand::Void),
        Expression::Range(r) => range_to_bc(bc_mod, scope, r, None, target),
        Expression::Enclosed(e) => expr_to_bc(bc_mod, scope, e, target),
    }
}

fn func_to_bc(
    sig: &FunctionSignature,
    bc_mod: &mut ByteCodeModule,
    expression: &Expression,
    target: &Target,
) -> CompileResult<ByteCodeFunction> {
    let mut func = ByteCodeFunction::new(sig, false, bc_mod.stack.clone());
    {
        let mut s = bc_mod.stack.borrow_mut();
        s.push();
        for arg in &sig.args {
            s.add(
                &arg.name,
                Operand::Var {
                    name: arg.name.clone(),
                    typ: arg.typ.clone(),
                },
            );
        }
    }

    let ret = expr_to_bc(bc_mod, &mut func.toplevel_scope, expression, target)?;
    // Pop final scope before returning
    if !func.toplevel_scope.last_instruction_is_return() {
        if func.sig.rvo {
            let rvo_var = func.toplevel_scope.rvo_var();
            func.toplevel_scope.add(store_instr(rvo_var, ret));
            func.toplevel_scope
                .exit(bc_mod, target, ret_instr(Operand::Void))?;
        } else {
            func.toplevel_scope.exit(bc_mod, target, ret_instr(ret))?;
        }
    }

    bc_mod.stack.borrow_mut().pop();

    Ok(func)
}

fn add_imported_functions(bc_mod: &mut ByteCodeModule, import: &Import) {
    for symbol in import.symbols.values() {
        if let Some(s) = FunctionSignature::from_type(&symbol.name, &symbol.typ) {
            if bc_mod.functions.contains_key(&symbol.name) || symbol.typ.is_generic() {
                continue;
            }
            bc_mod
                .imported_functions
                .push(ByteCodeFunction::new(&s, true, bc_mod.stack.clone()));
        }
    }
}

pub fn compile_to_byte_code(pkg: &Package, target: &Target) -> CompileResult<ByteCodeModule> {
    let mut ll_mod = ByteCodeModule {
        name: pkg.name.clone(),
        functions: BTreeMap::new(),
        globals: BTreeMap::new(),
        imported_functions: Vec::new(),
        stack: Stack::new(),
        external_vars: BTreeMap::new(),
    };

    for md in pkg.modules.values() {
        for ext in md.externals.values() {
            match ext {
                External::Function { sig, .. } => {
                    ll_mod
                        .functions
                        .insert(sig.name.clone(), ByteCodeFunction::new(sig, true, ll_mod.stack.clone()));
                }

                External::Variable { name, typ, .. } => {
                    ll_mod.external_vars.insert(name.clone(), ext.clone());
                    ll_mod.stack.borrow_mut().add(
                        name,
                        Operand::Var {
                            name: name.clone(),
                            typ: typ.clone(),
                        },
                    );
                }
            }
        }
    }

    for md in pkg.modules.values() {
        for global in md.globals.values() {
            if let Some(cst) = expr_to_const(&global.init) {
                ll_mod.globals.insert(
                    global.name.clone(),
                    Global {
                        value: cst,
                        thread_local: global.thread_local,
                    },
                );
                ll_mod.stack.borrow_mut().add(
                    &global.name,
                    Operand::VarPtr {
                        name: global.name.clone(),
                        typ: global.typ.ptr_of(),
                    },
                )
            } else {
                return type_error_result(
                    &global.span,
                    format!("Global {} must be initialized with a constant expression", global.name),
                );
            }
        }

        for func in md.functions.values() {
            if !func.is_generic() {
                let new_func = func_to_bc(&func.sig, &mut ll_mod, &func.expression, target)?;
                ll_mod.functions.insert(func.sig.name.clone(), new_func);
            }
        }
    }

    for import in pkg.import_data.imports.values() {
        add_imported_functions(&mut ll_mod, import);
    }

    for library in &pkg.import_data.libraries {
        for import in &library.imports {
            add_imported_functions(&mut ll_mod, import);
        }
    }

    Ok(ll_mod)
}
