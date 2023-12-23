use std::collections::HashMap;

use super::consteval::expr_to_const;
use super::function::ByteCodeFunction;
use super::instruction::{branch_if_instr, branch_instr, ret_instr, store_instr, Instruction, Label};
use super::operand::{call_arg, ByteCodeProperty, CallArg, Operand};
use super::patterns::pattern_to_bc;
use super::scope::Scope;
use super::ByteCodeModule;
use crate::ast::*;
use crate::build::Package;
use crate::compileerror::{type_error_result, CompileResult};
use crate::target::Target;

fn literal_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, lit: &Literal, target: &Target) -> Operand {
    match lit {
        Literal::Int(_, v, is) => Operand::const_int(*v, *is),
        Literal::UInt(_, v, is) => Operand::const_uint(*v, *is),
        Literal::Bool(_, v) => Operand::const_bool(*v),
        Literal::Char(_, c) => Operand::const_char(*c),
        Literal::Float(_, f, fs) => Operand::const_float(f.parse().expect("ICE: Invalid float constant"), *fs),
        Literal::String(_, s) => Operand::const_string(s.clone()),
        Literal::Array(a) => {
            let members = a
                .elements
                .iter()
                .map(|e| expr_to_bc(bc_mod, scope, e, target))
                .collect();
            Operand::Array {
                members,
                typ: a.array_type.clone(),
            }
        }
        Literal::NullPtr(_, typ) => Operand::Null { typ: typ.clone() },
    }
}

fn name_ref_to_bc(nr: &NameRef) -> Operand {
    let add_name_ref = |nr: &NameRef| Operand::Var {
        name: nr.name.clone(),
        typ: nr.typ.clone(),
    };

    match &nr.typ {
        Type::Sum(st) => {
            if let Some(idx) = st.index_of(&nr.name) {
                Operand::Sum {
                    variant: idx,
                    inner: None,
                    typ: nr.typ.clone(),
                }
            } else {
                add_name_ref(nr)
            }
        }

        Type::Enum(et) => {
            if let Some(idx) = et.index_of(&nr.name) {
                // enums are integers
                Operand::Enum {
                    variant: idx,
                    typ: nr.typ.clone(),
                }
            } else {
                add_name_ref(nr)
            }
        }

        Type::Func(_) => Operand::Func {
            name: nr.name.clone(),
            typ: nr.typ.clone(),
        },

        _ => add_name_ref(nr),
    }
}

fn call_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, call: &Call, target: &Target) -> Operand {
    let Type::Func(ft) = call.callee_type() else {
        panic!("ICE: Callee must have a function type");
    };

    let callee = name_ref_to_bc(&call.callee);
    let mut args: Vec<CallArg> = call
        .args
        .iter()
        .zip(ft.args.iter())
        .map(|(a, d)| call_arg(expr_to_bc(bc_mod, scope, a, target), d.mutable, d.typ.clone()))
        .collect();

    let rvo = ft.return_type.should_do_rvo();
    if rvo {
        // rvo, so alloc the destination and pass it to the function
        let rvo_arg = scope.declare(RVO_RETURN_ARG, None, ft.return_type.clone());
        args.push(call_arg(rvo_arg, true, ptr_type(ft.return_type.clone())));
    }

    Operand::Call {
        callee: Box::new(callee),
        args,
        typ: if rvo {
            ptr_type(call.return_type.clone())
        } else {
            call.return_type.clone()
        },
        rvo,
    }
}

fn block_expr_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    e: &Expression,
    target: &Target,
    is_last: bool,
) -> Operand {
    let op = expr_to_bc(bc_mod, scope, e, target);
    if is_last {
        return op;
    }
    if op.is_call() {
        if matches!(op, Operand::Void) {
            println!("blaat");
            dbg!(&op);
        }
        scope.add(Instruction::Exec { operand: op });
    }

    Operand::Void
}

fn block_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, b: &Block, target: &Target) -> Operand {
    for e in &b.deferred_expressions {
        scope.add_unwind_call(e.clone());
    }

    for df in &b.drop_flags {
        scope.declare(&df.0, Some(Operand::const_bool(false)), Type::Bool);
    }

    let mut ret = Operand::Void;
    for (idx, e) in b.expressions.iter().enumerate() {
        if idx == b.expressions.len() - 1 {
            let op = block_expr_to_bc(bc_mod, scope, e, target, true);
            if b.typ != Type::Void {
                ret = op;
            } else if op.is_call() {
                scope.add(Instruction::Exec { operand: op });
            }
        } else {
            block_expr_to_bc(bc_mod, scope, e, target, false);
        }
    }

    ret
}

fn add_binding(bc_mod: &mut ByteCodeModule, scope: &mut Scope, b: &Binding, target: &Target) {
    match &b.binding_type {
        BindingType::Name(name) => {
            let init = expr_to_bc(bc_mod, scope, &b.init, target);
            scope.declare(name, Some(init), b.typ.clone());
        }
        BindingType::Struct(sp) => {
            // Declare init so it is only executed once
            let init = expr_to_bc(bc_mod, scope, &b.init, target);
            let init_typ = b.init.get_type();
            let init_var = scope.declare("$init", Some(init), init_typ);
            for (idx, b) in sp.bindings.iter().enumerate() {
                scope.alias(
                    &b.name,
                    Operand::member_ptr(
                        init_var.safe_clone(),
                        Operand::const_uint(idx as u64, target.int_size),
                        ptr_type(b.typ.clone()),
                    ),
                );
            }
        }
    }
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
) -> Operand {
    let obj = expr_to_bc(bc_mod, scope, &ma.left, target);
    match &ma.right {
        MemberAccessType::Call(call) => {
            let callee = name_ref_to_bc(&call.callee);
            let Type::Func(ft) = call.callee_type() else {
                panic!("ICE: Callee must have a function type");
            };
            let mut args: Vec<CallArg> = call
                .args
                .iter()
                .zip(ft.args.iter())
                .map(|(e, d)| call_arg(expr_to_bc(bc_mod, scope, e, target), d.mutable, d.typ.clone()))
                .collect();
            if call.return_type.should_do_rvo() {
                // rvo, so alloc the destination and pass it to the function
                let rvo_arg = scope.declare(RVO_RETURN_ARG, None, call.return_type.clone());
                args.push(call_arg(rvo_arg, true, ptr_type(call.return_type.clone())));
            }

            Operand::Call {
                callee: Box::new(callee),
                args,
                typ: ma.typ.clone(),
                rvo: call.return_type.should_do_rvo(),
            }
        }
        MemberAccessType::Name(field) => {
            let idx = Operand::const_uint(field.index as u64, target.int_size);
            if field.typ.pass_by_value() && field_access_mode != FieldAccessMode::Pointer {
                Operand::member(obj, idx, ma.typ.clone())
            } else {
                Operand::member_ptr(obj, idx, ptr_type(ma.typ.clone()))
            }
        }
        MemberAccessType::Property(prop) => Operand::Property {
            operand: Box::new(obj),
            property: match prop {
                Property::Len => ByteCodeProperty::Len,
                Property::Data => ByteCodeProperty::Data,
            },
            typ: ma.typ.clone(),
        },
    }
}

fn assign_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, assign: &Assign, target: &Target) {
    // During type checking, other assigns, will be converted in a regular assign
    assert!(assign.operator == AssignOperator::Assign);

    let value = expr_to_bc(bc_mod, scope, &assign.right, target);
    match &assign.left {
        AssignTarget::Var(nr) => {
            scope.add(store_instr(
                Operand::Var {
                    name: nr.name.clone(),
                    typ: nr.typ.clone(),
                },
                value,
            ));
        }

        AssignTarget::MemberAccess(ma) => {
            let dst = member_access_to_bc(bc_mod, scope, ma, target, FieldAccessMode::Pointer);
            scope.add(store_instr(dst, value));
        }

        AssignTarget::Dereference(d) => {
            let inner = expr_to_bc(bc_mod, scope, &d.inner, target);
            scope.add(store_instr(
                /*Operand::Dereference {
                    inner: Box::new(inner),
                    typ: d.typ.clone(),
                },*/
                inner, value,
            ));
        }

        AssignTarget::IndexOperation(iop) => {
            let tgt = expr_to_bc(bc_mod, scope, &iop.target, target);

            match &iop.index_expr {
                IndexMode::Index(i) => {
                    let idx = expr_to_bc(bc_mod, scope, i, target);
                    scope.add(store_instr(
                        Operand::member_ptr(tgt, idx, ptr_type(iop.typ.clone())),
                        value,
                    ));
                }
                IndexMode::Range(_) => {
                    // Caught in the type checker
                    panic!("ICE: assigning to a ranged index target is not allowed");
                }
            }
        }
    }
}

fn unary_op_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, uop: &UnaryOp, target: &Target) -> Operand {
    let i = expr_to_bc(bc_mod, scope, &uop.expression, target);
    Operand::Unary {
        inner: Box::new(i),
        op: uop.operator,
        typ: uop.typ.clone(),
    }
}

fn optional_or_to_bc(scope: &mut Scope, l: Operand, r: Operand, target: &Target) -> Operand {
    let dst_type = r.get_type();
    let dst = scope.declare("$dst", None, dst_type.clone());
    let l_typ = l.get_type();
    let l_var = scope.declare("$l", Some(l), l_typ);

    let l_sti = Operand::sti(l_var.safe_clone(), target.int_size);
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
    scope.add(store_instr(dst.safe_clone(), r));
    scope.add(branch_instr(end_bb));

    scope.start_label(store_l_bb);
    scope.add(store_instr(
        dst.safe_clone(),
        Operand::member(l_var, Operand::const_uint(0, target.int_size), dst_type),
    ));
    scope.add(branch_instr(end_bb));
    scope.start_label(end_bb);
    dst
}

fn result_or_to_bc(scope: &mut Scope, l: Operand, r: Operand, target: &Target) -> Operand {
    let dst_type = r.get_type();
    let dst = scope.declare("$dst", None, dst_type.clone());
    let l_typ = l.get_type();
    let l_var = scope.declare("$l", Some(l), l_typ);
    let sti = Operand::sti(l_var.safe_clone(), target.int_size);

    let ok_branch = scope.label();
    let error_branch = scope.label();
    let end_bb = scope.label();

    let l_is_ok = Operand::binary(
        BinaryOperator::Equals,
        sti,
        Operand::const_int(0, target.int_size),
        Type::Bool,
    );
    scope.add(branch_if_instr(l_is_ok, ok_branch, error_branch));
    scope.start_label(ok_branch);
    scope.add(store_instr(
        dst.safe_clone(),
        Operand::member(l_var.safe_clone(), Operand::const_uint(0, target.int_size), dst_type),
    ));
    scope.add(branch_instr(end_bb));

    scope.start_label(error_branch);
    scope.add(store_instr(dst.safe_clone(), r));
    scope.add(branch_instr(end_bb));

    scope.start_label(end_bb);
    dst
}

fn binary_op_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, bop: &BinaryOp, target: &Target) -> Operand {
    let left = expr_to_bc(bc_mod, scope, &bop.left, target);
    let right = expr_to_bc(bc_mod, scope, &bop.right, target);
    match (&left.get_type(), &bop.operator) {
        (Type::Optional(_), BinaryOperator::Or) => optional_or_to_bc(scope, left, right, target),
        (Type::Result(_), BinaryOperator::Or) => result_or_to_bc(scope, left, right, target),
        _ => Operand::Binary {
            op: bop.operator,
            left: Box::new(left),
            right: Box::new(right),
            typ: bop.typ.clone(),
        },
    }
}

fn struct_initializer_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    si: &StructInitializer,
    target: &Target,
) -> Operand {
    match &si.typ {
        Type::Struct(_) => {
            let members = si
                .member_initializers
                .iter()
                .map(|e| expr_to_bc(bc_mod, scope, e, target))
                .collect();
            Operand::Struct {
                members,
                typ: si.typ.clone(),
            }
        }
        Type::Sum(st) => {
            let idx = st
                .index_of(&si.struct_name)
                .expect("ICE: Cannot determine sum type case");
            let members = si
                .member_initializers
                .iter()
                .map(|e| expr_to_bc(bc_mod, scope, e, target))
                .collect();
            Operand::Sum {
                variant: idx,
                inner: Some(Box::new(Operand::Struct {
                    members,
                    typ: st.cases[idx]
                        .typ
                        .clone()
                        .expect("ICE: Sum type case has no data"),
                })),
                typ: si.typ.clone(),
            }
        }
        _ => panic!("ICE: expecting sum type or struct type in a StructInitializer"),
    }
}

fn while_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, w: &WhileLoop, target: &Target) {
    let cond_bb = scope.label();
    let body_bb = scope.label();
    let post_while_bb = scope.label();

    scope.add(branch_instr(cond_bb));
    scope.start_label(cond_bb);

    let cond = expr_to_bc(bc_mod, scope, &w.cond, target);
    scope.add(branch_if_instr(cond, body_bb, post_while_bb));
    scope.scope(|scope| {
        scope.start_label(body_bb);
        expr_to_bc(bc_mod, scope, &w.body, target);
        let keep_looping = !scope.last_instruction_is_return();
        if keep_looping {
            scope.exit(bc_mod, target, branch_instr(cond_bb));
        }
    });
    scope.start_label(post_while_bb);
}

fn for_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, f: &ForLoop, target: &Target) {
    let end_bb = scope.label();
    scope.scope(|scope| {
        let iterable = expr_to_bc(bc_mod, scope, &f.iterable, target);
        let iterable = scope.declare("$iterable", Some(iterable), f.iterable.get_type());
        let index = scope.declare(
            "$index",
            Some(Operand::const_uint(0, target.int_size)),
            Type::UInt(target.int_size),
        );
        let len = if let Type::Array(at) = iterable.get_type() {
            Operand::const_uint(at.len as u64, target.int_size)
        } else {
            Operand::Property {
                operand: Box::new(iterable.safe_clone()),
                property: ByteCodeProperty::Len,
                typ: target.native_uint_type.clone(),
            }
        };

        let loop_var = scope.declare(&f.loop_variable, None, f.loop_variable_type.clone());

        let cond_bb = scope.label();
        let body_bb = scope.label();
        let post_for_bb = scope.label();
        scope.add(branch_instr(cond_bb));
        scope.start_label(cond_bb);

        let cmp = Operand::binary(BinaryOperator::LessThan, index.safe_clone(), len, Type::Bool);
        scope.add(branch_if_instr(cmp, body_bb, post_for_bb));

        scope.scope(|scope| {
            scope.start_label(body_bb);
            let element_type = iterable
                .get_type()
                .get_element_type()
                .expect("ICE: element type expected");
            let lv = Operand::member(iterable, index.safe_clone(), element_type);
            scope.add(store_instr(loop_var, lv));

            expr_to_bc(bc_mod, scope, &f.body, target);
            let keep_looping = !scope.last_instruction_is_return();
            if keep_looping {
                scope.add(store_instr(
                    index.safe_clone(),
                    Operand::binary(
                        BinaryOperator::Add,
                        index.safe_clone(),
                        Operand::const_uint(1, target.int_size),
                        Type::UInt(target.int_size),
                    ),
                ));
                scope.exit(bc_mod, target, branch_instr(cond_bb));
            }
        });

        scope.start_label(post_for_bb);
        scope.exit(bc_mod, target, branch_instr(end_bb));
    });
    scope.start_label(end_bb);
}

fn if_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, if_expr: &IfExpression, target: &Target) -> Operand {
    let dst = scope.declare("$dst", None, if_expr.typ.clone());
    let true_bb = scope.label();
    let end_bb = scope.label();

    let cond = expr_to_bc(bc_mod, scope, &if_expr.condition, target);

    if let Some(on_false) = &if_expr.on_false {
        let false_bb = scope.label();
        scope.add(branch_if_instr(cond, true_bb, false_bb));
        scope.scope(|scope| {
            scope.start_label(false_bb);
            let val = expr_to_bc(bc_mod, scope, on_false, target);
            let add_branch = !scope.last_instruction_is_return();
            if add_branch {
                scope.add(store_instr(dst.safe_clone(), val));
                scope.exit(bc_mod, target, branch_instr(end_bb));
            }
        });
    } else {
        scope.add(branch_if_instr(cond, true_bb, end_bb));
    }

    scope.scope(|scope| {
        scope.start_label(true_bb);
        let val = expr_to_bc(bc_mod, scope, &if_expr.on_true, target);
        let add_branch = !scope.last_instruction_is_return();
        if add_branch {
            scope.add(store_instr(dst.safe_clone(), val));
            scope.exit(bc_mod, target, branch_instr(end_bb));
        }
    });

    scope.start_label(end_bb);
    dst
}

fn lambda_to_bc(bc_mod: &mut ByteCodeModule, l: &Lambda, target: &Target) -> Operand {
    let lambda = func_to_bc(&l.sig, bc_mod, &l.expr, target);
    bc_mod.functions.insert(l.sig.name.clone(), lambda);
    Operand::Func {
        name: l.sig.name.clone(),
        typ: l.sig.typ.clone(),
    }
}

fn return_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, r: &Return, target: &Target) -> Operand {
    let ret = expr_to_bc(bc_mod, scope, &r.expression, target);
    if scope.rvo() {
        scope.add(store_instr(scope.rvo_var(), ret));
        scope.exit(bc_mod, target, ret_instr(Operand::Void));
    } else {
        scope.exit(bc_mod, target, ret_instr(ret));
    }
    Operand::Void
}

fn compiler_call_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, c: &CompilerCall, target: &Target) -> Operand {
    match c {
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
                return Operand::Void;
            };

            let end = scope.label();
            scope.scope(|scope| {
                let obj = expr_to_bc(bc_mod, scope, obj, target);
                let obj = scope.make_var("$tbd", obj);
                if let Some(df) = drop_flag {
                    let dropped = Operand::Var {
                        name: df.0.clone(),
                        typ: Type::Bool,
                    };
                    let not_dropped = Operand::Unary {
                        op: UnaryOperator::Not,
                        inner: Box::new(dropped.safe_clone()),
                        typ: Type::Bool,
                    };
                    let drop_obj = scope.label();
                    scope.add(branch_if_instr(not_dropped, drop_obj, end));
                    scope.start_label(drop_obj);
                    scope.add(store_instr(dropped, Operand::const_bool(true)));
                }

                let call = Operand::Call {
                    callee: Box::new(name_ref_to_bc(&ds.callee)),
                    args: vec![call_arg(obj.safe_clone(), true, obj.get_type())],
                    typ: Type::Void,
                    rvo: false,
                };
                scope.add(Instruction::Exec { operand: call });
                scope.exit(bc_mod, target, branch_instr(end));
            });

            scope.start_label(end);
            Operand::Void
        }
    }
}

fn array_to_slice_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    ats: &ArrayToSlice,
    target: &Target,
) -> Operand {
    let array = expr_to_bc(bc_mod, scope, &ats.inner, target);
    let array = scope.make_var("$array", array);
    let element_type = array
        .get_type()
        .get_element_type()
        .expect("ICE: Array must have element type");
    let range = Operand::Range {
        start: Box::new(Operand::const_uint(0, target.int_size)),
        end: Box::new(Operand::Property {
            operand: Box::new(array.safe_clone()),
            property: ByteCodeProperty::Len,
            typ: target.native_int_type.clone(),
        }),
        typ: Type::Range(target.int_size),
    };
    Operand::slice(array, range, slice_type(element_type.clone()))
}

fn bindings_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, b: &BindingList, target: &Target) -> Operand {
    for b in &b.bindings {
        add_binding(bc_mod, scope, b, target);
    }
    Operand::Void
}

fn match_case_body_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    body: &Expression,
    dst: Operand,
    match_case_bb: Label,
    match_end_bb: Label,
    target: &Target,
) {
    scope.start_label(match_case_bb);
    let val = expr_to_bc(bc_mod, scope, body, target);
    let add_branch = !scope.last_instruction_is_return();
    if add_branch {
        scope.add(store_instr(dst, val));
        scope.exit(bc_mod, target, branch_instr(match_end_bb));
    }
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
) {
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
        );
        match_case_body_to_bc(bc_mod, scope, body, dst, match_case_bb, match_end_bb, target_machine);
        scope.start_label(non_match_bb);
        scope.exit(bc_mod, target_machine, branch_instr(next_bb));
    });

    scope.start_label(next_bb);
}

fn match_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, m: &MatchExpression, target: &Target) -> Operand {
    let dst = scope.declare("$dst", None, m.typ.clone());
    let mt_expr = expr_to_bc(bc_mod, scope, &m.target, target);
    let mt = scope.make_var("$mt", mt_expr);
    let match_end_bb = scope.label();

    scope.scope(|scope| {
        for mc in &m.cases {
            match_case_to_bc(
                bc_mod,
                scope,
                &mc.pattern,
                &mc.to_execute,
                mt.safe_clone(),
                dst.safe_clone(),
                match_end_bb,
                target,
            );
        }

        scope.exit(bc_mod, target, branch_instr(match_end_bb));
    });

    scope.start_label(match_end_bb);
    dst.safe_clone()
}

fn delete_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, d: &DeleteExpression, target: &Target) {
    let object = expr_to_bc(bc_mod, scope, &d.inner, target);
    scope.add(Instruction::Delete { object });
}

fn address_of_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    a: &AddressOfExpression,
    target: &Target,
) -> Operand {
    let inner = expr_to_bc(bc_mod, scope, &a.inner, target);
    if let Operand::Member { obj, idx, typ } = inner {
        Operand::MemberPtr {
            obj,
            idx,
            typ: ptr_type(typ),
        }
    } else {
        Operand::AddressOf { obj: Box::new(inner) }
    }
}

fn dereference_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    d: &DereferenceExpression,
    target: &Target,
) -> Operand {
    let inner = expr_to_bc(bc_mod, scope, &d.inner, target);
    let inner_typ = inner.get_type();
    if let Type::Pointer(it) = &inner_typ {
        if it.pass_by_value() {
            Operand::Dereference {
                inner: inner.into(),
                typ: d.typ.clone(),
            }
        } else {
            inner // Don't dereference structs or other complex types
        }
    } else {
        panic!("ICE: cannot dereference a {}", inner_typ);
    }
}

fn range_to_bc(
    bc_mod: &mut ByteCodeModule,
    scope: &mut Scope,
    r: &Range,
    indexee: Operand,
    target: &Target,
) -> Operand {
    let start = if let Some(start) = &r.start {
        expr_to_bc(bc_mod, scope, start, target)
    } else {
        Operand::const_uint(0, target.int_size)
    };

    let end = if let Some(end) = &r.end {
        expr_to_bc(bc_mod, scope, end, target)
    } else {
        Operand::Property {
            operand: indexee.into(),
            property: ByteCodeProperty::Len,
            typ: target.native_uint_type.clone(),
        }
    };

    Operand::Range {
        start: start.into(),
        end: end.into(),
        typ: r.typ.clone(),
    }
}

fn index_op_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, iop: &IndexOperation, target: &Target) -> Operand {
    let t = expr_to_bc(bc_mod, scope, &iop.target, target);
    match &iop.index_expr {
        IndexMode::Index(i) => {
            let idx = expr_to_bc(bc_mod, scope, i, target);
            Operand::member(t, idx, iop.typ.clone())
        }
        IndexMode::Range(r) => {
            let t = scope.make_var("$indexee", t);
            let r = range_to_bc(bc_mod, scope, r, t.safe_clone(), target);
            Operand::slice(t, r, iop.typ.clone())
        }
    }
}

pub fn expr_to_bc(bc_mod: &mut ByteCodeModule, scope: &mut Scope, expr: &Expression, target: &Target) -> Operand {
    match expr {
        Expression::Literal(l) => literal_to_bc(bc_mod, scope, l, target),
        Expression::UnaryOp(uop) => unary_op_to_bc(bc_mod, scope, uop, target),
        Expression::BinaryOp(bop) => binary_op_to_bc(bc_mod, scope, bop, target),
        Expression::Block(b) => block_to_bc(bc_mod, scope, b, target),
        Expression::Call(c) => call_to_bc(bc_mod, scope, c, target),
        Expression::NameRef(nr) => name_ref_to_bc(nr),
        Expression::Match(m) => match_to_bc(bc_mod, scope, m, target),
        Expression::If(i) => if_to_bc(bc_mod, scope, i, target),
        Expression::Lambda(l) => lambda_to_bc(bc_mod, l, target),
        Expression::Bindings(b) => bindings_to_bc(bc_mod, scope, b, target),
        Expression::StructInitializer(si) => struct_initializer_to_bc(bc_mod, scope, si, target),
        Expression::MemberAccess(ma) => member_access_to_bc(bc_mod, scope, ma, target, FieldAccessMode::Value),
        Expression::New(n) => Operand::New {
            inner: Box::new(expr_to_bc(bc_mod, scope, &n.inner, target)),
            typ: n.typ.clone(),
        },
        Expression::Delete(d) => {
            delete_to_bc(bc_mod, scope, d, target);
            Operand::Void
        }
        Expression::ArrayToSlice(ats) => array_to_slice_to_bc(bc_mod, scope, ats, target),
        Expression::AddressOf(a) => address_of_to_bc(bc_mod, scope, a, target),
        Expression::Dereference(d) => dereference_to_bc(bc_mod, scope, d, target),
        Expression::Assign(a) => {
            assign_to_bc(bc_mod, scope, a, target);
            Operand::Void
        }
        Expression::While(w) => {
            while_to_bc(bc_mod, scope, w, target);
            Operand::Void
        }
        Expression::For(f) => {
            for_to_bc(bc_mod, scope, f, target);
            Operand::Void
        }
        Expression::Nil(n) => Operand::Optional {
            inner: None,
            typ: n.typ.clone(),
        },
        Expression::OptionalToBool(inner) => {
            let opt = expr_to_bc(bc_mod, scope, inner, target);
            Operand::Binary {
                op: BinaryOperator::Equals,
                left: Box::new(Operand::const_uint(0, target.int_size)),
                right: Box::new(Operand::sti(opt, target.int_size)),
                typ: Type::Bool,
            }
        }
        Expression::ResultToBool(inner) => {
            let res = expr_to_bc(bc_mod, scope, inner, target);
            Operand::Binary {
                op: BinaryOperator::Equals,
                left: Box::new(Operand::const_uint(0, target.int_size)),
                right: Box::new(Operand::sti(res, target.int_size)),
                typ: Type::Bool,
            }
        }
        Expression::ToOptional(inner) => {
            let i = expr_to_bc(bc_mod, scope, &inner.inner, target);
            Operand::Optional {
                inner: Some(Box::new(i)),
                typ: inner.optional_type.clone(),
            }
        }
        Expression::ToOkResult(r) => {
            let i = expr_to_bc(bc_mod, scope, &r.inner, target);
            Operand::Result {
                ok: true,
                inner: Box::new(i),
                typ: r.result_type.clone(),
            }
        }
        Expression::ToErrResult(r) => {
            let i = expr_to_bc(bc_mod, scope, &r.inner, target);
            Operand::Result {
                ok: false,
                inner: Box::new(i),
                typ: r.result_type.clone(),
            }
        }
        Expression::Cast(c) => {
            let inner = expr_to_bc(bc_mod, scope, &c.inner, target);
            Operand::Cast {
                inner: Box::new(inner),
                typ: c.destination_type.clone(),
            }
        }
        Expression::CompilerCall(c) => compiler_call_to_bc(bc_mod, scope, c, target),
        Expression::IndexOperation(i) => index_op_to_bc(bc_mod, scope, i, target),
        Expression::Return(r) => return_to_bc(bc_mod, scope, r, target),
        Expression::Void => Operand::Void,
    }
}

fn func_to_bc(
    sig: &FunctionSignature,
    bc_mod: &mut ByteCodeModule,
    expression: &Expression,
    target: &Target,
) -> ByteCodeFunction {
    let mut llfunc = ByteCodeFunction::new(sig, false);
    let ret = expr_to_bc(bc_mod, &mut llfunc.toplevel_scope, expression, target);
    // Pop final scope before returning
    if !llfunc.toplevel_scope.last_instruction_is_return() {
        if llfunc.sig.rvo {
            let rvo_var = llfunc.toplevel_scope.rvo_var();
            llfunc.toplevel_scope.add(store_instr(rvo_var, ret));
            llfunc
                .toplevel_scope
                .exit(bc_mod, target, ret_instr(Operand::Void));
        } else {
            llfunc.toplevel_scope.exit(bc_mod, target, ret_instr(ret));
        }
    }

    llfunc
}

fn add_imported_functions(bc_mod: &mut ByteCodeModule, import: &Import) {
    for symbol in import.symbols.values() {
        if let Some(s) = FunctionSignature::from_type(&symbol.name, &symbol.typ) {
            if bc_mod.functions.contains_key(&symbol.name) || symbol.typ.is_generic() {
                continue;
            }
            bc_mod
                .imported_functions
                .push(ByteCodeFunction::new(&s, true));
        }
    }
}

pub fn compile_to_byte_code(pkg: &Package, target: &Target) -> CompileResult<ByteCodeModule> {
    let mut ll_mod = ByteCodeModule {
        name: pkg.name.clone(),
        functions: HashMap::new(),
        globals: HashMap::new(),
        imported_functions: Vec::new(),
    };

    for md in pkg.modules.values() {
        for func in md.externals.values() {
            ll_mod
                .functions
                .insert(func.sig.name.clone(), ByteCodeFunction::new(&func.sig, true));
        }

        for global in md.globals.values() {
            if let Some(cst) = expr_to_const(&global.init) {
                ll_mod.globals.insert(global.name.clone(), cst);
            } else {
                return type_error_result(
                    &global.span,
                    format!("Global {} must be initialized with a constant expression", global.name),
                );
            }
        }

        for func in md.functions.values() {
            if !func.is_generic() {
                let new_func = func_to_bc(&func.sig, &mut ll_mod, &func.expression, target);
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