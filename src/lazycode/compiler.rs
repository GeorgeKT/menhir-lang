use std::collections::HashMap;

use super::consteval::expr_to_const;
use super::function::ByteCodeFunction;
use super::instruction::{branch_if_instr, branch_instr, ret_instr, store_instr, Instruction};
use super::operand::{call_arg, ByteCodeProperty, CallArg, Operand};
use super::patterns::pattern_to_bc;
use super::{BasicBlockRef, ByteCodeModule};
use crate::ast::{
    ptr_type, ArrayToSlice, Assign, AssignOperator, AssignTarget, BinaryOp, BinaryOperator, Binding, BindingList,
    BindingType, Block, Call, CompilerCall, Expression, ForLoop, FunctionSignature, IfExpression, Import, Lambda,
    Literal, MatchExpression, MemberAccess, MemberAccessType, NameRef, Pattern, Property, Return, StructInitializer,
    SumTypeCaseIndexOf, Type, UnaryOp, WhileLoop, RVO_RETURN_ARG,
};
use crate::build::Package;
use crate::compileerror::{type_error_result, CompileResult};
use crate::target::Target;

fn literal_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, lit: &Literal, target: &Target) -> Operand {
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
                .map(|e| expr_to_bc(bc_mod, func, e, target))
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

fn call_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, call: &Call, target: &Target) -> Operand {
    let Type::Func(ft) = call.callee_type() else {
        panic!("ICE: Callee must have a function type");
    };

    let callee = name_ref_to_bc(&call.callee);
    let mut args: Vec<CallArg> = call
        .args
        .iter()
        .zip(ft.args.iter())
        .map(|(a, d)| call_arg(expr_to_bc(bc_mod, func, a, target), d.mutable))
        .collect();

    let rvo = ft.return_type.should_do_rvo();
    if rvo {
        // rvo, so alloc the destination and pass it to the function
        let rvo_arg = func.declare(RVO_RETURN_ARG, None, ft.return_type.clone());
        args.push(call_arg(rvo_arg, true));
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

fn block_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Block, target: &Target) -> Operand {
    if !b.deferred_expressions.is_empty() {
        func.add_unwind_calls(&b.deferred_expressions);
    }

    let mut ret = Operand::Void;
    for (idx, e) in b.expressions.iter().enumerate() {
        if idx == b.expressions.len() - 1 {
            let op = expr_to_bc(bc_mod, func, e, target);
            if b.typ != Type::Void {
                ret = op;
            }
        } else {
            expr_to_bc(bc_mod, func, e, target);
        }
    }

    if !b.deferred_expressions.is_empty() {
        let exit_block = func.create_basic_block();
        func.add(branch_instr(exit_block));
        func.set_current_bb(exit_block);
        for e in &b.deferred_expressions {
            expr_to_bc(bc_mod, func, e, target);
        }
    }

    ret
}

fn add_binding(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Binding, target: &Target) {
    match &b.binding_type {
        BindingType::Name(name) => {
            let init = expr_to_bc(bc_mod, func, &b.init, target);
            func.declare(name, Some(init), b.typ.clone());
        }
        BindingType::Struct(sp) => {
            // Declare init so it is only executed once
            let init = expr_to_bc(bc_mod, func, &b.init, target);
            let init_var = func.declare("$init", Some(init), b.init.get_type(target.int_size));
            for (idx, b) in sp.bindings.iter().enumerate() {
                func.declare(
                    &b.name,
                    Some(Operand::member(
                        init_var.clone(),
                        Operand::const_uint(idx as u64, target.int_size),
                        ptr_type(b.typ.clone()),
                    )),
                    b.typ.clone(),
                );
            }
        }
    }
}

fn member_access_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    ma: &MemberAccess,
    target: &Target,
) -> Operand {
    let obj = expr_to_bc(bc_mod, func, &ma.left, target);
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
                .map(|(e, d)| call_arg(expr_to_bc(bc_mod, func, e, target), d.mutable))
                .collect();
            if call.return_type.should_do_rvo() {
                // rvo, so alloc the destination and pass it to the function
                let rvo_arg = func.declare(RVO_RETURN_ARG, None, call.return_type.clone());
                args.push(call_arg(rvo_arg, true));
            }

            Operand::Call {
                callee: Box::new(callee),
                args,
                typ: ma.typ.clone(),
                rvo: !call.return_type.pass_by_value(),
            }
        }
        MemberAccessType::Name(field) => Operand::member(
            obj,
            Operand::const_uint(field.index as u64, target.int_size),
            ptr_type(ma.typ.clone()),
        ),
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

fn assign_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, assign: &Assign, target: &Target) {
    // During type checking, other assigns, will be converted in a regular assign
    assert!(assign.operator == AssignOperator::Assign);

    let value = expr_to_bc(bc_mod, func, &assign.right, target);
    match &assign.left {
        AssignTarget::Var(nr) => {
            func.add(store_instr(
                Operand::Var {
                    name: nr.name.clone(),
                    typ: nr.typ.clone(),
                },
                value,
            ));
        }

        AssignTarget::MemberAccess(ma) => {
            let dst = member_access_to_bc(bc_mod, func, ma, target);
            func.add(store_instr(dst, value));
        }

        AssignTarget::Dereference(d) => {
            let inner = expr_to_bc(bc_mod, func, &d.inner, target);
            func.add(store_instr(
                Operand::Dereference {
                    inner: Box::new(inner),
                    typ: d.typ.clone(),
                },
                value,
            ));
        }

        AssignTarget::IndexOperation(iop) => {
            let tgt = expr_to_bc(bc_mod, func, &iop.target, target);
            let idx = expr_to_bc(bc_mod, func, &iop.index_expr, target);
            func.add(store_instr(Operand::member(tgt, idx, ptr_type(iop.typ.clone())), value));
        }
    }
}

fn unary_op_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, uop: &UnaryOp, target: &Target) -> Operand {
    let i = expr_to_bc(bc_mod, func, &uop.expression, target);
    Operand::Unary {
        inner: Box::new(i),
        op: uop.operator,
        typ: uop.typ.clone(),
    }
}

fn optional_or_to_bc(func: &mut ByteCodeFunction, l: Operand, r: Operand, target: &Target) -> Operand {
    let dst_type = r.get_type(target.int_size);
    let dst = func.declare("$dst", None, dst_type.clone());
    let l_sti = Operand::sti(l.clone(), target.int_size);
    let l_is_valid = Operand::binary(
        BinaryOperator::Equals,
        l_sti,
        Operand::const_uint(0, target.int_size),
        Type::Bool,
    );
    let store_r_bb = func.create_basic_block();
    let store_l_bb = func.create_basic_block();
    let end_bb = func.create_basic_block();

    func.add(branch_if_instr(l_is_valid, store_l_bb, store_r_bb));

    func.set_current_bb(store_r_bb);
    func.add(store_instr(dst.clone(), r));
    func.add(branch_instr(end_bb));

    func.set_current_bb(store_l_bb);
    func.add(store_instr(
        dst.clone(),
        Operand::member(l, Operand::const_uint(0, target.int_size), dst_type),
    ));
    func.add(branch_instr(end_bb));
    func.set_current_bb(end_bb);
    dst
}

fn result_or_to_bc(func: &mut ByteCodeFunction, l: Operand, r: Operand, target: &Target) -> Operand {
    let dst_type = r.get_type(target.int_size);
    let dst = func.declare("$dst", None, dst_type.clone());

    let sti = Operand::sti(l.clone(), target.int_size);

    let ok_branch = func.create_basic_block();
    let error_branch = func.create_basic_block();
    let end_bb = func.create_basic_block();

    let l_is_ok = Operand::binary(
        BinaryOperator::Equals,
        sti,
        Operand::const_int(0, target.int_size),
        Type::Bool,
    );
    func.add(branch_if_instr(l_is_ok, ok_branch, error_branch));
    func.set_current_bb(ok_branch);
    func.add(store_instr(
        dst.clone(),
        Operand::member(l, Operand::const_uint(0, target.int_size), dst_type),
    ));
    func.add(branch_instr(end_bb));

    func.set_current_bb(error_branch);
    func.add(store_instr(dst.clone(), r));
    func.add(branch_instr(end_bb));

    func.set_current_bb(end_bb);
    dst
}

fn binary_op_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    bop: &BinaryOp,
    target: &Target,
) -> Operand {
    let left = expr_to_bc(bc_mod, func, &bop.left, target);
    let right = expr_to_bc(bc_mod, func, &bop.right, target);
    match (&left.get_type(target.int_size), &bop.operator) {
        (Type::Optional(_), BinaryOperator::Or) => optional_or_to_bc(func, left, right, target),
        (Type::Result(_), BinaryOperator::Or) => result_or_to_bc(func, left, right, target),
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
    func: &mut ByteCodeFunction,
    si: &StructInitializer,
    target: &Target,
) -> Operand {
    let members = si
        .member_initializers
        .iter()
        .map(|e| expr_to_bc(bc_mod, func, e, target))
        .collect();
    Operand::Struct {
        members,
        typ: si.typ.clone(),
    }
}

fn while_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, w: &WhileLoop, target: &Target) {
    let cond_bb = func.create_basic_block();
    let body_bb = func.create_basic_block();
    let post_while_bb = func.create_basic_block();

    func.add(branch_instr(cond_bb));
    func.set_current_bb(cond_bb);
    let cond = expr_to_bc(bc_mod, func, &w.cond, target);
    func.add(branch_if_instr(cond, body_bb, post_while_bb));
    func.set_current_bb(body_bb);
    func.push_scope();
    expr_to_bc(bc_mod, func, &w.body, target);
    let keep_looping = !func.last_instruction_is_return();
    func.pop_scope(bc_mod, target);
    if keep_looping {
        func.add(branch_instr(cond_bb));
    }
    func.set_current_bb(post_while_bb);
}

fn for_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, f: &ForLoop, target: &Target) {
    func.push_scope();
    let iterable = expr_to_bc(bc_mod, func, &f.iterable, target);
    let index = func.declare(
        "$index",
        Some(Operand::const_uint(0, target.int_size)),
        Type::UInt(target.int_size),
    );
    let len = if let Type::Array(at) = iterable.get_type(target.int_size) {
        Operand::const_uint(at.len as u64, target.int_size)
    } else {
        Operand::Property {
            operand: Box::new(iterable.clone()),
            property: ByteCodeProperty::Len,
            typ: target.native_uint_type.clone(),
        }
    };

    let loop_var = func.declare(&f.loop_variable, None, f.loop_variable_type.clone());

    let cond_bb = func.create_basic_block();
    let body_bb = func.create_basic_block();
    let post_for_bb = func.create_basic_block();

    func.add(branch_instr(cond_bb));
    func.set_current_bb(cond_bb);

    let cmp = Operand::binary(BinaryOperator::LessThan, index.clone(), len, Type::Bool);
    func.add(branch_if_instr(cmp, body_bb, post_for_bb));

    func.set_current_bb(body_bb);
    let element_type = iterable
        .get_type(target.int_size)
        .get_element_type()
        .expect("ICE: element type expected");
    let lv = Operand::member(iterable, index.clone(), ptr_type(element_type));
    func.add(store_instr(loop_var, lv));

    func.push_scope();
    expr_to_bc(bc_mod, func, &f.body, target);
    let keep_looping = !func.last_instruction_is_return();
    func.pop_scope(bc_mod, target);
    if keep_looping {
        func.add(store_instr(
            index.clone(),
            Operand::binary(
                BinaryOperator::Add,
                index.clone(),
                Operand::const_uint(1, target.int_size),
                Type::UInt(target.int_size),
            ),
        ));
        func.add(branch_instr(cond_bb));
    }

    func.set_current_bb(post_for_bb);
    func.pop_scope(bc_mod, target);
}

fn if_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    if_expr: &IfExpression,
    target: &Target,
) -> Operand {
    let dst = func.declare("$dst", None, if_expr.typ.clone());
    let true_bb = func.create_basic_block();
    let end_bb = func.create_basic_block();

    let cond = expr_to_bc(bc_mod, func, &if_expr.condition, target);

    if let Some(on_false) = &if_expr.on_false {
        let false_bb = func.create_basic_block();
        func.add(branch_if_instr(cond, true_bb, false_bb));
        func.set_current_bb(false_bb);
        func.push_scope();
        let val = expr_to_bc(bc_mod, func, on_false, target);
        let add_branch = !func.last_instruction_is_return();
        func.pop_scope(bc_mod, target);
        if add_branch {
            func.add(store_instr(dst.clone(), val));
            func.add(branch_instr(end_bb));
        }
    } else {
        func.add(branch_if_instr(cond, true_bb, end_bb));
    }

    func.set_current_bb(true_bb);
    func.push_scope();
    let val = expr_to_bc(bc_mod, func, &if_expr.on_true, target);
    let add_branch = !func.last_instruction_is_return();
    func.pop_scope(bc_mod, target);
    if add_branch {
        func.add(store_instr(dst.clone(), val));
        func.add(branch_instr(end_bb));
    }

    func.set_current_bb(end_bb);
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

fn early_return(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, target: &Target) {
    let unwind_calls = func.get_unwind_calls();
    for c in unwind_calls {
        let operand = expr_to_bc(bc_mod, func, &c, target);
        func.add(Instruction::Exec { operand });
    }
}

fn return_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, r: &Return, target: &Target) -> Operand {
    let ret = expr_to_bc(bc_mod, func, &r.expression, target);
    early_return(bc_mod, func, target);
    if func.sig.rvo {
        func.add(store_instr(rvo_var(func), ret));
        func.add(ret_instr(Operand::Void));
    } else {
        func.add(ret_instr(ret));
    }
    Operand::Void
}

fn compiler_call_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    c: &CompilerCall,
    target: &Target,
) -> Operand {
    match c {
        CompilerCall::SizeOf(typ, _) => Operand::SizeOf { typ: typ.clone() },
        CompilerCall::Slice { data, len, typ, .. } => Operand::Slice {
            start: Box::new(expr_to_bc(bc_mod, func, data, target)),
            len: Box::new(expr_to_bc(bc_mod, func, len, target)),
            typ: typ.clone(),
        },
    }
}

fn array_to_slice_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    ats: &ArrayToSlice,
    target: &Target,
) -> Operand {
    let array = expr_to_bc(bc_mod, func, &ats.inner, target);
    let element_type = array
        .get_type(target.int_size)
        .get_element_type()
        .expect("ICE: Array must have element type");
    Operand::Slice {
        start: Box::new(Operand::Property {
            operand: Box::new(array.clone()),
            property: ByteCodeProperty::Data,
            typ: ptr_type(element_type),
        }),
        len: Box::new(Operand::Property {
            operand: Box::new(array.clone()),
            property: ByteCodeProperty::Len,
            typ: Type::UInt(target.int_size),
        }),
        typ: ats.slice_type.clone(),
    }
}

fn bindings_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    b: &BindingList,
    target: &Target,
) -> Operand {
    for b in &b.bindings {
        add_binding(bc_mod, func, b, target);
    }
    Operand::Void
}

fn match_case_body_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    body: &Expression,
    dst: Operand,
    match_case_bb: BasicBlockRef,
    match_end_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    target: &Target,
) {
    func.set_current_bb(match_case_bb);
    let val = expr_to_bc(bc_mod, func, body, target);
    let add_branch = !func.last_instruction_is_return();
    if add_branch {
        func.add(store_instr(dst, val));
        func.pop_scope(bc_mod, target);
        func.add(branch_instr(match_end_bb));
    } else {
        func.pop_scope(bc_mod, target);
    }
    func.set_current_bb(next_bb);
}

fn match_case_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    pattern: &Pattern,
    body: &Expression,
    match_target: &Operand,
    dst: Operand,
    match_end_bb: BasicBlockRef,
    target_machine: &Target,
) {
    let match_case_bb = func.create_basic_block();
    let next_bb = func.create_basic_block();

    func.push_scope();
    pattern_to_bc(
        bc_mod,
        func,
        pattern,
        match_target,
        match_case_bb,
        match_end_bb,
        next_bb,
        target_machine,
    );
    match_case_body_to_bc(
        bc_mod,
        func,
        body,
        dst,
        match_case_bb,
        match_end_bb,
        next_bb,
        target_machine,
    );
}

fn match_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    m: &MatchExpression,
    target: &Target,
) -> Operand {
    let dst = func.declare("$dst", None, m.typ.clone());
    let match_target = expr_to_bc(bc_mod, func, &m.target, target);
    let match_end_bb = func.create_basic_block();

    func.push_scope();
    for mc in &m.cases {
        match_case_to_bc(
            bc_mod,
            func,
            &mc.pattern,
            &mc.to_execute,
            &match_target,
            dst.clone(),
            match_end_bb,
            target,
        );
    }

    func.add(branch_instr(match_end_bb));
    func.set_current_bb(match_end_bb);
    func.pop_scope(bc_mod, target);
    dst
}

pub fn expr_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    expr: &Expression,
    target: &Target,
) -> Operand {
    match expr {
        Expression::Literal(l) => literal_to_bc(bc_mod, func, l, target),
        Expression::UnaryOp(uop) => unary_op_to_bc(bc_mod, func, uop, target),
        Expression::BinaryOp(bop) => binary_op_to_bc(bc_mod, func, bop, target),
        Expression::Block(b) => block_to_bc(bc_mod, func, b, target),
        Expression::Call(c) => call_to_bc(bc_mod, func, c, target),
        Expression::NameRef(nr) => name_ref_to_bc(nr),
        Expression::Match(m) => match_to_bc(bc_mod, func, m, target),
        Expression::If(i) => if_to_bc(bc_mod, func, i, target),
        Expression::Lambda(l) => lambda_to_bc(bc_mod, l, target),
        Expression::Bindings(b) => bindings_to_bc(bc_mod, func, b, target),
        Expression::StructInitializer(si) => struct_initializer_to_bc(bc_mod, func, si, target),
        Expression::MemberAccess(ma) => member_access_to_bc(bc_mod, func, ma, target),
        Expression::New(n) => Operand::New {
            inner: Box::new(expr_to_bc(bc_mod, func, &n.inner, target)),
            typ: n.typ.clone(),
        },
        Expression::Delete(d) => {
            let object = expr_to_bc(bc_mod, func, &d.inner, target);
            func.add(Instruction::Delete { object });
            Operand::Void
        }
        Expression::ArrayToSlice(ats) => array_to_slice_to_bc(bc_mod, func, ats, target),
        Expression::AddressOf(a) => Operand::AddressOf {
            obj: Box::new(expr_to_bc(bc_mod, func, &a.inner, target)),
        },
        Expression::Dereference(d) => Operand::Dereference {
            inner: expr_to_bc(bc_mod, func, &d.inner, target).into(),
            typ: d.typ.clone(),
        },
        Expression::Assign(a) => {
            assign_to_bc(bc_mod, func, a, target);
            Operand::Void
        }
        Expression::While(w) => {
            while_to_bc(bc_mod, func, w, target);
            Operand::Void
        }
        Expression::For(f) => {
            for_to_bc(bc_mod, func, f, target);
            Operand::Void
        }
        Expression::Nil(n) => Operand::Optional {
            inner: None,
            typ: n.typ.clone(),
        },
        Expression::OptionalToBool(inner) => {
            let opt = expr_to_bc(bc_mod, func, inner, target);
            Operand::sti(opt, target.int_size)
        }
        Expression::ResultToBool(inner) => {
            let res = expr_to_bc(bc_mod, func, inner, target);
            Operand::Binary {
                op: BinaryOperator::Equals,
                left: Box::new(Operand::const_uint(0, target.int_size)),
                right: Box::new(Operand::sti(res, target.int_size)),
                typ: Type::Bool,
            }
        }
        Expression::ToOptional(inner) => {
            let i = expr_to_bc(bc_mod, func, &inner.inner, target);
            Operand::Optional {
                inner: Some(Box::new(i)),
                typ: inner.optional_type.clone(),
            }
        }
        Expression::ToOkResult(r) => {
            let i = expr_to_bc(bc_mod, func, &r.inner, target);
            Operand::Result {
                ok: true,
                inner: Box::new(i),
                typ: r.result_type.clone(),
            }
        }
        Expression::ToErrResult(r) => {
            let i = expr_to_bc(bc_mod, func, &r.inner, target);
            Operand::Result {
                ok: false,
                inner: Box::new(i),
                typ: r.result_type.clone(),
            }
        }
        Expression::Cast(c) => {
            let inner = expr_to_bc(bc_mod, func, &c.inner, target);
            Operand::Cast {
                inner: Box::new(inner),
                typ: c.destination_type.clone(),
            }
        }
        Expression::CompilerCall(c) => compiler_call_to_bc(bc_mod, func, c, target),
        Expression::IndexOperation(i) => {
            let t = expr_to_bc(bc_mod, func, &i.target, target);
            let idx = expr_to_bc(bc_mod, func, &i.index_expr, target);
            Operand::member(t, idx, ptr_type(i.typ.clone()))
        }
        Expression::Return(r) => return_to_bc(bc_mod, func, r, target),
        Expression::Void => Operand::Void,
    }
}

fn rvo_var(func: &ByteCodeFunction) -> Operand {
    Operand::Var {
        name: RVO_RETURN_ARG.to_string(),
        typ: func.return_type(),
    }
}

fn func_to_bc(
    sig: &FunctionSignature,
    bc_mod: &mut ByteCodeModule,
    expression: &Expression,
    target: &Target,
) -> ByteCodeFunction {
    let mut llfunc = ByteCodeFunction::new(sig, false);
    let ret = expr_to_bc(bc_mod, &mut llfunc, expression, target);
    // Pop final scope before returning
    if !llfunc.last_instruction_is_return() {
        llfunc.pop_scope(bc_mod, target);
        if llfunc.sig.rvo {
            llfunc.add(store_instr(rvo_var(&llfunc), ret));
            llfunc.add(ret_instr(Operand::Void));
        } else {
            llfunc.add(ret_instr(ret));
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
