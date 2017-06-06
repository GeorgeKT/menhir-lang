use std::collections::HashMap;
use ast::*;
use target::{Target};
use bytecode::{ByteCodeModule, ByteCodeFunction};
use compileerror::{CompileResult, type_error_result};
use super::consteval::expr_to_const;
use super::function::*;
use super::instruction::*;


fn stack_alloc(func: &mut ByteCodeFunction, typ: &Type, name: Option<&str>) -> Var
{
    match name
    {
        Some(n) => {
            let var = Var::named(n, typ.clone());
            func.add_named_var(var.clone());
            var
        },
        None => {
            func.new_var(typ.clone())
        }
    }
}

fn get_dst(func: &mut ByteCodeFunction, typ: &Type) -> Var
{
    assert_ne!(*typ, Type::Unknown);
    if let Some(dst) = func.get_destination() {
        return dst;
    }

    stack_alloc(func, typ, None)
}

fn array_lit_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, a: &ArrayLiteral, dst: &Var, target: &Target)
{
    for (idx, element) in a.elements.iter().enumerate() {
        func.push_destination(None);
        let v = to_bc(bc_mod, func, element, target);
        func.pop_destination();
        func.add(store_member_instr(dst, idx, v, target.int_size));
    }
}

fn call_args_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, c: &Call, self_arg: Option<Var>, target: &Target) -> Vec<Operand>
{
    func.push_destination(None);
    let mut args = Vec::new();
    if let Some(s) = self_arg {
        args.push(Operand::Var(s));
    }

    args.extend(c.args.iter().map(|arg| Operand::Var(to_bc(bc_mod, func, arg, target))));
    func.pop_destination();
    args
}

fn call_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, c: &Call, self_arg: Option<Var>, target: &Target) -> Option<Var>
{
    if let Type::Void = c.return_type {
        let args = call_args_to_bc(bc_mod, func, c, self_arg, target);
        func.add(void_call_instr(&c.callee.name, args));
        None
    } else {
        let dst = get_dst(func, &c.return_type);
        let args = call_args_to_bc(bc_mod, func, c, self_arg, target);
        func.add(call_instr(&dst, &c.callee.name, args));
        Some(dst)
    }
}


fn struct_initializer_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, si: &StructInitializer, dst: &Var, target: &Target)
{
    let init_members = |bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, si: &StructInitializer, dst: &Var| {
        for (idx, expr) in si.member_initializers.iter().enumerate() {
            let v = to_bc(bc_mod, func, expr, target);
            func.add(store_member_instr(dst, idx, v, target.int_size));
        }
    };

    if let Type::Sum(ref st) = dst.typ {
        let idx = st.index_of(&si.struct_name).expect("Internal Compiler Error: cannot determine index of sum type case");
        func.add(set_prop_instr(dst, ByteCodeProperty::SumTypeIndex, idx));

        let struct_ptr = stack_alloc(func, &ptr_type(st.cases[idx].typ.clone()), None);
        func.add(load_member_instr(&struct_ptr, dst, idx, target.int_size));
        init_members(bc_mod, func, si, &struct_ptr);
    } else {
        init_members(bc_mod, func, si, dst);
    }
}

fn block_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Block, target: &Target) -> Option<Var>
{
    let do_block = |bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Block| {
        for (idx, e) in b.expressions.iter().enumerate() {
            if idx == b.expressions.len() - 1 {
                expr_to_bc(bc_mod, func, e, target);
            } else {
                func.push_destination(None);
                expr_to_bc(bc_mod, func, e, target);
                func.pop_destination();
            }
        }
    };


    if b.typ != Type::Void {
        let dst = get_dst(func, &b.typ);
        func.push_destination(Some(dst.clone()));
        do_block(bc_mod, func, b);
        func.pop_destination();
        Some(dst)
    } else {
        func.push_destination(None);
        do_block(bc_mod, func, b);
        func.pop_destination();
        None
    }
}

fn add_struct_pattern_bindings(p: &StructPattern, struct_var: &Var, func: &mut ByteCodeFunction, target: &Target)
{
    for (idx, b) in p.bindings.iter().enumerate() {
        if b.name == "_" {continue}
        let v = stack_alloc(func, &b.typ, Some(&b.name));

        match b.mode {
            StructPatternBindingMode::Value=> {
                func.add(load_member_instr(&v, struct_var, idx, target.int_size));
            },

            StructPatternBindingMode::Pointer => {
                func.add(address_of_member_instr(&v, struct_var, idx, target.int_size));
            }
        }

        func.add_named_var(v);
    }
}

fn add_binding(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Binding, target: &Target)
{
    match b.binding_type
    {
        BindingType::Name(ref name) => {
            let dst = stack_alloc(func, &b.typ, Some(name));
            func.push_destination(Some(dst));
            expr_to_bc(bc_mod, func, &b.init, target);
            func.pop_destination();
        },

        BindingType::Struct(ref s) => {
            let dst = stack_alloc(func, &b.typ, None);
            func.push_destination(Some(dst.clone()));
            expr_to_bc(bc_mod, func, &b.init, target);
            add_struct_pattern_bindings(s, &dst, func, target);
            func.pop_destination();
        },
    }
}

fn name_ref_to_bc(func: &mut ByteCodeFunction, nr: &NameRef, target: &Target) -> Option<Var>
{
    let add_name_ref = |func: &mut ByteCodeFunction, nr: &NameRef| {
        let v = Var::named(&nr.name, nr.typ.clone());
        match func.get_destination()
        {
            Some(var) => {
                func.add(store_instr(&var, &v));
                Some(var)
            },
            None => Some(v),
        }
    };

    match nr.typ
    {
        Type::Sum(ref st) => {
            if let Some(idx) = st.index_of(&nr.name) {
                let dst = get_dst(func, &nr.typ);
                func.add(set_prop_instr(&dst, ByteCodeProperty::SumTypeIndex, idx));
                Some(dst)
            } else {
                add_name_ref(func, nr)
            }
        },

        Type::Enum(ref et) => {
            if let Some(idx) = et.index_of(&nr.name) {
                // enums are integers
                let dst = get_dst(func, &nr.typ);
                func.add(store_operand_instr(&dst, Operand::const_uint(idx as u64, target.int_size)));
                Some(dst)
            } else {
                add_name_ref(func, nr)
            }
        },

        Type::Func(_) => {
            match func.get_destination()
            {
                Some(dst) => {
                    func.add(store_func_instr(&dst, &nr.name));
                    Some(dst)
                },
                None => Some(Var::named(&nr.name, nr.typ.clone())),
            }
        },

        _ => {
            add_name_ref(func, nr)
        }
    }
}

fn member_store_lhs_to_bc(func: &mut ByteCodeFunction, lhs: &Expression, target: &Target) -> (Var, Vec<(usize, Type)>)
{
    match *lhs {
        Expression::NameRef(ref nr) => {
            let var = name_ref_to_bc(func, nr, target).expect("Unknown variable");
            (var, Vec::new())
        },
        Expression::MemberAccess(ref inner_ma) => {
            let (var, mut fields) = member_store_lhs_to_bc(func, &inner_ma.left, target);
            let inner_ma_typ = if let Type::Pointer(ref p) = inner_ma.typ {
                p
            } else {
                &inner_ma.typ
            };

            match (inner_ma_typ, &inner_ma.right) {
                (&Type::Struct(_), &MemberAccessType::Name(ref field)) => {
                    fields.push((field.index, inner_ma.typ.clone()));
                    (var, fields)
                },

                _ => panic!("Internal Compiler Error: Invalid member store"),
            }

        }

        _ => panic!("Internal Compiler Error: Expecting name or member access"),
    }
}


fn member_store_to_bc(func: &mut ByteCodeFunction, sma: &MemberAccess, val: Var, target: &Target)
{
    func.push_destination(None);
    let (var, fields) = member_store_lhs_to_bc(func, &sma.left, target);
    func.pop_destination();

    let mut ptr = var;
    for (field, field_type) in fields {
        let dst = get_dst(func, &ptr_type(field_type));
        func.add(address_of_member_instr(&dst, &ptr, field, target.int_size));
        ptr = dst;
    }

    match sma.right {
        MemberAccessType::Name(ref field) => {
            func.add(store_member_instr(&ptr, field.index, val, target.int_size));
        }

        _ => panic!("Internal Compiler Error: Invalid member store"),
    }
}

fn member_access_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, sma: &MemberAccess, dst: &Var, target: &Target)
{
    func.push_destination(None);
    let var = to_bc(bc_mod, func, &sma.left, target);
    func.pop_destination();

    let var_typ = if let Type::Pointer(ref inner) = var.typ {
        &inner
    } else {
        &var.typ
    };

    match (var_typ, &sma.right)
    {
        (&Type::Struct(_), &MemberAccessType::Name(ref field)) => {
            if dst.typ.pass_by_value() {
                func.add(load_member_instr(dst, &var, field.index, target.int_size));
            } else {
                func.add(address_of_member_instr(dst, &var, field.index, target.int_size));
            }
        },

        (&Type::Array(ref at), &MemberAccessType::Property(Property::Len)) => {
            func.add(store_operand_instr(dst, Operand::const_uint(at.len as u64, target.int_size)))
        },

        (&Type::Slice(_), &MemberAccessType::Property(Property::Len)) |
        (&Type::String, &MemberAccessType::Property(Property::Len)) => {
            func.add(get_prop_instr(dst, &var, ByteCodeProperty::Len));
        },

        (&Type::Slice(_), &MemberAccessType::Property(Property::Data)) |
        (&Type::String, &MemberAccessType::Property(Property::Data))  => {
            func.add(get_prop_instr(dst, &var, ByteCodeProperty::Data));
        },

        _ => {
            panic!("Internal Compiler Error: Invalid member access")
        },
    }
}

fn name_pattern_match_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    target: &Var,
    match_end_bb: BasicBlockRef,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    nr: &NameRef,
    target_machine: &Target)
{
    match nr.typ
    {
        Type::Enum(ref et) => {
            let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(target), Operand::const_uint(idx as u64, target_machine.int_size)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        Type::Sum(ref st) => {
            let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let sum_type_index = stack_alloc(func, &target_machine.native_uint_type, None);
            func.add(get_prop_instr(&sum_type_index, target, ByteCodeProperty::SumTypeIndex));
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&sum_type_index), Operand::const_uint(idx as u64, target_machine.int_size)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
        }
    }

    match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
}


fn match_case_body_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    match_case_bb: BasicBlockRef,
    match_end_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    end_scope: bool,
    target: &Target)
{
    func.set_current_bb(match_case_bb);
    expr_to_bc(bc_mod, func, &mc.to_execute, target);
    if end_scope {
        func.pop_scope();
    }
    func.add(Instruction::Branch(match_end_bb));
    func.set_current_bb(next_bb);
}

fn array_pattern_match_to_bc(
    func: &mut ByteCodeFunction,
    ap: &ArrayPattern,
    seq: &Var,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    target: &Target)
{
    let head_type = seq.typ.get_element_type().expect("Invalid array type");
    let head = stack_alloc(func, &head_type, Some(&ap.head));
    func.add(load_member_instr(&head, seq, 0, target.int_size));

    let tail = stack_alloc(func, &slice_type(head_type), Some(&ap.tail));
    let tail_len = stack_alloc(func, &target.native_uint_type, None);
    let seq_len = stack_alloc(func, &target.native_uint_type, None);
    func.add(get_prop_instr(&seq_len, seq, ByteCodeProperty::Len));
    func.add(binary_op_instr(&tail_len, BinaryOperator::Sub, var_op(&seq_len), Operand::const_uint(1, target.int_size)));
    func.add(slice_instr(&tail, seq, Operand::const_uint(1, target.int_size), var_op(&tail_len)));


    let length = stack_alloc(func, &target.native_uint_type, None);
    func.add(get_prop_instr(&length, seq, ByteCodeProperty::Len));
    let cond = stack_alloc(func, &Type::Bool, None);
    func.add(binary_op_instr(&cond, BinaryOperator::GreaterThan, var_op(&length), Operand::const_uint(0, target.int_size)));
    func.add(branch_if_instr(&cond, match_case_bb, next_bb));
}

fn struct_pattern_match_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    target: &Var,
    match_end_bb: BasicBlockRef,
    match_case_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    p: &StructPattern,
    target_machine: &Target)
{
    func.push_destination(None);
    match p.typ
    {
        Type::Struct(_) => {
            func.add(Instruction::Branch(match_case_bb));
            func.set_current_bb(match_case_bb);

            func.push_scope();
            add_struct_pattern_bindings(p, target, func, target_machine);
        },
        Type::Sum(ref st) => {
            let target_sum_type_index = stack_alloc(func, &target_machine.native_uint_type, None);
            func.add(get_prop_instr(&target_sum_type_index, target, ByteCodeProperty::SumTypeIndex));
            let idx = st.index_of(&p.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&target_sum_type_index), Operand::const_uint(idx as u64, target_machine.int_size)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));

            func.set_current_bb(match_case_bb);

            func.push_scope();
            let struct_ptr = stack_alloc(func, &ptr_type(st.cases[idx].typ.clone()), None);
            func.add(address_of_member_instr(&struct_ptr, target, idx, target_machine.int_size));

            add_struct_pattern_bindings(p, &struct_ptr, func, target_machine);
        },
        _ => panic!("Internal Compiler Error: Expression is not a valid match pattern"),
    }

    func.pop_destination();
    match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, true, target_machine);
}

fn match_case_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    target: &Var,
    match_end_bb: BasicBlockRef,
    target_machine: &Target)
{
    let match_case_bb = func.create_basic_block();
    let next_bb = func.create_basic_block();

    let add_literal_case = |bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, op: Operand| {
        func.push_destination(None);
        let cond = stack_alloc(func, &Type::Bool, None);
        func.add(binary_op_instr(&cond, BinaryOperator::Equals, op, var_op(target)));
        func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        func.pop_destination();
        match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
    };

    match mc.pattern
    {
        Pattern::Literal(Literal::Int(_, v, int_size)) => {
            add_literal_case(bc_mod, func, Operand::const_int(v, int_size));
        },

        Pattern::Literal(Literal::UInt(_, v, int_size)) => {
            add_literal_case(bc_mod, func, Operand::const_uint(v, int_size));
        },

        Pattern::Literal(Literal::Float(_, ref v, float_size)) => {
            add_literal_case(bc_mod, func, float_op(v, float_size));
        },

        Pattern::Literal(Literal::Bool(_, v)) => {
            add_literal_case(bc_mod, func, Operand::const_bool(v));
        },

        Pattern::Literal(Literal::Char(_, v)) => {
            add_literal_case(bc_mod, func, Operand::const_char(v));
        },

        Pattern::Name(ref nr) => {
            name_pattern_match_to_bc(bc_mod, func, mc, target, match_end_bb, match_case_bb, next_bb, nr, target_machine)
        },

        Pattern::Any(_) => {
            func.add(Instruction::Branch(match_case_bb));
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
        },

        Pattern::EmptyArray(_) => {
            match target.typ
            {
                Type::Array(_) | Type::Slice(_) => {
                    let len = stack_alloc(func, &target_machine.native_uint_type, None);
                    let cond = stack_alloc(func, &Type::Bool, None);
                    func.add(get_prop_instr(&len, target, ByteCodeProperty::Len));
                    func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&len), Operand::const_uint(0, target_machine.int_size)));
                    func.add(branch_if_instr(&cond, match_case_bb, next_bb))
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an empty array pattern"),
            }

            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
        },

        Pattern::Array(ref ap) => {
            match target.typ
            {
                Type::Array(_) | Type::Slice(_) => {
                    func.push_destination(None);
                    array_pattern_match_to_bc(func, ap, target, match_case_bb, next_bb, target_machine);
                    func.pop_destination();
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }

            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
        },

        Pattern::Literal(Literal::Array(ref a)) => {
            func.push_destination(None);
            let arr = func.new_var(a.array_type.clone());
            array_lit_to_bc(bc_mod, func, a, &arr, target_machine);
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&arr), var_op(target)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
        },

        Pattern::Literal(Literal::String(_, ref s)) => {
            func.push_destination(None);
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, Operand::const_string(&s[..]), var_op(target)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
        },

        Pattern::Struct(ref p) => {
            struct_pattern_match_to_bc(bc_mod, func, mc, target, match_end_bb, match_case_bb, next_bb, p, target_machine);
        },

        Pattern::Nil(_) => {
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(load_optional_flag_instr(&cond, target));
            func.add(branch_if_instr(&cond, next_bb, match_case_bb));
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false, target_machine);
        },

        Pattern::Optional(ref o) => {
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(load_optional_flag_instr(&cond, target));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));

            func.set_current_bb(match_case_bb);
            func.push_scope();

            let binding = stack_alloc(func, &o.inner_type, Some(&o.binding));
            func.add(load_instr(&binding, target));
            func.add_named_var(binding);
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, true, target_machine);
        },
    }
}

fn match_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, m: &MatchExpression, target: &Target) -> Option<Var>
{
    func.push_destination(None);
    let target_var = match m.target {
        Expression::Dereference(ref de) => {
            let inner_type = de.inner.get_type(target.int_size);
            let v_inner_type = inner_type.get_pointer_element_type().expect("Dereference should be on a pointer type");
            if v_inner_type.pass_by_value() {
                dereference_to_bc(bc_mod, func, de, target)
            } else {
                to_bc(bc_mod, func, &de.inner, target)
            }
        }

        _ => to_bc(bc_mod, func, &m.target, target),
    };

    func.pop_destination();
    let match_end_bb = func.create_basic_block();

    let dst = if m.typ == Type::Void {
        None
    } else {
        let dst = get_dst(func, &m.typ);
        func.add(Instruction::StackAlloc(dst.clone()));
        Some(dst)
    };


    func.push_scope();
    func.push_destination(dst.clone());
    for mc in &m.cases {
        match_case_to_bc(bc_mod, func, mc, &target_var, match_end_bb, target);
    }
    func.pop_destination();

    func.add(Instruction::Branch(match_end_bb));
    func.set_current_bb(match_end_bb);
    func.pop_scope();
    dst
}

fn while_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, w: &WhileLoop, target: &Target)
{
    let cond_bb = func.create_basic_block();
    let body_bb = func.create_basic_block();
    let post_while_bb = func.create_basic_block();

    func.add(Instruction::Branch(cond_bb));
    func.set_current_bb(cond_bb);
    let cond = to_bc(bc_mod, func, &w.cond, target);
    func.add(branch_if_instr(&cond, body_bb, post_while_bb));
    func.set_current_bb(body_bb);
    expr_to_bc(bc_mod, func, &w.body, target);
    func.add(Instruction::Branch(cond_bb));

    func.set_current_bb(post_while_bb);
}

fn for_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, f: &ForLoop, target: &Target)
{
    func.push_scope();
    func.push_destination(None);
    let iterable = to_bc(bc_mod, func, &f.iterable, target);
    func.pop_destination();

    let loop_variable = stack_alloc(func, &f.loop_variable_type, Some(&f.loop_variable));

    let index = stack_alloc(func, &target.native_uint_type, None);
    func.add(store_operand_instr(&index, Operand::const_uint(0, target.int_size)));


    let len = if let Type::Array(ref at) = iterable.typ {
        Operand::const_uint(at.len as u64, target.int_size)
    } else {
        let len = stack_alloc(func, &target.native_uint_type, None);
        func.add(get_prop_instr(&len, &iterable, ByteCodeProperty::Len));
        var_op(&len)
    };

    let cond_bb = func.create_basic_block();
    let body_bb = func.create_basic_block();
    let post_for_bb = func.create_basic_block();

    func.add(Instruction::Branch(cond_bb));
    func.set_current_bb(cond_bb);
    let cmp = stack_alloc(func, &Type::Bool, None);
    func.add(binary_op_instr(&cmp, BinaryOperator::LessThan, var_op(&index), len));
    func.add(branch_if_instr(&cmp, body_bb, post_for_bb));

    func.set_current_bb(body_bb);
    func.add(load_member_instr_with_var(&loop_variable, &iterable, &index));
    func.push_destination(None);
    expr_to_bc(bc_mod, func, &f.body, target);
    func.pop_destination();
    func.add(binary_op_instr(&index, BinaryOperator::Add, var_op(&index), Operand::const_uint(1, target.int_size)));
    func.add(Instruction::Branch(cond_bb));

    func.set_current_bb(post_for_bb);
    func.pop_scope();
}

fn cast_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, c: &TypeCast, target: &Target) -> Var
{
    let dst = get_dst(func, &c.destination_type);
    func.push_destination(None);
    let inner = to_bc(bc_mod, func, &c.inner, target);
    func.pop_destination();
    func.add(cast_instr(&dst, &inner));
    dst
}

fn to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, expr: &Expression, target: &Target) -> Var
{
    expr_to_bc(bc_mod, func, expr, target).expect("Expression must return a value")
}

fn optional_compare_to_bc(
    func: &mut ByteCodeFunction,
    l: &Var,
    r: &Var,
    dst: &Var,
    equals: bool,
    inner_type: &Type)
{
    let set_to_true_bb = func.create_basic_block();
    let set_to_false_bb = func.create_basic_block();
    let compare_inner_bb = func.create_basic_block();
    let end_bb = func.create_basic_block();

    let l_is_ok = stack_alloc(func, &Type::Bool, None);
    let r_is_ok = stack_alloc(func, &Type::Bool, None);
    let both_ok = stack_alloc(func, &Type::Bool, None);
    func.add(load_optional_flag_instr(&l_is_ok, l));
    func.add(load_optional_flag_instr(&r_is_ok, r));
    func.add(binary_op_instr(&both_ok, BinaryOperator::And, var_op(&l_is_ok), var_op(&r_is_ok)));
    func.add(branch_if_instr(&both_ok, compare_inner_bb, set_to_false_bb));


    func.set_current_bb(compare_inner_bb);
    let l_inner = stack_alloc(func, inner_type, None);
    let r_inner = stack_alloc(func, inner_type, None);
    let cmp = stack_alloc(func, &Type::Bool, None);
    func.add(load_instr(&l_inner, l));
    func.add(load_instr(&r_inner, r));
    func.add(binary_op_instr(&cmp, BinaryOperator::Equals, var_op(&l_inner), var_op(&r_inner)));
    func.add(branch_if_instr(&cmp, set_to_true_bb, set_to_false_bb));

    func.set_current_bb(set_to_true_bb);
    func.add(store_operand_instr(dst, Operand::const_bool(equals)));
    func.add(Instruction::Branch(end_bb));

    func.set_current_bb(set_to_false_bb);
    func.add(store_operand_instr(dst, Operand::const_bool(!equals)));
    func.add(Instruction::Branch(end_bb));

    func.set_current_bb(end_bb);
}

fn binary_op_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, op: &BinaryOp, target: &Target) -> Var
{
    func.push_destination(None);
    let l = to_bc(bc_mod, func, &op.left, target);
    let r = to_bc(bc_mod, func, &op.right, target);
    func.pop_destination();

    let dst = get_dst(func, &op.typ);
    match l.typ
    {
        Type::Optional(ref inner) => match op.operator {
            BinaryOperator::Equals => {
                optional_compare_to_bc(func, &l, &r, &dst, true, inner);
            },

            BinaryOperator::NotEquals => {
                optional_compare_to_bc(func, &l, &r, &dst, false, inner);
            },

            BinaryOperator::Or => {
                let l_is_valid = stack_alloc(func, &Type::Bool, None);
                let store_r_bb = func.create_basic_block();
                let store_l_bb = func.create_basic_block();
                let end_bb = func.create_basic_block();
                func.add(load_optional_flag_instr(&l_is_valid, &l));
                func.add(branch_if_instr(&l_is_valid, store_l_bb, store_r_bb));

                func.set_current_bb(store_r_bb);
                func.add(store_instr(&dst, &r));
                func.add(Instruction::Branch(end_bb));

                func.set_current_bb(store_l_bb);
                func.add(load_instr(&dst, &l));
                func.add(Instruction::Branch(end_bb));
                func.set_current_bb(end_bb);
            },

            _ => {
                panic!("Operator {} not supported on optional", op.operator);
            }
        },

        _ => {
            func.add(binary_op_instr(&dst, op.operator, var_op(&l), var_op(&r)));
        }
    }

    dst
}

fn if_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, if_expr: &IfExpression, target: &Target) -> Var
{
    let dst = get_dst(func, &if_expr.typ);
    let true_bb = func.create_basic_block();
    let end_bb = func.create_basic_block();

    func.push_destination(None);
    let cond = to_bc(bc_mod, func, &if_expr.condition, target);
    func.pop_destination();

    func.push_destination(Some(dst.clone()));

    if let Some(ref on_false) = if_expr.on_false {
        let false_bb = func.create_basic_block();
        func.add(branch_if_instr(&cond, true_bb, false_bb));
        func.set_current_bb(false_bb);
        expr_to_bc(bc_mod, func, on_false, target);
        func.add(Instruction::Branch(end_bb));
    } else {
        func.add(branch_if_instr(&cond, true_bb, end_bb));
    }

    func.set_current_bb(true_bb);
    expr_to_bc(bc_mod, func, &if_expr.on_true, target);
    func.add(Instruction::Branch(end_bb));

    func.pop_destination();
    func.set_current_bb(end_bb);
    dst
}

fn assign_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, assign: &Assign, target: &Target)
{
    // During type checking, other assigns, will be converted in a regular assign
    assert!(assign.operator == AssignOperator::Assign);

    func.push_destination(None);
    let r = to_bc(bc_mod, func, &assign.right, target);
    match assign.left {
        AssignTarget::Var(ref nr) => {
            let var = Var::named(&nr.name, nr.typ.clone());
            func.add(store_instr(&var, &r));
        },

        AssignTarget::MemberAccess(ref ma) => {
            member_store_to_bc(func, ma, r, target);
        },

        AssignTarget::Dereference(ref d) => {
            let var = to_bc(bc_mod, func, &d.inner, target);
            func.add(store_instr(&var, &r));
        },

        AssignTarget::IndexOperation(ref iop) => {
            let tgt = to_bc(bc_mod, func, &iop.target, target);
            let idx = to_bc(bc_mod, func, &iop.index_expr, target);
            func.add(store_member_with_var_instr(tgt, idx, r));
        }
    }

    func.pop_destination();
}

fn dereference_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, d: &DereferenceExpression, target: &Target) -> Var
{
    let inner_var = to_bc(bc_mod, func, &d.inner, target);
    let dst = get_dst(func, &d.typ);
    func.add(store_operand_instr(&dst, Operand::Dereference(inner_var)));
    dst
}

fn expr_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, expr: &Expression, target: &Target) -> Option<Var>
{
    match *expr
    {
        Expression::Void => None,

        Expression::UnaryOp(ref u) => {
            func.push_destination(None);
            let v = to_bc(bc_mod, func, &u.expression, target);
            func.pop_destination();
            let dst = get_dst(func, &u.typ);
            func.add(unary_op_instr(&dst, u.operator, var_op(&v)));
            Some(dst)
        },

        Expression::BinaryOp(ref op) => {
            Some(binary_op_to_bc(bc_mod, func, op, target))
        },

        Expression::Literal(Literal::Int(_, v, int_size)) => {
            let dst = get_dst(func, &Type::Int(int_size));
            func.add(store_operand_instr(&dst, Operand::const_int(v, int_size)));
            Some(dst)
        },

        Expression::Literal(Literal::UInt(_, v, int_size)) => {
            let dst = get_dst(func, &Type::UInt(int_size));
            func.add(store_operand_instr(&dst, Operand::const_uint(v, int_size)));
            Some(dst)
        },

        Expression::Literal(Literal::Float(_, ref v_str, float_size)) => {
            let dst = get_dst(func, &Type::Float(float_size));
            func.add(store_operand_instr(&dst, float_op(v_str, float_size)));
            Some(dst)
        },

        Expression::Literal(Literal::String(_, ref s))  => {
            let dst = get_dst(func, &string_type());
            func.add(store_operand_instr(&dst, Operand::const_string(&s[..])));
            Some(dst)
        },

        Expression::Literal(Literal::Bool(_, v)) => {
            let dst = get_dst(func, &Type::Bool);
            func.add(store_operand_instr(&dst, Operand::const_bool(v)));
            Some(dst)
        },

        Expression::Literal(Literal::Char(_, v)) => {
            let dst = get_dst(func, &Type::Char);
            func.add(store_operand_instr(&dst, Operand::const_char(v)));
            Some(dst)
        },

        Expression::Literal(Literal::Array(ref a)) => {
            let dst = get_dst(func, &a.array_type);
            func.push_destination(None);
            array_lit_to_bc(bc_mod, func, a, &dst, target);
            func.pop_destination();
            Some(dst)
        },

        Expression::Call(ref c) => {
            call_to_bc(bc_mod, func, c, None, target)
        },

        Expression::StructInitializer(ref si) => {
            let dst = get_dst(func, &si.typ);
            func.push_destination(None);
            struct_initializer_to_bc(bc_mod, func, si, &dst, target);
            func.pop_destination();
            Some(dst)
        },

        Expression::Block(ref b) => {
            block_to_bc(bc_mod, func, b, target)
        },

        Expression::Bindings(ref l) => {
            for b in &l.bindings {
                add_binding(bc_mod, func, b, target);
            }
            None
        },

        Expression::New(ref n) => {
            let dst = get_dst(func, &n.typ);
            func.add(Instruction::HeapAlloc(dst.clone()));
            func.push_destination(Some(dst.clone()));
            expr_to_bc(bc_mod, func, &n.inner, target);
            func.pop_destination();
            Some(dst)
        },

        Expression::Delete(ref d) => {
            let to_delete = to_bc(bc_mod, func, &d.inner, target);
            func.add(Instruction::Delete(to_delete));
            None
        },

        Expression::Lambda(ref l) => {
            let lambda = func_to_bc(&l.sig, bc_mod, &l.expr, target);
            let dst = get_dst(func, &l.sig.get_type());
            func.add(store_func_instr(&dst, &lambda.sig.name));
            bc_mod.functions.insert(l.sig.name.clone(), lambda);
            Some(dst)
        },

        Expression::NameRef(ref nr) => {
            name_ref_to_bc(func, nr, target)
        },

        Expression::MemberAccess(ref sma) => {
            let dst = if sma.typ.pass_by_value() {
                get_dst(func, &sma.typ)
            } else {
                get_dst(func, &sma.typ.ptr_of())
            };

            member_access_to_bc(bc_mod, func, sma, &dst, target);
            Some(dst)
        },

        Expression::Match(ref m) => {
            match_to_bc(bc_mod, func, m, target)
        },

        Expression::If(ref i) => {
            Some(if_to_bc(bc_mod, func, i, target))
        },

        Expression::ArrayToSlice(ref ats) => {
            let dst = get_dst(func, &ats.slice_type);
            let array_var = to_bc(bc_mod, func, &ats.inner, target);
            let end = stack_alloc(func, &target.native_uint_type, None);
            func.add(get_prop_instr(&end, &array_var, ByteCodeProperty::Len));
            func.add(slice_instr(&dst, &array_var, Operand::const_uint(0, target.int_size), var_op(&end)));
            Some(dst)
        },

        Expression::AddressOf(ref a) => {
            let inner_var = to_bc(bc_mod, func, &a.inner, target);
            let dst = get_dst(func, &a.typ);
            func.add(address_of_instr(&dst, &inner_var));
            Some(dst)
        },

        Expression::Dereference(ref d) => {
            Some(dereference_to_bc(bc_mod, func, d, target))
        },

        Expression::Assign(ref a) => {
            assign_to_bc(bc_mod, func, a, target);
            None
        },

        Expression::While(ref w) => {
            while_to_bc(bc_mod, func, w, target);
            None
        },

        Expression::For(ref f) => {
            for_to_bc(bc_mod, func, f, target);
            None
        },

        Expression::Nil(ref nt) => {
            let dst = get_dst(func, &nt.typ);
            func.add(Instruction::StoreNil(dst.clone()));
            Some(dst)
        },

        Expression::OptionalToBool(ref n) => {
            let dst = get_dst(func, &Type::Bool);
            func.push_destination(None);
            let inner_var = to_bc(bc_mod, func, n, target);
            func.pop_destination();
            func.add(load_optional_flag_instr(&dst, &inner_var));
            Some(dst)
        }

        Expression::ToOptional(ref t) => {
            let dst = get_dst(func, &t.optional_type);
            func.push_destination(None);
            let inner = to_bc(bc_mod, func, &t.inner, target);
            func.pop_destination();
            func.add(store_instr(&dst, &inner));
            Some(dst)
        },

        Expression::Cast(ref c) => {
            Some(cast_to_bc(bc_mod, func, c, target))
        },

        Expression::CompilerCall(CompilerCall::SizeOf(ref typ, _)) => {
            let dst = get_dst(func, &target.native_uint_type);
            func.add(store_operand_instr(&dst, Operand::SizeOf(typ.clone())));
            Some(dst)
        }

        Expression::IndexOperation(ref iop) => {
            let tgt = to_bc(bc_mod, func, &iop.target, target);
            let idx = to_bc(bc_mod, func, &iop.index_expr, target);
            let dst = get_dst(func, &iop.typ);
            func.add(load_member_instr_with_var(&dst, &tgt, &idx));
            Some(dst)
        }

        Expression::Return(ref r) => {
            func.push_destination(None);
            if let Some(var) = expr_to_bc(bc_mod, func, &r.expression, target) {
                func.add(Instruction::Return(Operand::Var(var)));
            } else {
                func.add(Instruction::ReturnVoid)
            }
            func.pop_destination();
            None
        }
    }
}

fn func_to_bc(sig: &FunctionSignature, bc_mod: &mut ByteCodeModule, expression: &Expression, target: &Target) -> ByteCodeFunction
{
    let mut llfunc = ByteCodeFunction::new(sig, false);
    match expr_to_bc(bc_mod, &mut llfunc, expression, target)
    {
        Some(ref var) if var.typ != Type::Void => {
            // Pop final scope before returning
            llfunc.pop_scope();
            llfunc.add(ret_instr(var));
        },

        _ => {
            llfunc.pop_scope();
            llfunc.add(Instruction::ReturnVoid);
        }
    }

    llfunc
}

pub fn compile_to_byte_code(pkg: &Package, target: &Target) -> CompileResult<ByteCodeModule>
{
    let mut ll_mod = ByteCodeModule{
        name: pkg.name.clone(),
        functions: HashMap::new(),
        globals: HashMap::new(),
        imported_functions: Vec::new(),
    };


    for md in pkg.modules.values() {
        for func in md.externals.values() {
            ll_mod.functions.insert(func.sig.name.clone(), ByteCodeFunction::new(&func.sig, true));
        }

        for global in md.globals.values() {
            if let Some(cst) = expr_to_const(&global.init) {
                ll_mod.globals.insert(global.name.clone(), cst);
            } else {
                return type_error_result(&global.span, format!("Global {} must be initialized with a constant expression", global.name));
            }
        }

        for func in md.functions.values() {
            if !func.is_generic() {
                let new_func = func_to_bc(&func.sig, &mut ll_mod, &func.expression, target);
                ll_mod.functions.insert(func.sig.name.clone(), new_func);
            }
        }
    }


    for import in pkg.imports.values() {
        for symbol in import.symbols.values() {
            if let Some(s) = FunctionSignature::from_type(&symbol.name, &symbol.typ) {
                if ll_mod.functions.contains_key(&symbol.name) {
                    continue;
                }
                ll_mod.imported_functions.push(ByteCodeFunction::new(&s, true));
            }
        }
    }


    Ok(ll_mod)
}
