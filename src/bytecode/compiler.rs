use std::collections::HashMap;
use ast::*;
use bytecode::{ByteCodeModule, ByteCodeFunction};
use super::function::*;
use super::instruction::*;


fn stack_alloc(func: &mut ByteCodeFunction, typ: &Type, name: Option<&str>) -> Var
{
    match name
    {
        Some(n) => {
            let var = Var::named(n, typ.clone());
            func.add_named_var(var.clone());
            if *typ != Type::Void {
                func.add(Instruction::StackAlloc(var.clone()));
            }
            var
        },
        None => {
            let var = func.new_var(typ.clone());
            if *typ != Type::Void {
                func.add(Instruction::StackAlloc(var.clone()));
            }
            var
        }
    }
}

fn get_dst(func: &mut ByteCodeFunction, typ: &Type) -> Var
{
    assert!(*typ != Type::Unknown);
    if let Some(dst) = func.get_destination() {
        return dst;
    }

    stack_alloc(func, typ, None)
}

fn array_lit_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, a: &ArrayLiteral, dst: &Var)
{
    for (idx, element) in a.elements.iter().enumerate() {
        func.push_destination(None);
        let v = to_bc(bc_mod, func, element);
        func.pop_destination();
        func.add(store_member_instr(dst, idx, v));
    }
}

fn call_args_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, c: &Call, self_arg: Option<Var>) -> Vec<Operand>
{
    func.push_destination(None);
    let mut args = Vec::new();
    if let Some(s) = self_arg {
        args.push(Operand::Var(s));
    }

    args.extend(c.args.iter().map(|arg| Operand::Var(to_bc(bc_mod, func, arg))));
    func.pop_destination();
    args
}

fn call_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, c: &Call, self_arg: Option<Var>) -> Option<Var>
{
    if let Type::Void = c.return_type {
        let args = call_args_to_bc(bc_mod, func, c, self_arg);
        func.add(void_call_instr(&c.callee.name, args));
        None
    } else {
        let dst = get_dst(func, &c.return_type);
        let args = call_args_to_bc(bc_mod, func, c, self_arg);
        func.add(call_instr(&dst, &c.callee.name, args));
        Some(dst)
    }
}


fn struct_initializer_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, si: &StructInitializer, dst: &Var)
{
    let init_members = |bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, si: &StructInitializer, dst: &Var| {
        for (idx, expr) in si.member_initializers.iter().enumerate() {
            let v = to_bc(bc_mod, func, expr);
            func.add(store_member_instr(dst, idx, v));
        }
    };

    if let Type::Sum(ref st) = dst.typ {
        let idx = st.index_of(&si.struct_name).expect("Internal Compiler Error: cannot determine index of sum type case");
        func.add(set_prop_instr(dst, ByteCodeProperty::SumTypeIndex, idx));

        let struct_ptr = stack_alloc(func, &ptr_type(st.cases[idx].typ.clone()), None);
        func.add(load_member_instr(&struct_ptr, dst, idx));
        init_members(bc_mod, func, si, &struct_ptr);
    } else {
        init_members(bc_mod, func, si, dst);
    }
}

fn block_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Block) -> Option<Var>
{
    let do_block = |bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Block| {
        for (idx, e) in b.expressions.iter().enumerate() {
            if idx == b.expressions.len() - 1 {
                expr_to_bc(bc_mod, func, e);
            } else {
                func.push_destination(None);
                expr_to_bc(bc_mod, func, e);
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

fn add_struct_pattern_bindings(p: &StructPattern, struct_var: &Var, func: &mut ByteCodeFunction)
{
    for (idx, name) in p.bindings.iter().enumerate() {
        if name != "_" {
            let v = stack_alloc(func, &p.types[idx], Some(name));
            func.add(load_member_instr(&v, struct_var, idx));
            func.add_named_var(v);
        }
    }
}

fn add_binding(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, b: &Binding)
{
    match b.binding_type
    {
        BindingType::Name(ref name) => {
            let dst = stack_alloc(func, &b.typ, Some(name));
            func.push_destination(Some(dst));
            expr_to_bc(bc_mod, func, &b.init);
            func.pop_destination();
        },

        BindingType::Struct(ref s) => {
            let dst = stack_alloc(func, &b.typ, None);
            func.push_destination(Some(dst.clone()));
            expr_to_bc(bc_mod, func, &b.init);
            add_struct_pattern_bindings(s, &dst, func);
            func.pop_destination();
        },
    }
}

fn binding_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, l: &BindingExpression) -> Option<Var>
{
    let dst = get_dst(func, &l.typ);
    func.push_scope();
    for b in &l.bindings{
        add_binding(bc_mod, func, b);
    }

    func.push_destination(Some(dst.clone()));
    to_bc(bc_mod, func, &l.expression);
    func.pop_destination();
    func.pop_scope();
    Some(dst)
}


fn name_ref_to_bc(func: &mut ByteCodeFunction, nr: &NameRef) -> Option<Var>
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
                func.add(store_operand_instr(&dst, Operand::const_uint(idx)));
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

fn member_access_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, sma: &MemberAccess, dst: &Var)
{
    func.push_destination(None);
    let var = to_bc(bc_mod, func, &sma.left);
    func.pop_destination();

    let var_typ = if let Type::Pointer(ref inner) = var.typ {
        &inner
    } else {
        &var.typ
    };

    match (var_typ, &sma.right)
    {
        (&Type::Struct(_), &MemberAccessType::Name(ref field)) => {
            func.add(load_member_instr(&dst, &var, field.index));
        },

        (&Type::Array(ref at), &MemberAccessType::Property(Property::Len)) => {
            func.add(store_operand_instr(dst, Operand::const_uint(at.len)))
        },

        (&Type::String, &MemberAccessType::Property(Property::Len)) => {
            func.add(get_prop_instr(dst, &var, ByteCodeProperty::Len));
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
    nr: &NameRef)
{
    match nr.typ
    {
        Type::Enum(ref et) => {
            let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&target), Operand::const_uint(idx)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        Type::Sum(ref st) => {
            let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let sum_type_index = stack_alloc(func, &Type::UInt, None);
            func.add(get_prop_instr(&sum_type_index, target, ByteCodeProperty::SumTypeIndex));
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&sum_type_index), Operand::const_uint(idx)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
        }
    }

    match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
}


fn match_case_body_to_bc(
    bc_mod: &mut ByteCodeModule,
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    match_case_bb: BasicBlockRef,
    match_end_bb: BasicBlockRef,
    next_bb: BasicBlockRef,
    end_scope: bool)
{
    func.set_current_bb(match_case_bb);
    expr_to_bc(bc_mod, func, &mc.to_execute);
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
    next_bb: BasicBlockRef)
{
    let head_type = seq.typ.get_element_type().expect("Invalid array type");
    let head = stack_alloc(func, &head_type, Some(&ap.head));
    func.add(load_member_instr(&head, seq, 0));

    let tail = stack_alloc(func, &slice_type(head_type), Some(&ap.tail));
    let tail_len = stack_alloc(func, &Type::UInt, None);
    let seq_len = stack_alloc(func, &Type::UInt, None);
    func.add(get_prop_instr(&seq_len, seq, ByteCodeProperty::Len));
    func.add(binary_op_instr(&tail_len, BinaryOperator::Sub, var_op(&seq_len), Operand::const_uint(1)));
    func.add(slice_instr(&tail, seq, Operand::const_uint(1), var_op(&tail_len)));


    let length = stack_alloc(func, &Type::UInt, None);
    func.add(get_prop_instr(&length, seq, ByteCodeProperty::Len));
    let cond = stack_alloc(func, &Type::Bool, None);
    func.add(binary_op_instr(&cond, BinaryOperator::GreaterThan, var_op(&length), Operand::const_uint(0)));
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
    p: &StructPattern)
{
    func.push_destination(None);
    match p.typ
    {
        Type::Struct(_) => {
            func.add(Instruction::Branch(match_case_bb));
            func.set_current_bb(match_case_bb);

            func.push_scope();
            add_struct_pattern_bindings(p, target, func);
        },
        Type::Sum(ref st) => {
            let target_sum_type_index = stack_alloc(func, &Type::UInt, None);
            func.add(get_prop_instr(&target_sum_type_index, target, ByteCodeProperty::SumTypeIndex));
            let idx = st.index_of(&p.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&target_sum_type_index), Operand::const_uint(idx)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));

            func.set_current_bb(match_case_bb);

            func.push_scope();
            let struct_ptr = stack_alloc(func, &ptr_type(st.cases[idx].typ.clone()), None);
            func.add(address_of_member_instr(&struct_ptr, target, idx));

            add_struct_pattern_bindings(p, &struct_ptr, func);
        },
        _ => panic!("Internal Compiler Error: Expression is not a valid match pattern"),
    }

    func.pop_destination();
    match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, true);
}

fn match_case_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, mc: &MatchCase, target: &Var, match_end_bb: BasicBlockRef)
{
    let match_case_bb = func.create_basic_block();
    let next_bb = func.create_basic_block();

    let add_literal_case = |bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, op: Operand| {
        func.push_destination(None);
        let cond = stack_alloc(func, &Type::Bool, None);
        func.add(binary_op_instr(&cond, BinaryOperator::Equals, op, var_op(&target)));
        func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        func.pop_destination();
        match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
    };

    match mc.pattern
    {
        Pattern::Literal(Literal::Int(_, v)) => {
            add_literal_case(bc_mod, func, Operand::const_int(v));
        },

        Pattern::Literal(Literal::UInt(_, v)) => {
            add_literal_case(bc_mod, func, Operand::const_uint(v));
        },

        Pattern::Literal(Literal::Float(_, ref v)) => {
            add_literal_case(bc_mod, func, float_op(v));
        },

        Pattern::Literal(Literal::Bool(_, v)) => {
            add_literal_case(bc_mod, func, Operand::const_bool(v));
        },

        Pattern::Literal(Literal::Char(_, v)) => {
            add_literal_case(bc_mod, func, Operand::const_char(v));
        },

        Pattern::Name(ref nr) => {
            name_pattern_match_to_bc(bc_mod, func, mc, target, match_end_bb, match_case_bb, next_bb, nr)
        },

        Pattern::Any(_) => {
            func.add(Instruction::Branch(match_case_bb));
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
        },

        Pattern::EmptyArray(_) => {
            match target.typ
            {
                Type::Array(_) | Type::Slice(_) => {
                    let len = stack_alloc(func, &Type::UInt, None);
                    let cond = stack_alloc(func, &Type::Bool, None);
                    func.add(get_prop_instr(&len, target, ByteCodeProperty::Len));
                    func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&len), Operand::const_uint(0)));
                    func.add(branch_if_instr(&cond, match_case_bb, next_bb))
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an empty array pattern"),
            }

            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
        },

        Pattern::Array(ref ap) => {
            match target.typ
            {
                Type::Array(_) | Type::Slice(_) => {
                    func.push_destination(None);
                    array_pattern_match_to_bc(func, ap, target, match_case_bb, next_bb);
                    func.pop_destination();
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }

            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
        },

        Pattern::Literal(Literal::Array(ref a)) => {
            func.push_destination(None);
            let arr = func.new_var(a.array_type.clone());
            array_lit_to_bc(bc_mod, func, a, &arr);
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, var_op(&arr), var_op(&target)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
        },

        Pattern::Literal(Literal::String(_, ref s)) => {
            func.push_destination(None);
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, BinaryOperator::Equals, Operand::const_string(&s[..]), var_op(&target)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
        },

        Pattern::Struct(ref p) => {
            struct_pattern_match_to_bc(bc_mod, func, mc, target, match_end_bb, match_case_bb, next_bb, p);
        },

        Pattern::Nil(_) => {
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(is_nil_instr(&cond, &target));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, false);
        },

        Pattern::Optional(ref o) => {
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(is_nil_instr(&cond, &target));
            func.add(unary_op_instr(&cond, UnaryOperator::Not, var_op(&cond)));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));

            func.set_current_bb(match_case_bb);
            func.push_scope();

            let binding = stack_alloc(func, &o.inner_type, Some(&o.binding));
            func.add(load_instr(&binding, target));
            func.add_named_var(binding);
            match_case_body_to_bc(bc_mod, func, mc, match_case_bb, match_end_bb, next_bb, true);
        },
    }
}

fn match_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, m: &MatchExpression) -> Var
{
    func.push_destination(None);
    let target_var = to_bc(bc_mod, func, &m.target);
    func.pop_destination();
    let match_end_bb = func.create_basic_block();

    let dst = get_dst(func, &m.typ);
    func.push_scope();
    func.push_destination(Some(dst.clone()));
    for mc in &m.cases {
        match_case_to_bc(bc_mod, func, mc, &target_var, match_end_bb);
    }
    func.pop_destination();

    func.add(Instruction::Branch(match_end_bb));
    func.set_current_bb(match_end_bb);
    func.pop_scope();
    dst
}

fn while_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, w: &WhileLoop)
{
    let cond_bb = func.create_basic_block();
    let body_bb = func.create_basic_block();
    let post_while_bb = func.create_basic_block();

    func.add(Instruction::Branch(cond_bb));
    func.set_current_bb(cond_bb);
    let cond = to_bc(bc_mod, func, &w.cond);
    func.add(branch_if_instr(&cond, body_bb, post_while_bb));
    func.set_current_bb(body_bb);
    expr_to_bc(bc_mod, func, &w.body);
    func.add(Instruction::Branch(cond_bb));

    func.set_current_bb(post_while_bb);
}

fn for_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, f: &ForLoop)
{
    func.push_scope();
    func.push_destination(None);
    let iterable = to_bc(bc_mod, func, &f.iterable);
    func.pop_destination();

    let loop_variable = stack_alloc(func, &f.loop_variable_type, Some(&f.loop_variable));

    let index = stack_alloc(func, &Type::UInt, None);
    func.add(store_operand_instr(&index, Operand::const_uint(0)));


    let len = if let Type::Array(ref at) = iterable.typ {
        Operand::const_uint(at.len)
    } else {
        let len = stack_alloc(func, &Type::UInt, None);
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
    expr_to_bc(bc_mod, func, &f.body);
    func.pop_destination();
    func.add(binary_op_instr(&index, BinaryOperator::Add, var_op(&index), Operand::const_uint(1)));
    func.add(Instruction::Branch(cond_bb));

    func.set_current_bb(post_for_bb);
    func.pop_scope();
}

fn cast_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, c: &TypeCast) -> Var
{
    let dst = get_dst(func, &c.destination_type);
    func.push_destination(None);
    let inner = to_bc(bc_mod, func, &c.inner);
    func.pop_destination();
    func.add(cast_instr(&dst, &inner));
    dst
}

fn to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, expr: &Expression) -> Var
{
    expr_to_bc(bc_mod, func, expr).expect("Expression must return a value")
}

fn optional_compare_to_bc(
    func: &mut ByteCodeFunction,
    l: &Var,
    r: &Var,
    dst: &Var,
    equals: bool,
    inner_type: &Type)
{
    let l_is_nil_bb = func.create_basic_block();
    let l_is_not_nil_bb = func.create_basic_block();
    let set_to_true_bb = func.create_basic_block();
    let set_to_false_bb = func.create_basic_block();
    let compare_inner_bb = func.create_basic_block();
    let end_bb = func.create_basic_block();

    let l_is_nil = stack_alloc(func, &Type::Bool, None);
    let r_is_nil = stack_alloc(func, &Type::Bool, None);
    func.add(is_nil_instr(&l_is_nil, l));
    func.add(is_nil_instr(&r_is_nil, r));

    func.add(branch_if_instr(&l_is_nil, l_is_nil_bb, l_is_not_nil_bb));
    func.set_current_bb(l_is_nil_bb);
    func.add(branch_if_instr(&r_is_nil, set_to_true_bb, set_to_false_bb));

    func.set_current_bb(l_is_not_nil_bb);
    func.add(branch_if_instr(&r_is_nil, set_to_false_bb, compare_inner_bb));

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

fn binary_op_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, op: &BinaryOp) -> Var
{
    func.push_destination(None);
    let l = to_bc(bc_mod, func, &op.left);
    let r = to_bc(bc_mod, func, &op.right);
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
                let l_is_nil = stack_alloc(func, &Type::Bool, None);
                let true_bb = func.create_basic_block();
                let false_bb = func.create_basic_block();
                let end_bb = func.create_basic_block();
                func.add(is_nil_instr(&l_is_nil, &l));
                func.add(branch_if_instr(&l_is_nil, true_bb, false_bb));

                func.set_current_bb(true_bb);
                func.add(store_instr(&dst, &r));
                func.add(Instruction::Branch(end_bb));

                func.set_current_bb(false_bb);
                func.add(load_instr(&dst, &l));
                func.add(Instruction::Branch(end_bb));
                func.set_current_bb(end_bb);
            },

            _ => {
                panic!("Operator {} not supported on optioanl", op.operator);
            }
        },

        _ => {
            func.add(binary_op_instr(&dst, op.operator, var_op(&l), var_op(&r)));
        }
    }

    dst
}

fn expr_to_bc(bc_mod: &mut ByteCodeModule, func: &mut ByteCodeFunction, expr: &Expression) -> Option<Var>
{
    match *expr
    {
        Expression::Void => None,

        Expression::UnaryOp(ref u) => {
            func.push_destination(None);
            let v = to_bc(bc_mod, func, &u.expression);
            func.pop_destination();
            let dst = get_dst(func, &u.typ);
            func.add(unary_op_instr(&dst, u.operator, var_op(&v)));
            Some(dst)
        },

        Expression::BinaryOp(ref op) => {
            Some(binary_op_to_bc(bc_mod, func, op))
        },

        Expression::Literal(Literal::Int(_, v)) => {
            let dst = get_dst(func, &Type::Int);
            func.add(store_operand_instr(&dst, Operand::const_int(v)));
            Some(dst)
        },

        Expression::Literal(Literal::UInt(_, v)) => {
            let dst = get_dst(func, &Type::UInt);
            func.add(store_operand_instr(&dst, Operand::const_uint(v)));
            Some(dst)
        },

        Expression::Literal(Literal::Float(_, ref v_str)) => {
            let dst = get_dst(func, &Type::Float);
            func.add(store_operand_instr(&dst, float_op(v_str)));
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
            array_lit_to_bc(bc_mod, func, a, &dst);
            func.pop_destination();
            Some(dst)
        },

        Expression::Call(ref c) => {
            call_to_bc(bc_mod, func, c, None)
        },

        Expression::StructInitializer(ref si) => {
            let dst = get_dst(func, &si.typ);
            func.push_destination(None);
            struct_initializer_to_bc(bc_mod, func, si, &dst);
            func.pop_destination();
            Some(dst)
        },

        Expression::Block(ref b) => {
            block_to_bc(bc_mod, func, b)
        },

        Expression::Binding(ref l) => {
            binding_to_bc(bc_mod, func, l)
        },

        Expression::Bindings(ref l) => {
            for b in &l.bindings {
                add_binding(bc_mod, func, b);
            }
            None
        },

        Expression::New(ref n) => {
            let dst = get_dst(func, &n.typ);
            func.add(Instruction::HeapAlloc(dst.clone()));
            func.push_destination(Some(dst.clone()));
            expr_to_bc(bc_mod, func, &n.inner);
            func.pop_destination();
            Some(dst)
        },

        Expression::Delete(ref d) => {
            let to_delete = to_bc(bc_mod, func, &d.inner);
            func.add(Instruction::Delete(to_delete));
            None
        },

        Expression::Lambda(ref l) => {
            let lambda = func_to_bc(&l.sig, bc_mod, &l.expr);
            let dst = get_dst(func, &l.sig.get_type());
            func.add(store_func_instr(&dst, &lambda.sig.name));
            bc_mod.functions.insert(l.sig.name.clone(), lambda);
            Some(dst)
        },

        Expression::NameRef(ref nr) => {
            name_ref_to_bc(func, nr)
        },

        Expression::MemberAccess(ref sma) => {
            let dst = get_dst(func, &sma.typ);
            member_access_to_bc(bc_mod, func, sma, &dst);
            Some(dst)
        },

        Expression::Match(ref m) => {
            Some(match_to_bc(bc_mod, func, m))
        },

        Expression::If(ref i) => {
            let match_expr = i.to_match();
            Some(match_to_bc(bc_mod, func, &match_expr))
        },

        Expression::ArrayToSlice(ref ats) => {
            let dst = get_dst(func, &ats.slice_type);
            let array_var = to_bc(bc_mod, func, &ats.inner);
            let end = stack_alloc(func, &Type::UInt, None);
            func.add(get_prop_instr(&end, &array_var, ByteCodeProperty::Len));
            func.add(slice_instr(&dst, &array_var, Operand::const_uint(0), var_op(&end)));
            Some(dst)
        },

        Expression::AddressOf(ref a) => {
            let inner_var = to_bc(bc_mod, func, &a.inner);
            let dst = get_dst(func, &a.typ);
            func.add(address_of_instr(&dst, &inner_var));
            Some(dst)
        },

        Expression::Assign(ref a) => {
            func.push_destination(None);
            let r = to_bc(bc_mod, func, &a.right);
            let l = to_bc(bc_mod, func, &a.left);
            func.add(store_instr(&l, &r));
            func.pop_destination();
            Some(l)
        },

        Expression::While(ref w) => {
            while_to_bc(bc_mod, func, w);
            None
        },

        Expression::For(ref f) => {
            for_to_bc(bc_mod, func, f);
            None
        },

        Expression::Nil(ref nt) => {
            let dst = get_dst(func, &nt.typ);
            func.add(Instruction::StoreNil(dst.clone()));
            Some(dst)
        },

        Expression::ToOptional(ref t) => {
            let dst = get_dst(func, &t.optional_type);
            func.push_destination(None);
            let inner = to_bc(bc_mod, func, &t.inner);
            func.pop_destination();
            func.add(store_instr(&dst, &inner));
            Some(dst)
        },

        Expression::Cast(ref c) => {
            Some(cast_to_bc(bc_mod, func, c))
        },
    }
}

fn func_to_bc(sig: &FunctionSignature, bc_mod: &mut ByteCodeModule, expression: &Expression) -> ByteCodeFunction
{
    let mut llfunc = ByteCodeFunction::new(sig);
    match expr_to_bc(bc_mod, &mut llfunc, expression)
    {
        Some(var) => {
            llfunc.add(ret_instr(&var));
        },

        None => {
            llfunc.add(Instruction::ReturnVoid);
        }
    }

    llfunc
}

pub const START_CODE_FUNCTION : &'static str = "@start";

fn generate_start_code(md: &Module, bc_mod: &mut ByteCodeModule) -> ByteCodeFunction
{
    use span::Span;
    let sig = sig(START_CODE_FUNCTION, Type::Int, vec![], Span::default());
    let mut start_code = ByteCodeFunction::new(&sig);

    for global in md.globals.values() {
        let dst = Var::named(&global.name, global.typ.clone());
        start_code.add(Instruction::GlobalAlloc(dst.clone()));
        start_code.push_destination(Some(dst));
        expr_to_bc(bc_mod, &mut start_code, &global.init);
        start_code.pop_destination();
    }

    let result = stack_alloc(&mut start_code, &Type::Int, None);
    start_code.add(call_instr(&result, "main", vec![]));
    start_code.add(ret_instr(&result));
    start_code
}

pub fn compile_to_byte_code(md: &Module) -> ByteCodeModule
{
    let mut ll_mod = ByteCodeModule{
        name: md.name.clone(),
        functions: HashMap::new(),
        globals: HashMap::new(),
        exit_function: ByteCodeFunction::exit(),
    };

    for func in md.externals.values() {
        ll_mod.functions.insert(func.sig.name.clone(), ByteCodeFunction::new(&func.sig));
    }

    let start_code = generate_start_code(md, &mut ll_mod);
    ll_mod.functions.insert(START_CODE_FUNCTION.to_string(), start_code);

    for func in md.functions.values() {
        if !func.is_generic() {
            let new_func = func_to_bc(&func.sig, &mut ll_mod, &func.expression);
            ll_mod.functions.insert(func.sig.name.clone(), new_func);
        }
    }

    ll_mod
}
