use ast::*;
use bytecode::*;
use parser::Operator;

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
        if dst.typ != *typ {
            panic!("Internal compiler error: dst.typ ({}) != *typ ({}) in get_dst", dst.typ, typ);
        }
        return dst;
    }

    stack_alloc(func, typ, None)
}

fn array_lit_to_bc(func: &mut ByteCodeFunction, a: &ArrayLiteral, dst: &Var)
{
    let vars = a.elements.iter()
        .map(|e| to_bc(func, e))
        .collect();

    func.add(store_lit_instr(dst, ByteCodeLiteral::Array(vars)));
}

fn call_to_bc(func: &mut ByteCodeFunction, c: &Call, self_arg: Option<Var>) -> Var
{
    let dst = get_dst(func, &c.return_type);
    func.push_destination(None);
    let mut args = Vec::new();
    if let Some(s) = self_arg {
        args.push(s);
    }

    args.extend(c.args.iter().map(|arg| to_bc(func, arg)));
    func.pop_destination();
    func.add(call_instr(&dst, &c.callee.name, args));
    dst
}


fn struct_initializer_to_bc(func: &mut ByteCodeFunction, si: &StructInitializer, dst: &Var)
{
    let init_members = |func: &mut ByteCodeFunction, si: &StructInitializer, dst: &Var| {
        for (idx, expr) in si.member_initializers.iter().enumerate() {
            let v = to_bc(func, expr);
            let member_ptr = get_dst(func, &ptr_type(v.typ.clone()));
            func.add(load_member_instr(&member_ptr, dst, idx));
            func.add(store_instr(&member_ptr, &v));
        }
    };

    if let Type::Sum(ref st) = dst.typ {
        let idx = st.index_of(&si.struct_name).expect("Internal Compiler Error: cannot determine index of sum type case");
        func.add(set_prop_instr(dst, ByteCodeProperty::SumTypeIndex, idx));

        let struct_ptr = func.new_var(ptr_type(st.cases[idx].typ.clone()));
        func.add(load_member_instr(&struct_ptr, &dst, idx));
        init_members(func, si, &struct_ptr);
    } else {
        init_members(func, si, dst);
    }
}

fn block_to_bc(func: &mut ByteCodeFunction, b: &Block) -> Option<Var>
{
    let do_block = |func: &mut ByteCodeFunction, b: &Block| {
        for (idx, e) in b.expressions.iter().enumerate() {
            if idx == b.expressions.len() - 1 {
                expr_to_bc(func, e);
            } else {
                func.push_destination(None);
                expr_to_bc(func, e);
                func.pop_destination();
            }
        }
    };


    if b.typ != Type::Void {
        let dst = get_dst(func, &b.typ);
        func.push_destination(Some(dst.clone()));
        do_block(func, b);
        func.pop_destination();
        Some(dst)
    } else {
        func.push_destination(None);
        do_block(func, b);
        func.pop_destination();
        None
    }
}

fn add_struct_pattern_bindings(p: &StructPattern, struct_var: &Var, func: &mut ByteCodeFunction)
{
    for (idx, name) in p.bindings.iter().enumerate() {
        if name != "_" {
            let v = Var::named(name, ptr_type(p.types[idx].clone()));
            func.add(load_member_instr(&v, struct_var, idx));
            func.add_named_var(v);
        }
    }
}

fn add_binding(func: &mut ByteCodeFunction, b: &LetBinding)
{
    match b.binding_type
    {
        LetBindingType::Name(ref name) => {
            let dst = stack_alloc(func, &b.typ, Some(name));
            func.push_destination(Some(dst));
            expr_to_bc(func, &b.init);
            func.pop_destination();
        },

        LetBindingType::Struct(ref s) => {
            let dst = stack_alloc(func, &b.typ, None);
            func.push_destination(Some(dst.clone()));
            expr_to_bc(func, &b.init);
            add_struct_pattern_bindings(s, &dst, func);
            func.pop_destination();
        },
    }
}

fn let_to_bc(func: &mut ByteCodeFunction, l: &LetExpression) -> Option<Var>
{
    let dst = get_dst(func, &l.typ);
    func.push_scope();
    for b in &l.bindings{
        add_binding(func, b);
    }

    func.push_destination(Some(dst.clone()));
    to_bc(func, &l.expression);
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
                assert!(var.typ == v.typ);
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
                func.add(store_lit_instr(&dst, ByteCodeLiteral::Int(idx as u64)));
                Some(dst)
            } else {
                add_name_ref(func, nr)
            }
        },
        _ => {
            add_name_ref(func, nr)
        }
    }
}

fn member_access_to_bc(func: &mut ByteCodeFunction, sma: &MemberAccess, dst: &Var)
{
    func.push_destination(None);
    let var = to_bc(func, &sma.left);
    func.pop_destination();

    let var_typ = if let Type::Pointer(ref inner) = var.typ {
        &inner
    } else {
        &var.typ
    };

    match (var_typ, &sma.right)
    {
        (&Type::Struct(_), &MemberAccessType::Name(ref field)) => {

            func.add(load_member_instr(dst, &var, field.index));
        },

        (&Type::Array(_), &MemberAccessType::Property(Property::Len)) => {
            func.add(get_prop_instr(dst, &var, ByteCodeProperty::Len));
        },

        _ => {
            panic!("Internal Compiler Error: Invalid member access")
        },
    }
}

fn make_lit(func: &mut ByteCodeFunction, lit: ByteCodeLiteral, typ: Type) -> Var
{
    let var = stack_alloc(func, &typ, None);
    func.add(store_lit_instr(&var, lit));
    var
}

fn name_pattern_match_to_bc(
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
            let cv = make_lit(func, ByteCodeLiteral::Int(idx as u64), Type::Int);
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, Operator::Equals, target.clone(), cv));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        Type::Sum(ref st) => {
            let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cv = make_lit(func, ByteCodeLiteral::Int(idx as u64), Type::Int);
            let sum_type_index = stack_alloc(func, &Type::Int, None);
            func.add(get_prop_instr(&sum_type_index, &target, ByteCodeProperty::SumTypeIndex));
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, Operator::Equals, sum_type_index, cv));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        },
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
        }
    }

    match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
}


fn match_case_body_to_bc(
    func: &mut ByteCodeFunction,
    mc: &MatchCase,
    match_case_bb: BasicBlockRef,
    match_end_bb: BasicBlockRef,
    next_bb: BasicBlockRef)
{
    func.set_current_bb(match_case_bb);
    expr_to_bc(func, &mc.to_execute);
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
    let head_type = ptr_type(seq.typ.get_element_type().expect("Invalid array type"));
    let head = stack_alloc(func, &head_type, Some(&ap.head));
    func.add(load_member_instr(&head, seq, 0));

    let tail = stack_alloc(func, &slice_type(head_type.clone()), Some(&ap.tail));
    let tail_start = make_lit(func, ByteCodeLiteral::Int(1), Type::Int);
    let tail_len = stack_alloc(func, &Type::Int, None);
    let seq_len = stack_alloc(func, &Type::Int, None);
    func.add(get_prop_instr(&seq_len, seq, ByteCodeProperty::Len));
    func.add(binary_op_instr(&tail_len, Operator::Sub, seq_len, tail_start.clone()));
    func.add(slice_instr(&tail, seq, tail_start, tail_len));


    let length = stack_alloc(func, &Type::Int, None);
    func.add(get_prop_instr(&length, seq, ByteCodeProperty::Len));
    let zero = make_lit(func, ByteCodeLiteral::Int(0), Type::Int);
    let cond = stack_alloc(func, &Type::Bool, None);
    func.add(binary_op_instr(&cond, Operator::GreaterThan, length, zero));
    func.add(branch_if_instr(&cond, match_case_bb, next_bb));
}

fn struct_pattern_match_to_bc(
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
            add_struct_pattern_bindings(p, target, func);
        },
        Type::Sum(ref st) => {
            let target_sum_type_index = stack_alloc(func, &Type::Int, None);
            func.add(get_prop_instr(&target_sum_type_index, &target, ByteCodeProperty::Len));
            let idx = st.index_of(&p.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let sum_type_index = make_lit(func, ByteCodeLiteral::Int(idx as u64), Type::Int);
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, Operator::Equals, target_sum_type_index, sum_type_index));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));

            func.set_current_bb(match_case_bb);
            let struct_ptr = stack_alloc(func, &ptr_type(st.cases[idx].typ.clone()), None);
            func.add(load_member_instr(&struct_ptr, &target, idx));
            add_struct_pattern_bindings(p, &struct_ptr, func);
        },
        _ => panic!("Internal Compiler Error: Expression is not a valid match pattern"),
    }

    func.pop_destination();
    match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
}

fn match_case_to_bc(func: &mut ByteCodeFunction, mc: &MatchCase, target: &Var, match_end_bb: BasicBlockRef)
{
    let match_case_bb = func.create_basic_block();
    func.add_basic_block(match_case_bb);
    let next_bb = func.create_basic_block();
    func.add_basic_block(next_bb);

    let add_literal_case = |func: &mut ByteCodeFunction, lit: ByteCodeLiteral, typ: Type| {
        func.push_destination(None);
        let iv = make_lit(func, lit, typ);
        let cond = stack_alloc(func, &Type::Bool, None);
        func.add(binary_op_instr(&cond, Operator::Equals, iv, target.clone()));
        func.add(branch_if_instr(&cond, match_case_bb, next_bb));
        func.pop_destination();
        match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
    };

    match mc.pattern
    {
        Pattern::Literal(Literal::Int(_, v)) => {
            add_literal_case(func, ByteCodeLiteral::Int(v), Type::Int);
        },

        Pattern::Literal(Literal::Float(_, ref v)) => {
            add_literal_case(func, ByteCodeLiteral::Float(v.clone()), Type::Float);
        },

        Pattern::Literal(Literal::Bool(_, v)) => {
            add_literal_case(func, ByteCodeLiteral::Bool(v), Type::Bool);
        },

        Pattern::Literal(Literal::Char(_, v)) => {
            add_literal_case(func, ByteCodeLiteral::Char(v), Type::Char);
        },

        Pattern::Name(ref nr) => {
            name_pattern_match_to_bc(func, mc, target, match_end_bb, match_case_bb, next_bb, nr)
        },

        Pattern::Any(_) => {
            func.add(Instruction::Branch(match_case_bb));
            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::EmptyArray(_) => {
            match target.typ
            {
                Type::Array(_) | Type::Slice(_) => {
                    let len = stack_alloc(func, &Type::Int, None);
                    let zero = make_lit(func, ByteCodeLiteral::Int(0), Type::Int);
                    let cond = stack_alloc(func, &Type::Bool, None);
                    func.add(get_prop_instr(&len, &target, ByteCodeProperty::Len));
                    func.add(binary_op_instr(&cond, Operator::Equals, len, zero));
                    func.add(branch_if_instr(&cond, match_case_bb, next_bb))
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an empty array pattern"),
            }

            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
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

            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Literal(Literal::Array(ref a)) => {
            func.push_destination(None);
            let arr = func.new_var(a.array_type.clone());
            array_lit_to_bc(func, a, &arr);
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, Operator::Equals, arr, target.clone()));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Literal(Literal::String(_, ref s)) => {
            func.push_destination(None);
            let arr = make_lit(func, ByteCodeLiteral::String(s.clone()), string_type());
            let cond = stack_alloc(func, &Type::Bool, None);
            func.add(binary_op_instr(&cond, Operator::Equals, arr, target.clone()));
            func.add(branch_if_instr(&cond, match_case_bb, next_bb));
            func.pop_destination();
            match_case_body_to_bc(func, mc, match_case_bb, match_end_bb, next_bb);
        },

        Pattern::Struct(ref p) => {
            struct_pattern_match_to_bc(func, mc, target, match_end_bb, match_case_bb, next_bb, p);
        }
    }
}

fn match_to_bc(func: &mut ByteCodeFunction, m: &MatchExpression) -> Var
{
    func.push_destination(None);
    let target_var = to_bc(func, &m.target);
    func.pop_destination();
    let match_end_bb = func.create_basic_block();

    let dst = get_dst(func, &m.typ);
    func.push_scope();
    func.push_destination(Some(dst.clone()));
    for mc in &m.cases {
        match_case_to_bc(func, mc, &target_var, match_end_bb);
    }
    func.pop_destination();

    func.add(Instruction::Branch(match_end_bb));
    func.add_basic_block(match_end_bb);
    func.set_current_bb(match_end_bb);
    func.pop_scope();
    dst
}

fn to_bc(func: &mut ByteCodeFunction, expr: &Expression) -> Var
{
    expr_to_bc(func, expr).expect("Expression must return a value")
}

fn expr_to_bc(func: &mut ByteCodeFunction, expr: &Expression) -> Option<Var>
{
    match *expr
    {
        Expression::Void => None,
        
        Expression::UnaryOp(ref u) => {
            func.push_destination(None);
            let v = to_bc(func, &u.expression);
            func.pop_destination();
            let dst = get_dst(func, &u.typ);
            func.add(unary_op_instr(&dst, u.operator, v));
            Some(dst)
        },

        Expression::BinaryOp(ref op) => {
            func.push_destination(None);
            let l = to_bc(func, &op.left);
            let r = to_bc(func, &op.right);
            func.pop_destination();
            let dst = get_dst(func, &op.typ);
            func.add(binary_op_instr(&dst, op.operator, l, r));
            Some(dst)
        },

        Expression::Literal(Literal::Int(_, v)) => {
            let dst = get_dst(func, &Type::Int);
            func.add(store_lit_instr(&dst, ByteCodeLiteral::Int(v)));
            Some(dst)
        },

        Expression::Literal(Literal::Float(_, ref v_str)) => {
            let dst = get_dst(func, &Type::Float);
            func.add(store_lit_instr(&dst, ByteCodeLiteral::Float(v_str.clone())));
            Some(dst)
        },

        Expression::Literal(Literal::String(_, ref s))  => {
            let dst = get_dst(func, &string_type());
            func.add(store_lit_instr(&dst, ByteCodeLiteral::String(s.clone())));
            Some(dst)
        },

        Expression::Literal(Literal::Bool(_, v)) => {
            let dst = get_dst(func, &Type::Bool);
            func.add(store_lit_instr(&dst, ByteCodeLiteral::Bool(v)));
            Some(dst)
        },

        Expression::Literal(Literal::Char(_, v)) => {
            let dst = get_dst(func, &Type::Char);
            func.add(store_lit_instr(&dst, ByteCodeLiteral::Char(v)));
            Some(dst)
        },

        Expression::Literal(Literal::Array(ref a)) => {
            let dst = get_dst(func, &a.array_type);
            func.push_destination(None);
            array_lit_to_bc(func, a, &dst);
            func.pop_destination();
            Some(dst)
        },

        Expression::Call(ref c) => {
            Some(call_to_bc(func, c, None))
        },

        Expression::StructInitializer(ref si) => {
            let dst = get_dst(func, &si.typ);
            func.push_destination(None);
            struct_initializer_to_bc(func, si, &dst);
            func.pop_destination();
            Some(dst)
        },

        Expression::Block(ref b) => {
            block_to_bc(func, b)
        },

        Expression::Let(ref l) => {
            let_to_bc(func, l)
        },

        Expression::LetBindings(ref l) => {
            for b in &l.bindings {
                add_binding(func, b);
            }
            None
        },

        Expression::New(ref n) => {
            let dst = get_dst(func, &n.typ);
            func.add(Instruction::HeapAlloc(dst.clone()));
            func.push_destination(Some(dst.clone()));
            expr_to_bc(func, &n.inner);
            func.pop_destination();
            Some(dst)
        },

        Expression::Delete(ref d) => {
            let to_delete = to_bc(func, &d.inner);
            func.add(Instruction::Delete(to_delete));
            None
        },

        Expression::Lambda(ref l) => {
            let lambda = func_to_bc(&l.sig, &l.expr);
            func.lambdas.push(lambda);
            let dst = get_dst(func, &l.sig.get_type());
            func.add(store_func_instr(&dst, &l.sig.name));
            Some(dst)
        },

        Expression::NameRef(ref nr) => {
            name_ref_to_bc(func, nr)
        },

        Expression::MemberAccess(ref sma) => {
            let dst = get_dst(func, &sma.typ);
            member_access_to_bc(func, sma, &dst);
            Some(dst)
        },

        Expression::Match(ref m) => {
            Some(match_to_bc(func, m))
        },

        Expression::If(ref i) => {
            let match_expr = i.to_match();
            Some(match_to_bc(func, &match_expr))
        },
    }
}

fn func_to_bc(sig: &FunctionSignature, expression: &Expression) -> ByteCodeFunction
{
    let mut llfunc = ByteCodeFunction::new(&sig);
    match expr_to_bc(&mut llfunc, &expression)
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

pub fn compile_to_byte_code(md: &Module) -> ByteCodeModule
{
    let mut ll_mod = ByteCodeModule{
        name: md.name.clone(),
        functions: Vec::new(),
    };

    for func in md.externals.values() {
        ll_mod.functions.push(ByteCodeFunction::new(&func.sig));
    }

    for func in md.functions.values() {
        if !func.is_generic() {
            ll_mod.functions.push(func_to_bc(&func.sig, &func.expression));
        }
    }

    ll_mod
}