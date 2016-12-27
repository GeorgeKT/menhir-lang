use ast::*;
use bytecode::*;

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

fn to_bc(func: &mut ByteCodeFunction, expr: &Expression) -> Var
{
    expr_to_bc(func, expr).expect("Expression must return a value")
}

fn expr_to_bc(func: &mut ByteCodeFunction, expr: &Expression) -> Option<Var>
{
    match *expr
    {
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

        /*
        Expression::NameRef(ref nr) => {
            name_ref_to_bc(func, nr)
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



        Expression::Lambda(ref l) => {
            let lambda = func_to_bc(&l.sig, &l.expr);
            func.lambdas.push(lambda);
            let dst = get_dst(func, &l.sig.get_type());
            add_set(func, ByteCodeExpression::Func(l.sig.name.clone()), &dst);
            Some(dst)
        },

        Expression::New(ref n) => {
            let dst = get_dst(func, &n.typ);
            let inner_type = n.typ.get_element_type().expect("Empty inner type of new expression");
            add_set(func, ByteCodeExpression::HeapAlloc(inner_type), &dst);
            expr_to_bc(func, &n.inner);
            Some(dst)
        },

        Expression::Delete(ref d) => {
            let to_delete = to_bc(func, &d.inner);
            func.add(Instruction::Delete(to_delete));
            None
        },
        Expression::ArrayGenerator(ref _a) => panic!("NYI"),
        */

        _ => {
            panic!("NYI");
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
