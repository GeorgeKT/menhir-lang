mod llfunction;
mod llinstruction;

use std::fmt;
use ast::*;
use parser::Operator;
pub use self::llfunction::{LLFunction, LLVar, LLBasicBlockRef};
pub use self::llinstruction::*;


pub struct LLModule
{
    pub name: String,
    pub functions: Vec<LLFunction>,
}

impl fmt::Display for LLModule
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        for func in &self.functions {
            try!(func.fmt(f));
            try!(writeln!(f, " "));
        }
        Ok(())
    }
}

fn make_var(func: &mut LLFunction, expr: LLExpr, typ: Type) -> LLVar
{
    let var = func.new_var(typ);
    func.add(set_instr(var.clone(), expr));
    var
}

fn make_lit(func: &mut LLFunction, lit: LLLiteral, typ: Type) -> LLVar
{
    make_var(func, LLExpr::Literal(lit), typ)
}

fn bind(func: &mut LLFunction, name: &str, var: LLVar)
{
    func.add(bind_instr(name, var))
}

fn call_to_llrep(func: &mut LLFunction, c: &Call) -> LLVar
{
    let var = func.new_var(c.return_type.clone());
    let args = c.args.iter().map(|arg| expr_to_llrep(func, arg).expect("Expression must return a var")).collect();
    func.add(set_instr(
        var.clone(),
        LLExpr::Call(
            c.callee.name.clone(),
            args,
        )
    ));
    var
}

fn name_ref_to_llrep(nr: &NameRef) -> LLVar
{
    LLVar::named(&nr.name, nr.typ.clone())
}


fn add_binding(func: &mut LLFunction, b: &LetBinding)
{
    let e = expr_to_llrep(func, &b.init).expect("Expression must return a var");
    bind(func, &b.name, e);
    func.add_named_var(LLVar::named(&b.name, b.typ.clone()));
}

fn let_to_llrep(func: &mut LLFunction, l: &LetExpression) -> LLVar
{
    for b in &l.bindings{
        add_binding(func, b);
    }

    func.add(LLInstruction::StartScope);
    func.push_scope();
    let result = expr_to_llrep(func, &l.expression).expect("Expression must return a var");
    func.pop_scope();
    func.add(LLInstruction::EndScope(result.clone()));
    result
}

fn array_lit_to_llrep(func: &mut LLFunction, a: &ArrayLiteral) -> LLVar
{
    let vars = a.elements.iter()
        .map(|e| expr_to_llrep(func, e).expect("Expression must return a var"))
        .collect();

    make_lit(func, LLLiteral::Array(vars), a.array_type.clone())
}

fn struct_initializer_to_llrep(func: &mut LLFunction, si: &StructInitializer) -> LLVar
{
    let var = func.new_var(si.typ.clone());
    func.add(LLInstruction::StackAlloc(var.clone()));
    for (idx, expr) in si.member_initializers.iter().enumerate() {
        let v = expr_to_llrep(func, expr).expect("Expression must return a var");
        func.add(set_struct_member_instr(var.clone(), idx, v));
    }

    var
}

fn make_array_len(func: &mut LLFunction, array: LLVar) -> LLVar
{
    let expr = LLExpr::ArrayProperty(array, ArrayProperty::Len);
    make_var(func, expr, Type::Int)
}

fn member_access_to_llrep(func: &mut LLFunction, sma: &MemberAccess) -> LLVar
{
    let mut var = func.get_named_var(&sma.name).expect("Internal Compiler Error: Unknown variable");
    for at in &sma.access_types
    {
        var = match (&var.typ, at)
        {
            (&Type::Struct(ref st), &MemberAccessType::StructMember(idx)) => {
                let expr = LLExpr::StructMember(var.clone(), idx);
                make_var(func, expr, st.members[idx].typ.clone())
            },

            (&Type::Array(_), &MemberAccessType::ArrayProperty(ArrayProperty::Len)) => {
                make_array_len(func, var.clone())
            },

            _ => panic!("Internal Compiler Error: Invalid member access"),
        };
    }
    var
}


fn name_pattern_match_to_llrep(
    func: &mut LLFunction,
    mc: &MatchCase,
    target: &LLVar,
    match_end_bb: LLBasicBlockRef,
    match_case_bb: LLBasicBlockRef,
    next_bb: LLBasicBlockRef,
    dst: &LLVar,
    nr: &NameRef)
{
    match nr.typ
    {
        Type::Enum(ref et) => {
            let idx = et.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cv = make_lit(func, LLLiteral::Int(idx as u64), Type::Int);
            let cond = make_var(func, LLExpr::EQ(target.clone(), cv), Type::Bool);
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
        },
        Type::Sum(ref st) => {
            let idx = st.index_of(&nr.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let cv = make_lit(func, LLLiteral::Int(idx as u64), Type::Int);
            let sum_type_index = make_var(func, LLExpr::SumTypeIndex(target.clone()), Type::Int);
            let cond = make_var(func,  LLExpr::EQ(sum_type_index, cv), Type::Bool);
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
        },
        _ => {
            panic!("Internal Compiler Error: Expression is not a valid match pattern");
        }
    }

    match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
}


fn match_case_body_to_llrep(
    func: &mut LLFunction,
    mc: &MatchCase,
    match_case_bb: LLBasicBlockRef,
    match_end_bb: LLBasicBlockRef,
    next_bb: LLBasicBlockRef,
    dst: &LLVar)
{
    func.set_current_bb(match_case_bb);
    let ret = expr_to_llrep(func, &mc.to_execute).expect("Expression must return a var");
    func.rename(&ret.name, &dst.name);
    func.add(LLInstruction::Branch(match_end_bb));
    func.set_current_bb(next_bb);
}

fn array_pattern_match_to_llrep(
    func: &mut LLFunction,
    ap: &ArrayPattern,
    seq: &LLVar,
    match_case_bb: LLBasicBlockRef,
    next_bb: LLBasicBlockRef)
{
    let head = make_var(func, LLExpr::ArrayHead(seq.clone()), seq.typ.get_element_type().expect("Invalid array type"));
    bind(func, &ap.head, head);
    let tail = make_var(func, LLExpr::ArrayTail(seq.clone()), seq.typ.clone());
    bind(func, &ap.tail, tail);

    let length = make_array_len(func, seq.clone());
    let zero = make_lit(func, LLLiteral::Int(0), Type::Int);
    let cond = make_var(func, LLExpr::GT(length, zero), Type::Bool);
    func.add(branch_if_instr(cond, match_case_bb, next_bb));
}

fn struct_pattern_match_to_llrep(
    func: &mut LLFunction,
    mc: &MatchCase,
    target: &LLVar,
    match_end_bb: LLBasicBlockRef,
    match_case_bb: LLBasicBlockRef,
    next_bb: LLBasicBlockRef,
    dst: &LLVar,
    p: &StructPattern)
{
    func.push_scope();

    let add_bindings = |var: &LLVar, func: &mut LLFunction| {
        for (idx, b) in p.bindings.iter().enumerate() {
            if b != "_" {
                let expr = LLExpr::StructMember(var.clone(), idx);
                let member_ptr = make_var(func, expr, p.types[idx].clone());
                bind(func, b, member_ptr);
            }
        }
    };

    match p.typ
    {
        Type::Struct(_) => {
            func.add(LLInstruction::Branch(match_case_bb));
            func.set_current_bb(match_case_bb);
            add_bindings(target, func);
        },
        Type::Sum(ref st) => {
            let target_sum_type_index = make_var(func, LLExpr::SumTypeIndex(target.clone()), Type::Int);
            let idx = st.index_of(&p.name).expect("Internal Compiler Error: cannot determine index of sum type case");
            let sum_type_index = make_lit(func, LLLiteral::Int(idx as u64), Type::Int);
            let cond = make_var(func, LLExpr::EQ(target_sum_type_index, sum_type_index), Type::Bool);
            func.add(branch_if_instr(cond, match_case_bb, next_bb));

            let struct_ptr = make_var(func, LLExpr::SumTypeStruct(target.clone(), idx), st.cases[idx].typ.clone());
            add_bindings(&struct_ptr, func);
        },
        _ => panic!("Internal Compiler Error: Expression is not a valid match pattern"),
    }

    match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
    func.pop_scope();
}

fn match_case_to_llrep(func: &mut LLFunction, mc: &MatchCase, target: &LLVar, match_end_bb: LLBasicBlockRef, dst: &LLVar)
{
    let match_case_bb = func.create_basic_block();
    func.add_basic_block(match_case_bb);
    let next_bb = func.create_basic_block();
    func.add_basic_block(next_bb);

    let add_literal_case = |func: &mut LLFunction, lit: LLLiteral, typ: Type| {
        let iv = make_lit(func, lit, typ);
        let cond = make_var(func, LLExpr::EQ(iv, target.clone()), Type::Bool);
        func.add(branch_if_instr(cond, match_case_bb, next_bb));
        match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
    };

    match mc.pattern
    {
        Pattern::Literal(Literal::Int(_, v)) => {
            add_literal_case(func, LLLiteral::Int(v), Type::Int);
        },

        Pattern::Literal(Literal::Float(_, ref v)) => {
            add_literal_case(func, LLLiteral::Float(v.clone()), Type::Float);
        },

        Pattern::Literal(Literal::Bool(_, v)) => {
            add_literal_case(func, LLLiteral::Bool(v), Type::Bool);
        },

        Pattern::Literal(Literal::Char(_, v)) => {
            add_literal_case(func, LLLiteral::Char(v), Type::Char);
        },

        Pattern::Name(ref nr) => {
            name_pattern_match_to_llrep(func, mc, target, match_end_bb, match_case_bb, next_bb, dst, nr)
        },

        Pattern::Any(_) => {
            func.add(LLInstruction::Branch(match_case_bb));
            match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
        },

        Pattern::EmptyArray(_) => {
            match target.typ
            {
                Type::Array(_) => {
                    let length = make_array_len(func, target.clone());
                    let zero = make_lit(func, LLLiteral::Int(0), Type::Int);
                    let cond = make_var(func, LLExpr::EQ(length, zero), Type::Bool);
                    func.add(branch_if_instr(cond, match_case_bb, next_bb));
                    match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
                },
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }
        },

        Pattern::Array(ref ap) => {
            func.push_scope();
            match target.typ
            {
                Type::Array(_) => array_pattern_match_to_llrep(func, ap, target, match_case_bb, next_bb),
                _ => panic!("Internal Compiler Error: Match expression cannot be matched with an array pattern"),
            }

            match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
            func.pop_scope();
        },

        Pattern::Literal(Literal::Array(ref a)) => {
            let arr = array_lit_to_llrep(func, a);
            let cond = make_var(func, LLExpr::EQ(arr, target.clone()), Type::Bool);
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
            match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
        },

        Pattern::Literal(Literal::String(_, ref s)) => {
            let arr = make_lit(func, LLLiteral::String(s.clone()), string_type());
            let cond = make_var(func, LLExpr::EQ(arr, target.clone()), Type::Bool);
            func.add(branch_if_instr(cond, match_case_bb, next_bb));
            match_case_body_to_llrep(func, mc, match_case_bb, match_end_bb, next_bb, dst);
        },

        Pattern::Struct(ref p) => {
            struct_pattern_match_to_llrep(func, mc, target, match_end_bb, match_case_bb, next_bb, dst, p);
        }
    }
}

fn match_to_llrep(func: &mut LLFunction, m: &MatchExpression) -> LLVar
{
    let target_var = expr_to_llrep(func, &m.target).expect("Expression must return a var");
    let match_end_bb = func.create_basic_block();
    let dst = func.new_var(m.typ.clone());

    for mc in &m.cases {
        match_case_to_llrep(func, mc, &target_var, match_end_bb, &dst);
    }

    func.add(LLInstruction::Branch(match_end_bb));
    func.add_basic_block(match_end_bb);
    func.set_current_bb(match_end_bb);
    dst
}

fn expr_to_llrep(func: &mut LLFunction, expr: &Expression) -> Option<LLVar>
{
    match *expr
    {
        Expression::UnaryOp(ref u) => {
            let v = expr_to_llrep(func, &u.expression).expect("Expression must return a var");
            let var = func.new_var(u.typ.clone());
            match u.operator
            {
                Operator::Sub => {
                    func.add(set_instr(var.clone(), LLExpr::USub(v)));
                },
                Operator::Not => {
                    func.add(set_instr(var.clone(), LLExpr::Not(v)));
                },
                _ => panic!("Internal Compiler Error: Invalid unary operator {}", u.operator),
            }

            Some(var)
        },

        Expression::BinaryOp(ref op) => {
            let l = expr_to_llrep(func, &op.left).expect("Expression must return a var");
            let r = expr_to_llrep(func, &op.right).expect("Expression must return a var");
            let var = func.new_var(op.typ.clone());
            let llexpr = match op.operator
            {
                Operator::Add => LLExpr::Add(l, r),
                Operator::Sub => LLExpr::Sub(l, r),
                Operator::Mul => LLExpr::Mul(l, r),
                Operator::Div => LLExpr::Div(l, r),
                Operator::Mod => LLExpr::Mod(l, r),
                Operator::LessThan => LLExpr::LT(l, r),
                Operator::LessThanEquals => LLExpr::LTE(l, r),
                Operator::GreaterThan => LLExpr::GT(l, r),
                Operator::GreaterThanEquals => LLExpr::GTE(l, r),
                Operator::Equals => LLExpr::EQ(l, r),
                Operator::NotEquals => LLExpr::NEQ(l, r),
                Operator::And => LLExpr::And(l, r),
                Operator::Or => LLExpr::Or(l, r),
                _ => panic!("Internal Compiler Error: Invalid binary operator {}", op.operator),
            };

            func.add(set_instr(var.clone(), llexpr));
            Some(var)
        },

        Expression::Literal(Literal::Int(_, v)) => {
            let var = make_lit(func, LLLiteral::Int(v), Type::Int);
            Some(var)
        },

        Expression::Literal(Literal::Float(_, ref v_str)) => {
            let var = make_lit(func, LLLiteral::Float(v_str.clone()), Type::Float);
            Some(var)
        },

        Expression::Literal(Literal::String(_, ref s))  => {
            let var = make_lit(func, LLLiteral::String(s.clone()), string_type());
            Some(var)
        },

        Expression::Literal(Literal::Bool(_, v)) => {
            let var = make_lit(func, LLLiteral::Bool(v), Type::Bool);
            Some(var)
        },

        Expression::Literal(Literal::Char(_, v)) => {
            let var = make_lit(func, LLLiteral::Char(v), Type::Char);
            Some(var)
        },

        Expression::Literal(Literal::Array(ref a)) => {
            Some(array_lit_to_llrep(func, a))
        },

        Expression::Call(ref c) => {
            Some(call_to_llrep(func, c))
        },

        Expression::NameRef(ref nr) => {
            Some(name_ref_to_llrep(nr))
        },

        Expression::Let(ref l) => {
            Some(let_to_llrep(func, l))
        },

        Expression::LetBindings(ref l) => {
            for b in &l.bindings {
                add_binding(func, b);
            }
            None
        },

        Expression::StructInitializer(ref si) => {
            Some(struct_initializer_to_llrep(func, si))
        },

        Expression::MemberAccess(ref sma) => {
            Some(member_access_to_llrep(func, sma))
        },

        Expression::Match(ref m) => {
            Some(match_to_llrep(func, m))
        },

        Expression::If(ref i) => {
            let match_expr = i.to_match();
            Some(match_to_llrep(func, &match_expr))
        },

        /*
        Expression::ArrayGenerator(ref _a) => panic!("NYI"),
        Expression::NameRef(ref nr) => gen_name_ref(ctx, nr),
        Expression::Lambda(ref l) => gen_lambda(ctx, l),
        Expression::Block(ref b) => gen_block(ctx, b),
        */

        _ => panic!("NYI"),
    }
}

fn func_to_llrep(func: &Function) -> LLFunction
{
    let mut llfunc = LLFunction::new(&func.sig);
    let var = expr_to_llrep(&mut llfunc, &func.expression).expect("Expression must return a var");
    if func.sig.return_type.return_by_ptr() {
        llfunc.rename(&var.name, "$ret");
        llfunc.add(LLInstruction::ReturnVoid);
    } else {
        llfunc.add(ret_instr(var));
    }
    llfunc
}

pub fn compile_to_llrep(md: &Module) -> LLModule
{
    let mut ll_mod = LLModule{
        name: md.name.clone(),
        functions: Vec::new(),
    };

    for func in md.externals.values() {
        ll_mod.functions.push(LLFunction::new(&func.sig));
    }

    for func in md.functions.values() {
        if !func.is_generic() {
            ll_mod.functions.push(func_to_llrep(func));
        }
    }

    ll_mod
}
