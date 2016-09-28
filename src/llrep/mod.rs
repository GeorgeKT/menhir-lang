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

fn add_set(func: &mut LLFunction, expr: LLExpr, dst: &LLVar)
{
    func.add(set_instr(dst.clone(), expr));
}

fn make_var(func: &mut LLFunction, expr: LLExpr, typ: Type) -> LLVar
{
    let var = func.new_var(typ);
    add_set(func, expr, &var);
    var
}

fn add_lit(func: &mut LLFunction, lit: LLLiteral, dst: &LLVar)
{
    add_set(func, LLExpr::Literal(lit), dst);
}

fn make_lit(func: &mut LLFunction, lit: LLLiteral, typ: Type) -> LLVar
{
    let var = func.new_var(typ);
    add_set(func, LLExpr::Literal(lit), &var);
    var
}

fn bind(func: &mut LLFunction, name: &str, var: LLVar)
{
    func.add(bind_instr(name, var))
}

fn call_to_llrep(func: &mut LLFunction, c: &Call, dst: &LLVar)
{
    let args = c.args.iter().map(|arg| expr_to_llrep_ret(func, arg)).collect();
    func.add(set_instr(
        dst.clone(),
        LLExpr::Call(
            c.callee.name.clone(),
            args,
        )
    ));
}

fn add_binding(func: &mut LLFunction, b: &LetBinding)
{
    let e = expr_to_llrep_ret(func, &b.init);
    bind(func, &b.name, e);
    func.add_named_var(LLVar::named(&b.name, b.typ.clone()));
}

fn let_to_llrep(func: &mut LLFunction, l: &LetExpression, dst: &LLVar)
{
    for b in &l.bindings{
        add_binding(func, b);
    }

    func.add(LLInstruction::StartScope);
    func.push_scope();
    expr_to_llrep(func, &l.expression, dst);
    func.pop_scope();
    func.add(LLInstruction::EndScope);
}

fn array_lit_to_llrep(func: &mut LLFunction, a: &ArrayLiteral, dst: &LLVar)
{
    let vars = a.elements.iter()
        .map(|e| expr_to_llrep_ret(func, e))
        .collect();

    add_lit(func, LLLiteral::Array(vars), dst);
}

fn struct_initializer_to_llrep(func: &mut LLFunction, si: &StructInitializer, dst: &LLVar)
{
    func.add(LLInstruction::StackAlloc(dst.clone()));
    for (idx, expr) in si.member_initializers.iter().enumerate() {
        let v = expr_to_llrep_ret(func, expr);
        func.add(set_struct_member_instr(dst.clone(), idx, v));
    }
}

fn add_array_len(func: &mut LLFunction, array: LLVar, dst: &LLVar)
{
    let expr = LLExpr::ArrayProperty(array, ArrayProperty::Len);
    add_set(func, expr, &dst);
}

fn make_array_len(func: &mut LLFunction, array: LLVar) -> LLVar
{
    let var = func.new_var(Type::Int);
    add_array_len(func, array, &var);
    var
}

fn member_access_to_llrep(func: &mut LLFunction, sma: &MemberAccess, dst: &LLVar)
{
    let mut obj = func.get_named_var(&sma.name).expect("Internal Compiler Error: Unknown variable");
    for (access_idx, at) in sma.access_types.iter().enumerate()
    {
        obj = match (&obj.typ, at)
        {
            (&Type::Struct(ref st), &MemberAccessType::StructMember(idx)) => {
                let expr = LLExpr::StructMember(obj.clone(), idx);
                if access_idx == sma.access_types.len() - 1 {
                    add_set(func, expr, dst);
                    return;
                } else {
                    let mtyp = st.members[idx].typ.clone();
                    let mvar = func.new_var(mtyp);
                    add_set(func, expr, &mvar);
                    mvar
                }
            },

            (&Type::Array(_), &MemberAccessType::ArrayProperty(ArrayProperty::Len)) => {
                add_array_len(func, obj.clone(), dst);
                return;
            },

            _ => panic!("Internal Compiler Error: Invalid member access"),
        };
    }
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
    expr_to_llrep(func, &mc.to_execute, dst);
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
            let arr = func.new_var(a.array_type.clone());
            array_lit_to_llrep(func, a, &arr);
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

fn match_to_llrep(func: &mut LLFunction, m: &MatchExpression, dst: &LLVar)
{
    let target_var = expr_to_llrep_ret(func, &m.target);
    let match_end_bb = func.create_basic_block();

    for mc in &m.cases {
        match_case_to_llrep(func, mc, &target_var, match_end_bb, &dst);
    }

    func.add(LLInstruction::Branch(match_end_bb));
    func.add_basic_block(match_end_bb);
    func.set_current_bb(match_end_bb);
}

fn expr_to_llrep_ret(func: &mut LLFunction, expr: &Expression) -> LLVar
{
    let var = func.new_var(expr.get_type());
    expr_to_llrep(func, expr, &var);
    var
}

fn expr_to_llrep(func: &mut LLFunction, expr: &Expression, dst: &LLVar)
{
    match *expr
    {
        Expression::UnaryOp(ref u) => {
            match u.operator
            {
                Operator::Sub => {
                    let v = expr_to_llrep_ret(func, &u.expression);
                    func.add(set_instr(dst.clone(), LLExpr::USub(v)));
                },
                Operator::Not => {
                    let v = expr_to_llrep_ret(func, &u.expression);
                    func.add(set_instr(dst.clone(), LLExpr::Not(v)));
                },
                _ => panic!("Internal Compiler Error: Invalid unary operator {}", u.operator),
            }
        },

        Expression::BinaryOp(ref op) => {
            let l = expr_to_llrep_ret(func, &op.left);
            let r = expr_to_llrep_ret(func, &op.right);
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

            func.add(set_instr(dst.clone(), llexpr));
        },

        Expression::Literal(Literal::Int(_, v)) => {
            add_lit(func, LLLiteral::Int(v), dst);
        },

        Expression::Literal(Literal::Float(_, ref v_str)) => {
            add_lit(func, LLLiteral::Float(v_str.clone()), dst);
        },

        Expression::Literal(Literal::String(_, ref s))  => {
            add_lit(func, LLLiteral::String(s.clone()), dst);
        },

        Expression::Literal(Literal::Bool(_, v)) => {
            add_lit(func, LLLiteral::Bool(v), dst);
        },

        Expression::Literal(Literal::Char(_, v)) => {
            add_lit(func, LLLiteral::Char(v), dst);
        },

        Expression::Literal(Literal::Array(ref a)) => {
            array_lit_to_llrep(func, a, dst);
        },

        Expression::Call(ref c) => {
            call_to_llrep(func, c, dst);
        },

        Expression::NameRef(ref nr) => {
            let v = LLVar::named(&nr.name, nr.typ.clone());
            add_set(func, LLExpr::Ref(v), dst);
        },

        Expression::Let(ref l) => {
            let_to_llrep(func, l, dst);
        },

        Expression::LetBindings(ref l) => {
            for b in &l.bindings {
                add_binding(func, b);
            }
        },

        Expression::StructInitializer(ref si) => {
            struct_initializer_to_llrep(func, si, dst);
        },

        Expression::MemberAccess(ref sma) => {
            member_access_to_llrep(func, sma, dst);
        },

        Expression::Match(ref m) => {
            match_to_llrep(func, m, dst);
        },

        Expression::If(ref i) => {
            let match_expr = i.to_match();
            match_to_llrep(func, &match_expr, dst);
        },

        Expression::Block(ref b) => {
            for (idx, e) in b.expressions.iter().enumerate() {
                if idx == b.expressions.len() - 1 {
                    expr_to_llrep(func, e, dst);
                } else {
                    expr_to_llrep_ret(func, e);
                }
            }
        },

        /*
        Expression::ArrayGenerator(ref _a) => panic!("NYI"),
        Expression::Lambda(ref l) => gen_lambda(ctx, l),

        */

        _ => panic!("NYI"),
    }
}

fn func_to_llrep(func: &Function) -> LLFunction
{
    let mut llfunc = LLFunction::new(&func.sig);

    if func.sig.return_type.return_by_ptr() {
        let ret = LLVar::named("$ret", func.sig.return_type.clone());
        expr_to_llrep(&mut llfunc,  &func.expression, &ret);
        llfunc.add(LLInstruction::ReturnVoid);
    } else {
        let var = llfunc.new_var(func.sig.return_type.clone());
        llfunc.add(LLInstruction::StackAlloc(var.clone()));
        expr_to_llrep(&mut llfunc, &func.expression, &var);
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
