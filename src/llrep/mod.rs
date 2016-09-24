mod llfunction;
mod llinstruction;

use std::fmt;
use ast::*;
use parser::Operator;
use self::llfunction::{LLFunction, LLVar};
use self::llinstruction::{LLExpr, LLInstruction, LLLiteral};


pub struct LLModule
{
    functions: Vec<LLFunction>,
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

fn expr_to_llrep(func: &mut LLFunction, expr: &Expression) -> LLVar
{
    match *expr
    {
        Expression::UnaryOp(ref u) => {
            let v = expr_to_llrep(func, &u.expression);
            let var = func.new_var(u.typ.clone());
            match u.operator
            {
                Operator::Sub => {
                    func.add(LLInstruction::set(var.clone(), LLExpr::USub(v)));
                },
                Operator::Not => {
                    func.add(LLInstruction::set(var.clone(), LLExpr::Not(v)));
                },
                _ => panic!("Internal Compiler Error: Invalid unary operator {}", u.operator),
            }

            var
        },

        Expression::BinaryOp(ref op) => {
            let l = expr_to_llrep(func, &op.left);
            let r = expr_to_llrep(func, &op.right);
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
                _ => panic!("Internal Compiler Error: Invalid binary operator {}", op.operator),
            };

            func.add(LLInstruction::set(var.clone(), llexpr));
            var
        },

        Expression::Literal(Literal::Int(_, v)) => {
            let var = func.new_var(Type::Int);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Int(v))));
            var
        },
        Expression::Literal(Literal::Float(_, ref v_str)) => {
            let var = func.new_var(Type::Float);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Float(v_str.clone()))));
            var
        },
        Expression::Literal(Literal::String(_, ref s))  => {
            let var = func.new_var(string_type());
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::String(s.clone()))));
            var
        },
        Expression::Literal(Literal::Bool(_, v)) => {
            let var = func.new_var(Type::Bool);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Bool(v))));
            var
        },
        Expression::Literal(Literal::Char(_, v)) => {
            let var = func.new_var(Type::Char);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Char(v))));
            var
        },

        /*
        Expression::Literal(Literal::Array(ref a)) => gen_array_literal(ctx, a),



        Expression::ArrayGenerator(ref _a) => panic!("NYI"),
        Expression::Call(ref c) => gen_call(ctx, c),
        Expression::NameRef(ref nr) => gen_name_ref(ctx, nr),
        Expression::Match(ref m) => gen_match(ctx, m),
        Expression::Lambda(ref l) => gen_lambda(ctx, l),
        Expression::Let(ref l) => gen_let(ctx, l),
        Expression::LetBindings(ref l) => {
            let mut v = None;
            for b in &l.bindings {
                v = Some(gen_let_binding(ctx, b));
            }
            v.expect("Empting binding list")
        },
        Expression::If(ref i) => {
            let match_expr = i.to_match();
            gen_match(ctx, &match_expr)
        },
        Expression::Block(ref b) => gen_block(ctx, b),

        Expression::StructInitializer(ref si) => gen_struct_initializer(ctx, si),
        Expression::MemberAccess(ref sma) => gen_member_access(ctx, sma),
        */

        _ => panic!("NYI"),
    }
}

fn func_to_llrep(func: &Function) -> LLFunction
{
    let mut llfunc = LLFunction::new(&func.sig);
    let var = expr_to_llrep(&mut llfunc, &func.expression);
    llfunc.add(LLInstruction::ret(var));
    llfunc
}

pub fn compile_to_llrep(md: &Module) -> LLModule
{
    let mut ll_mod = LLModule{
        functions: Vec::new(),
    };

    for func in md.functions.values() {
        if !func.is_generic() {
            ll_mod.functions.push(func_to_llrep(func));
        }
    }

    ll_mod
}
