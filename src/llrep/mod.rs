mod llfunction;
mod llinstruction;

use std::fmt;
use ast::*;
use parser::Operator;
pub use self::llfunction::{LLFunction, LLVar};
pub use self::llinstruction::{LLExpr, LLInstruction, LLLiteral};


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

fn call_to_llrep(func: &mut LLFunction, c: &Call) -> LLVar
{
    let var = func.new_var(c.return_type.clone());
    let args = c.args.iter().map(|arg| expr_to_llrep(func, arg).expect("Expression must return a var")).collect();
    func.add(LLInstruction::set(
        var.clone(),
        LLExpr::Call{
            name: c.callee.name.clone(),
            args: args,
        }
    ));
    var
}

fn name_ref_to_llrep(func: &mut LLFunction, nr: &NameRef) -> LLVar
{
    let var = func.new_var(nr.typ.clone());
    func.add(LLInstruction::set(
        var.clone(),
        LLExpr::Load(nr.name.clone()),
    ));
    var
}


fn add_binding(func: &mut LLFunction, b: &LetBinding)
{
    let v = expr_to_llrep(func, &b.init).expect("Expression must return a var");
    func.add(LLInstruction::bind(&b.name, v));
    func.add_named_var(LLVar::named(&b.name, b.typ.clone()));
}

fn let_to_llrep(func: &mut LLFunction, l: &LetExpression) -> LLVar
{
    for b in &l.bindings{
        add_binding(func, b);
    }

    func.add(LLInstruction::StartScope);
    let result = expr_to_llrep(func, &l.expression).expect("Expression must return a var");
    func.add(LLInstruction::EndScope{ret_var: result.clone()});
    result
}

fn array_lit_to_llrep(func: &mut LLFunction, a: &ArrayLiteral) -> LLVar
{
    let vars = a.elements.iter()
        .map(|e| expr_to_llrep(func, e).expect("Expression must return a var"))
        .collect();
    let var = func.new_var(a.array_type.clone());
    func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Array(vars))));
    var
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
                    func.add(LLInstruction::set(var.clone(), LLExpr::USub(v)));
                },
                Operator::Not => {
                    func.add(LLInstruction::set(var.clone(), LLExpr::Not(v)));
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

            func.add(LLInstruction::set(var.clone(), llexpr));
            Some(var)
        },

        Expression::Literal(Literal::Int(_, v)) => {
            let var = func.new_var(Type::Int);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Int(v))));
            Some(var)
        },

        Expression::Literal(Literal::Float(_, ref v_str)) => {
            let var = func.new_var(Type::Float);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Float(v_str.clone()))));
            Some(var)
        },

        Expression::Literal(Literal::String(_, ref s))  => {
            let var = func.new_var(string_type());
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::String(s.clone()))));
            Some(var)
        },

        Expression::Literal(Literal::Bool(_, v)) => {
            let var = func.new_var(Type::Bool);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Bool(v))));
            Some(var)
        },

        Expression::Literal(Literal::Char(_, v)) => {
            let var = func.new_var(Type::Char);
            func.add(LLInstruction::set(var.clone(), LLExpr::Literal(LLLiteral::Char(v))));
            Some(var)
        },

        Expression::Literal(Literal::Array(ref a)) => {
            Some(array_lit_to_llrep(func, a))
        },

        Expression::Call(ref c) => {
            Some(call_to_llrep(func, c))
        },

        Expression::NameRef(ref nr) => {
            Some(name_ref_to_llrep(func, nr))
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
        /*




        Expression::ArrayGenerator(ref _a) => panic!("NYI"),

        Expression::NameRef(ref nr) => gen_name_ref(ctx, nr),
        Expression::Match(ref m) => gen_match(ctx, m),
        Expression::Lambda(ref l) => gen_lambda(ctx, l),

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
    let var = expr_to_llrep(&mut llfunc, &func.expression).expect("Expression must return a var");
    llfunc.add(LLInstruction::ret(var));
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
