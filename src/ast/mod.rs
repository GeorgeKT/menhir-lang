mod arrays;
mod call;
mod expression;
mod function;
mod lambda;
mod matchexpression;
mod nameref;
mod operations;
mod types;

pub use self::arrays::{ArrayLiteral, ArrayInitializer, ArrayPattern, array_lit, array_init, array_pattern};
pub use self::call::Call;
pub use self::expression::Expression;
pub use self::function::{Function, FunctionSignature, Argument, sig};
pub use self::lambda::{Lambda, lambda};
pub use self::matchexpression::{MatchExpression, match_case, match_expression};
pub use self::nameref::NameRef;
pub use self::operations::{BinaryOp, UnaryOp, unary_op, bin_op, bin_op2};
pub use self::types::{Type, to_primitive, func_type, array_type};


fn prefix(level: usize) -> String
{
    let mut s = String::with_capacity(level);
    for _ in 0..level {
        s.push(' ')
    }
    s
}

pub trait TreePrinter
{
    fn print(&self, level: usize);
}

pub struct Module
{
    pub name: String,
    pub expressions: Vec<Expression>,
}

impl TreePrinter for Module
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}Module: {}", p, self.name);
        for e in &self.expressions {
            e.print(level + 1);
        }
    }
}
