use std::collections::HashMap;

mod arrays;
mod call;
mod expression;
mod function;
mod lambda;
mod letexpression;
mod matchexpression;
mod nameref;
mod operations;
mod types;

pub use self::arrays::{ArrayLiteral, ArrayPattern, ArrayGenerator, array_lit, array_pattern, array_generator};
pub use self::call::{Call};
pub use self::expression::Expression;
pub use self::function::{Function, FunctionSignature, Argument, ArgumentPassingMode, sig};
pub use self::lambda::{Lambda, lambda};
pub use self::letexpression::{LetExpression, Binding, let_expression, let_binding};
pub use self::matchexpression::{MatchExpression, MatchCase, match_case, match_expression};
pub use self::nameref::NameRef;
pub use self::operations::{BinaryOp, UnaryOp, unary_op, bin_op};
pub use self::types::{Type, to_primitive, func_type, array_type, slice_type};


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
    pub functions: HashMap<String, Function>,
}

impl TreePrinter for Module
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}Module: {}", p, self.name);
        for (_, ref func) in &self.functions {
            func.print(level + 1);
        }
    }
}
