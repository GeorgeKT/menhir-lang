mod arrays;
mod call;
mod expression;
mod function;
mod nameref;
mod operations;
mod types;

pub use self::arrays::{ArrayLiteral, ArrayInitializer};
pub use self::call::Call;
pub use self::expression::Expression;
pub use self::function::{Function, FunctionSignature};
pub use self::nameref::NameRef;
pub use self::operations::{BinaryOp, UnaryOp};
pub use self::types::{Type};


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
