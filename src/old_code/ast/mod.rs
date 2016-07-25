mod arrays;
mod block;
mod call;
mod expression;
mod function;
mod generics;
mod ifelse;
mod import;
mod matchstmt;
mod memberaccess;
mod module;
mod modulename;
mod nameref;
mod objectconstruction;
mod operations;
mod returnstmt;
mod statement;
mod structs;
mod traits;
mod types;
mod union;
mod variable;
mod whilestmt;

pub use self::arrays::*;
pub use self::block::*;
pub use self::call::*;
pub use self::expression::Expression;
pub use self::function::*;
pub use self::ifelse::*;
pub use self::import::*;
pub use self::matchstmt::*;
pub use self::memberaccess::*;
pub use self::module::*;
pub use self::modulename::*;
pub use self::nameref::*;
pub use self::objectconstruction::*;
pub use self::operations::*;
pub use self::statement::*;
pub use self::returnstmt::*;
pub use self::structs::*;
pub use self::traits::*;
pub use self::types::*;
pub use self::union::*;
pub use self::variable::*;
pub use self::whilestmt::*;


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
