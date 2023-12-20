mod arrays;
mod assign;
mod bindings;
mod block;
mod call;
mod compilercall;
mod expression;
mod function;
mod heap;
mod ifexpression;
mod import;
mod indexoperation;
mod interface;
mod lambda;
mod literal;
mod loops;
mod matchexpression;
mod memberaccess;
mod module;
mod nameref;
mod operations;
mod operator;
mod pattern;
mod structs;
mod sumtype;
mod typedeclaration;
mod types;

pub use self::arrays::*;
pub use self::assign::*;
pub use self::bindings::*;
pub use self::block::*;
pub use self::call::*;
pub use self::compilercall::*;
pub use self::expression::*;
pub use self::function::*;
pub use self::heap::*;
pub use self::ifexpression::*;
pub use self::import::*;
pub use self::indexoperation::*;
pub use self::interface::*;
pub use self::lambda::*;
pub use self::literal::*;
pub use self::loops::*;
pub use self::matchexpression::*;
pub use self::memberaccess::*;
pub use self::module::*;
pub use self::nameref::NameRef;
pub use self::operations::*;
pub use self::operator::*;
pub use self::pattern::*;
pub use self::structs::*;
pub use self::sumtype::*;
pub use self::typedeclaration::*;
pub use self::types::*;

pub fn prefix(level: usize) -> String {
    let mut s = String::with_capacity(level);
    for _ in 0..level {
        s.push_str("  ")
    }
    s
}

pub trait TreePrinter {
    fn print(&self, level: usize);
}

pub trait TreeFormatter {
    fn fmt(&self, level: usize, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error>;
}

use itertools::join;
use std::collections::HashMap;

pub type GenericMapping = HashMap<Type, Type>;

pub fn new_func_name(func_name: &str, generic_args: &GenericMapping) -> String {
    format!("{}<{}>", func_name, join(generic_args.values(), ","))
}
