use std::collections::{HashMap};
use std::rc::Rc;

mod arrays;
mod assign;
mod bindings;
mod block;
mod call;
mod expression;
mod function;
mod heap;
mod ifexpression;
mod import;
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
pub use self::expression::*;
pub use self::function::*;
pub use self::heap::*;
pub use self::ifexpression::*;
pub use self::import::*;
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

pub fn prefix(level: usize) -> String
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

pub type GenericMapping = HashMap<Type, Type>;


pub struct Package
{
    pub name: String,
    pub modules: HashMap<String, Module>,
    pub imports: HashMap<String, Rc<Import>>,
}

impl Package
{
    pub fn new(name: &str) -> Package
    {
        Package{
            name: name.into(),
            modules: HashMap::new(),
            imports: HashMap::new(),
        }
    }
}