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
mod structs;
mod sumtype;
mod types;

pub use self::arrays::*;
pub use self::call::*;
pub use self::expression::*;
pub use self::function::*;
pub use self::lambda::*;
pub use self::letexpression::*;
pub use self::matchexpression::*;
pub use self::nameref::NameRef;
pub use self::operations::*;
pub use self::structs::*;
pub use self::sumtype::*;
pub use self::types::*;

use compileerror::{Span};

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


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeDeclaration
{
    Struct(StructDeclaration),
    Sum(SumTypeDeclaration),
    Alias(TypeAlias),
}

impl TypeDeclaration
{
    pub fn span(&self) -> Span
    {
        match *self
        {
            TypeDeclaration::Struct(ref sd) => sd.span,
            TypeDeclaration::Sum(ref s) => s.span,
            TypeDeclaration::Alias(ref t) => t.span,
        }
    }

    pub fn name(&self) -> &str
    {
        match *self
        {
            TypeDeclaration::Struct(ref sd) => &sd.name,
            TypeDeclaration::Sum(ref s) => &s.name,
            TypeDeclaration::Alias(ref t) => &t.name,
        }
    }
}

impl TreePrinter for TypeDeclaration
{
    fn print(&self, level: usize)
    {
        match *self
        {
            TypeDeclaration::Struct(ref sd) => sd.print(level),
            TypeDeclaration::Sum(ref s) => s.print(level),
            TypeDeclaration::Alias(ref t) => t.print(level),
        }
    }
}

pub struct Module
{
    pub name: String,
    pub functions: HashMap<String, Function>,
    pub types: HashMap<String, TypeDeclaration>,
}

impl TreePrinter for Module
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}Module: {}", p, self.name);
        for ref t in self.types.values() {
            t.print(level + 1);
        }

        for ref func in self.functions.values() {
            func.print(level + 1);
        }
    }
}
