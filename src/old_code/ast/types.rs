use std::fmt;
use ast::{TreePrinter, prefix};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct GenericType // e.g. Foo<X, Y>
{
    pub name: String,
    pub generic_args: Vec<Type>,
}

impl GenericType
{
    pub fn new(name: String, generic_args: Vec<Type>) -> GenericType
    {
        GenericType{
            name: name,
            generic_args: generic_args,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type
{
    Void,
    Unknown,
    Primitive(String),
    Complex(String),
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Generic(GenericType),
}

impl Type
{
    pub fn ptr(t: Type) -> Type
    {
        Type::Pointer(Box::new(t))
    }
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Type::Void => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
            Type::Primitive(ref t) => write!(f, "{}", t),
            Type::Complex(ref s) => write!(f, "{}", s),
            Type::Pointer(ref st) => write!(f, "*{}", st),
            Type::Array(ref at, count) => write!(f, "[{}, {}]", at, count),
            Type::Slice(ref at) => write!(f, "[{}]", at),
            Type::Generic(ref g) => write!(f, "{}<{}>", g.name, g.generic_args.iter().map(|s| format!("{}", s)).collect::<Vec<_>>().join(",")),
        }
    }
}

impl TreePrinter for Type
{
    fn print(&self, level: usize)
    {
        println!("{}{}", prefix(level), self);
    }
}
