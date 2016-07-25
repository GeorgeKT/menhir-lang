use std::fmt;
use ast::{TreePrinter, prefix};


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type
{
    Void,
    Unknown,
    Primitive(String),
    Complex(String),
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Generic(String),
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
            Type::Array(ref at, count) => write!(f, "[{}, {}]", at, count),
            Type::Slice(ref at) => write!(f, "[{}]", at),
            Type::Generic(ref g) => write!(f, "${}", g),
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
