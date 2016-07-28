use std::fmt;
use itertools::free::join;
use ast::{TreePrinter, prefix};


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type
{
    Void,
    Unknown,
    Int,
    Float,
    String,
    Bool,
    Complex(String),
    EmptyArray,
    Array(Box<Type>),
    Generic(String),
    Func(Vec<Type>, Box<Type>), // args and return type
}

impl Type
{
    pub fn concat_allowed(&self, other: &Type) -> bool
    {
        use std::ops::Deref;
        if *self == Type::EmptyArray || *other == Type::EmptyArray {
            return true;
        }
        
        match (self, other)
        {
            (&Type::Array(ref s), &Type::Array(ref t)) => s == t,
            (ref s, &Type::Array(ref t)) => **s == *t.deref(),
            (&Type::Array(ref t), ref s) => **s == *t.deref(),
            _ => false,
        }
    }


}

pub fn func_type(args: Vec<Type>, ret: Type) -> Type 
{
    Type::Func(args, Box::new(ret))
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Type::Void => write!(f, "void"),
            Type::Unknown => write!(f, "unknown"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Complex(ref s) => write!(f, "{}", s),
            Type::EmptyArray => write!(f, "[]"),
            Type::Array(ref at) => write!(f, "[{}]", at),
            Type::Generic(ref g) => write!(f, "${}", g),
            Type::Func(ref args, ref ret) => write!(f, "({}) -> {}", join(args.iter(), ", "), ret),
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

pub fn to_primitive(name: &str) -> Option<Type>
{
    match name
    {
        "int" => Some(Type::Int),
        "float" => Some(Type::Float),
        "string" => Some(Type::String),
        "bool" => Some(Type::Bool),
        _ => None,
    }
}
