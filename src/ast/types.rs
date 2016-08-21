use std::fmt;
use std::hash::{Hasher, Hash};
use std::ops::Deref;
use itertools::free::join;
use ast::{Expression, TreePrinter, prefix};


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
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Generic(String),
    Func(Vec<Type>, Box<Type>), // args and return type
}

impl Type
{
    pub fn concat_allowed(&self, other: &Type) -> bool
    {
        if self.is_empty_array() || other.is_empty_array() {
            return true;
        }

        if self.is_sequence() && other.is_sequence() {
            return self.get_element_type() == other.get_element_type();
        }

        if self.is_sequence() && !other.is_sequence() {
            return self.get_element_type().map(|et| et == *other).unwrap_or(false);
        }

        if other.is_sequence() && !self.is_sequence() {
            return other.get_element_type().map(|et| et == *self).unwrap_or(false);
        }

        false
    }

    pub fn is_empty_array(&self) -> bool
    {
        match *self
        {
            Type::Array(_, 0) => true,
            _ => false,
        }
    }

    pub fn is_sequence(&self) -> bool
    {
        match *self
        {
            Type::Array(_, _) => true,
            Type::Slice(_) => true,
            _ => false,
        }
    }

    pub fn get_element_type(&self) -> Option<Type>
    {
        match *self
        {
            Type::Array(ref et, _) => Some(et.deref().clone()),
            Type::Slice(ref et) => Some(et.deref().clone()),
            _ => None,
        }
    }

    pub fn is_matchable(&self, other: &Type) -> bool
    {
        if (self.is_empty_array() && other.is_sequence()) || (other.is_empty_array() && self.is_sequence()) {
            return true;
        }

        if self.is_sequence() && other.is_sequence() {
            return self.get_element_type() == other.get_element_type();
        }

        *self == *other
    }

    // If possible generate a conversion expression
    pub fn convert(&self, other: &Type, expr: &Expression) -> Option<Expression>
    {
        match (self, other)
        {
            (&Type::Slice(ref s), &Type::Array(ref t, _)) if s == t =>
                // arrays can be converted to slices if the element type is the same
                Some(Expression::ArrayToSliceConversion(Box::new(expr.clone())))
            ,
            _ => None,
        }
    }

    pub fn is_generic(&self) -> bool
    {
        match *self
        {
            Type::Generic(_) => true,
            Type::Array(ref inner, _) => inner.is_generic(),
            Type::Slice(ref inner) => inner.is_generic(),
            Type::Func(ref args, ref ret) => ret.is_generic() || args.iter().any(|a| a.is_generic()),
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool
    {
        match *self
        {
            Type::Func(_, _) => true,
            _ => false,
        }
    }
}

pub fn func_type(args: Vec<Type>, ret: Type) -> Type
{
    Type::Func(args, Box::new(ret))
}

pub fn array_type(element_type: Type, len: usize) -> Type
{
    Type::Array(Box::new(element_type), len)
}

pub fn slice_type(element_type: Type) -> Type
{
    Type::Slice(Box::new(element_type))
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
            Type::Array(ref at, len) =>
                if len == 0 {
                    write!(f, "[]")
                } else {
                    write!(f, "[{}; {}]", at, len)
                },
            Type::Slice(ref at) => write!(f, "[{}]", at),
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

impl Hash for Type
{
    fn hash<H>(&self, state: &mut H) where H: Hasher
    {
        let s = format!("{}", self);
        s.hash(state);
    }
}
