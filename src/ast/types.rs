use std::fmt;


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type
{
    Void,
    Unknown,
    Primitive(String),
    Complex(String),
    Trait(String),
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Slice(Box<Type>),
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
            Type::Trait(ref t) => write!(f, "{}", t),
        }
    }
}
