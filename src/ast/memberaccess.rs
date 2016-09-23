use std::fmt::{Formatter, Display, Error};
use itertools::free::join;
use ast::*;
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ArrayProperty
{
    Len,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MemberAccessType
{
    StructMember(usize),
    ArrayProperty(ArrayProperty),
}



#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MemberAccessMethod
{
    ByName(String),
    ByIndex(usize),
}

impl Display for MemberAccessMethod
{
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error>
    {
        match *self
        {
            MemberAccessMethod::ByName(ref name) => write!(f, "{}", name),
            MemberAccessMethod::ByIndex(index) => write!(f, "{}", index),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberAccess
{
    pub name: String,
    pub access_methods: Vec<MemberAccessMethod>,
    pub access_types: Vec<MemberAccessType>,
    pub span: Span,
    pub typ: Type,
}

pub fn member_access(name: &str, methods: Vec<MemberAccessMethod>, span: Span) -> MemberAccess
{
    MemberAccess{
        name: name.into(),
        access_types: Vec::with_capacity(methods.len()),
        access_methods: methods,
        span: span,
        typ: Type::Unknown,
    }
}


impl TreePrinter for MemberAccess
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}{}.{} ({})", p, self.name, join(self.access_methods.iter(), "."), self.span);
    }
}
