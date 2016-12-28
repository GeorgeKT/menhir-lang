use std::fmt;
use ast::*;
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Property
{
    Len,
}

impl fmt::Display for Property
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Property::Len => write!(f, "len"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Field
{
    pub name: String,
    pub index: usize,
}

pub fn field(name: &str, index: usize) -> Field
{
    Field{
        name: name.into(),
        index: index,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum MemberAccessType
{
    Call(Call),
    Name(Field),
    Property(Property),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MemberAccess
{
    pub left: Box<Expression>,
    pub right: MemberAccessType,
    pub span: Span,
    pub typ: Type,
}

pub fn member_access(left: Expression, right: MemberAccessType, span: Span) -> Expression
{
    Expression::MemberAccess(MemberAccess{
        left: Box::new(left),
        right: right,
        span: span,
        typ: Type::Unknown,
    })
}


impl TreePrinter for MemberAccess
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}member access (span: {}, type: {})", p, self.span, self.typ);
        self.left.print(level + 1);
        match self.right
        {
            MemberAccessType::Call(ref call) => call.print(level + 1),
            MemberAccessType::Name(ref field) => println!("{} .{} (idx {})", p, field.name, field.index),
            MemberAccessType::Property(ref prop) => {
                match prop
                {
                    &Property::Len => println!("{} .len", p),
                }
            }
        }
    }
}
