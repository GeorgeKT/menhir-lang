use crate::ast::*;
use crate::span::Span;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum Property {
    Len,
    Data,
}

impl fmt::Display for Property {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Property::Len => write!(f, "len"),
            Property::Data => write!(f, "data"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub index: usize,
}

pub fn field(name: &str, index: usize) -> Field {
    Field {
        name: name.into(),
        index: index,
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum MemberAccessType {
    Call(Box<Call>),
    Name(Field),
    Property(Property),
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct MemberAccess {
    pub left: Expression,
    pub right: MemberAccessType,
    pub span: Span,
    pub typ: Type,
}

pub fn member_access(left: Expression, right: MemberAccessType, span: Span) -> Expression {
    Expression::MemberAccess(Box::new(MemberAccess {
        left,
        right,
        span,
        typ: Type::Unknown,
    }))
}

impl TreePrinter for MemberAccess {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}member access (span: {}, type: {})", p, self.span, self.typ);
        self.left.print(level + 1);
        match self.right {
            MemberAccessType::Call(ref call) => call.print(level + 1),
            MemberAccessType::Name(ref field) => println!("{} .{} (idx {})", p, field.name, field.index),
            MemberAccessType::Property(ref prop) => match *prop {
                Property::Len => println!("{} .len", p),
                Property::Data => println!("{} .data", p),
            },
        }
    }
}
