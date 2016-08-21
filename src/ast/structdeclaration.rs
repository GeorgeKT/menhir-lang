use ast::{TreePrinter, Type, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructMember
{
    pub name: String,
    pub typ: Type,
    pub span: Span,
}

pub fn struct_member(name: &str, typ: Type, span: Span) -> StructMember
{
    StructMember{
        name: name.into(),
        typ: typ,
        span: span,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructDeclaration
{
    pub name: String,
    pub members: Vec<StructMember>,
    pub span: Span,
}

pub fn struct_declaration(name: &str, members: Vec<StructMember>, span: Span) -> StructDeclaration
{
    StructDeclaration{
        name: name.into(),
        members: members,
        span: span,
    }
}

impl TreePrinter for StructMember
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}{}:{} ({})", p, self.name, self.typ, self.span);
    }
}

impl TreePrinter for StructDeclaration
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}struct {} ({})", p, self.name, self.span);
        for m in &self.members {
            m.print(level + 1)
        }
    }
}
