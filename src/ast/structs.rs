use itertools::free::join;
use ast::{Expression, TreePrinter, Type, prefix};
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

impl StructDeclaration
{
    pub fn get_type(&self) -> Type
    {
        Type::Struct(self.name.clone(), self.members.iter().map(|m| m.typ.clone()).collect())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructInitializer
{
    pub struct_name: String,
    pub member_initializers: Vec<Expression>,
    pub span: Span,
}

pub fn struct_initializer(struct_name: &str, member_initializers: Vec<Expression>, span: Span) -> StructInitializer
{
    StructInitializer{
        struct_name: struct_name.into(),
        member_initializers: member_initializers,
        span: span,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructMemberAccess
{
    pub name: String,
    pub members: Vec<String>,
    pub span: Span,
}

pub fn struct_member_access(name: &str, members: Vec<String>, span: Span) -> StructMemberAccess
{
    StructMemberAccess{
        name: name.into(),
        members: members,
        span: span,
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

impl TreePrinter for StructInitializer
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}struct initializer {} ({})", p, self.struct_name, self.span);
        for m in &self.member_initializers {
            m.print(level + 1)
        }
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

impl TreePrinter for StructMemberAccess
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}{}.{} ({})", p, self.name, join(self.members.iter(), "."), self.span);
    }
}
