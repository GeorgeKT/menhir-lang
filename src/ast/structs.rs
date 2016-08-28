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
    pub typ: Type,
}

pub fn struct_declaration(name: &str, members: Vec<StructMember>, span: Span) -> StructDeclaration
{
    StructDeclaration{
        name: name.into(),
        members: members,
        span: span,
        typ: Type::Unknown,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructInitializer
{
    pub struct_name: String,
    pub member_initializers: Vec<Expression>,
    pub span: Span,
    pub typ: Type,
}

impl StructInitializer
{
    pub fn to_struct_pattern(self) -> Expression
    {
        let mut bindings = Vec::with_capacity(self.member_initializers.len());
        for e in &self.member_initializers {
            if let Expression::NameRef(ref nr) = *e {
                bindings.push(nr.name.clone());
            } else {
                break;
            }
        }

        if bindings.len() != self.member_initializers.len() {
            return Expression::StructInitializer(self); // Not an array pattern
        }

        Expression::StructPattern(StructPattern{
            name: self.struct_name,
            types: Vec::with_capacity(bindings.len()),
            bindings: bindings,
            span: self.span,
        })
    }
}

pub fn struct_initializer(struct_name: &str, member_initializers: Vec<Expression>, span: Span) -> StructInitializer
{
    StructInitializer{
        struct_name: struct_name.into(),
        member_initializers: member_initializers,
        span: span,
        typ: Type::Unknown,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructMemberAccess
{
    pub name: String,
    pub members: Vec<String>,
    pub indices: Vec<usize>, // Index of each member
    pub span: Span,
    pub typ: Type,
}

pub fn struct_member_access(name: &str, members: Vec<String>, span: Span) -> StructMemberAccess
{
    StructMemberAccess{
        name: name.into(),
        indices: Vec::with_capacity(members.len()),
        members: members,
        span: span,
        typ: Type::Unknown,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructPattern
{
    pub name: String,
    pub bindings: Vec<String>,
    pub types: Vec<Type>,
    pub span: Span,
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


impl TreePrinter for StructPattern
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}struct pattern {}{{{}}} ({})", p, self.name, join(self.bindings.iter(), ","), self.span);
    }
}
