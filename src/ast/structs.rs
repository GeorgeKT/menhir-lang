use crate::ast::{prefix, Expression, GenericMapping, TreePrinter, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructMemberDeclaration {
    pub name: String,
    pub typ: Type,
    pub span: Span,
}

pub fn struct_member_declaration(name: &str, typ: Type, span: Span) -> StructMemberDeclaration {
    StructMemberDeclaration {
        name: name.into(),
        typ: typ,
        span: span,
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructDeclaration {
    pub name: String,
    pub members: Vec<StructMemberDeclaration>,
    pub span: Span,
    pub typ: Type,
}

pub fn struct_declaration(name: &str, members: Vec<StructMemberDeclaration>, span: Span) -> StructDeclaration {
    StructDeclaration {
        name: name.into(),
        members: members,
        span: span,
        typ: Type::Unknown,
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructInitializer {
    pub struct_name: String,
    pub member_initializers: Vec<Expression>,
    pub span: Span,
    pub typ: Type,
    pub generic_args: GenericMapping,
}

pub fn struct_initializer(struct_name: &str, member_initializers: Vec<Expression>, span: Span) -> StructInitializer {
    StructInitializer {
        struct_name: struct_name.into(),
        member_initializers: member_initializers,
        span: span,
        typ: Type::Unknown,
        generic_args: GenericMapping::new(),
    }
}

impl TreePrinter for StructDeclaration {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}struct {} ({})", p, self.name, self.span);
        for m in &self.members {
            m.print(level + 1)
        }
    }
}

impl TreePrinter for StructInitializer {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}struct initializer {} ({})", p, self.struct_name, self.span);
        for m in &self.member_initializers {
            m.print(level + 1)
        }
    }
}

impl TreePrinter for StructMemberDeclaration {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}{}:{} ({})", p, self.name, self.typ, self.span);
    }
}
