use crate::ast::{prefix, Expression, GenericMapping, TreePrinter, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructMemberDeclaration {
    pub name: String,
    pub typ: Type,
    pub span: Span,
}

pub fn struct_member_declaration(name: &str, typ: Type, span: Span) -> StructMemberDeclaration {
    StructMemberDeclaration {
        name: name.into(),
        typ,
        span,
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructDeclaration {
    pub name: String,
    pub members: Vec<StructMemberDeclaration>,
    pub implements: Vec<(Type, Span)>,
    pub span: Span,
    pub typ: Type,
}

pub fn struct_declaration(
    name: &str,
    members: Vec<StructMemberDeclaration>,
    implements: Vec<(Type, Span)>,
    span: Span,
) -> StructDeclaration {
    StructDeclaration {
        name: name.into(),
        members,
        implements,
        span,
        typ: Type::Unknown,
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructMemberInitializer {
    pub name: String,
    pub initializer: Expression,
    pub member_idx: usize,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructInitializer {
    pub struct_name: Option<String>,
    pub member_initializers: Vec<StructMemberInitializer>,
    pub span: Span,
    pub typ: Type,
    pub generic_args: GenericMapping,
}

pub fn struct_initializer(
    struct_name: Option<String>,
    member_initializers: Vec<StructMemberInitializer>,
    span: Span,
) -> StructInitializer {
    StructInitializer {
        struct_name,
        member_initializers,
        span,
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
        println!(
            "{}struct initializer {} ({})",
            p,
            self.struct_name
                .as_ref()
                .map(|s| &s[..])
                .unwrap_or_else(|| "<anonymous>"),
            self.span
        );
        for mi in &self.member_initializers {
            println!("{} {} (idx: {}):", p, mi.name, mi.member_idx);
            mi.initializer.print(level + 2);
        }
    }
}

impl TreePrinter for StructMemberDeclaration {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}{}:{} ({})", p, self.name, self.typ, self.span);
    }
}
