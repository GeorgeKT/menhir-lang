use crate::ast::{prefix, Literal, NameRef, TreePrinter, Type};
use crate::span::Span;
use itertools::free::join;
use serde_derive::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct EmptyArrayPattern {
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ArrayPattern {
    pub head: String,
    pub tail: String,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum StructPatternBindingMode {
    Value,
    Pointer,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructPatternBinding {
    pub name: String,
    pub typ: Type,
    pub mode: StructPatternBindingMode,
}

impl fmt::Display for StructPatternBinding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.mode {
            StructPatternBindingMode::Value => write!(f, "{}", self.name),
            StructPatternBindingMode::Pointer => write!(f, "*{}", self.name),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct StructPattern {
    pub name: String,
    pub bindings: Vec<StructPatternBinding>,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct OptionalPattern {
    pub binding: String,
    pub span: Span,
    pub inner_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct OkPattern {
    pub inner: Box<Pattern>,
    pub span: Span,
    pub inner_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ErrorPattern {
    pub inner: Box<Pattern>,
    pub span: Span,
    pub inner_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum Pattern {
    Literal(Literal),
    Array(ArrayPattern), // [hd | tail]
    EmptyArray(EmptyArrayPattern),
    Name(NameRef),
    Binding(NameRef),
    Struct(StructPattern),
    Any(Span),
    Nil(Span),
    Optional(OptionalPattern),
    Ok(OkPattern),
    Error(ErrorPattern),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Literal(l) => l.span(),
            Pattern::Array(a) => a.span.clone(),
            Pattern::EmptyArray(a) => a.span.clone(),
            Pattern::Name(n) => n.span.clone(),
            Pattern::Binding(n) => n.span.clone(),
            Pattern::Struct(s) => s.span.clone(),
            Pattern::Any(span) | Pattern::Nil(span) => span.clone(),
            Pattern::Optional(o) => o.span.clone(),
            Pattern::Ok(o) => o.span.clone(),
            Pattern::Error(e) => e.span.clone(),
        }
    }
}

pub fn array_pattern(head: &str, tail: &str, span: Span) -> Pattern {
    Pattern::Array(ArrayPattern {
        head: head.into(),
        tail: tail.into(),
        span,
    })
}

pub fn empty_array_pattern(span: Span) -> Pattern {
    Pattern::EmptyArray(EmptyArrayPattern { span })
}

pub fn struct_pattern(name: &str, bindings: Vec<StructPatternBinding>, typ: Type, span: Span) -> StructPattern {
    StructPattern {
        name: name.into(),
        bindings,
        typ,
        span,
    }
}

pub fn optional_pattern(binding: String, span: Span) -> Pattern {
    Pattern::Optional(OptionalPattern {
        binding,
        span,
        inner_type: Type::Unknown,
    })
}

pub fn ok_pattern(pattern: Pattern, span: Span) -> Pattern {
    Pattern::Ok(OkPattern {
        inner: Box::new(pattern),
        span,
        inner_type: Type::Unknown,
    })
}

pub fn error_pattern(pattern: Pattern, span: Span) -> Pattern {
    Pattern::Error(ErrorPattern {
        inner: Box::new(pattern),
        span,
        inner_type: Type::Unknown,
    })
}

pub fn name_pattern(name: &str, span: Span) -> Pattern {
    Pattern::Name(NameRef::new(name.into(), span))
}

impl TreePrinter for Pattern {
    fn print(&self, level: usize) {
        let p = prefix(level);
        match self {
            Pattern::Literal(l) => l.print(level),
            Pattern::Array(a) => println!("{}array pattern [{} | {}] ({})", p, a.head, a.tail, a.span),
            Pattern::EmptyArray(a) => println!("{}empty array pattern [] ({})", p, a.span),
            Pattern::Name(n) => println!("{}name pattern {} ({})", p, n.name, n.span),
            Pattern::Binding(n) => println!("{}name binding {} ({})", p, n.name, n.span),
            Pattern::Struct(s) => println!(
                "{}struct pattern {}{{{}}} (span: {}, type: {})",
                p,
                s.name,
                join(s.bindings.iter(), ","),
                s.span,
                s.typ
            ),
            Pattern::Any(span) => println!("{}any pattern ({})", p, span),
            Pattern::Nil(span) => println!("{}nil pattern ({})", p, span),
            Pattern::Optional(o) => println!("{}optional pattern {} ({})", p, o.binding, o.span),
            Pattern::Ok(o) => {
                println!("{}ok", p);
                o.inner.print(level + 1);
            }
            Pattern::Error(e) => {
                println!("{p}error");
                e.inner.print(level + 1);
            }
        }
    }
}
