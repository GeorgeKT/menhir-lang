use std::fmt;
use itertools::free::join;
use span::Span;
use ast::{TreePrinter, NameRef, Literal, Type, prefix};


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct EmptyArrayPattern
{
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayPattern
{
    pub head: String,
    pub tail: String,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum StructPatternBindingMode
{
    Value,
    Pointer
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructPatternBinding
{
    pub name: String,
    pub typ: Type,
    pub mode: StructPatternBindingMode,
}

impl fmt::Display for StructPatternBinding
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self.mode {
            StructPatternBindingMode::Value => write!(f, "{}", self.name),
            StructPatternBindingMode::Pointer => write!(f, "*{}", self.name),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructPattern
{
    pub name: String,
    pub bindings: Vec<StructPatternBinding>,
    pub typ: Type,
    pub span: Span,
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptionalPattern
{
    pub binding: String,
    pub span: Span,
    pub inner_type: Type,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Pattern
{
    Literal(Literal),
    Array(ArrayPattern), // [hd | tail]
    EmptyArray(EmptyArrayPattern),
    Name(NameRef),
    Struct(StructPattern),
    Any(Span),
    Nil(Span),
    Optional(OptionalPattern),
}

impl Pattern
{
    pub fn span(&self) -> Span
    {
        match *self
        {
            Pattern::Literal(ref l) => l.span(),
            Pattern::Array(ref a) => a.span.clone(),
            Pattern::EmptyArray(ref a) => a.span.clone(),
            Pattern::Name(ref n) => n.span.clone(),
            Pattern::Struct(ref s) => s.span.clone(),
            Pattern::Any(ref span) |
            Pattern::Nil(ref span) => span.clone(),
            Pattern::Optional(ref o) => o.span.clone(),
        }
    }
}

pub fn array_pattern(head: &str, tail: &str, span: Span) -> Pattern
{
    Pattern::Array(ArrayPattern{
        head: head.into(),
        tail: tail.into(),
        span: span,
    })
}

pub fn empty_array_pattern(span: Span) -> Pattern
{
    Pattern::EmptyArray(EmptyArrayPattern{span: span})
}

pub fn struct_pattern(name: &str, bindings: Vec<StructPatternBinding>, typ: Type, span: Span) -> StructPattern
{
    StructPattern{
        name: name.into(),
        bindings: bindings,
        typ: typ,
        span: span,
    }
}

pub fn optional_pattern(binding: String, span: Span) -> Pattern
{
    Pattern::Optional(OptionalPattern{
        binding: binding.into(),
        span: span,
        inner_type: Type::Unknown,
    })
}

impl TreePrinter for Pattern
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self
        {
            Pattern::Literal(ref l) => l.print(level),
            Pattern::Array(ref a) => println!("{}array pattern [{} | {}] ({})", p, a.head, a.tail, a.span),
            Pattern::EmptyArray(ref a) => println!("{}empty array pattern [] ({})", p, a.span),
            Pattern::Name(ref n) => println!("{}name pattern {} ({})", p, n.name, n.span),
            Pattern::Struct(ref s) => println!("{}struct pattern {}{{{}}} (span: {}, type: {})", p, s.name, join(s.bindings.iter(), ","), s.span, s.typ),
            Pattern::Any(ref span) => println!("{}any pattern ({})", p, span),
            Pattern::Nil(ref span) => println!("{}nil pattern ({})", p, span),
            Pattern::Optional(ref o) => println!("{}optional pattern {} ({})", p, o.binding, o.span),
        }
    }
}
