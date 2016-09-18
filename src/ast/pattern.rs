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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructPattern
{
    pub name: String,
    pub bindings: Vec<String>,
    pub types: Vec<Type>,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Pattern
{
    Literal(Literal),
    Array(ArrayPattern), // [hd | tail]
    EmptyArray(EmptyArrayPattern),
    Name(NameRef),
    Struct(StructPattern),
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

pub fn struct_pattern(name: &str, bindings: Vec<String>, types: Vec<Type>, typ: Type, span: Span) -> StructPattern
{
    StructPattern{
        name: name.into(),
        bindings: bindings,
        types: types,
        typ: typ,
        span: span,
    }
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
        }
    }
}
