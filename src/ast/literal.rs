use ast::{Type, ArrayLiteral, TreePrinter, prefix};
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Literal
{
    Int(Span, u64),
    Bool(Span, bool),
    Char(Span, u8),
    Float(Span, String), // Keep as string until we generate code, so we can compare it
    String(Span, String),
    Array(ArrayLiteral),
}

impl Literal
{
    pub fn get_type(&self) -> Type
    {
        match *self
        {
            Literal::Int(_, _) => Type::Int,
            Literal::Float(_, _) => Type::Float,
            Literal::Bool(_, _) => Type::Bool,
            Literal::Char(_, _) => Type::Char,
            Literal::String(_, _) => Type::String,
            Literal::Array(ref a) => a.array_type.clone(),
        }
    }

    pub fn span(&self) -> Span
    {
        match *self
        {
            Literal::Int(ref span, _) => span.clone(),
            Literal::Float(ref span, _) => span.clone(),
            Literal::Bool(ref span, _) => span.clone(),
            Literal::Char(ref span, _) => span.clone(),
            Literal::String(ref span, _) => span.clone(),
            Literal::Array(ref a) => a.span.clone(),
        }
    }
}

impl TreePrinter for Literal
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self
        {
            Literal::Int(ref s, v) => println!("{}int {} ({})", p, v, s),
            Literal::Float(ref s, ref v) => println!("{}float {} ({})", p, v, s),
            Literal::Bool(ref s, v) => println!("{}bool {} ({})", p, v, s),
            Literal::Char(ref s, v) => println!("{}char {} ({})", p, v, s),
            Literal::String(ref s, ref v) => println!("{}string {} ({})", p, v, s),
            Literal::Array(ref a) => {
                println!("{}array ({})", p, a.span);
                for e in &a.elements {
                    e.print(level + 1);
                }
            },
        }
    }
}
