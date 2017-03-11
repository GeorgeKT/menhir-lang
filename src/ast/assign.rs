use ast::{Expression, Type, TreePrinter, prefix};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Assign
{
    pub left: Expression,
    pub right: Expression,
    pub typ: Type,
    pub span: Span,
}

pub fn assign(left: Expression, right: Expression, span: Span) -> Expression
{
    Expression::Assign(Box::new(Assign{
        left: left,
        right: right,
        typ: Type::Unknown,
        span: span,
    }))
}


impl TreePrinter for Assign
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}assign (span: {}; type: {})", p, self.span, self.typ);
        self.left.print(level + 1);
        self.right.print(level + 1);
    }
}
