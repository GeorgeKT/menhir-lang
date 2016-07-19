use ast::{Expression, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq)]
pub struct Return
{
    pub expr: Expression,
    pub span: Span,
}

impl Return
{
    pub fn new(expr: Expression, span: Span) -> Return
    {
        Return{
            expr: expr,
            span: span,
        }
    }
}

impl TreePrinter for Return
{
    fn print(&self, level: usize)
    {
        println!("{}return {}", prefix(level), self.span);
        self.expr.print(level + 1)
    }
}
