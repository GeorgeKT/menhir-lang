use ast::{Expression, TreePrinter, prefix};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct WhileLoop
{
    pub cond: Expression,
    pub body: Expression,
    pub span: Span,
}

pub fn while_loop(cond: Expression, body: Expression, span: Span) -> Expression
{
    Expression::While(Box::new(WhileLoop{
        cond: cond,
        body: body,
        span: span,
    }))
}


impl TreePrinter for WhileLoop
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}while (span: {})", p, self.span);
        self.cond.print(level + 1);
        self.body.print(level + 1);
    }
}
