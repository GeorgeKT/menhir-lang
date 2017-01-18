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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ForLoop
{
    pub loop_variable: String,
    pub iterable: Expression,
    pub body: Expression,
    pub span: Span,
}

pub fn for_loop(loop_variable: &str, iterable: Expression, body: Expression, span: Span) -> Expression
{
    Expression::For(Box::new(ForLoop{
        loop_variable: loop_variable.into(),
        iterable: iterable,
        body: body,
        span: span,
    }))
}


impl TreePrinter for ForLoop
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}for {}(span: {})", p, self.loop_variable, self.span);
        self.iterable.print(level + 1);
        self.body.print(level + 1);
    }
}
