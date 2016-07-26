use ast::{Expression, Argument, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Lambda
{
    pub args: Vec<Argument>,
    pub expr: Box<Expression>,
    pub span: Span,
}

pub fn lambda(args: Vec<Argument>, expr: Expression, span: Span) -> Lambda
{
    Lambda{
        args: args,
        expr: Box::new(expr),
        span: span,
    }
}

impl TreePrinter for Lambda
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}lambda ({})", p, self.span);
        for a in &self.args {
            a.print(level + 1);
        }
    }
}
