use ast::{Expression, TreePrinter, prefix};
use compileerror::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchCase
{
    pub match_expr: Expression,
    pub to_execute: Expression,
}

pub fn match_case(e: Expression, to_execute: Expression) -> MatchCase
{
    MatchCase{
        match_expr: e,
        to_execute: to_execute,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchExpression
{
    pub target: Box<Expression>,
    pub cases: Vec<MatchCase>,
    pub span: Span,
}

pub fn match_expression(target: Expression, cases: Vec<MatchCase>, span: Span) -> MatchExpression
{
    MatchExpression{
        target: Box::new(target),
        cases: cases,
        span: span,
    }
}

impl TreePrinter for MatchExpression
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}match ({})", p, self.span);
        self.target.print(level + 1);
        for c in &self.cases {
            println!("{} case", p);
            c.match_expr.print(level + 2);
            println!("{} =>", p);
            c.to_execute.print(level + 2);
        }
    }
}
