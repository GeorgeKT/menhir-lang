use ast::{Expression, Type, TreePrinter, Pattern, prefix};
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchCase
{
    pub pattern: Pattern,
    pub to_execute: Expression,
    pub span: Span,
}

pub fn match_case(p: Pattern, to_execute: Expression, span: Span) -> MatchCase
{
    MatchCase{
        pattern: p,
        to_execute: to_execute,
        span: span,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchExpression
{
    pub target: Expression,
    pub cases: Vec<MatchCase>,
    pub typ: Type,
    pub span: Span,
}

pub fn match_expression(target: Expression, cases: Vec<MatchCase>, span: Span) -> Expression
{
    Expression::Match(Box::new(MatchExpression{
        target: target,
        cases: cases,
        typ: Type::Unknown,
        span: span,
    }))
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
            c.pattern.print(level + 2);
            println!("{} =>", p);
            c.to_execute.print(level + 2);
        }
    }
}
