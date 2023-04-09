use crate::ast::{prefix, Expression, Pattern, TreePrinter, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub to_execute: Expression,
    pub span: Span,
}

pub fn match_case(p: Pattern, to_execute: Expression, span: Span) -> MatchCase {
    MatchCase {
        pattern: p,
        to_execute: to_execute,
        span: span,
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct MatchExpression {
    pub target: Expression,
    pub cases: Vec<MatchCase>,
    pub typ: Type,
    pub span: Span,
}

pub fn match_expression(target: Expression, cases: Vec<MatchCase>, span: Span) -> Expression {
    Expression::Match(Box::new(MatchExpression {
        target: target,
        cases: cases,
        typ: Type::Unknown,
        span: span,
    }))
}

impl TreePrinter for MatchExpression {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}match ({}) (type {})", p, self.span, self.typ);
        self.target.print(level + 1);
        for c in &self.cases {
            println!("{} case", p);
            c.pattern.print(level + 2);
            println!("{} =>", p);
            c.to_execute.print(level + 2);
        }
    }
}
