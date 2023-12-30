use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

use super::prefix;
use super::Expression;
use super::TreePrinter;
use super::Type;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Range {
    pub start: Option<Expression>,
    pub end: Option<Expression>,
    pub span: Span,
    pub typ: Type,
}

pub fn range(start: Option<Expression>, end: Option<Expression>, typ: Type, span: Span) -> Expression {
    Expression::Range(Box::new(Range { start, end, span, typ }))
}

impl TreePrinter for Range {
    fn print(&self, level: usize) {
        let pfx = prefix(level);
        println!("{pfx}range {}", self.span);
        if let Some(start) = &self.start {
            println!("{pfx} start:");
            start.print(level + 1);
        }

        if let Some(end) = &self.end {
            println!("{pfx} end:");
            end.print(level + 1);
        }
    }
}
