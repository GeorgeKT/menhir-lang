use crate::ast::{prefix, Expression, TreePrinter, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

use super::Range;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum IndexMode {
    Index(Expression),
    Range(Range),
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct IndexOperation {
    pub target: Expression,
    pub index_expr: IndexMode,
    pub span: Span,
    pub typ: Type,
}

pub fn index_op(target: Expression, index_expr: IndexMode, span: Span) -> Expression {
    Expression::IndexOperation(Box::new(IndexOperation {
        target,
        index_expr,
        span,
        typ: Type::Unknown,
    }))
}

impl TreePrinter for IndexOperation {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}index operation ({})", p, self.span);
        println!("{} target:", p);
        self.target.print(level + 2);
        println!("{} index:", p);
        match &self.index_expr {
            IndexMode::Index(i) => i.print(level + 2),
            IndexMode::Range(r) => r.print(level + 2),
        }
    }
}
