use crate::ast::{Expression, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

use super::DropFlag;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Block {
    pub expressions: Vec<Expression>,
    // expressions to execute before leaving the block
    // (destructors mostly)
    pub drop_flags: Vec<DropFlag>,
    pub deferred_expressions: Vec<Expression>,
    pub typ: Type,
    pub span: Span,
}

pub fn block(e: Vec<Expression>, span: Span) -> Expression {
    Expression::Block(Box::new(Block {
        expressions: e,
        drop_flags: Vec::new(),
        deferred_expressions: Vec::new(),
        typ: Type::Unknown,
        span,
    }))
}
