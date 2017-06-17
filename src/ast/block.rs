use ast::{Expression, Type};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Block
{
    pub expressions: Vec<Expression>,
    pub typ: Type,
    pub span: Span,
}

pub fn block(e: Vec<Expression>, span: Span) -> Expression
{
    Expression::Block(Box::new(Block{
        expressions: e,
        typ: Type::Unknown,
        span: span,
    }))
}
