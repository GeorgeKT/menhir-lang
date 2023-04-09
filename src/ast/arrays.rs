use crate::ast::{Expression, Literal, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
    pub array_type: Type,
    pub span: Span,
}

pub fn array_lit(e: Vec<Expression>, span: Span) -> Literal {
    Literal::Array(ArrayLiteral {
        elements: e,
        array_type: Type::Unknown,
        span: span,
    })
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ArrayToSlice {
    pub inner: Expression,
    pub slice_type: Type,
    pub span: Span,
}

pub fn array_to_slice(inner: Expression, span: Span) -> Expression {
    Expression::ArrayToSlice(Box::new(ArrayToSlice {
        inner: inner,
        slice_type: Type::Unknown,
        span: span,
    }))
}
