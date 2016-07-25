use ast::{Expression};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayLiteral
{
    pub elements: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayInitializer
{
    // syntax [init; times]
    pub init: Box<Expression>,
    pub times: u64,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IndexOperation
{
    pub target: Box<Expression>,
    pub index_expr: Box<Expression>,
    pub span: Span,
}

/*
pub fn index_op(target: Expression, index_expr: Expression, span: Span) -> Expression
{
    Expression::IndexOperation(
        IndexOperation{
            target: Box::new(target),
            index_expr: Box::new(index_expr),
            span: span,
        }
    )
}
*/

pub fn array_lit(e: Vec<Expression>, span: Span) -> Expression
{
    Expression::ArrayLiteral(ArrayLiteral{
        elements: e,
        span: span,
    })
}

pub fn array_init(init: Expression, times: u64, span: Span) -> Expression
{
    Expression::ArrayInitializer(ArrayInitializer{
        init: Box::new(init),
        times: times,
        span: span,
    })
}
