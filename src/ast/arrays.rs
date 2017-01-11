use ast::{Expression, Literal, Type};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayLiteral
{
    pub elements: Vec<Expression>,
    pub array_type: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayGenerator
{
    // syntax [expr | x <- array]
    pub left: Expression,
    pub var: String,
    pub iterable: Expression,
    pub array_type: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayToSlice
{
    pub inner: Expression,
    pub slice_type: Type,
    pub span: Span,
}

pub fn array_to_slice(inner: Expression, span: Span) -> Expression
{
    Expression::ArrayToSlice(
        Box::new(
            ArrayToSlice{
                inner: inner,
                slice_type: Type::Unknown,
                span: span,
            }
        )
    )
}

/*
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IndexOperation
{
    pub target: Box<Expression>,
    pub index_expr: Box<Expression>,
    pub span: Span,
}
*/


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

pub fn array_lit(e: Vec<Expression>, span: Span) -> Literal
{
    Literal::Array(ArrayLiteral{
        elements: e,
        array_type: Type::Unknown,
        span: span,
    })
}
