use ast::{Expression};
use compileerror::{Span};
use parser::{Operator};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnaryOp
{
    pub operator: Operator,
    pub expression: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryOp
{
    pub operator: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}


pub fn bin_op(op: Operator, left: Expression, right: Expression, span: Span) -> Expression
{
    Expression::BinaryOp(BinaryOp{
        operator: op,
        left: Box::new(left),
        right: Box::new(right),
        span: span,
    })
}

pub fn bin_op2(op: Operator, left: Expression, right: Box<Expression>, span: Span) -> Expression
{
    Expression::BinaryOp(BinaryOp{
        operator: op,
        left: Box::new(left),
        right: right,
        span: span,
    })
}


pub fn unary_op(operator: Operator, expression: Expression, span: Span) -> Expression
{
    Expression::UnaryOp(UnaryOp{
        operator: operator,
        expression: Box::new(expression),
        span: span,
    })
}
