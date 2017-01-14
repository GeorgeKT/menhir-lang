use ast::{Expression, Type};
use span::{Span};
use parser::{Operator};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnaryOp
{
    pub operator: Operator,
    pub expression: Expression,
    pub span: Span,
    pub typ: Type,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryOp
{
    pub operator: Operator,
    pub left: Expression,
    pub right: Expression,
    pub span: Span,
    pub typ: Type,
}


pub fn bin_op(op: Operator, left: Expression, right: Expression, span: Span) -> Expression
{
    bin_op_with_type(op, left, right, span, Type::Unknown)
}

pub fn bin_op_with_type(op: Operator, left: Expression, right: Expression, span: Span, typ: Type) -> Expression
{
    Expression::BinaryOp(Box::new(BinaryOp{
        operator: op,
        left: left,
        right: right,
        span: span,
        typ: typ,
    }))
}

pub fn unary_op(operator: Operator, expression: Expression, span: Span) -> Expression
{
    Expression::UnaryOp(Box::new(UnaryOp{
        operator: operator,
        expression: expression,
        span: span,
        typ: Type::Unknown,
    }))
}
