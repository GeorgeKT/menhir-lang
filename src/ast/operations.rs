use ast::{Expression, Type, UnaryOperator, BinaryOperator};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnaryOp
{
    pub operator: UnaryOperator,
    pub expression: Expression,
    pub span: Span,
    pub typ: Type,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BinaryOp
{
    pub operator: BinaryOperator,
    pub left: Expression,
    pub right: Expression,
    pub span: Span,
    pub typ: Type,
    pub precedence: usize,
}


pub fn bin_op(op: BinaryOperator, left: Expression, right: Expression, span: Span) -> Expression
{
    bin_op_with_type(op, left, right, span, Type::Unknown)
}

#[cfg(test)]
pub fn bin_op_with_precedence(op: BinaryOperator, left: Expression, right: Expression, span: Span, precedence: usize) -> Expression
{
    Expression::BinaryOp(Box::new(BinaryOp{
        operator: op,
        left: left,
        right: right,
        span: span,
        typ: Type::Unknown,
        precedence: precedence,
    }))
}

pub fn bin_op_with_type(op: BinaryOperator, left: Expression, right: Expression, span: Span, typ: Type) -> Expression
{
    Expression::BinaryOp(Box::new(BinaryOp{
        operator: op,
        left: left,
        right: right,
        span: span,
        typ: typ,
        precedence: op.precedence(),
    }))
}

pub fn unary_op(operator: UnaryOperator, expression: Expression, span: Span) -> Expression
{
    Expression::UnaryOp(Box::new(UnaryOp{
        operator: operator,
        expression: expression,
        span: span,
        typ: Type::Unknown,
    }))
}
