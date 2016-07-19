use ast::{Expression, NameRef};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjectConstruction
{
    pub object_type: NameRef,
    pub args: Vec<Expression>,
    pub span: Span,
}


pub fn object_construction(object_type: NameRef, args: Vec<Expression>, span: Span) -> Expression
{
    Expression::ObjectConstruction(ObjectConstruction{
        object_type: object_type,
        args: args,
        span: span,
    })
}
