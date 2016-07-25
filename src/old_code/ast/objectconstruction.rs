use ast::{Expression, NameRef};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ObjectConstruction
{
    pub object_type: NameRef,
    pub args: Vec<Expression>,
    pub span: Span,
}

impl ObjectConstruction
{
    pub fn new(object_type: NameRef, args: Vec<Expression>, span: Span) -> ObjectConstruction
    {
        ObjectConstruction{
            object_type: object_type,
            args: args,
            span: span,
        }
    }
}

pub fn object_construction(object_type: NameRef, args: Vec<Expression>, span: Span) -> Expression
{
    Expression::ObjectConstruction(ObjectConstruction::new(object_type, args, span))
}
