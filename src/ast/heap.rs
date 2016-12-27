use ast::{Type, Expression, TreePrinter, prefix};
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NewExpression
{
    pub inner: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DeleteExpression
{
    pub inner: Expression,
    pub span: Span,
}


pub fn new(inner: Expression, span: Span) -> Expression
{
    Expression::New(
        Box::new(
            NewExpression{
                inner: inner,
                typ: Type::Unknown,
                span: span,
            }
        )
    )
}

pub fn new_with_type(inner: Expression, typ: Type, span: Span) -> Expression
{
    Expression::New(
        Box::new(
            NewExpression{
                inner: inner,
                typ: typ,
                span: span,
            }
        )
    )
}

pub fn delete(inner: Expression, span: Span) -> Expression
{
    Expression::Delete(
        Box::new(
            DeleteExpression{
                inner: inner,
                span: span,
            }
        )
    )
}

impl TreePrinter for NewExpression
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}new (span: {}, typ: {})", p, self.span, self.typ);
        self.inner.print(level + 1)
    }
}

impl TreePrinter for DeleteExpression
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}delete (span: {})", p, self.span);
        self.inner.print(level + 1)
    }
}
