use crate::ast::{prefix, Expression, TreePrinter, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct NewExpression {
    pub inner: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct DeleteExpression {
    pub inner: Expression,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct AddressOfExpression {
    pub inner: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct DereferenceExpression {
    pub inner: Expression,
    pub typ: Type,
    pub span: Span,
}

pub fn new(inner: Expression, span: Span) -> Expression {
    Expression::New(Box::new(NewExpression {
        inner,
        typ: Type::Unknown,
        span,
    }))
}

pub fn new_with_type(inner: Expression, typ: Type, span: Span) -> Expression {
    Expression::New(Box::new(NewExpression { inner, typ, span }))
}

pub fn delete(inner: Expression, span: Span) -> Expression {
    Expression::Delete(Box::new(DeleteExpression { inner, span }))
}

pub fn address_of(inner: Expression, span: Span) -> Expression {
    Expression::AddressOf(Box::new(AddressOfExpression {
        inner,
        typ: Type::Unknown,
        span,
    }))
}

pub fn dereference(inner: Expression, span: Span) -> Expression {
    Expression::Dereference(Box::new(DereferenceExpression {
        inner,
        typ: Type::Unknown,
        span,
    }))
}

impl TreePrinter for NewExpression {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}new (span: {}, typ: {})", p, self.span, self.typ);
        self.inner.print(level + 1)
    }
}

impl TreePrinter for DeleteExpression {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}delete (span: {})", p, self.span);
        self.inner.print(level + 1)
    }
}

impl TreePrinter for AddressOfExpression {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}address of (span: {})", p, self.span);
        self.inner.print(level + 1)
    }
}

impl TreePrinter for DereferenceExpression {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}dereference (span: {})", p, self.span);
        self.inner.print(level + 1)
    }
}
