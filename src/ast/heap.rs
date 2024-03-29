use crate::ast::{prefix, Expression, TreePrinter, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

use super::Block;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct NewExpression {
    pub inner: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum DeleteExpression {
    Delete { inner: Expression, span: Span },
    BlockWithDestructor { block: Block, span: Span },
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
    Expression::Delete(Box::new(DeleteExpression::Delete { inner, span }))
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

impl DeleteExpression {
    pub fn span(&self) -> Span {
        match self {
            DeleteExpression::Delete { span, .. } => span.clone(),
            DeleteExpression::BlockWithDestructor { span, .. } => span.clone(),
        }
    }
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
        match self {
            DeleteExpression::Delete { inner, span } => {
                let p = prefix(level);
                println!("{}delete (span: {})", p, span);
                inner.print(level + 1)
            }
            DeleteExpression::BlockWithDestructor { block, .. } => block.print(level),
        }
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
        println!("{}dereference (span: {}, type: {})", p, self.span, self.typ);
        self.inner.print(level + 1)
    }
}
