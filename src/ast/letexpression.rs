use ast::{Expression, Type, TreePrinter, prefix};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetBinding
{
    pub name: String,
    pub init: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetBindingList
{
    pub bindings: Vec<LetBinding>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetExpression
{
    pub bindings: Vec<LetBinding>,
    pub expression: Expression,
    pub typ: Type,
    pub span: Span,
}

pub fn let_binding(name: String, init: Expression, span: Span) -> LetBinding
{
    LetBinding{
        name: name,
        init: init,
        typ: Type::Unknown,
        span: span,
    }
}

pub fn let_bindings(bindings: Vec<LetBinding>, span: Span) -> Expression
{
    Expression::LetBindings(Box::new(LetBindingList{
        bindings: bindings,
        span: span,
    }))
}

pub fn let_expression(bindings: Vec<LetBinding>, e: Expression, span: Span) -> Expression
{
    Expression::Let(Box::new(LetExpression{
        bindings: bindings,
        expression: e,
        typ: Type::Unknown,
        span: span,
    }))
}


impl TreePrinter for LetExpression
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}let ({})", p, self.span);
        for c in &self.bindings {
            c.print(level + 1);
        }
        self.expression.print(level + 1);
    }
}

impl TreePrinter for LetBinding
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}binding {} ({}) =", p, self.name, self.span);
        self.init.print(level + 1);
    }
}

impl TreePrinter for LetBindingList
{
    fn print(&self, level: usize)
    {
        for c in &self.bindings {
            c.print(level);
        }
    }
}
