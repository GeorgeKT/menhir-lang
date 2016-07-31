use ast::{Expression, Type, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Binding 
{
    pub name: String,
    pub init: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LetExpression
{
    pub bindings: Vec<Binding>,
    pub expression: Expression,
    pub typ: Type,
    pub span: Span,
}

pub fn let_binding(name: String, init: Expression, span: Span) -> Binding
{
    Binding{
        name: name,
        init: init,
        typ: Type::Unknown,
        span: span,
    }
}

pub fn let_expression(bindings: Vec<Binding>, e: Expression, span: Span) -> Expression
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
        for c in &self.bindings 
        {
            println!("{} binding {} ({}) =", p, c.name, c.span);
            c.init.print(level + 2);
        }
        self.expression.print(level + 1);
    }
}
