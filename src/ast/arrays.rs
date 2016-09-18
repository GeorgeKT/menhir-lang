use ast::{Expression, TreePrinter, Literal, Type, prefix};
use span::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayLiteral
{
    pub elements: Vec<Expression>,
    pub array_type: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayGenerator
{
    // syntax [expr | x <- array]
    pub left: Expression,
    pub var: String,
    pub iterable: Expression,
    pub array_type: Type,
    pub span: Span,
}

/*
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IndexOperation
{
    pub target: Box<Expression>,
    pub index_expr: Box<Expression>,
    pub span: Span,
}
*/


/*
pub fn index_op(target: Expression, index_expr: Expression, span: Span) -> Expression
{
    Expression::IndexOperation(
        IndexOperation{
            target: Box::new(target),
            index_expr: Box::new(index_expr),
            span: span,
        }
    )
}
*/

pub fn array_lit(e: Vec<Expression>, span: Span) -> Literal
{
    Literal::Array(ArrayLiteral{
        elements: e,
        array_type: Type::Unknown,
        span: span,
    })
}

pub fn array_generator(left: Expression, var: &str, iterable: Expression, span: Span) -> Expression
{
    Expression::ArrayGenerator(Box::new(ArrayGenerator{
        left: left,
        var: var.into(),
        iterable: iterable,
        array_type: Type::Unknown,
        span: span,
    }))
}

impl TreePrinter for ArrayGenerator
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}array generator {} ({})", p, self.var, self.span);
        self.left.print(level + 1);
        self.iterable.print(level + 1)
    }
}
