use ast::*;
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpression
{
    pub condition: Expression,
    pub on_true: Expression,
    pub on_false: Option<Expression>,
    pub span: Span,
    pub typ: Type,
}

impl IfExpression
{
    pub fn to_match(&self) -> MatchExpression
    {
        MatchExpression{
            target: self.condition.clone(),
            cases: vec![
                MatchCase{
                    pattern: Pattern::Literal(Literal::Bool(Span::default(), true)),
                    to_execute: self.on_true.clone(),
                    span: self.on_true.span(),
                },
                MatchCase{
                    pattern: Pattern::Any(Span::default()),
                    to_execute: if let Some(ref expr) = self.on_false {
                        expr.clone()
                    } else {
                        Expression::Void
                    },
                    span: if let Some(ref expr) = self.on_false {
                        expr.span()
                    } else {
                        Span::default()
                    },
                },
            ],
            typ: self.typ.clone(),
            span: self.span.clone(),
        }
    }
}

pub fn single_if_expression(condition: Expression, on_true: Expression, span: Span) -> Expression
{
    Expression::If(Box::new(IfExpression{
        condition: condition,
        on_true: on_true,
        on_false: None,
        span: span,
        typ: Type::Unknown,
    }))
}

pub fn if_expression(condition: Expression, on_true: Expression, on_false: Expression, span: Span) -> Expression
{
    Expression::If(Box::new(IfExpression{
        condition: condition,
        on_true: on_true,
        on_false: Some(on_false),
        span: span,
        typ: Type::Unknown,
    }))
}

impl TreePrinter for IfExpression
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}if ({}) (type {})", p, self.span, self.typ);
        self.condition.print(level + 1);
        println!("{} then", p);
        self.on_true.print(level + 2);
        if let Some(ref on_false) = self.on_false {
            println!("{} else", p);
            on_false.print(level + 2);
        }
    }
}
