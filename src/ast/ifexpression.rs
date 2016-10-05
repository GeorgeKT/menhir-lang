use ast::*;
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct IfExpression
{
    pub condition: Expression,
    pub on_true: Expression,
    pub on_false: Expression,
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
                    to_execute: self.on_false.clone(),
                    span: self.on_false.span(),
                },
            ],
            typ: self.typ.clone(),
            span: self.span.clone(),
        }
    }
}

pub fn if_expression(condition: Expression, on_true: Expression, on_false: Expression, span: Span) -> Expression
{
    Expression::If(Box::new(IfExpression{
        condition: condition,
        on_true: on_true,
        on_false: on_false,
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
        println!("{} else", p);
        self.on_false.print(level + 2);
    }
}
