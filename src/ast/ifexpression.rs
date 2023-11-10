use crate::ast::*;
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct IfExpression {
    pub condition: Expression,
    pub on_true: Expression,
    pub on_false: Option<Expression>,
    pub span: Span,
    pub typ: Type,
}

pub fn single_if_expression(condition: Expression, on_true: Expression, span: Span) -> Expression {
    Expression::If(Box::new(IfExpression {
        condition,
        on_true,
        on_false: None,
        span,
        typ: Type::Unknown,
    }))
}

pub fn if_expression(condition: Expression, on_true: Expression, on_false: Expression, span: Span) -> Expression {
    Expression::If(Box::new(IfExpression {
        condition,
        on_true,
        on_false: Some(on_false),
        span,
        typ: Type::Unknown,
    }))
}

impl TreePrinter for IfExpression {
    fn print(&self, level: usize) {
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
