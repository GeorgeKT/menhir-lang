use ast::{prefix, Expression, TreePrinter, Type};
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct IndexOperation {
    pub target: Expression,
    pub index_expr: Expression,
    pub span: Span,
    pub typ: Type,
}

pub fn index_op(target: Expression, index_expr: Expression, span: Span) -> Expression {
    Expression::IndexOperation(Box::new(IndexOperation {
        target: target,
        index_expr: index_expr,
        span: span,
        typ: Type::Unknown,
    }))
}

impl TreePrinter for IndexOperation {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}index operation ({})", p, self.span);
        println!("{} target:", p);
        self.target.print(level + 2);
        println!("{} index:", p);
        self.index_expr.print(level + 2);
    }
}
