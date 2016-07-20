use ast::{Expression, Block, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MatchCase
{
    pub name: String,
    pub bindings: Vec<String>,
    pub block: Block,
    pub span: Span,
}

impl MatchCase
{
    pub fn new(name: String, bindings: Vec<String>, block: Block, span: Span) -> MatchCase
    {
        MatchCase{
            name: name,
            bindings: bindings,
            block: block,
            span: span,
        }
    }
}

impl TreePrinter for MatchCase
{
    fn print(&self, level: usize)
    {
        println!("{}case {} {:?} (span: {})", prefix(level), self.name, self.bindings, self.span);
        self.block.print(level)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Match
{
    pub expr: Expression,
    pub cases: Vec<MatchCase>,
    pub span: Span,
}

impl Match
{
    pub fn new(expr: Expression, span: Span) -> Match
    {
        Match{
            expr: expr,
            cases: Vec::new(),
            span: span,
        }
    }
}

impl TreePrinter for Match
{
    fn print(&self, level: usize)
    {
        println!("{}match (span: {})", prefix(level), self.span);
        self.expr.print(level + 1);
        for c in &self.cases {
            c.print(level + 1);
        }
    }
}
