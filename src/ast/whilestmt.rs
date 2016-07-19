use ast::{Expression, Block, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq)]
pub struct While
{
    pub cond: Expression,
    pub block: Block,
    pub span: Span,
}

impl While
{
    pub fn new(cond: Expression, block: Block, span: Span) -> While
    {
        While{
            cond: cond,
            block: block,
            span: span,
        }
    }
}

impl TreePrinter for While
{
    fn print(&self, level: usize)
    {
        println!("{}while {}", prefix(level), self.span);
        self.cond.print(level + 1);
        self.block.print(level + 1);
    }
}
