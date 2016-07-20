use ast::{Expression, Block, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ElsePart
{
    Empty,
    Block(Block),
    If(Box<If>),
}

impl TreePrinter for ElsePart
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        match *self
        {
            ElsePart::Empty => {
                println!("{}no else", p);
            },
            ElsePart::Block(ref b) => {
                println!("{}else", p);
                b.print(level + 1);
            },
            ElsePart::If(ref i) => i.print(level),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct If
{
    pub cond: Expression,
    pub if_block: Block,
    pub else_part: ElsePart,
    pub span: Span,
}

impl If
{
    pub fn new(cond: Expression, if_block: Block, ep: ElsePart, span: Span) -> If
    {
        If{
            cond: cond,
            if_block: if_block,
            else_part: ep,
            span: span,
        }
    }
}

impl TreePrinter for If
{
    fn print(&self, level: usize)
    {
        println!("{}if {}", prefix(level), self.span);
        self.cond.print(level + 1);
        self.if_block.print(level + 1);
        self.else_part.print(level + 1);
    }
}
