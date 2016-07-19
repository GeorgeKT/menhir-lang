use ast::{Block, TreePrinter};

#[derive(Debug, Eq, PartialEq)]
pub struct Module
{
    pub name: String,
    pub block: Block,
}

impl Module
{
    pub fn new(name: &str, block: Block) -> Module
    {
        Module{
            name: name.into(),
            block: block,
        }
    }
}

impl TreePrinter for Module
{
    fn print(&self, level: usize)
    {
        self.block.print(level)
    }
}
