use ast::{Statement, TreePrinter, prefix};

#[derive(Debug, Eq, PartialEq)]
pub struct Block
{
    pub statements: Vec<Statement>,
}

impl Block
{
    pub fn new(s: Vec<Statement>) -> Block
    {
        Block{
            statements: s,
        }
    }
}

impl TreePrinter for Block
{
    fn print(&self, level: usize)
    {
        println!("{}block: ", prefix(level));
        for s in &self.statements
        {
            s.print(level + 1);
        }
    }
}
