use ast::{ModuleName, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq)]
pub struct Import
{
    pub modules: Vec<ModuleName>,
    pub span: Span,
}

impl Import
{
    pub fn new(modules: Vec<ModuleName>, span: Span) -> Import
    {
        Import{
            modules: modules,
            span: span,
        }
    }
}

impl TreePrinter for Import
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}import ({})", p, self.span);
        for m in &self.modules {
            println!("{} {} ({})", p, m.to_string(), m.span);
        }
    }
}
