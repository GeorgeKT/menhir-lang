use ast::{TreePrinter, prefix};
use compileerror::{Span};


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NameRef
{
    pub name: String,
    pub span: Span,
}

impl NameRef
{
    pub fn new(name: String, span: Span) -> NameRef
    {
        NameRef{
            name: name,
            span: span,
        }
    }
}

impl TreePrinter for NameRef
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}name {} ({})", p, self.name, self.span);
    }
}
