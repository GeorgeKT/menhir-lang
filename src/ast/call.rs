use ast::{Expression, Type, NameRef, TreePrinter, prefix};
use span::{Span};
use passes::GenericMapper;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Call
{
    pub callee: NameRef,
    pub args: Vec<Expression>,
    pub span: Span,
    pub generic_args: GenericMapper,
    pub return_type: Type,
}

impl Call
{
    pub fn new(callee: NameRef, args: Vec<Expression>, span: Span) -> Call
    {
        Call{
            callee: callee,
            args: args,
            span: span,
            generic_args: GenericMapper::new(),
            return_type: Type::Unknown,
        }
    }
}

impl TreePrinter for Call
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}call {} {}", p, self.callee.name, self.span);
        for a in &self.args {
            a.print(level + 1);
        }
    }
}
