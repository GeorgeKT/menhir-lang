use std::ops::Deref;
use ast::{Expression, TreePrinter, prefix};
use compileerror::{CompileResult, ErrorCode, Span, err};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Call
{
    pub name: Box<Expression>,
    pub args: Vec<Expression>,
    pub span: Span,
}

impl Call
{
    pub fn new(name: Expression, args: Vec<Expression>, span: Span) -> Call
    {
        Call{
            name: Box::new(name),
            args: args,
            span: span,
        }
    }

    pub fn get_function_name(&self) -> CompileResult<String>
    {
        match *self.name.deref()
        {
            Expression::NameRef(ref nr) => Ok(nr.name.clone()),
            _ => err(self.name.span().start, ErrorCode::TypeError, format!("Unable to determine function name"))
        }
    }
}

impl TreePrinter for Call
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}call {}", p, self.span);
        self.name.print(level + 1);
        for a in &self.args {
            a.print(level + 2);
        }
    }
}
