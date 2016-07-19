use ast::{Variable, Function, Type, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq)]
pub struct Struct
{
    pub name: String,
    pub impls: Vec<Type>,
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
    pub public: bool,
    pub span: Span,
}

impl Struct
{
    pub fn new(name: String, public: bool, span: Span) -> Struct
    {
        Struct{
            name: name,
            impls: Vec::new(),
            variables: Vec::new(),
            functions: Vec::new(),
            public: public,
            span: span,
        }
    }
}

impl TreePrinter for Struct
{
    fn print(&self, level: usize)
    {
        println!("{}struct {} (public: {}, span: {})", prefix(level), self.name, self.public, self.span);
        for v in &self.variables {
            v.print(level + 1);
        }

        for fun in &self.functions {
            fun.print(level + 1);
        }
    }
}
