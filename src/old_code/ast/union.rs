use ast::{Function, Argument, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnionCase
{
    pub name: String,
    pub vars: Vec<Argument>,
    pub span: Span,
}

impl UnionCase
{
    pub fn new(name: String, span: Span) -> UnionCase
    {
        UnionCase{
            name: name,
            vars: Vec::new(),
            span: span,
        }
    }
}

impl TreePrinter for UnionCase
{
    fn print(&self, level: usize)
    {
        println!("{}case {} (span: {})", prefix(level), self.name, self.span);
        for v in &self.vars {
            v.print(level + 1);
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Union
{
    pub name: String,
    pub public: bool,
    pub cases: Vec<UnionCase>,
    pub functions: Vec<Function>,
    pub span: Span,
}

impl Union
{
    pub fn new(name: String, public: bool, span: Span) -> Union
    {
        Union{
            name: name,
            public: public,
            cases: Vec::new(),
            functions: Vec::new(),
            span: span,
        }
    }
}

impl TreePrinter for Union
{
    fn print(&self, level: usize)
    {
        println!("{}union {} (public: {}, span: {})", prefix(level), self.name, self.public, self.span);
        for c in &self.cases {
            c.print(level + 1);
        }

        for fun in &self.functions {
            fun.print(level + 1);
        }
    }
}
