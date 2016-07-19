use ast::{Expression, Type, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq)]
pub struct Variable
{
    pub name: String,
    pub typ: Type,
    pub is_const: bool,
    pub public: bool,
    pub init: Expression,
    pub span: Span,
}

impl Variable
{
    pub fn new(name: String, typ: Type, is_const: bool, public: bool, init: Expression, span: Span) -> Variable
    {
        Variable{
            name: name,
            typ: typ,
            is_const: is_const,
            public: public,
            init: init,
            span: span,
        }
    }
}

impl TreePrinter for Variable
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}var {}: {:?} (public: {}, constant: {}, span: {}) =",
            p, self.name, self.typ, self.public, self.is_const, self.span);
        self.init.print(level + 1);
    }
}
