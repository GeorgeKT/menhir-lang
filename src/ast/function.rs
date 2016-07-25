use ast::{Type, Expression, TreePrinter, prefix};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Argument
{
    pub name: String,
    pub typ: Type,
    pub constant: bool,
    pub span: Span,
}

impl Argument
{
    pub fn new(name: String, typ: Type, constant: bool, span: Span) -> Argument
    {
        Argument{
            name: name,
            typ: typ,
            constant: constant,
            span: span,
        }
    }
}

impl TreePrinter for Argument
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}{}: {} (constant: {}, span; {})", p, self.name, self.typ, self.constant, self.span);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionSignature
{
    pub name: String,
    pub return_type: Type,
    pub args: Vec<Argument>,
    pub span: Span,
}

impl TreePrinter for FunctionSignature
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}sig {} (span: {})", p, self.name, self.span);
        println!("{} return_type: {}", p, self.return_type);
        println!("{} args:", p);
        for a in &self.args {
            a.print(level + 2);
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Function
{
    pub sig: FunctionSignature,
    pub public: bool,
    pub expression: Box<Expression>,
    pub span: Span,
}

impl Function
{
    pub fn new(sig: FunctionSignature, public: bool, expr: Expression, span: Span) -> Function
    {
        Function{
            sig: sig,
            public: public,
            expression: Box::new(expr),
            span: span,
        }
    }
}

impl TreePrinter for Function
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}function ({})", p, self.span);
        self.sig.print(level + 1);
        self.expression.print(level + 2)
    }
}

/*

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ExternalFunction
{
    pub sig: FunctionSignature,
    pub span: Span,
}

impl ExternalFunction
{
    pub fn new(sig: FunctionSignature, span: Span) -> ExternalFunction
    {
        ExternalFunction{
            sig: sig,
            span: span,
        }
    }
}

impl TreePrinter for ExternalFunction
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}external function {} (span: {})", p, self.sig.name, self.span);
        println!("{} return_type: {}", p, self.sig.return_type);
        println!("{} args:", p);
        for a in &self.sig.args {
            a.print(level + 2);
        }
    }
}
*/
