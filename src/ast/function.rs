use ast::{Type, Expression, TreePrinter, prefix, func_type};
use compileerror::{Span};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ArgumentPassingMode
{
    ByValue,
    ByPtr,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Argument
{
    pub name: String,
    pub typ: Type,
    pub passing_mode: ArgumentPassingMode,
    pub span: Span,
}

impl Argument
{
    pub fn new(name: String, typ: Type, span: Span) -> Argument
    {
        Argument{
            name: name,
            typ: typ,
            passing_mode: ArgumentPassingMode::ByValue, // Will be filled in during type checking
            span: span,
        }
    }
}

impl TreePrinter for Argument
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}{}: {} (span; {})", p, self.name, self.typ, self.span);
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
    pub type_checked: bool,
    pub generics_resolved: bool,
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
            type_checked: false,
            generics_resolved: false,
        }
    }

    pub fn is_generic(&self) -> bool
    {
        self.sig.return_type.is_generic() || self.sig.args.iter().any(|a| a.typ.is_generic())
    }

    pub fn get_type(&self) -> Type
    {
        func_type(self.sig.args.iter().map(|a| a.typ.clone()).collect(), self.sig.return_type.clone())
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

pub fn sig(name: &str, ret: Type, args: Vec<Argument>, span: Span) -> FunctionSignature
{
    FunctionSignature{
        name: name.into(),
        return_type: ret,
        args: args,
        span: span,
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
