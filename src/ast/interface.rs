use ast::*;
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Interface
{
    pub name: String,
    pub functions: Vec<FunctionSignature>,
    pub typ: Type,
    pub span: Span,
}

pub fn interface(name: String, functions: Vec<FunctionSignature>, span: Span) -> Interface
{
    Interface{
        name: name,
        functions: functions,
        typ: Type::Unknown,
        span: span,
    }
}

impl TreePrinter for Interface
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}interface {} ({})", p, self.name, self.span);
        for func in &self.functions {
            func.print(level + 1);
        }
    }
}
