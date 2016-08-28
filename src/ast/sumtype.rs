use ast::{TreePrinter, StructDeclaration, Type, prefix};
use compileerror::{Span};


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SumTypeCase
{
    pub name: String,
    pub data: Option<StructDeclaration>,
    pub span: Span,
    pub typ: Type,
}

pub fn sum_type_case(name: &str, data: Option<StructDeclaration>, span: Span) -> SumTypeCase
{
    SumTypeCase{
        name: name.into(),
        data: data,
        span: span,
        typ: Type::Unknown,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SumType
{
    pub name: String,
    pub cases: Vec<SumTypeCase>,
    pub span: Span,
    pub typ: Type,
}

pub fn sum_type(name: &str, cases: Vec<SumTypeCase>, span: Span) -> SumType
{
    SumType{
        name: name.into(),
        cases: cases,
        span: span,
        typ: Type::Unknown,
    }
}

impl TreePrinter for SumType
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}sum {} ({})", p, self.name, self.span);
        for case in &self.cases {
            case.print(level + 1);
        }
    }
}

impl TreePrinter for SumTypeCase
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}case {} ({})", p, self.name, self.span);
        if let Some(ref sd) = self.data {
            sd.print(level + 1);
        }
    }
}
