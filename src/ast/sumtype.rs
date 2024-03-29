use crate::ast::{prefix, StructDeclaration, TreePrinter, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SumTypeCaseDeclaration {
    pub name: String,
    pub data: Option<StructDeclaration>,
    pub span: Span,
    pub typ: Type,
}

pub fn sum_type_case_decl(name: &str, data: Option<StructDeclaration>, span: Span) -> SumTypeCaseDeclaration {
    SumTypeCaseDeclaration {
        name: name.into(),
        data,
        span,
        typ: Type::Unknown,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SumTypeDeclaration {
    pub name: String,
    pub cases: Vec<SumTypeCaseDeclaration>,
    pub implements: Vec<(Type, Span)>,
    pub span: Span,
    pub typ: Type,
}

pub fn sum_type_decl(
    name: &str,
    cases: Vec<SumTypeCaseDeclaration>,
    implements: Vec<(Type, Span)>,
    span: Span,
) -> SumTypeDeclaration {
    SumTypeDeclaration {
        name: name.into(),
        cases,
        implements,
        span,
        typ: Type::Unknown,
    }
}

impl TreePrinter for SumTypeDeclaration {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}sum {} ({})", p, self.name, self.span);
        for case in &self.cases {
            case.print(level + 1);
        }
    }
}

impl TreePrinter for SumTypeCaseDeclaration {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}case {} ({})", p, self.name, self.span);
        if let Some(ref sd) = self.data {
            sd.print(level + 1);
        }
    }
}
