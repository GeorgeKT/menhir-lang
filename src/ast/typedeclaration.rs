use super::{Interface, StructDeclaration, SumTypeDeclaration, TreePrinter, Type};
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeDeclaration {
    Interface(Interface),
    Struct(StructDeclaration),
    Sum(SumTypeDeclaration),
    //Alias(TypeAlias),
}

impl TypeDeclaration {
    pub fn span(&self) -> Span {
        match *self {
            TypeDeclaration::Interface(ref i) => i.span.clone(),
            TypeDeclaration::Struct(ref sd) => sd.span.clone(),
            TypeDeclaration::Sum(ref s) => s.span.clone(),
            //TypeDeclaration::Alias(ref t) => &t.typ.clone(),
        }
    }

    pub fn get_type(&self) -> Type {
        match *self {
            TypeDeclaration::Interface(ref i) => i.typ.clone(),
            TypeDeclaration::Struct(ref sd) => sd.typ.clone(),
            TypeDeclaration::Sum(ref s) => s.typ.clone(),
            //TypeDeclaration::Alias(ref t) => &t.typ.clone(),
        }
    }
}

impl TreePrinter for TypeDeclaration {
    fn print(&self, level: usize) {
        match *self {
            TypeDeclaration::Interface(ref i) => i.print(level),
            TypeDeclaration::Struct(ref sd) => sd.print(level),
            TypeDeclaration::Sum(ref s) => s.print(level),
            //TypeDeclaration::Alias(ref t) => t.print(level),
        }
    }
}
