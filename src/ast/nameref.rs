use crate::ast::{prefix, TreePrinter, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct NameRef {
    pub name: String,
    pub typ: Type,
    pub span: Span,
}

impl NameRef {
    pub fn new(name: String, span: Span) -> NameRef {
        NameRef {
            name,
            typ: Type::Unknown,
            span,
        }
    }
}

impl TreePrinter for NameRef {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}name {} (span: {}, type: {})", p, self.name, self.span, self.typ);
    }
}
