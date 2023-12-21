use crate::ast::{prefix, IntSize, TreePrinter, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum CompilerCall {
    SizeOf(Type, IntSize, Span),
}

impl CompilerCall {
    pub fn get_type(&self) -> Type {
        match self {
            CompilerCall::SizeOf(_, int_size, _) => Type::UInt(*int_size),
        }
    }
}

impl TreePrinter for CompilerCall {
    fn print(&self, level: usize) {
        let p = prefix(level);
        match self {
            CompilerCall::SizeOf(typ, _, span) => println!("{}@size({}) (span: {})", p, typ, span),
        }
    }
}
