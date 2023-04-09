use crate::ast::{prefix, Expression, IntSize, TreePrinter, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum CompilerCall {
    SizeOf(Type, Span),
    Slice {
        data: Box<Expression>,
        len: Box<Expression>,
        typ: Type,
        span: Span,
    },
}

impl CompilerCall {
    pub fn get_type(&self, int_size: IntSize) -> Type {
        match *self {
            CompilerCall::SizeOf(_, _) => Type::UInt(int_size),
            CompilerCall::Slice { ref typ, .. } => typ.clone(),
        }
    }
}

impl TreePrinter for CompilerCall {
    fn print(&self, level: usize) {
        let p = prefix(level);
        match *self {
            CompilerCall::SizeOf(ref typ, ref span) => println!("{}@size({}) (span: {})", p, typ, span),
            CompilerCall::Slice {
                ref data,
                ref len,
                ref typ,
                ref span,
            } => {
                println!("{}@slice (span: {}, type: {})", p, span, typ);
                data.print(level + 1);
                len.print(level + 1);
            }
        }
    }
}
