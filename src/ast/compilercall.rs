use crate::ast::{prefix, IntSize, TreePrinter, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

use super::{Call, Expression};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct DropFlag(pub String);

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum CompilerCall {
    SizeOf {
        typ: Type,
        int_size: IntSize,
        span: Span,
    },
    Drop {
        obj: Expression,
        span: Span,
        drop_flag: Option<DropFlag>,
        destructor_call: Option<Call>,
    },
}

impl CompilerCall {
    pub fn get_type(&self) -> Type {
        match self {
            CompilerCall::SizeOf { int_size, .. } => Type::UInt(*int_size),
            CompilerCall::Drop { .. } => Type::Void,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CompilerCall::SizeOf { span, .. } => span.clone(),
            CompilerCall::Drop { span, .. } => span.clone(),
        }
    }
}

pub fn cc_size_of(typ: Type, int_size: IntSize, span: Span) -> Expression {
    Expression::CompilerCall(Box::new(CompilerCall::SizeOf { typ, int_size, span }))
}

pub fn cc_drop(obj: Expression, span: Span, destructor_call: Option<Call>, drop_flag: Option<DropFlag>) -> Expression {
    Expression::CompilerCall(Box::new(CompilerCall::Drop {
        obj,
        span,
        destructor_call,
        drop_flag,
    }))
}

impl TreePrinter for CompilerCall {
    fn print(&self, level: usize) {
        let p = prefix(level);
        match self {
            CompilerCall::SizeOf { typ, span, .. } => println!("{}@size({}) (span: {})", p, typ, span),
            CompilerCall::Drop {
                obj,
                span,
                destructor_call,
                drop_flag,
            } => {
                println!("{}@drop (span: {}) (flag: {:?})", p, span, drop_flag);
                obj.print(level + 1);
                println!("{p} destructor_call:");
                if let Some(ds) = destructor_call {
                    ds.print(level + 2);
                }
            }
        }
    }
}
