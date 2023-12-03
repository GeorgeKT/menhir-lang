use crate::ast::{prefix, Expression, GenericMapping, NameRef, TreePrinter, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Call {
    pub callee: NameRef,
    pub args: Vec<Expression>,
    pub span: Span,
    pub generic_args: GenericMapping,
    pub return_type: Type,
    pub function_type: Type,
}

impl Call {
    pub fn new(callee: NameRef, args: Vec<Expression>, span: Span) -> Call {
        Call {
            callee,
            args,
            span,
            generic_args: GenericMapping::new(),
            return_type: Type::Unknown,
            function_type: Type::Unknown,
        }
    }

    pub fn callee_type(&self) -> Type {
        self.function_type.clone()
    }
}

impl TreePrinter for Call {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}call {} {}", p, self.callee.name, self.span);
        for a in &self.args {
            a.print(level + 1);
        }
    }
}
