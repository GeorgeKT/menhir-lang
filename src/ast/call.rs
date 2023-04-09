use crate::ast::{func_type, prefix, Expression, GenericMapping, IntSize, NameRef, TreePrinter, Type};
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Call {
    pub callee: NameRef,
    pub args: Vec<Expression>,
    pub span: Span,
    pub generic_args: GenericMapping,
    pub return_type: Type,
}

impl Call {
    pub fn new(callee: NameRef, args: Vec<Expression>, span: Span) -> Call {
        Call {
            callee: callee,
            args: args,
            span: span,
            generic_args: GenericMapping::new(),
            return_type: Type::Unknown,
        }
    }

    pub fn callee_type(&self, int_size: IntSize) -> Type {
        let arg_types = self.args.iter().map(|e| e.get_type(int_size)).collect();
        func_type(arg_types, self.return_type.clone())
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
