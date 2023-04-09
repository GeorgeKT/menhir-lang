use crate::ast::{prefix, Expression, StructPattern, TreePrinter, Type};
use itertools::free::join;
use crate::span::Span;

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub enum BindingType {
    Name(String),
    Struct(StructPattern),
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Binding {
    pub mutable: bool,
    pub binding_type: BindingType,
    pub init: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct GlobalBinding {
    pub mutable: bool,
    pub name: String,
    pub init: Expression,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct BindingList {
    pub bindings: Vec<Binding>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct BindingExpression {
    pub bindings: Vec<Binding>,
    pub expression: Expression,
    pub typ: Type,
    pub span: Span,
}

pub fn name_binding(name: String, init: Expression, mutable: bool, span: Span) -> Binding {
    Binding {
        mutable: mutable,
        binding_type: BindingType::Name(name),
        init: init,
        typ: Type::Unknown,
        span: span,
    }
}

pub fn binding(bt: BindingType, init: Expression, mutable: bool, span: Span) -> Binding {
    Binding {
        mutable: mutable,
        binding_type: bt,
        init: init,
        typ: Type::Unknown,
        span: span,
    }
}

pub fn global_binding(name: String, init: Expression, mutable: bool, span: Span) -> GlobalBinding {
    GlobalBinding {
        mutable: mutable,
        name: name,
        init: init,
        typ: Type::Unknown,
        span: span,
    }
}

pub fn bindings(bindings: Vec<Binding>, span: Span) -> Expression {
    Expression::Bindings(Box::new(BindingList {
        bindings: bindings,
        span: span,
    }))
}

impl TreePrinter for Binding {
    fn print(&self, level: usize) {
        let p = prefix(level);
        match self.binding_type {
            BindingType::Name(ref name) => {
                println!("{}binding {} ({}) =", p, name, self.span);
            }

            BindingType::Struct(ref s) => {
                println!("{}struct binding {{{}}} =", p, join(s.bindings.iter(), ","));
            }
        }

        self.init.print(level + 1);
    }
}

impl TreePrinter for GlobalBinding {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}global {} ({}) =", p, self.name, self.span);
        self.init.print(level + 1);
    }
}

impl TreePrinter for BindingList {
    fn print(&self, level: usize) {
        for c in &self.bindings {
            c.print(level);
        }
    }
}
