use crate::ast::{func_type, prefix, Expression, TreePrinter, Type};
use crate::span::Span;
use serde_derive::{Deserialize, Serialize};

use super::{ptr_type, FuncArg};

#[derive(Debug, Eq, PartialEq, Clone, Hash, Serialize, Deserialize)]
pub struct Argument {
    pub name: String,
    pub typ: Type,
    pub mutable: bool,
    pub span: Span,
}

impl Argument {
    pub fn new<S: Into<String>>(name: S, typ: Type, mutable: bool, span: Span) -> Argument {
        Argument {
            name: name.into(),
            typ,
            mutable,
            span,
        }
    }
}

impl TreePrinter for Argument {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}{}: {} (span: {})", p, self.name, self.typ, self.span);
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Serialize, Deserialize)]
pub struct FunctionSignature {
    pub name: String,
    pub return_type: Type,
    pub args: Vec<Argument>,
    pub span: Span,
    pub typ: Type,
    pub rvo: bool,
}

pub const RVO_RETURN_ARG: &str = "$rvo_ret";

impl FunctionSignature {
    pub fn do_rvo(&mut self) {
        // Do rvo after generics are instantiated
        if self.rvo || self.return_type.pass_by_value() || self.typ.is_generic() || self.return_type == Type::Void {
            return;
        }

        self.args.push(Argument {
            name: RVO_RETURN_ARG.to_string(),
            typ: ptr_type(self.return_type.clone()),
            mutable: true,
            span: self.span.clone(),
        });
        self.return_type = Type::Void;
        self.typ = self.get_type();
        self.rvo = true;
    }

    pub fn from_type(name: &str, typ: &Type) -> Option<FunctionSignature> {
        if let Type::Func(ref ft) = *typ {
            let s = FunctionSignature {
                name: name.into(),
                return_type: ft.return_type.clone(),
                args: ft
                    .args
                    .iter()
                    .enumerate()
                    .map(|(idx, at)| Argument::new(format!("arg{}", idx), at.typ.clone(), at.mutable, Span::default()))
                    .collect(),
                span: Span::default(),
                typ: typ.clone(),
                rvo: false,
            };

            Some(s)
        } else {
            None
        }
    }

    pub fn get_type(&self) -> Type {
        func_type(
            self.args
                .iter()
                .map(|arg| FuncArg {
                    typ: arg.typ.clone(),
                    mutable: arg.mutable,
                })
                .collect(),
            self.return_type.clone(),
        )
    }
}

impl TreePrinter for FunctionSignature {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}sig {} (span: {})", p, self.name, self.span);
        println!("{} return_type: {}", p, self.return_type);
        println!("{} args:", p);
        for a in &self.args {
            a.print(level + 2);
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct Function {
    pub sig: FunctionSignature,
    pub public: bool,
    pub expression: Expression,
    pub span: Span,
    pub type_checked: bool,
    pub generics_resolved: bool,
}

impl Function {
    pub fn new(sig: FunctionSignature, public: bool, expr: Expression, span: Span) -> Function {
        Function {
            sig,
            public,
            expression: expr,
            span,
            type_checked: false,
            generics_resolved: false,
        }
    }

    pub fn is_generic(&self) -> bool {
        self.sig.return_type.is_generic() || self.sig.args.iter().any(|a| a.typ.is_generic())
    }
}

impl TreePrinter for Function {
    fn print(&self, level: usize) {
        let p = prefix(level);
        println!("{}function ({})", p, self.span);
        self.sig.print(level + 1);
        self.expression.print(level + 2)
    }
}

pub fn sig(name: &str, ret: Type, args: Vec<Argument>, span: Span) -> FunctionSignature {
    FunctionSignature {
        name: name.into(),
        return_type: ret,
        args,
        span,
        typ: Type::Unknown,
        rvo: false,
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum External {
    Function {
        sig: FunctionSignature,
        span: Span,
    },
    Variable {
        name: String,
        span: Span,
        typ: Type,
        mutable: bool,
        thread_local: bool,
    },
}

impl External {
    pub fn name(&self) -> &str {
        match self {
            External::Function { sig, .. } => &sig.name,
            External::Variable { name, .. } => name,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            External::Function { span, .. } => span,
            External::Variable { span, .. } => span,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            External::Function { sig, .. } => sig.typ.clone(),
            External::Variable { typ, .. } => typ.clone(),
        }
    }

    pub fn mutable(&self) -> bool {
        match self {
            External::Variable { mutable, .. } => *mutable,
            External::Function { .. } => false,
        }
    }
}

impl TreePrinter for External {
    fn print(&self, level: usize) {
        let p = prefix(level);
        match self {
            External::Function { sig, span } => {
                println!("{}external function {} (span: {})", p, sig.name, span);
                sig.print(level + 1);
            }
            External::Variable {
                name,
                span,
                typ,
                mutable,
                thread_local,
            } => {
                println!(
                    "{p}external {} {} {name}: {typ} (span: {span})",
                    if *thread_local { "thread_local" } else { "" },
                    if *mutable { "var" } else { "let" }
                );
            }
        }
    }
}
