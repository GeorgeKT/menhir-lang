use super::Type;
use crate::ast::Function;
use crate::span::Span;
use itertools::join;
use serde_derive::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone, Serialize, Deserialize)]
pub struct ImportName {
    namespace: Vec<String>,
    pub span: Span,
}

impl ImportName {
    pub fn new(namespace: Vec<String>, span: Span) -> ImportName {
        ImportName { namespace, span }
    }

    pub fn to_namespace_string(&self) -> String {
        join(self.namespace.iter(), "::")
    }
}

impl fmt::Display for ImportName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_namespace_string())
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
pub enum SymbolType {
    Normal,
    Global,
    External,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Symbol {
    pub name: String,
    pub typ: Type,
    pub mutable: bool,
    pub span: Span,
    pub symbol_type: SymbolType,
}

pub type SymbolPtr = Rc<Symbol>;

impl Symbol {
    pub fn new(name: &str, typ: &Type, mutable: bool, span: &Span, symbol_type: SymbolType) -> SymbolPtr {
        Rc::new(Symbol {
            name: name.into(),
            typ: typ.clone(),
            mutable,
            span: span.clone(),
            symbol_type,
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct Import {
    pub namespace: String,
    pub symbols: HashMap<String, SymbolPtr>,
    pub generics: HashMap<String, Function>,
    pub imported_symbols: HashMap<String, SymbolPtr>,
}

impl Import {
    pub fn new(namespace: String) -> Import {
        Import {
            namespace,
            symbols: HashMap::new(),
            generics: HashMap::new(),
            imported_symbols: HashMap::new(),
        }
    }

    pub fn resolve(&self, name: &str, allow_imported_symbols: bool) -> Option<SymbolPtr> {
        let resolve = |symbols: &HashMap<String, SymbolPtr>| {
            if let Some(s) = symbols.get(name) {
                return Some(s.clone());
            }

            let namespaced = format!("{}::{}", self.namespace, name);
            if let Some(s) = symbols.get(&namespaced) {
                return Some(s.clone());
            }
            None
        };

        resolve(&self.symbols).or_else(|| {
            if allow_imported_symbols {
                resolve(&self.imported_symbols)
            } else {
                None
            }
        })
    }
}

impl fmt::Display for Import {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Module {}:", self.namespace)?;
        for symbol in self.symbols.values() {
            writeln!(f, " S {}: {}", symbol.name, symbol.typ)?;
        }

        for function in self.generics.values() {
            writeln!(f, " G {}: {}", function.sig.name, function.sig.typ)?;
        }

        for symbol in self.imported_symbols.values() {
            writeln!(f, " U {}: {}", symbol.name, symbol.typ)?;
        }

        Ok(())
    }
}

pub type ImportMap = HashMap<String, Rc<Import>>;
