use std::collections::HashMap;
use std::fmt;
use itertools::join;
use span::Span;
use super::{Type};


#[derive(Eq, PartialEq, Hash)]
pub struct ImportName
{
    namespace: Vec<String>,
    pub span: Span,
}

impl ImportName
{
    pub fn new(namespace: Vec<String>, span: Span) -> ImportName
    {
        ImportName{namespace, span}
    }

    pub fn to_namespace_string(&self) -> String
    {
        join(self.namespace.iter(), "::")
    }
}

impl fmt::Display for ImportName
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.to_namespace_string())
    }
}

#[derive(Serialize, Deserialize)]
pub struct ImportSymbol
{
    pub name: String,
    pub typ: Type,
    pub mutable: bool,
    pub span: Span,
}

impl ImportSymbol
{
    pub fn new(name: &str, typ: &Type, mutable: bool, span: &Span) -> ImportSymbol
    {
        ImportSymbol{
            name: name.into(),
            typ: typ.clone(),
            mutable: mutable,
            span: span.clone(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Import
{
    pub namespace: String,
    pub symbols: HashMap<String, ImportSymbol>
}

impl Import
{
    pub fn new(namespace: String) -> Import
    {
        Import{
            namespace,
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, sym: ImportSymbol)
    {
        self.symbols.insert(sym.name.clone(), sym);
    }
}


impl fmt::Display for Import
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        writeln!(f, "{}:", self.namespace)?;
        for symbol in self.symbols.values() {
            writeln!(f, "  {}: type {}", symbol.name, symbol.typ)?;
        }

        Ok(())
    }
}
