use std::collections::HashMap;
use std::fmt;
use itertools::join;
use span::Span;
use ast::Function;
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
pub enum ImportSymbol
{
    Symbol{
        name: String,
        typ: Type,
        mutable: bool,
        span: Span,
    },

    GenericFunction(Function)
}

impl ImportSymbol
{
    pub fn new(name: &str, typ: &Type, mutable: bool, span: &Span) -> ImportSymbol
    {
        ImportSymbol::Symbol{
            name: name.into(),
            typ: typ.clone(),
            mutable: mutable,
            span: span.clone(),
        }
    }

    pub fn from_generic_function(func: &Function) -> ImportSymbol
    {
        ImportSymbol::GenericFunction(func.clone())
    }

    pub fn get_name(&self) -> &str 
    {
        match *self {
            ImportSymbol::Symbol{ref name, ..} => &name,
            ImportSymbol::GenericFunction(ref func) => &func.sig.name,
        }
    }

    pub fn get_type(&self) -> &Type
    {
        match *self {
            ImportSymbol::Symbol{ref typ, ..} => typ,
            ImportSymbol::GenericFunction(ref func) => &func.sig.typ,
        }
    } 

    pub fn is_mutable(&self) -> bool
    {
        match *self {
            ImportSymbol::Symbol{mutable, ..} => mutable,
            ImportSymbol::GenericFunction(_) => false,
        }
    } 

    pub fn get_span(&self) -> &Span 
    {
        match *self {
            ImportSymbol::Symbol{ref span, ..} => span,
            ImportSymbol::GenericFunction(ref func) => &func.span,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Import
{
    pub namespace: String,
    pub symbols: HashMap<String, ImportSymbol>,
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
        self.symbols.insert(sym.get_name().into(), sym);
    }
}


impl fmt::Display for Import
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        writeln!(f, "Module {}:", self.namespace)?;
        for symbol in self.symbols.values() {
            match *symbol {
                ImportSymbol::Symbol{ref name, ref typ, ..} =>  
                    writeln!(f, "  {}: type {}", name, typ)?,
                ImportSymbol::GenericFunction(ref func) => 
                    writeln!(f, "  {}: type {}", func.sig.name, func.sig.typ)?,
            }
           
        }

        Ok(())
    }
}
