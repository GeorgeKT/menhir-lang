use std::rc::Rc;
use std::collections::{HashMap, HashSet};
use super::{TreePrinter, TypeDeclaration, Import, ImportName, ImportSymbol, GlobalBinding, Function, ExternalFunction, prefix};

pub struct Module
{
    pub name: String,
    pub globals: HashMap<String, GlobalBinding>,
    pub functions: HashMap<String, Function>,
    pub externals: HashMap<String, ExternalFunction>,
    pub types: HashMap<String, TypeDeclaration>,
    pub imports: HashMap<String, Rc<Import>>,
    pub import_names: HashSet<ImportName>,
    pub type_checked: bool,
}

impl Module
{
    pub fn new(name: &str) -> Module
    {
        Module{
            name: name.into(),
            globals: HashMap::new(),
            functions: HashMap::new(),
            externals: HashMap::new(),
            types: HashMap::new(),
            imports: HashMap::new(),
            import_names: HashSet::new(),
            type_checked: false,
        }
    }

    pub fn get_exported_symbols(&self) -> Import
    {
        let mut import = Import::new(self.name.split("::").map(String::from).collect());
        for (name, binding) in &self.globals {
            import.add_symbol(ImportSymbol::new(name, &binding.typ, binding.mutable, &binding.span));
        }

        for (name, function) in &self.functions {
            import.add_symbol(ImportSymbol::new(name, &function.sig.typ, false, &function.span));
        }

        for (name, function) in &self.externals {
            import.add_symbol(ImportSymbol::new(name, &function.sig.typ, false, &function.span));
        }

        for (name, type_decl) in &self.types {
            import.add_symbol(ImportSymbol::new(name, &type_decl.get_type(), false, &type_decl.span()));
        }

        import
    }
}

impl TreePrinter for Module
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        println!("{}Module: {}", p, self.name);
        for i in &self.import_names {
            println!("{} import {}", p, i.to_namespace_string());
        }

        for t in self.types.values() {
            t.print(level + 1);
        }

        for global in self.globals.values() {
            global.print(level + 1);
        }

        for func in self.externals.values() {
            func.print(level + 1);
        }

        for func in self.functions.values() {
            func.print(level + 1);
        }
    }
}
