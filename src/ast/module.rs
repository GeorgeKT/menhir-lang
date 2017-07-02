use std::collections::{HashMap, HashSet};
use super::{Expression, Call, TreePrinter, TypeDeclaration, Import, ImportName, Symbol, SymbolType, GlobalBinding, Function, ExternalFunction, prefix};
use target::Target;
use compileerror::CompileResult;

pub struct Module
{
    pub name: String,
    pub globals: HashMap<String, GlobalBinding>,
    pub functions: HashMap<String, Function>,
    pub externals: HashMap<String, ExternalFunction>,
    pub types: HashMap<String, TypeDeclaration>,
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
            import_names: HashSet::new(),
            type_checked: false,
        }
    }

    fn is_imported_call(&self, call: &Call) -> bool {
        !self.functions.contains_key(&call.callee.name) &&
        !self.externals.contains_key(&call.callee.name)
    }

    fn get_imported_symbols(&self, target: &Target) -> HashMap<String, Symbol>
    {
        let mut symbols = HashMap::new();
        for func in self.functions.values() {
            let mut find_imported_calls = |e: &Expression| -> CompileResult<()> {
                match *e {
                    Expression::Call(ref call) if self.is_imported_call(call) => {
                        let typ = call.callee_type(target.int_size);
                        let symbol = Symbol::new(&call.callee.name, &typ, false, &call.span, SymbolType::External);
                        symbols.insert(call.callee.name.clone(), symbol);
                    }

                    _ => (),
                }
                Ok(())
            };

            let _ = func.expression.visit(&mut find_imported_calls);
        }
        symbols
    }

    pub fn get_exported_symbols(&self, target: &Target) -> Import
    {
        let mut import = Import::new(self.name.clone());
        for (name, binding) in &self.globals {
            import.symbols.insert(name.clone(), Symbol::new(name, &binding.typ, binding.mutable, &binding.span, SymbolType::Global));
        }

        for (name, function) in &self.functions {
            import.symbols.insert(name.clone(), Symbol::new(name, &function.sig.typ, false, &function.span, SymbolType::Normal));
            if function.is_generic() {
                import.generics.insert(name.clone(), function.clone());
            }
        }

        for (name, function) in &self.externals {
            import.symbols.insert(name.clone(), Symbol::new(name, &function.sig.typ, false, &function.span, SymbolType::External));
        }

        for (name, type_decl) in &self.types {
            import.symbols.insert(name.clone(), Symbol::new(name, &type_decl.get_type(), false, &type_decl.span(), SymbolType::Normal));
        }


        import.imported_symbols = self.get_imported_symbols(target);
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

        println!("{}", p);
        for t in self.types.values() {
            t.print(level + 1);
            println!("{}", p);
        }

        for global in self.globals.values() {
            global.print(level + 1);
            println!("{}", p);
        }

        for func in self.externals.values() {
            func.print(level + 1);
            println!("{}", p);
        }

        for func in self.functions.values() {
            func.print(level + 1);
            println!("{}", p);
        }
    }
}
