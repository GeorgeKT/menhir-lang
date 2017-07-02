use std::collections::hash_map::{HashMap, Entry};
use ast::*;
use compileerror::*;

struct Scope
{
    symbols: HashMap<String, Symbol>,
    function_return_type: Option<Type>,
}


impl Scope
{
    pub fn new(function_return_type: Option<Type>) -> Scope
    {
        Scope {
            symbols: HashMap::new(),
            function_return_type,
        }
    }

    pub fn update(&mut self, symbol: Symbol)
    {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    fn resolve(&self, name: &str) -> Option<Symbol>
    {
        let name_with_double_colons = format!("::{}", name);
        for (symbol_name, symbol) in &self.symbols {
            if symbol_name == name || symbol_name.ends_with(&name_with_double_colons) {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn add(&mut self, symbol: Symbol) -> CompileResult<()>
    {
        match self.symbols.entry(symbol.name.clone()) {
            Entry::Occupied(e) => {
                let value = e.get();
                if value.typ != symbol.typ {
                    type_error_result(&symbol.span, format!("Symbol {} has already been defined with type {}", symbol.name, value.typ))
                } else {
                    Ok(())
                }
            }

            Entry::Vacant(v) => {
                v.insert(symbol);
                Ok(())
            }
        }
    }
}

pub enum ImportSymbolResolver<'a>
{
    ImportMap(&'a ImportMap),
    ExternalImport(&'a Import),
}

impl<'a> ImportSymbolResolver<'a>
{
    pub fn resolve(&self, name: &str) -> Option<Symbol>
    {
        match *self {
            ImportSymbolResolver::ImportMap(ref imports) => {
                for import in imports.values() {
                    if let Some(s) = import.resolve(name, false) {
                        return Some(s)
                    }
                }

                None
            }

            ImportSymbolResolver::ExternalImport(ref import) => {
                import.resolve(name, true)
            }
        }
    }
}

pub struct TypeCheckerContext<'a>
{
    stack: Vec<Scope>,
    globals: Scope,
    externals: Scope,
    import_resolver: ImportSymbolResolver<'a>,
}

impl<'a> TypeCheckerContext<'a>
{
    pub fn new(isr: ImportSymbolResolver<'a>) -> TypeCheckerContext<'a>
    {
        TypeCheckerContext {
            stack: Vec::new(),
            globals: Scope::new(None),
            externals: Scope::new(None),
            import_resolver: isr
        }
    }

    pub fn update(&mut self, symbol: Symbol)
    {
        self.stack.last_mut().expect("Empty stack").update(symbol)
    }

    pub fn enter_scope(&mut self, function_return_type: Option<Type>)
    {
        self.stack.push(Scope::new(function_return_type));
    }

    pub fn exit_scope(&mut self)
    {
        self.stack.pop();
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol>
    {
        for sf in self.stack.iter().rev() {
            if let Some(s) = sf.resolve(name) {
                return Some(s);
            }

            if sf.function_return_type.is_some() {
                break;
            }
        }

        if let Some(s) = self.globals.resolve(name) {
            return Some(s);
        }

        if let Some(s) = self.externals.resolve(name) {
            return Some(s)
        }

        self.import_resolver.resolve(name)
    }

    pub fn add(&mut self, symbol: Symbol) -> CompileResult<()>
    {
        match symbol.symbol_type {
            SymbolType::Normal => {
                if let Some(ref mut sf) = self.stack.last_mut() {
                    sf.add(symbol)
                } else {
                    self.globals.add(symbol)
                }
            },

            SymbolType::Global => {
                self.globals.add(symbol)
            },

            SymbolType::External => {
                self.externals.add(symbol)
            }
        }
    }

    pub fn get_function_return_type(&self) -> Option<Type>
    {
        for sf in self.stack.iter().rev() {
            if sf.function_return_type.is_some() {
                return sf.function_return_type.clone();
            }
        }

        None
    }
}
