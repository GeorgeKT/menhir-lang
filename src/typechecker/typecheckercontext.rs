use crate::ast::*;
use crate::compileerror::*;
use std::collections::hash_map::{Entry, HashMap};

use super::destructors::destructor_name;

struct Scope {
    symbols: HashMap<String, Symbol>,
    destructor_calls: Vec<Expression>,
    drop_flags: HashMap<String, DropFlag>,
    function_return_type: Option<Type>,
}

impl Scope {
    pub fn new(function_return_type: Option<Type>) -> Scope {
        Scope {
            symbols: HashMap::new(),
            destructor_calls: Vec::new(),
            drop_flags: HashMap::new(),
            function_return_type,
        }
    }

    pub fn update(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    fn resolve(&self, name: &str) -> Option<Symbol> {
        let name_with_double_colons = format!("::{}", name);
        for (symbol_name, symbol) in &self.symbols {
            if symbol_name == name || symbol_name.ends_with(&name_with_double_colons) {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn add(&mut self, symbol: Symbol, destructor: Option<Expression>) -> CompileResult<()> {
        match self.symbols.entry(symbol.name.clone()) {
            Entry::Occupied(e) => {
                let value = e.get();
                if value.typ != symbol.typ {
                    type_error_result(
                        &symbol.span,
                        format!(
                            "Symbol {} has already been defined with type {}",
                            symbol.name, value.typ
                        ),
                    )
                } else {
                    if let Some(d) = destructor {
                        self.destructor_calls.push(d);
                    }
                    Ok(())
                }
            }

            Entry::Vacant(v) => {
                v.insert(symbol);
                if let Some(d) = destructor {
                    self.destructor_calls.push(d);
                }
                Ok(())
            }
        }
    }
}

pub enum ImportSymbolResolver<'a> {
    ImportMap(&'a ImportMap),
    ExternalImport(&'a Import),
}

impl<'a> ImportSymbolResolver<'a> {
    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match *self {
            ImportSymbolResolver::ImportMap(imports) => {
                for import in imports.values() {
                    if let Some(s) = import.resolve(name, false) {
                        return Some(s);
                    }
                }

                None
            }

            ImportSymbolResolver::ExternalImport(import) => import.resolve(name, true),
        }
    }
}

pub struct TypeCheckerContext<'a> {
    stack: Vec<Scope>,
    globals: Scope,
    externals: Scope,
    import_resolver: ImportSymbolResolver<'a>,
    drop_flag_counter: usize,
}

impl<'a> TypeCheckerContext<'a> {
    pub fn new(isr: ImportSymbolResolver<'a>) -> TypeCheckerContext<'a> {
        TypeCheckerContext {
            stack: Vec::new(),
            globals: Scope::new(None),
            externals: Scope::new(None),
            import_resolver: isr,
            drop_flag_counter: 0,
        }
    }

    pub fn update(&mut self, symbol: Symbol) {
        self.stack.last_mut().expect("Empty stack").update(symbol)
    }

    pub fn enter_scope(&mut self, function_return_type: Option<Type>) {
        self.stack.push(Scope::new(function_return_type));
    }

    pub fn get_destructor(&self, typ: &Type) -> CompileResult<Option<Symbol>> {
        let name = match typ {
            Type::Struct(st) => st.name.clone(),
            Type::Sum(et) => et.name.clone(),
            _ => {
                return Ok(None);
            }
        };

        let destructor_name = destructor_name(&name);
        let Some(ds) = self.resolve(&destructor_name) else {
            return Ok(None);
        };

        if let Type::Func(ft) = &ds.typ {
            if ft.return_type != Type::Void || ft.args.len() != 1 {
                return type_error_result(
                    &ds.span,
                    "Destructors must not return anything and only have self as argument",
                );
            }
        } else {
            return type_error_result(&ds.span, "Destructor must be a function");
        }

        Ok(Some(ds))
    }

    pub fn get_destructor_call(
        &self,
        self_expr: Expression,
        df: Option<DropFlag>,
        typ: &Type,
    ) -> CompileResult<Option<Expression>> {
        let Some(ds) = self.get_destructor(typ)? else {
            return Ok(None);
        };

        let call = Call::new(
            NameRef::new(ds.name.clone(), ds.span.clone()),
            vec![self_expr.clone()],
            ds.span.clone(),
        );

        Ok(Some(cc_drop(self_expr, ds.span.clone(), Some(call), df)))
    }

    pub fn add_destructors(&mut self, b: &mut Block) -> CompileResult<()> {
        let scope = self.stack.last_mut().expect("Empty stack");
        for ds in scope.destructor_calls.drain(..).rev() {
            b.deferred_expressions.push(ds);
        }
        b.drop_flags = scope.drop_flags.values().cloned().collect();
        Ok(())
    }

    pub fn exit_scope(&mut self) {
        self.stack.pop();
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
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
            return Some(s);
        }

        self.import_resolver.resolve(name)
    }

    pub fn next_drop_flag(&mut self, var_name: &str) -> DropFlag {
        let df = DropFlag(format!("$df_{}_{}", var_name, self.drop_flag_counter));
        self.drop_flag_counter += 1;
        if let Some(s) = self.stack.last_mut() {
            s.drop_flags.insert(var_name.into(), df.clone());
        }
        df
    }

    pub fn get_drop_flag(&self, expr: &Expression) -> Option<DropFlag> {
        let name = match expr {
            Expression::NameRef(nr) => &nr.name,
            Expression::AddressOf(aof) => {
                if let Expression::NameRef(nr) = &aof.inner {
                    &nr.name
                } else {
                    return None;
                }
            }
            _ => return None,
        };

        for s in self.stack.iter().rev() {
            if let Some(df) = s.drop_flags.get(name) {
                return Some(df.clone());
            }
        }

        None
    }

    pub fn add(&mut self, symbol: Symbol) -> CompileResult<()> {
        match symbol.symbol_type {
            SymbolType::Normal => {
                let param = address_of(
                    Expression::NameRef(NameRef::new(symbol.name.clone(), symbol.span.clone())),
                    symbol.span.clone(),
                );
                let df = self.next_drop_flag(&symbol.name);
                let ds = self.get_destructor_call(param, Some(df), &symbol.typ)?;
                if let Some(sf) = self.stack.last_mut() {
                    sf.add(symbol, ds)
                } else {
                    self.globals.add(symbol, ds)
                }
            }

            SymbolType::Global => self.globals.add(symbol, None),

            SymbolType::External => self.externals.add(symbol, None),
        }
    }

    pub fn get_function_return_type(&self) -> Option<Type> {
        for sf in self.stack.iter().rev() {
            if sf.function_return_type.is_some() {
                return sf.function_return_type.clone();
            }
        }

        None
    }
}
