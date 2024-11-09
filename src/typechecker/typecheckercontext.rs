use crate::ast::*;
use crate::compileerror::*;
use crate::span::Span;
use std::collections::hash_map::DefaultHasher;
use std::collections::hash_map::{Entry, HashMap};
use std::hash::Hash;
use std::hash::Hasher;

use super::destructors::destructor_name;

#[derive(Debug)]
struct Scope {
    symbols: HashMap<String, SymbolPtr>,
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

    pub fn update(&mut self, symbol: SymbolPtr) {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    fn resolve(&self, name: &str) -> Option<SymbolPtr> {
        let name_with_double_colons = format!("::{}", name);
        for (symbol_name, symbol) in &self.symbols {
            if symbol_name == name || symbol_name.ends_with(&name_with_double_colons) {
                return Some(symbol.clone());
            }
        }

        None
    }

    fn add(&mut self, symbol: SymbolPtr, destructor: Option<Expression>) -> CompileResult<()> {
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
    pub fn resolve(&self, name: &str) -> Option<SymbolPtr> {
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

#[derive(Debug, Clone)]
pub struct DestructorToCreate {
    pub name: String,
    pub typ: Type,
    pub destructor_type: Type,
}

fn destructor_name_for_type(typ: &Type) -> String {
    let def_name = || {
        let mut s = DefaultHasher::new();
        typ.hash(&mut s);
        let h = s.finish();
        format!("$destructor-{h:x}")
    };
    match typ {
        Type::Struct(st) => {
            if let Some(name) = &st.name {
                destructor_name(name)
            } else {
                def_name()
            }
        }
        Type::Sum(st) => destructor_name(&st.name),

        _ => def_name(),
    }
}

pub struct TypeCheckerContext<'a> {
    stack: Vec<Scope>,
    globals: Scope,
    externals: Scope,
    import_resolver: ImportSymbolResolver<'a>,
    drop_flag_counter: usize,
    destructors_to_create: HashMap<String, DestructorToCreate>,
}

impl<'a> TypeCheckerContext<'a> {
    pub fn new(isr: ImportSymbolResolver<'a>) -> TypeCheckerContext<'a> {
        TypeCheckerContext {
            stack: Vec::new(),
            globals: Scope::new(None),
            externals: Scope::new(None),
            import_resolver: isr,
            drop_flag_counter: 0,
            destructors_to_create: HashMap::new(),
        }
    }

    pub fn update(&mut self, symbol: SymbolPtr) {
        self.stack.last_mut().expect("Empty stack").update(symbol)
    }

    pub fn enter_scope(&mut self, function_return_type: Option<Type>) {
        self.stack.push(Scope::new(function_return_type));
    }

    pub fn get_destructors_to_create(&self) -> Vec<DestructorToCreate> {
        self.destructors_to_create.values().cloned().collect()
    }

    fn destructor_needed(&self, typ: &Type) -> bool {
        let name = destructor_name_for_type(typ);
        if self.resolve(&name).is_some() {
            return true;
        }

        match typ {
            Type::Array(at) => self.destructor_needed(&at.element_type),
            Type::Struct(s) => {
                for m in &s.members {
                    // If a member has a destructor, this one needs one too
                    if self.destructor_needed(&m.typ) {
                        return true;
                    }
                }
                false
            }
            Type::Sum(st) => {
                for c in &st.cases {
                    if let Some(ct) = &c.typ {
                        if self.destructor_needed(ct) {
                            return true;
                        }
                    }
                }
                false
            }
            Type::Optional(ot) => self.destructor_needed(ot),
            Type::Result(rt) => self.destructor_needed(&rt.ok_typ) || self.destructor_needed(&rt.err_typ),
            _ => false,
        }
    }

    pub fn get_destructor(&mut self, typ: &Type) -> CompileResult<Option<SymbolPtr>> {
        let name = destructor_name_for_type(typ);
        if let Some(ds) = self.resolve(&name) {
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
        } else if self.destructor_needed(typ) {
            let ds_to_create = DestructorToCreate {
                name: name.clone(),
                typ: typ.clone(),
                destructor_type: func_type(vec![func_arg(ptr_type(typ.clone()), true)], Type::Void),
            };

            let sym = Symbol::new(
                &name,
                &ds_to_create.destructor_type,
                false,
                &Span::Internal,
                SymbolType::Global,
            );

            self.destructors_to_create
                .insert(name.clone(), ds_to_create);

            self.globals.add(sym.clone(), None)?;
            Ok(Some(sym))
        } else {
            Ok(None)
        }
    }

    pub fn get_destructor_call(
        &mut self,
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

    pub fn resolve(&self, name: &str) -> Option<SymbolPtr> {
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
        df
    }

    pub fn add_drop_flag(&mut self, var_name: &str, df: DropFlag) {
        if let Some(s) = self.stack.last_mut() {
            s.drop_flags.insert(var_name.into(), df.clone());
        }
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

    pub fn add(&mut self, symbol: SymbolPtr) -> CompileResult<()> {
        match symbol.symbol_type {
            SymbolType::Normal => {
                let param = address_of(
                    Expression::NameRef(NameRef::new(symbol.name.clone(), symbol.span.clone())),
                    symbol.span.clone(),
                );
                let df = self.next_drop_flag(&symbol.name);
                let ds = self.get_destructor_call(param, Some(df.clone()), &symbol.typ)?;
                if ds.is_some() {
                    self.add_drop_flag(&symbol.name, df);
                }

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
