use std::collections::{HashMap};
use ast::*;
use compileerror::*;
use span::Span;


#[derive(Debug)]
pub struct StackFrame
{
    symbols: HashMap<String, Type>,
    start_of_function: bool,
}

pub struct ResolvedName
{
    pub full_name: String,
    pub typ: Type,
}

impl ResolvedName
{
    pub fn new(full_name: &str, typ: Type) -> ResolvedName
    {
        ResolvedName{
            full_name: full_name.into(),
            typ: typ,
        }
    }
}

impl StackFrame
{
    pub fn new(start_of_function: bool) -> StackFrame
    {
        StackFrame{
            symbols: HashMap::new(),
            start_of_function: start_of_function,
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<ResolvedName>
    {
        let name_with_double_colons = format!("::{}", name);
        for (symbol_name, typ) in &self.symbols {
            if symbol_name == name || symbol_name.ends_with(&name_with_double_colons) {
                return Some(ResolvedName::new(symbol_name, typ.clone()));
            }
        }

        None
    }

    pub fn add(&mut self, name: &str, t: Type, span: &Span) -> CompileResult<()>
    {
        if self.symbols.insert(name.into(), t).is_some() {
            err(span, ErrorCode::RedefinitionOfVariable, format!("Symbol {} has already been defined", name))
        } else {
            Ok(())
        }
    }

    pub fn update(&mut self, name: &str, t: Type)
    {
        self.symbols.insert(name.into(), t);
    }
}

#[derive(Debug)]
pub struct TypeCheckerContext
{
    stack: Vec<StackFrame>,
    globals: StackFrame,
}

impl TypeCheckerContext
{
    pub fn new() -> TypeCheckerContext
    {
        TypeCheckerContext{
            stack: vec![],
            globals: StackFrame::new(false),
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<ResolvedName>
    {
        for sf in self.stack.iter().rev() {
            let t = sf.resolve_type(name);
            if t.is_some() {
                return t;
            }

            if sf.start_of_function {
                break;
            }
        }

        self.globals.resolve_type(name)
    }

    pub fn add(&mut self, name: &str, t: Type, span: &Span) -> CompileResult<()>
    {
        if let Some(ref mut sf) = self.stack.last_mut() {
            sf.add(name, t, span)
        } else {
            self.globals.add(name, t, span)
        }
    }

    pub fn add_global(&mut self, name: &str, t: Type, span: &Span) -> CompileResult<()>
    {
        self.globals.add(name, t, span)
    }

    pub fn update(&mut self, name: &str, t: Type)
    {
        self.stack.last_mut().expect("Empty stack").update(name, t)
    }

    pub fn push_stack(&mut self, start_of_function: bool)
    {
        self.stack.push(StackFrame::new(start_of_function));
    }

    pub fn pop_stack(&mut self)
    {
        self.stack.pop();
    }
}
