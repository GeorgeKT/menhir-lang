use std::collections::{HashMap};
use ast::*;
use compileerror::*;
use span::Span;


#[derive(Debug)]
pub struct StackFrame
{
    symbols: HashMap<String, Type>,
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
    pub fn new() -> StackFrame
    {
        StackFrame{
            symbols: HashMap::new(),
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<ResolvedName>
    {
        let name_with_double_colons = format!("::{}", name);
        for (symbol_name, typ) in self.symbols.iter() {
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
}

impl TypeCheckerContext
{
    pub fn new() -> TypeCheckerContext
    {
        TypeCheckerContext{
            stack: vec![StackFrame::new()],
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<ResolvedName>
    {
        for sf in self.stack.iter().rev() {
            let t = sf.resolve_type(name);
            if t.is_some() {
                return t;
            }
        }

        None
    }

    pub fn add(&mut self, name: &str, t: Type, span: &Span) -> CompileResult<()>
    {
        self.stack.last_mut().expect("Empty stack").add(name, t, span)
    }

    pub fn update(&mut self, name: &str, t: Type)
    {
        self.stack.last_mut().expect("Empty stack").update(name, t)
    }

    pub fn push_stack(&mut self)
    {
        self.stack.push(StackFrame::new());
    }

    pub fn pop_stack(&mut self)
    {
        self.stack.pop();
    }
}
