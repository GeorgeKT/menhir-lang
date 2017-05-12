use std::collections::{HashMap};
use ast::*;
use compileerror::*;
use span::Span;
use target::Target;


#[derive(Debug)]
pub struct StackFrame
{
    symbols: HashMap<String, (Type, bool)>,
    start_of_function: bool,
}

pub struct ResolvedName
{
    pub full_name: String,
    pub typ: Type,
    pub mutable: bool,
}

impl ResolvedName
{
    pub fn new(full_name: &str, typ: Type, mutable: bool) -> ResolvedName
    {
        ResolvedName{
            full_name: full_name.into(),
            typ: typ,
            mutable: mutable,
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

    pub fn resolve(&self, name: &str) -> Option<ResolvedName>
    {
        let name_with_double_colons = format!("::{}", name);
        for (symbol_name, &(ref typ, ref mutable)) in &self.symbols {
            if symbol_name == name || symbol_name.ends_with(&name_with_double_colons) {
                return Some(ResolvedName::new(symbol_name, typ.clone(), *mutable));
            }
        }

        None
    }

    pub fn add(&mut self, name: &str, t: Type, mutable: bool, span: &Span) -> CompileResult<()>
    {
        if self.symbols.insert(name.into(), (t, mutable)).is_some() {
            type_error_result(span, format!("Symbol {} has already been defined", name))
        } else {
            Ok(())
        }
    }

    pub fn update(&mut self, name: &str, t: Type, mutable: bool)
    {
        self.symbols.insert(name.into(), (t, mutable));
    }
}

#[derive(Debug)]
pub struct TypeCheckerContext<'a>
{
    stack: Vec<StackFrame>,
    globals: StackFrame,
    pub target: &'a Target
}

impl<'a> TypeCheckerContext<'a>
{
    pub fn new(target: &'a Target) -> TypeCheckerContext<'a>
    {
        TypeCheckerContext{
            stack: vec![],
            globals: StackFrame::new(false),
            target: target
        }
    }

    pub fn resolve(&self, name: &str) -> Option<ResolvedName>
    {
        for sf in self.stack.iter().rev() {
            let t = sf.resolve(name);
            if t.is_some() {
                return t;
            }

            if sf.start_of_function {
                break;
            }
        }

        self.globals.resolve(name)
    }

    pub fn add(&mut self, name: &str, t: Type, mutable: bool, span: &Span) -> CompileResult<()>
    {
        if let Some(ref mut sf) = self.stack.last_mut() {
            sf.add(name, t, mutable, span)
        } else {
            self.globals.add(name, t, mutable, span)
        }
    }

    pub fn add_global(&mut self, name: &str, t: Type, mutable: bool, span: &Span) -> CompileResult<()>
    {
        self.globals.add(name, t, mutable, span)
    }

    pub fn update(&mut self, name: &str, t: Type, mutable: bool)
    {
        self.stack.last_mut().expect("Empty stack").update(name, t, mutable)
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
