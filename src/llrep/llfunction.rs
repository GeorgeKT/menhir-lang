use std::fmt;
use std::collections::HashMap;
use ast::{Type, FunctionSignature};
use llrep::llinstruction::LLInstruction;

#[derive(Debug, Clone)]
pub struct LLVar
{
    pub name: String,
    pub typ: Type,
}

impl LLVar
{
    pub fn new(idx: usize, typ: Type) -> LLVar
    {
        LLVar{
            name: format!("$var{}", idx),
            typ: typ,
        }
    }

    pub fn named(name: &str, typ: Type) -> LLVar
    {
        LLVar{
            name: name.into(),
            typ: typ,
        }
    }

    pub fn replace_by_ret(&mut self, name: &str)
    {
        if self.name == name {
            self.name = "$ret".into();
        }
    }
}

impl fmt::Display for LLVar
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "({}: {})", self.name, self.typ)
    }
}

#[derive(Debug)]
pub struct Scope
{
    named_vars: HashMap<String, LLVar>,
}

impl Scope
{
    pub fn new() -> Scope
    {
        Scope{
            named_vars: HashMap::new()
        }
    }

    pub fn add_named_var(&mut self, var: LLVar)
    {
        self.named_vars.insert(var.name.clone(), var);
    }

    pub fn get_named_var(&self, var: &str) -> Option<LLVar>
    {
        self.named_vars.get(var).map(|v| v.clone())
    }
}

#[derive(Debug)]
pub struct LLFunction
{
    pub sig: FunctionSignature,
    pub instructions: Vec<LLInstruction>,
    var_counter: usize,
    scopes: Vec<Scope>
}

impl LLFunction
{
    pub fn new(sig: &FunctionSignature) -> LLFunction
    {
        let mut f = LLFunction{
            sig: sig.clone(),
            instructions: Vec::new(),
            var_counter: 0,
            scopes: vec![Scope::new()],
        };

        for arg in &sig.args {
            f.add_named_var(LLVar::named(&arg.name, arg.typ.clone()));
        }
        f
    }

    pub fn add(&mut self, inst: LLInstruction)
    {
        self.instructions.push(inst);
    }

    pub fn new_var(&mut self, typ: Type) -> LLVar
    {
        let idx = self.var_counter;
        self.var_counter += 1;
        LLVar::new(idx, typ)
    }

    pub fn push_scope(&mut self)
    {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self)
    {
        self.scopes.pop();
    }

    pub fn add_named_var(&mut self, var: LLVar)
    {
        let scope = self.scopes.last_mut().expect("Empty Scope Stack");
        scope.add_named_var(var);
    }

    pub fn get_named_var(&self, var: &str) -> Option<LLVar>
    {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get_named_var(var) {
                return Some(v)
            }
        }

        None
    }

    pub fn replace_by_ret(&mut self, bad_name: &str)
    {
        for inst in &mut self.instructions {
            let replace_by_nop = if let LLInstruction::StackAlloc(ref var) = *inst {
                var.name == bad_name
            } else {
                false
            };

            if replace_by_nop {
                *inst = LLInstruction::NOP;
            } else {
                inst.replace_by_ret(bad_name);
            }
        }
    }
}

impl fmt::Display for LLFunction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        try!(writeln!(f, "{}:", self.sig.name));
        for inst in &self.instructions {
            try!(inst.fmt(f));
        }
        Ok(())
    }
}
