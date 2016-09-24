use std::fmt;
use std::collections::HashMap;
use ast::{Type, FunctionSignature};
use llrep::llinstruction::LLInstruction;

#[derive(Debug, Clone)]
pub struct LLVar
{
    idx: usize,
    typ: Type,
}

impl fmt::Display for LLVar
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "%{}: {}", self.idx, self.typ)
    }
}

#[derive(Debug, Clone)]
pub struct LLFunction
{
    sig: FunctionSignature,
    instructions: Vec<LLInstruction>,
    var_counter: usize,
    named_vars: HashMap<String, LLVar>,
}

impl LLFunction
{
    pub fn new(sig: &FunctionSignature) -> LLFunction
    {
        let mut f = LLFunction{
            sig: sig.clone(),
            instructions: Vec::new(),
            var_counter: 0,
            named_vars: HashMap::new(),
        };

        for arg in &sig.args {
            let _ = f.named_var(&arg.name, arg.typ.clone());
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
        LLVar{
            idx: idx,
            typ: typ,
        }
    }

    pub fn named_var(&mut self, name: &str, typ: Type) -> LLVar
    {
        let v = self.new_var(typ);
        self.named_vars.insert(name.into(), v.clone());
        v
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
