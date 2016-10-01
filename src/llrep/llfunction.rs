use std::fmt;
use std::collections::{BTreeMap, HashMap};
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


pub type LLBasicBlockRef = usize;

pub fn bb_name(bb: LLBasicBlockRef) -> String
{
    if bb == 0 {
        "entry".into()
    } else {
        format!("block{}", bb)
    }
}

#[derive(Debug)]
pub struct LLBasicBlock
{
    pub name: String,
    pub instructions: Vec<LLInstruction>
}

impl LLBasicBlock
{
    pub fn new(name: String) -> LLBasicBlock
    {
        LLBasicBlock{
            name: name,
            instructions: Vec::new(),
        }
    }
}



#[derive(Debug)]
pub struct LLFunction
{
    pub sig: FunctionSignature,
    pub blocks: BTreeMap<LLBasicBlockRef, LLBasicBlock>,
    pub block_order: Vec<LLBasicBlockRef>,
    pub lambdas: Vec<LLFunction>,
    current_bb: usize,
    bb_counter: usize,
    var_counter: usize,
    scopes: Vec<Scope>
}


impl LLFunction
{
    pub fn new(sig: &FunctionSignature) -> LLFunction
    {
        let mut f = LLFunction{
            sig: sig.clone(),
            blocks: BTreeMap::new(),
            block_order: Vec::new(),
            lambdas: Vec::new(),
            current_bb: 0,
            bb_counter: 0,
            var_counter: 0,
            scopes: vec![Scope::new()],
        };

        let entry = f.create_basic_block();
        f.add_basic_block(entry);

        for arg in &sig.args {
            f.add_named_var(LLVar::named(&arg.name, arg.typ.clone()));
        }
        f
    }

    pub fn is_empty(&self) -> bool
    {
        self.blocks.get(&0).map(|bb| bb.instructions.is_empty()).unwrap_or(false)
    }

    pub fn add(&mut self, inst: LLInstruction)
    {
        let idx = self.current_bb;
        self.blocks.get_mut(&idx).map(|bb| bb.instructions.push(inst));
    }

    pub fn create_basic_block(&mut self) -> LLBasicBlockRef
    {
        let bb_ref = self.bb_counter;
        self.bb_counter += 1;
        let name = bb_name(bb_ref);
        self.blocks.insert(bb_ref, LLBasicBlock::new(name));
        bb_ref
    }

    pub fn add_basic_block(&mut self, bb_ref: LLBasicBlockRef)
    {
        self.block_order.push(bb_ref);
    }

    pub fn set_current_bb(&mut self, bb_ref: LLBasicBlockRef)
    {
        assert!(bb_ref < self.blocks.len());
        self.current_bb = bb_ref;
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
}

impl fmt::Display for LLFunction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        for lambda in &self.lambdas {
            try!(lambda.fmt(f));
            try!(writeln!(f, ""));
        }

        try!(writeln!(f, "{}:", self.sig.name));
        for bb_ref in &self.block_order {
            let bb = self.blocks.get(bb_ref).expect("Unknown basic block");
            try!(writeln!(f, " {}:", bb.name));
            for inst in &bb.instructions {
                try!(inst.fmt(f));
            }
        }

        Ok(())
    }
}
