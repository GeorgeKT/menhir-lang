use std::fmt;
use std::collections::{BTreeMap, HashMap};
use itertools::free::join;
use ast::{Type, FunctionSignature, sig};
use bytecode::instruction::Instruction;
use span::Span;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Var
{
    pub name: String,
    pub typ: Type,
}

impl Var
{
    pub fn new(idx: usize, typ: Type) -> Var
    {
        Var{
            name: format!("$var{}", idx),
            typ: typ,
        }
    }

    pub fn named(name: &str, typ: Type) -> Var
    {
        Var{
            name: name.into(),
            typ: typ,
        }
    }
}

impl fmt::Display for Var
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "({}: {})", self.name, self.typ)
    }
}

#[derive(Debug)]
pub struct Scope
{
    named_vars: HashMap<String, Var>,
    to_cleanup: Vec<Var>,
}

impl Scope
{
    pub fn new() -> Scope
    {
        Scope{
            named_vars: HashMap::new(),
            to_cleanup: Vec::new(),
        }
    }

    pub fn add_named_var(&mut self, var: Var)
    {
        self.named_vars.insert(var.name.clone(), var);
    }

/*
    pub fn add_cleanup_target(&mut self, v: &Var) -> bool
    {
        if self.named_vars.get(&v.name).is_none() {
            false
        } else {
            self.to_cleanup.push(v.clone());
            true
        }
    }

    pub fn remove_cleanup_target(&mut self, v: &Var) -> bool
    {
        let len = self.to_cleanup.len();
        self.to_cleanup.retain(|e| e != v);
        self.to_cleanup.len() < len
    }
*/
    pub fn cleanup(&self, _func: &mut ByteCodeFunction)
    {
        // Cleanup in reverse construction order
        for _v in self.to_cleanup.iter().rev() {
            panic!("TODO: add destructor calls");
        }
    }
}


pub type BasicBlockRef = usize;

pub fn bb_name(bb: BasicBlockRef) -> String
{
    if bb == 0 {
        "entry".into()
    } else {
        format!("block{}", bb)
    }
}

#[derive(Debug)]
pub struct BasicBlock
{
    pub name: String,
    pub instructions: Vec<Instruction>
}

impl BasicBlock
{
    pub fn new(name: String) -> BasicBlock
    {
        BasicBlock{
            name: name,
            instructions: Vec::new(),
        }
    }
}



#[derive(Debug)]
pub struct ByteCodeFunction
{
    pub sig: FunctionSignature,
    pub blocks: BTreeMap<BasicBlockRef, BasicBlock>,
    pub block_order: Vec<BasicBlockRef>,
    pub lambdas: Vec<ByteCodeFunction>,
    current_bb: usize,
    bb_counter: usize,
    var_counter: usize,
    scopes: Vec<Scope>,
    destinations: Vec<Option<Var>>,
}


impl ByteCodeFunction
{
    pub fn new(sig: &FunctionSignature) -> ByteCodeFunction
    {
        let mut f = ByteCodeFunction{
            sig: sig.clone(),
            blocks: BTreeMap::new(),
            block_order: Vec::new(),
            lambdas: Vec::new(),
            current_bb: 0,
            bb_counter: 0,
            var_counter: 0,
            scopes: vec![Scope::new()],
            destinations: Vec::new(),
        };

        let entry = f.create_basic_block();
        f.add_basic_block(entry);

        for arg in &sig.args {
            f.add_named_var(Var::named(&arg.name, arg.typ.clone()));
        }
        f
    }

    pub fn exit() -> ByteCodeFunction
    {
        let function_sig = sig("exit", Type::Void, vec![], Span::default());
        let mut function = ByteCodeFunction::new(&function_sig);
        let entry = function.create_basic_block();
        function.add_basic_block(entry);
        function.add(Instruction::Exit);
        function.add(Instruction::Exit);
        function.add(Instruction::Exit);
        function
    }

    pub fn add(&mut self, inst: Instruction)
    {
        // Pop final scope before returning
        match inst
        {
            Instruction::Return(_) | Instruction::ReturnVoid => self.pop_scope(),
            _ => (),
        }

        let idx = self.current_bb;
        self.blocks.get_mut(&idx).map(|bb| bb.instructions.push(inst));
    }

    pub fn create_basic_block(&mut self) -> BasicBlockRef
    {
        let bb_ref = self.bb_counter;
        self.bb_counter += 1;
        let name = bb_name(bb_ref);
        self.blocks.insert(bb_ref, BasicBlock::new(name));
        bb_ref
    }

    pub fn add_basic_block(&mut self, bb_ref: BasicBlockRef)
    {
        self.block_order.push(bb_ref);
    }

    pub fn set_current_bb(&mut self, bb_ref: BasicBlockRef)
    {
        assert!(bb_ref < self.blocks.len());
        self.current_bb = bb_ref;
    }

    pub fn new_var(&mut self, typ: Type) -> Var
    {
        let idx = self.var_counter;
        self.var_counter += 1;
        let v = Var::new(idx, typ);
        self.add_named_var(v.clone());
        v
    }

    pub fn push_scope(&mut self)
    {
        self.scopes.push(Scope::new());
        self.add(Instruction::StartScope);
    }

    pub fn pop_scope(&mut self)
    {
        let s = self.scopes.pop().expect("Empty Scope Stack");
        s.cleanup(self);
        if !self.scopes.is_empty() {
            // Add an endscope instruction, but not at function exit
            self.add(Instruction::EndScope);
        }
    }

    pub fn push_destination(&mut self, var: Option<Var>)
    {
        self.destinations.push(var);
    }

    pub fn pop_destination(&mut self)
    {
        let _ = self.destinations.pop();
    }

    pub fn get_destination(&self) -> Option<Var>
    {
        match self.destinations.last() {
            Some(&Some(ref var)) => Some(var.clone()),
            _ => None
        }
    }

    pub fn add_named_var(&mut self, var: Var)
    {
        let scope = self.scopes.last_mut().expect("Empty Scope Stack");
        scope.add_named_var(var);
    }

/*
    pub fn add_cleanup_target(&mut self, v: &Var)
    {
        for scope in self.scopes.iter_mut().rev() {
            if scope.add_cleanup_target(v) {
                break;
            }
        }
    }
    */
}

impl fmt::Display for ByteCodeFunction
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        for lambda in &self.lambdas {
            lambda.fmt(f)?;
            writeln!(f, "")?;
        }

        writeln!(f, "{}({}) -> {}:",
            self.sig.name,
            join(self.sig.args.iter().map(|arg| format!("{}: {}", arg.name, arg.typ)), ", "),
            self.sig.return_type)?;
        for bb_ref in &self.block_order {
            let bb = self.blocks.get(bb_ref).expect("Unknown basic block");
            writeln!(f, " {}:", bb.name)?;
            for inst in &bb.instructions {
                inst.fmt(f)?;
            }
        }

        Ok(())
    }
}
