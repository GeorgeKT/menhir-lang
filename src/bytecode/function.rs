use std::fmt;
use std::collections::{BTreeMap, HashMap};
use itertools::free::join;
use ast::{Type, FunctionSignature};
use bytecode::instruction::Instruction;

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
    insert_block: BasicBlockRef,
    insert_position: usize,
}

impl Scope
{
    pub fn new(insert_block: BasicBlockRef, insert_position: usize) -> Scope
    {
        Scope{
            named_vars: HashMap::new(),
            to_cleanup: Vec::new(),
            insert_block: insert_block,
            insert_position: insert_position,
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
    pub external: bool,
    current_bb: usize,
    bb_counter: usize,
    var_counter: usize,
    scopes: Vec<Scope>,
    destinations: Vec<Option<Var>>,
}


impl ByteCodeFunction
{
    pub fn new(sig: &FunctionSignature, external: bool) -> ByteCodeFunction
    {
        let mut f = ByteCodeFunction{
            sig: sig.clone(),
            blocks: BTreeMap::new(),
            external: external,
            current_bb: 0,
            bb_counter: 0,
            var_counter: 0,
            scopes: vec![Scope::new(0, 0)],
            destinations: Vec::new(),
        };

        if !external {
            let entry = f.create_basic_block();
            f.set_current_bb(entry);

            for arg in &sig.args {
                f.add_named_var(Var::named(&arg.name, arg.typ.clone()));
            }
        }
        f
    }

    fn add_instruction(&mut self, inst: Instruction)
    {
        let idx = self.current_bb;
        self.blocks.get_mut(&idx).map(|bb| bb.instructions.push(inst));
    }

    pub fn add(&mut self, inst: Instruction)
    {
        match inst
        {
            Instruction::Return(_) | Instruction::ReturnVoid => {
                // Pop final scope before returning
                self.pop_scope();
                self.add_instruction(inst);
            },

            Instruction::StackAlloc(_) => {
                let mut scope = self.scopes.last_mut().expect("Empty scope stack");
                let insert_position = scope.insert_position;
                self.blocks
                    .get_mut(&scope.insert_block)
                    .map(|bb| bb.instructions.insert(insert_position, inst));
                scope.insert_position += 1;
            },

            _ => {
                self.add_instruction(inst);
            },
        }
    }

    pub fn create_basic_block(&mut self) -> BasicBlockRef
    {
        let bb_ref = self.bb_counter;
        self.bb_counter += 1;
        let name = bb_name(bb_ref);
        self.blocks.insert(bb_ref, BasicBlock::new(name));
        bb_ref
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
        let idx = self.current_bb;
        let insert_position = self.blocks.get_mut(&idx)
            .map(|bb| bb.instructions.len() + 1)
            .expect("Unknown block");
        self.scopes.push(Scope::new(self.current_bb, insert_position));
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

    pub fn for_each_instruction<Func: FnMut(&Instruction) -> bool>(&self, mut f: Func)
    {
        for block in self.blocks.values() {
            for instr in &block.instructions {
                if !f(instr) {
                    return;
                }
            }
        }
    }

    pub fn for_each_instruction_mut<Func: Fn(&mut Instruction) -> bool>(&mut self, f: Func)
    {
        for block in self.blocks.values_mut() {
            for instr in &mut block.instructions {
                if !f(instr) {
                    return;
                }
            }
        }
    }

    pub fn replace_instruction<Func>(&mut self, f: Func)
        where Func: Fn(&Instruction) -> Vec<Instruction>
    {
        for block in self.blocks.values_mut() {
            let mut idx = 0;
            while idx < block.instructions.len() {
                let replacements = f(&block.instructions[idx]);
                if !replacements.is_empty() {
                    block.instructions.remove(idx);
                    for r in replacements {
                        block.instructions.insert(idx, r);
                        idx += 1;
                    }
                } else {
                    idx += 1;
                }
            }
        }
    }

    pub fn remove_instruction<Pred: Fn(&Instruction) -> bool>(&mut self, pred: Pred) {
        for block in self.blocks.values_mut() {
            block.instructions.retain(|instr| !pred(instr));
        }
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
        writeln!(f, "{}({}) -> {}:",
            self.sig.name,
            join(self.sig.args.iter().map(|arg| format!("{}: {}", arg.name, arg.typ)), ", "),
            self.sig.return_type)?;
        for bb in self.blocks.values() {
            writeln!(f, " {}:", bb.name)?;
            for inst in &bb.instructions {
                inst.fmt(f)?;
            }
        }

        Ok(())
    }
}
