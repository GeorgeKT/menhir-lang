use std::collections::{BTreeMap, HashSet};
use std::fmt;

use itertools::join;

use super::blockorder::determine_block_order;
use super::compiler::expr_to_bc;
use super::ByteCodeModule;
use super::{
    instruction::{BasicBlockRef, Instruction},
    operand::Operand,
};
use crate::ast::{Expression, FunctionSignature, Type};
use crate::target::Target;

#[derive(Debug)]
pub struct Scope {
    unwind: Vec<Expression>, // Destructor calls
}

impl Scope {
    pub fn new() -> Scope {
        Scope { unwind: Vec::new() }
    }
}

pub fn bb_name(bb: BasicBlockRef) -> String {
    if bb == 0 {
        "entry".into()
    } else {
        format!("block{}", bb)
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub name: String,
    pub bb_ref: BasicBlockRef,
    pub instructions: Vec<Instruction>,
}

impl BasicBlock {
    pub fn new(name: String, bb_ref: BasicBlockRef) -> BasicBlock {
        BasicBlock {
            name,
            instructions: Vec::new(),
            bb_ref,
        }
    }

    pub fn add(&mut self, inst: Instruction) {
        if inst.is_terminator()
            && self
                .instructions
                .last()
                .map(|i| i.is_terminator())
                .unwrap_or(false)
        {
            // Already a terminator drop this, this only happens with an early return
            return;
        }

        if inst.is_void_store() {
            // Ignore void stores
            return;
        }

        self.instructions.push(inst);
    }

    pub fn ends_with_return(&self) -> bool {
        matches!(self.instructions.last(), Some(Instruction::Return { .. }))
    }
}

#[derive(Debug)]
pub struct ByteCodeFunction {
    pub sig: FunctionSignature,
    pub blocks: BTreeMap<BasicBlockRef, BasicBlock>,
    pub external: bool,
    current_bb: usize,
    bb_counter: usize,
    scopes: Vec<Scope>,
    names: HashSet<String>,
    pub block_order: Vec<BasicBlockRef>,
}

impl ByteCodeFunction {
    pub fn new(sig: &FunctionSignature, external: bool) -> ByteCodeFunction {
        let mut f = ByteCodeFunction {
            sig: sig.clone(),
            blocks: BTreeMap::new(),
            external,
            current_bb: 0,
            bb_counter: 0,
            scopes: vec![Scope::new()],
            names: HashSet::new(),
            block_order: Vec::new(),
        };

        for arg in &f.sig.args {
            f.names.insert(arg.name.clone());
        }

        f.sig.do_rvo();

        if !external {
            let entry = f.create_basic_block();
            f.set_current_bb(entry);
        }
        f
    }

    pub fn return_type(&self) -> Type {
        if !self.sig.rvo {
            self.sig.return_type.clone()
        } else {
            self.sig
                .args
                .last()
                .expect("ICE: a function with rvo should have a last argument")
                .typ
                .clone()
        }
    }

    pub fn add(&mut self, inst: Instruction) {
        let idx = self.current_bb;
        if let Some(bb) = self.blocks.get_mut(&idx) {
            bb.add(inst);
        }
    }

    /// Add a name, if already in use append a number
    fn add_name(&mut self, name: &str) -> String {
        let mut full_name = String::from(name);
        let mut cnt = 1;

        while self.names.contains(&full_name) {
            full_name = format!("{name}{cnt}");
            cnt += 1;
        }

        self.names.insert(full_name.clone());
        full_name
    }

    pub fn alias(&mut self, name: &str, value: Operand, typ: Type) -> Operand {
        let name = self.add_name(name);
        let var = Operand::Var {
            name: name.clone(),
            typ: typ.clone(),
        };

        self.add(Instruction::Alias { name, value, typ });
        var
    }

    pub fn declare(&mut self, name: &str, init: Option<Operand>, typ: Type) -> Operand {
        let name = self.add_name(name);
        let var = Operand::Var {
            name: name.clone(),
            typ: typ.clone(),
        };

        // Ignore void types
        if typ != Type::Void {
            self.add(Instruction::Declare { name, init, typ });
        }
        var
    }

    pub fn create_basic_block(&mut self) -> BasicBlockRef {
        let bb_ref = self.bb_counter;
        self.bb_counter += 1;
        let name = bb_name(bb_ref);
        self.blocks.insert(bb_ref, BasicBlock::new(name, bb_ref));
        bb_ref
    }

    pub fn set_current_bb(&mut self, bb_ref: BasicBlockRef) {
        assert!(bb_ref < self.blocks.len());
        self.current_bb = bb_ref;
    }

    pub fn last_instruction_is_return(&self) -> bool {
        let idx = self.current_bb;
        self.blocks
            .get(&idx)
            .and_then(|bb| bb.instructions.last())
            .map(|i| matches!(i, Instruction::Return { .. }))
            .unwrap_or(false)
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.add(Instruction::ScopeStart);
    }

    pub fn pop_scope_no_destructors(&mut self) {
        self.scopes.pop().expect("ICE: Empty Scope Stack");
    }

    pub fn pop_scope(&mut self, bc_mod: &mut ByteCodeModule, target: &Target) {
        let s = self.scopes.pop().expect("ICE: Empty Scope Stack");
        for uc in &s.unwind {
            let operand = expr_to_bc(bc_mod, self, uc, target);
            self.add(Instruction::Exec { operand });
        }
        if !self.scopes.is_empty() {
            // Add an endscope instruction, but not at function exit
            self.add(Instruction::ScopeEnd);
        }
    }

    pub fn add_unwind_calls(&mut self, unwind: &Vec<Expression>) {
        if let Some(s) = self.scopes.last_mut() {
            for c in unwind {
                s.unwind.push(c.clone());
            }
        }
    }

    pub fn get_unwind_calls(&mut self) -> Vec<Expression> {
        let mut unwind_calls = Vec::new();
        for s in self.scopes.iter_mut().rev() {
            for e in &s.unwind {
                unwind_calls.push(e.clone());
            }
        }
        unwind_calls
    }

    pub fn visit_operands<Func: FnMut(&Operand)>(&self, f: &mut Func) {
        for block in self.blocks.values() {
            for instr in &block.instructions {
                instr.visit_operands(f);
            }
        }
    }

    pub fn for_each_instruction_mut<Func: Fn(&mut Instruction) -> bool>(&mut self, f: Func) {
        for block in self.blocks.values_mut() {
            for instr in &mut block.instructions {
                if !f(instr) {
                    return;
                }
            }
        }
    }

    pub fn calculate_block_order(&mut self) {
        determine_block_order(self);
    }
}

impl fmt::Display for ByteCodeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(
            f,
            "{}({}) -> {}:",
            self.sig.name,
            join(
                self.sig
                    .args
                    .iter()
                    .map(|arg| format!("{}: {}", arg.name, arg.typ)),
                ", "
            ),
            self.sig.return_type
        )?;
        for bb_ref in &self.block_order {
            let Some(bb) = self.blocks.get(bb_ref) else {
                continue;
            };

            writeln!(f, " {}:", bb.name)?;
            for inst in &bb.instructions {
                inst.fmt(f)?;
            }
        }

        Ok(())
    }
}
