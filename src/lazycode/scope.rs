use crate::{
    ast::{prefix, Expression, FunctionSignature, TreeFormatter, Type, RVO_RETURN_ARG},
    target::Target,
};

use super::{compiler::expr_to_bc, instruction::Label, ByteCodeModule, Instruction, Operand};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Default, Debug)]
pub struct StackFrame {
    vars: HashMap<String, Operand>,
}

#[derive(Debug)]
pub struct Stack {
    frames: Vec<StackFrame>,
}

pub type StackPtr = Rc<RefCell<Stack>>;

impl Stack {
    pub fn new() -> StackPtr {
        Rc::new(RefCell::new(Stack {
            frames: vec![StackFrame::default()],
        }))
    }

    fn push(&mut self) {
        self.frames.push(StackFrame::default())
    }

    fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn add(&mut self, name: &str, var: Operand) {
        self.frames
            .last_mut()
            .map(|f| f.vars.insert(name.into(), var));
    }

    pub fn get(&self, name: &str) -> Option<Operand> {
        for f in self.frames.iter().rev() {
            if let Some(var) = f.vars.get(name) {
                return Some(var.safe_clone());
            }
        }
        None
    }
}

#[derive(Debug)]
pub enum ScopeNode {
    Instruction(Instruction),
    Scope(Scope),
}

#[derive(Debug)]
pub struct Scope {
    pub nodes: Vec<ScopeNode>,
    unwind: Vec<Expression>,
    parent_unwind: Vec<Expression>,
    label_counter: usize,
    sig: FunctionSignature,
    stack: StackPtr,
}

impl Scope {
    pub fn new(sig: FunctionSignature, toplevel: bool, stack: StackPtr) -> Self {
        {
            let mut s = stack.borrow_mut();
            s.push();

            if toplevel {
                for arg in &sig.args {
                    s.add(
                        &arg.name,
                        Operand::Var {
                            name: arg.name.clone(),
                            typ: arg.typ.clone(),
                        },
                    );
                }
            }
        }
        Scope {
            sig,
            nodes: Vec::new(),
            unwind: Vec::new(),
            parent_unwind: Vec::new(),
            label_counter: 1,
            stack,
        }
    }
    pub fn add_unwind_call(&mut self, e: Expression) {
        self.unwind.push(e);
    }

    pub fn rvo_var(&self) -> Operand {
        Operand::VarPtr {
            name: RVO_RETURN_ARG.into(),
            typ: self.sig.return_type.ptr_of(),
        }
    }

    pub fn rvo(&self) -> bool {
        self.sig.rvo
    }

    pub fn label(&mut self) -> Label {
        let lbl = self.label_counter;
        self.label_counter += 1;
        Label::new(lbl)
    }

    pub fn start_label(&mut self, lbl: Label) {
        self.nodes
            .push(ScopeNode::Instruction(Instruction::Label { label: lbl }));
    }

    pub fn add(&mut self, inst: Instruction) {
        if inst.is_void_store() {
            // Ignore void stores
            return;
        }
        self.nodes.push(ScopeNode::Instruction(inst));
    }

    pub fn make_var(&mut self, name: &str, value: Operand) -> Operand {
        if value.cloneable() {
            return value;
        }

        self.alias(name, value)
    }

    pub fn alias(&mut self, name: &str, value: Operand) -> Operand {
        let name: String = name.into();
        let typ = value.get_type();
        let var = Operand::Var {
            name: name.clone(),
            typ: typ.clone(),
        };

        self.stack.borrow_mut().add(&name, var.safe_clone());
        self.add(Instruction::Alias { name, value, typ });
        var
    }

    pub fn declare(&mut self, name: &str, init: Option<Operand>, typ: Type) -> Operand {
        let name: String = name.into(); //self.add_name(name);
        let var = Operand::VarPtr {
            name: name.clone(),
            typ: typ.ptr_of(),
        };

        self.stack.borrow_mut().add(&name, var.safe_clone());
        // Ignore void types
        if typ != Type::Void {
            self.add(Instruction::Declare { name, init, typ });
        }
        var
    }

    pub fn get_var(&self, name: &str) -> Option<Operand> {
        match self.stack.borrow().get(name) {
            Some(Operand::VarPtr { name, typ }) => Some(Operand::Var {
                name: name.clone(),
                typ: typ.clone(),
            }),
            Some(v) => Some(v.safe_clone()),
            None => None,
        }
    }

    pub fn exit(&mut self, bc_mod: &mut ByteCodeModule, target: &Target, with: Instruction) {
        let unwind = self.unwind.clone();
        for e in unwind.iter().rev() {
            let op = expr_to_bc(bc_mod, self, e, target);
            if op.is_call() {
                self.add(Instruction::Exec { operand: op });
            }
        }
        if let Instruction::Return { .. } = with {
            let parent_unwind = self.parent_unwind.clone();
            for e in parent_unwind.iter().rev() {
                let op = expr_to_bc(bc_mod, self, e, target);
                if op.is_call() {
                    self.add(Instruction::Exec { operand: op });
                }
            }
        }

        self.add(Instruction::Mark {
            tag: "scope exit".into(),
        });
        self.add(with);
    }

    pub fn scope<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Scope),
    {
        let mut scope = Scope::new(self.sig.clone(), false, self.stack.clone());
        scope.label_counter = self.label_counter;
        scope.parent_unwind = self.parent_unwind.clone();
        scope.parent_unwind.extend(self.unwind.iter().cloned());
        f(&mut scope);
        self.label_counter = scope.label_counter;
        self.nodes.push(ScopeNode::Scope(scope));
        let mut s = self.stack.borrow_mut();
        s.pop();
    }

    pub fn visit_operands<Func: FnMut(&Operand)>(&self, f: &mut Func) {
        for node in &self.nodes {
            match node {
                ScopeNode::Instruction(i) => i.visit_operands(f),
                ScopeNode::Scope(s) => s.visit_operands(f),
            }
        }
    }

    pub fn for_each_instruction<Func: FnMut(&Instruction) -> bool>(&self, f: &mut Func) {
        for node in &self.nodes {
            match node {
                ScopeNode::Instruction(i) => {
                    if !f(i) {
                        return;
                    }
                }
                ScopeNode::Scope(s) => s.for_each_instruction(f),
            }
        }
    }

    pub fn last_instruction_is_return(&self) -> bool {
        self.nodes
            .last()
            .map(|i| matches!(i, ScopeNode::Instruction(Instruction::Return { .. })))
            .unwrap_or(false)
    }
}

impl TreeFormatter for Scope {
    fn fmt(&self, level: usize, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let pfx = prefix(level);
        writeln!(f, "{pfx}scope:")?;
        for node in &self.nodes {
            match node {
                ScopeNode::Instruction(i) => write!(f, "{pfx}{i}")?,
                ScopeNode::Scope(s) => s.fmt(level + 1, f)?,
            }
        }

        Ok(())
    }
}
