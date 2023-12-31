use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::compileerror::CompileResult;

use super::Operand;

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

    pub fn push(&mut self) {
        self.frames.push(StackFrame::default())
    }

    pub fn pop(&mut self) {
        self.frames.pop();
    }

    pub fn add(&mut self, name: &str, var: Operand) {
        self.frames
            .last_mut()
            .map(|f| f.vars.insert(name.into(), var));
    }

    pub fn get(&self, name: &str) -> CompileResult<Option<Operand>> {
        for f in self.frames.iter().rev() {
            if let Some(var) = f.vars.get(name) {
                return Ok(Some(var.safe_clone()?));
            }
        }
        Ok(None)
    }
}
