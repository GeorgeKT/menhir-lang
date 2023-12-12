use crate::ast::Type;

use super::operand::Operand;
use std::fmt;

pub type BasicBlockRef = usize;

#[derive(Debug)]
pub enum Instruction {
    Exec {
        operand: Operand,
    },
    Declare {
        name: String,
        init: Option<Operand>,
        typ: Type,
    },
    Store {
        dst: Operand,
        value: Operand,
    },
    Branch {
        block: BasicBlockRef,
    },
    BranchIf {
        cond: Operand,
        on_true: BasicBlockRef,
        on_false: BasicBlockRef,
    },
    Return {
        value: Operand,
    },
    Delete {
        object: Operand,
    },
    ScopeStart,
    ScopeEnd,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Exec { operand } => writeln!(f, "  exec {operand}"),
            Instruction::Declare { name, init, typ } => {
                if let Some(init) = init {
                    writeln!(f, "  decl {name}: {typ} {init}")
                } else {
                    writeln!(f, "  decl {name}: {typ}")
                }
            }
            Instruction::Store { dst, value } => writeln!(f, "  store {dst} {value}"),
            Instruction::Branch { block } => writeln!(f, "  br {block}"),
            Instruction::BranchIf {
                cond,
                on_true,
                on_false,
            } => writeln!(f, "  brif {cond} {on_true} {on_false}"),
            Instruction::Return { value } => writeln!(f, "  ret {value}"),
            Instruction::ScopeStart => writeln!(f, "  scope start"),
            Instruction::ScopeEnd => writeln!(f, "  scope end"),
            Instruction::Delete { object } => writeln!(f, "  del {object}"),
        }
    }
}

pub fn ret_instr(value: Operand) -> Instruction {
    Instruction::Return { value }
}

pub fn branch_if_instr(cond: Operand, on_true: BasicBlockRef, on_false: BasicBlockRef) -> Instruction {
    Instruction::BranchIf {
        cond,
        on_true,
        on_false,
    }
}

pub fn branch_instr(block: BasicBlockRef) -> Instruction {
    Instruction::Branch { block }
}

pub fn store_instr(dst: Operand, value: Operand) -> Instruction {
    Instruction::Store { dst, value }
}

impl Instruction {
    pub fn is_terminator(&self) -> bool {
        match self {
            Instruction::Return { .. } | Instruction::Branch { .. } | Instruction::BranchIf { .. } => true,
            _ => false,
        }
    }
}
