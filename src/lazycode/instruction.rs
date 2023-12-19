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
    Alias {
        name: String,
        value: Operand,
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
            Instruction::Alias { name, value, typ } => writeln!(f, "  alias {name}: {typ} = {value}"),
            Instruction::Declare { name, init, typ } => {
                if let Some(init) = init {
                    writeln!(f, "  decl {name}: {typ} = {init}")
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
    if let Operand::Var { typ: Type::Void, .. } = &value {
        // Ignore void variables
        Instruction::Return { value: Operand::Void }
    } else {
        Instruction::Return { value }
    }
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
    pub fn visit_operands<F>(&self, f: &mut F)
    where
        F: FnMut(&Operand),
    {
        match self {
            Instruction::Exec { operand } => operand.visit(f),
            Instruction::Declare { init, .. } => {
                if let Some(op) = init {
                    op.visit(f);
                }
            }
            Instruction::Store { dst, value } => {
                dst.visit(f);
                value.visit(f);
            }
            Instruction::BranchIf { cond, .. } => cond.visit(f),
            Instruction::Return { value } => value.visit(f),
            Instruction::Delete { object } => object.visit(f),
            Instruction::Alias { value, .. } => value.visit(f),
            Instruction::Branch { .. } | Instruction::ScopeStart | Instruction::ScopeEnd => (),
        }
    }

    pub fn is_terminator(&self) -> bool {
        match self {
            Instruction::Return { .. } | Instruction::Branch { .. } | Instruction::BranchIf { .. } => true,
            _ => false,
        }
    }

    pub fn is_void_store(&self) -> bool {
        match self {
            Instruction::Store { dst, .. } => {
                if let Operand::Var { typ, .. } = dst {
                    *typ == Type::Void
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}
