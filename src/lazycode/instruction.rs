use crate::ast::Type;

use super::operand::Operand;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Label {
    pub id: usize,
}

impl Label {
    pub fn new(id: usize) -> Label {
        Label { id }
    }

    pub fn block_name(&self) -> String {
        format!("block{}", self.id)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.id)
    }
}

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
        to: Label,
    },
    BranchIf {
        cond: Operand,
        on_true: Label,
        on_false: Label,
    },
    Return {
        value: Operand,
    },
    Delete {
        object: Operand,
    },
    Label {
        label: Label,
    },
    Mark {
        tag: String,
    },
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
            Instruction::Branch { to } => writeln!(f, "  br {to}"),
            Instruction::BranchIf {
                cond,
                on_true,
                on_false,
            } => writeln!(f, "  brif {cond} {on_true} {on_false}"),
            Instruction::Return { value } => writeln!(f, "  ret {value}"),
            Instruction::Delete { object } => writeln!(f, "  del {object}"),
            Instruction::Label { label } => writeln!(f, " {label}:"),
            Instruction::Mark { tag } => writeln!(f, "  #{tag}"),
        }
    }
}

pub fn ret_instr(value: Operand) -> Instruction {
    match &value {
        Operand::VarPtr { typ, .. } | Operand::Var { typ, .. } => {
            if typ == &Type::Void || typ.is_pointer_to(&Type::Void) {
                Instruction::Return { value: Operand::Void }
            } else {
                Instruction::Return { value }
            }
        }

        _ => Instruction::Return { value },
    }
}

pub fn branch_if_instr(cond: Operand, on_true: Label, on_false: Label) -> Instruction {
    Instruction::BranchIf {
        cond,
        on_true,
        on_false,
    }
}

pub fn branch_instr(label: Label) -> Instruction {
    Instruction::Branch { to: label }
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
            Instruction::Branch { .. } | Instruction::Label { .. } | Instruction::Mark { .. } => (),
        }
    }

    pub fn is_void_store(&self) -> bool {
        match self {
            Instruction::Store { value, .. } => value.get_type() == Type::Void,
            _ => false,
        }
    }
}
