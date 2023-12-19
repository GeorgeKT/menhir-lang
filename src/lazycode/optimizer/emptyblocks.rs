use crate::lazycode::instruction::Instruction;
use crate::lazycode::{BasicBlock, BasicBlockRef, ByteCodeFunction};

// If the block only has a branch instruction to another block, it is considered empty
fn empty_block(bb: &BasicBlock) -> Option<BasicBlockRef> {
    if bb.instructions.len() > 1 {
        return None;
    }

    if let Some(Instruction::Branch { block }) = bb.instructions.first() {
        Some(*block)
    } else {
        None
    }
}

fn replace_branch_target(func: &mut ByteCodeFunction, to_replace: BasicBlockRef, replacement: BasicBlockRef) {
    func.for_each_instruction_mut(|instr: &mut Instruction| {
        match instr {
            Instruction::Branch { block } => {
                if *block == to_replace {
                    *block = replacement;
                }
            }
            Instruction::BranchIf { on_true, on_false, .. } => {
                if *on_true == to_replace {
                    *on_true = replacement;
                }
                if *on_false == to_replace {
                    *on_false = replacement
                }
            }
            _ => (),
        }
        true
    })
}

pub fn remove_empty_blocks(func: &mut ByteCodeFunction) {
    let mut candidates = Vec::new();
    for (idx, bb) in func.blocks.values().enumerate() {
        if let Some(next_bb) = empty_block(bb) {
            candidates.push((idx, next_bb));
        }
    }

    for &(to_replace, _) in &candidates {
        func.blocks.remove(&to_replace);
    }

    for &(to_replace, replacement) in &candidates {
        replace_branch_target(func, to_replace, replacement);
    }

    func.calculate_block_order();
}
