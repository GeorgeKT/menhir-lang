use bytecode::function::{BasicBlock, BasicBlockRef, ByteCodeFunction};
use bytecode::instruction::Instruction;

// If the block only has a branch instruction to another block, it is considered empty
fn empty_block(bb: &BasicBlock) -> Option<BasicBlockRef> {
    if bb.instructions.len() > 1 {
        return None;
    }

    if let Instruction::Branch(bb_ref) = bb.instructions[0] {
        Some(bb_ref)
    } else {
        None
    }
}

fn replace_branch_target(func: &mut ByteCodeFunction, to_replace: BasicBlockRef, replacement: BasicBlockRef) {
    func.for_each_instruction_mut(|instr: &mut Instruction| {
        match *instr {
            Instruction::Branch(ref mut bb_ref) => {
                if *bb_ref == to_replace {
                    *bb_ref = replacement;
                }
            }
            Instruction::BranchIf {
                ref mut on_true,
                ref mut on_false,
                ..
            } => {
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
}
