use std::collections::HashMap;

use crate::lazycode::{instruction::Label, ByteCodeFunction, ByteCodeModule, Instruction, Scope, ScopeNode};

fn add_empty_block(empty_blocks: &mut HashMap<Label, Label>, from: Label, to: Label) {
    empty_blocks.insert(from, to);
    for (_, e_to) in empty_blocks.iter_mut() {
        if *e_to == from {
            *e_to = to;
        }
    }
}

fn find_empty_blocks_in_scope(scope: &Scope, empty_blocks: &mut HashMap<Label, Label>) {
    let mut current_label = None;
    let mut instruction_count = 0;
    for n in &scope.nodes {
        match n {
            ScopeNode::Instruction(Instruction::Label { label }) => {
                instruction_count = 0;
                current_label = Some(*label);
            }

            ScopeNode::Instruction(Instruction::Branch { to }) => {
                if let Some(label) = &current_label {
                    if instruction_count == 0 {
                        add_empty_block(empty_blocks, *label, *to);
                    }
                }

                current_label = None;
                instruction_count = 0;
            }

            ScopeNode::Instruction(Instruction::BranchIf { .. })
            | ScopeNode::Instruction(Instruction::Return { .. }) => {
                current_label = None;
                instruction_count = 0;
            }

            ScopeNode::Instruction(Instruction::Mark { .. }) => {
                // ignore
            }

            ScopeNode::Instruction(_) => {
                instruction_count += 1;
            }

            ScopeNode::Scope(s) => {
                instruction_count += 1;
                find_empty_blocks_in_scope(s, empty_blocks);
            }
        }
    }
}

fn eliminate_empty_blocks_in_scope(scope: &mut Scope, empty_blocks: &HashMap<Label, Label>) -> usize {
    let mut eliminate = false;
    let mut scope_instructions = 0;
    scope.nodes.retain_mut(|node| match node {
        ScopeNode::Instruction(Instruction::Label { label }) => {
            if empty_blocks.contains_key(label) {
                eliminate = true;
            }

            !eliminate
        }

        ScopeNode::Instruction(Instruction::Branch { to }) => {
            if eliminate {
                eliminate = false;
                false
            } else {
                scope_instructions += 1;
                if let Some(new_to) = empty_blocks.get(to) {
                    *to = *new_to;
                }
                true
            }
        }

        ScopeNode::Instruction(Instruction::BranchIf { on_true, on_false, .. }) => {
            if eliminate {
                eliminate = false;
                false
            } else {
                scope_instructions += 1;
                if let Some(new_to) = empty_blocks.get(on_true) {
                    *on_true = *new_to;
                }
                if let Some(new_to) = empty_blocks.get(on_false) {
                    *on_false = *new_to;
                }
                true
            }
        }

        ScopeNode::Instruction(Instruction::Return { .. }) => {
            eliminate = false;
            scope_instructions += 1;
            !eliminate
        }

        ScopeNode::Instruction(_) => {
            scope_instructions += 1;
            !eliminate
        }

        ScopeNode::Scope(s) => eliminate_empty_blocks_in_scope(s, empty_blocks) > 0,
    });

    scope_instructions
}

fn eliminate_empty_blocks_in_func(func: &mut ByteCodeFunction) {
    let mut empty_blocks = HashMap::new();
    find_empty_blocks_in_scope(&func.toplevel_scope, &mut empty_blocks);
    eliminate_empty_blocks_in_scope(&mut func.toplevel_scope, &empty_blocks);
}

pub fn eliminate_empty_blocks(bc_mod: &mut ByteCodeModule) {
    for func in bc_mod.functions.values_mut() {
        if !func.external {
            eliminate_empty_blocks_in_func(func);
        }
    }
}
