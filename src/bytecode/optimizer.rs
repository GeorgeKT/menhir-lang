use std::collections::HashSet;
use bytecode::function::{BasicBlock, BasicBlockRef, ByteCodeFunction};
use bytecode::instruction::Instruction;
use bytecode::{START_CODE_FUNCTION, ByteCodeModule};
use compileerror::print_message;


// Erase all calls we come accross from the calls HashSet
fn erase_calls(module: &ByteCodeModule, func: &ByteCodeFunction, calls: &mut HashSet<String>)
{
    for block in func.blocks.values() {
        for instr in &block.instructions {
            if let Instruction::Call{ref func, ..} = *instr {
                if calls.contains(func) {
                    calls.remove(func);
                    module.get_function(func)
                        .map(|callee| erase_calls(module, callee, calls));
                }
            }
        }
    }
}

fn eliminate_unused_functions(module: &mut ByteCodeModule)
{
    let mut calls: HashSet<String> = module.functions.keys().cloned().collect();
    calls.remove(START_CODE_FUNCTION);
    calls.remove("main");
    if let Some(main) = module.get_function("main") {
        erase_calls(module, main, &mut calls);
    } else {
        return;
    }

    for call in &calls {
        module.functions
            .get(call)
            .map(|func| print_message(&format!("Warning: unused function {}", func.sig.name), &func.sig.span));
        module.functions.remove(call);
    }
}

// If the block only has a branch instruction to another block, it is considered empty
fn empty_block(bb: &BasicBlock) -> Option<BasicBlockRef>
{
    if bb.instructions.len() > 1 {
        return None
    }

    if let Instruction::Branch(bb_ref) = bb.instructions[0] {
        Some(bb_ref)
    } else {
        None
    }
}

fn replace_branch_target(func: &mut ByteCodeFunction, to_replace: BasicBlockRef, replacement: BasicBlockRef)
{
    for block in func.blocks.values_mut() {
        for instr in &mut block.instructions {
            match *instr
            {
                Instruction::Branch(ref mut bb_ref) => {
                    if *bb_ref == to_replace {
                        *bb_ref = replacement;
                    }
                },
                Instruction::BranchIf{ref mut on_true, ref mut on_false, ..} => {
                    if *on_true == to_replace {
                        *on_true = replacement;
                    }
                    if *on_false == to_replace {
                        *on_false = replacement
                    }
                },
                _ => (),
            }
        }
    }
}

fn remove_empty_blocks(func: &mut ByteCodeFunction)
{
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

pub fn optimize_function(func: &mut ByteCodeFunction)
{
    remove_empty_blocks(func);
}

pub fn optimize_module(module: &mut ByteCodeModule)
{
    eliminate_unused_functions(module);
    for func in module.functions.values_mut() {
        optimize_function(func);
    }
}

#[cfg(test)]
mod test
{
    use super::*;
    use bytecode::tests::generate_byte_code;
    use bytecode::instruction::Instruction;
    use bytecode::function::ByteCodeFunction;
    use ast::{sig, Type};
    use span::Span;

    #[test]
    fn test_block_elimination()
    {
        let func_sig = sig("foo", Type::Void, vec![], Span::default());
        let mut func = ByteCodeFunction::new(&func_sig);
        let bb1 = func.create_basic_block();
        let bb2 = func.create_basic_block();
        func.add(Instruction::Branch(bb1));
        func.set_current_bb(bb1);
        func.add(Instruction::Branch(bb2));
        func.set_current_bb(bb2);
        func.add(Instruction::ReturnVoid);

        optimize_function(&mut func);
        assert!(func.blocks.get(&bb1).is_none());
        assert!(func.blocks.get(&bb2).is_some());

    }

    #[test]
    fn test_function_elimination()
    {
        let mut m = generate_byte_code(r#"
            foo() -> int = 6
            bar() -> int = 7
            main() -> int = bar()
        "#, false).expect("Parsing succeeded");

        assert!(m.get_function("test::foo").is_some());
        assert!(m.get_function("test::bar").is_some());
        assert!(m.get_function("main").is_some());

        optimize_module(&mut m);

        assert!(m.get_function("test::foo").is_none());
        assert!(m.get_function("test::bar").is_some());
        assert!(m.get_function("main").is_some());
    }
}
