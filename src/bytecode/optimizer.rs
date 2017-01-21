use std::collections::HashSet;
use bytecode::function::ByteCodeFunction;
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
            .map(|func| print_message(&format!("Warning unused function {}", func.sig.name), &func.sig.span));
        module.functions.remove(call);
    }
}

fn optimize_function(_func: &mut ByteCodeFunction)
{

}

pub fn optimize_module(module: &mut ByteCodeModule)
{
    eliminate_unused_functions(module);
    for func in module.functions.values_mut() {
        optimize_function(func);
    }
}
