use std::collections::HashSet;
use bytecode::{ByteCodeModule};
use bytecode::function::{ByteCodeFunction};
use bytecode::instruction::{Instruction, Operand};
use compileerror::print_message;


// Find all calls recursively, and remove them from the unused_calls HashSet
fn find_used_calls_rec(module: &ByteCodeModule, func: &ByteCodeFunction, unused_calls: &mut HashSet<String>, visited: &mut HashSet<String>)
{
    let mut handle_func = |func: &str| {
        unused_calls.remove(func);
        if !visited.contains(func) {
            visited.insert(func.into());
            module.get_function(func)
                .map(|callee| find_used_calls_rec(module, callee, unused_calls, visited));
        }
    };

    func.for_each_instruction(|instr: &Instruction| {
        match *instr
        {
            Instruction::Call{ref func, ..} => {
                handle_func(func);
            }

            Instruction::Store{ref src, ..} => {
                if let Operand::Func(ref func) = *src {
                    handle_func(func);
                }
            }

            _ => ()
        }
        true
    })
}

fn find_used_calls(module: &ByteCodeModule, func: &ByteCodeFunction, unused_calls: &mut HashSet<String>)
{
    let mut visited_calls = HashSet::new();
    find_used_calls_rec(module, func, unused_calls, &mut visited_calls)
}

pub fn eliminate_unused_functions(module: &mut ByteCodeModule)
{

    let mut unused_calls: HashSet<String> = module.functions.keys().cloned().collect();
    let mut unused_imported: HashSet<String> = module.imported_functions.iter().map(|func| func.sig.name.clone()).collect();
    if let Some(main) = module.get_function(&module.main_function_name()) {
        unused_calls.remove(&main.sig.name);
        find_used_calls(module, main, &mut unused_calls);
        find_used_calls(module, main, &mut unused_imported);
    } else {
        return;
    }


    for call in &unused_calls {
        module.functions
            .get(call)
            .map(|func| print_message(&format!("Warning: unused function {}", func.sig.name), &func.sig.span));
        module.functions.remove(call);
    }

    module.imported_functions.retain(|func: &ByteCodeFunction| !unused_imported.contains(&func.sig.name));
}
