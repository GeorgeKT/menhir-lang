use crate::compileerror::print_message;
use crate::lazycode::function::ByteCodeFunction;
use crate::lazycode::ByteCodeModule;
use crate::lazycode::Operand;
use std::collections::HashSet;

// Find all calls recursively, and remove them from the unused_calls HashSet
fn find_used_calls_rec(
    module: &ByteCodeModule,
    func: &ByteCodeFunction,
    unused_calls: &mut HashSet<String>,
    visited: &mut HashSet<String>,
) {
    let mut handle_func = |func: &str| {
        unused_calls.remove(func);
        if !visited.contains(func) {
            visited.insert(func.into());
            if let Some(callee) = module.get_function(func) {
                find_used_calls_rec(module, callee, unused_calls, visited)
            }
        }
    };

    func.visit_operands(&mut |op: &Operand| match op {
        Operand::Func { name, .. } => handle_func(&name),
        _ => (),
    })
}

fn find_used_calls(module: &ByteCodeModule, func: &ByteCodeFunction, unused_calls: &mut HashSet<String>) {
    let mut visited_calls = HashSet::new();
    find_used_calls_rec(module, func, unused_calls, &mut visited_calls)
}

pub fn eliminate_unused_functions(module: &mut ByteCodeModule) {
    let mut unused_calls: HashSet<String> = module.functions.keys().cloned().collect();
    let mut unused_imported: HashSet<String> = module
        .imported_functions
        .iter()
        .map(|func| func.sig.name.clone())
        .collect();
    if let Some(main) = module.get_function(&module.main_function_name()) {
        unused_calls.remove(&main.sig.name);
        find_used_calls(module, main, &mut unused_calls);
        find_used_calls(module, main, &mut unused_imported);
    } else {
        return;
    }

    for call in &unused_calls {
        if let Some(func) = module.functions.get(call) {
            print_message(&format!("Warning: unused function {}", func.sig.name), &func.sig.span)
        }
        module.functions.remove(call);
    }

    module
        .imported_functions
        .retain(|func: &ByteCodeFunction| !unused_imported.contains(&func.sig.name));
}
