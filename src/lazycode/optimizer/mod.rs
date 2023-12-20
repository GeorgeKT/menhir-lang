use crate::lazycode::function::ByteCodeFunction;
use crate::lazycode::ByteCodeModule;

mod emptyblocks;
mod unusedfunctions;

use self::emptyblocks::remove_empty_blocks;
use self::unusedfunctions::eliminate_unused_functions;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum OptimizationLevel {
    Minimal,
    Normal,
}

pub fn optimize_function(func: &mut ByteCodeFunction, _lvl: OptimizationLevel) {
    remove_empty_blocks(func);
}

pub fn optimize_module(module: &mut ByteCodeModule, lvl: OptimizationLevel) {
    eliminate_unused_functions(module);
    for func in module.functions.values_mut() {
        if !func.external {
            optimize_function(func, lvl);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::{sig, Type};
    use crate::lazycode::function::ByteCodeFunction;
    use crate::lazycode::instruction::Instruction;
    use crate::lazycode::test::generate_byte_code;
    use crate::lazycode::Operand;
    use crate::span::Span;

    #[test]
    fn test_block_elimination() {
        let func_sig = sig("foo", Type::Void, vec![], Span::default());
        let mut func = ByteCodeFunction::new(&func_sig, false);
        let bb1 = func.create_basic_block();
        let bb2 = func.create_basic_block();
        func.add(Instruction::Branch { block: bb1 });
        func.set_current_bb(bb1);
        func.add_basic_block(bb1);
        func.add(Instruction::Branch { block: bb2 });
        func.set_current_bb(bb2);
        func.add_basic_block(bb2);
        func.add(Instruction::Return { value: Operand::Void });

        optimize_function(&mut func, OptimizationLevel::Normal);
        assert!(func.blocks.get(&bb1).is_none());
        assert!(func.blocks.get(&bb2).is_some());
    }

    #[test]
    fn test_function_elimination() {
        let mut m = generate_byte_code(
            r#"
            fn foo() -> int: 6
            fn bar() -> int: 7
            fn main() -> int: bar()
        "#,
            false,
        )
        .expect("Parsing succeeded");

        assert!(m.get_function("test::foo").is_some());
        assert!(m.get_function("test::bar").is_some());
        assert!(m.get_function("test::main").is_some());

        optimize_module(&mut m, OptimizationLevel::Normal);

        assert!(m.get_function("test::foo").is_none());
        assert!(m.get_function("test::bar").is_some());
        assert!(m.get_function("test::main").is_some());
    }
}
