use bytecode::{ByteCodeModule};
use bytecode::function::{ByteCodeFunction};

mod emptyblocks;
mod unusedfunctions;
mod varelimination;

use self::emptyblocks::remove_empty_blocks;
use self::unusedfunctions::eliminate_unused_functions;
use self::varelimination::eliminate_vars;

pub fn optimize_function(func: &mut ByteCodeFunction)
{
    remove_empty_blocks(func);
    eliminate_vars(func);
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
