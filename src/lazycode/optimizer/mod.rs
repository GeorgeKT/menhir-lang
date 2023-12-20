use crate::lazycode::ByteCodeModule;

mod unusedfunctions;

use self::unusedfunctions::eliminate_unused_functions;

pub fn optimize_module(module: &mut ByteCodeModule) {
    eliminate_unused_functions(module);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lazycode::test::generate_byte_code;

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

        optimize_module(&mut m);

        assert!(m.get_function("test::foo").is_none());
        assert!(m.get_function("test::bar").is_some());
        assert!(m.get_function("test::main").is_some());
    }
}
