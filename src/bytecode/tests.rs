use std::io::Cursor;
use typechecker::{type_check_module};
use ast::{TreePrinter};
use parser::*;
use bytecode::*;
use compileerror::*;

fn generate_byte_code(prog: &str, dump: bool) -> CompileResult<ByteCodeModule>
{
    let mut cursor = Cursor::new(prog);
    let parser_options = ParserOptions::default();
    let mut md = parse_module(&parser_options, &mut cursor, "test", "")?;

    if dump {
        println!("Before type check");
        md.print(2);
        println!("-----------------");
    }

    type_check_module(&mut md)?;

    if dump {
        println!("After type check");
        md.print(2);
        println!("-----------------");
    }

    let bc_mod = compile_to_byte_code(&md);
    if dump {
        println!("ByteCode:");
        println!("{}", bc_mod);
        println!("-----------------");
    }

    Ok(bc_mod)
}

fn run(prog: &str, dump: bool) -> Result<i64, ExecutionError>
{
    let bc_mod = match generate_byte_code(prog, dump)
    {
        Ok(bc_mod) => bc_mod,
        Err(e) => return Err(ExecutionError(format!("Compile error: {}", e))),
    };

    let result = run_byte_code(&bc_mod, "main")?;
    match result
    {
        Value::Int(r) => Ok(r),
        _ => {
            let msg = format!("Expecting int return type, got {}", result);
            Err(ExecutionError(msg))
        },
    }
}

struct Test
{
    name: &'static str,
    ret: i64,
    debug: bool,
    code: &'static str,
}


const ALL_TESTS: [Test; 12] = [
    Test{
        name: "number",
        ret: 5,
        debug: false,
        code: "main() -> int = 5"
    },

    Test{
        name: "unary sub",
        ret: -5,
        debug: false,
        code: "main() -> int = -5"
    },

    Test{
        name: "unary not",
        ret: 8,
        debug: false,
        code: "main() -> int = if !true: 7 else 8"
    },

    Test{
        name: "arithmethic operators",
        ret: 4 + 35 - 3 + 1,
        debug: false,
        code: "main() -> int = 4 + 5 * 7 - 9 / 3 + 5 % 4",
    },

    Test{
        name: "boolean operators",
        ret: 1,
        debug: false,
        code: "main() -> int = if 4 < 5 * 7 && 9 / 3 > 5 % 4: 1 else 0"
    },

    Test{
        name: "call",
        ret: 13,
        debug: false,
        code: r#"
            add(a: int, b: int) -> int = a + b
            main() -> int = add(6, 7)
        "#
    },

    Test{
        name: "match int",
        ret: 299,
        debug: false,
        code: r#"
            foo(a: int) -> int =
                match a:
                    0 => 100,
                    1 => 299,
                    _ => 0

            main() -> int = foo(1)
        "#
    },

    Test{
        name: "match bool",
        ret: 100,
        debug: false,
        code: r#"
            foo(a: bool) -> int =
                match a:
                    true => 100,
                    false => 299

            main() -> int = foo(true)
        "#
    },

    Test{
        name: "let",
        ret: 18,
        debug: false,
        code: r#"foo(a: int, b: int, c: int) -> int =
            let x = a * b, y = b * c in
                x + y

            main() -> int = foo(2, 3, 4)
        "#
    },

    Test{
        name: "array",
        ret: 5,
        debug: false,
        code: r#"
            main() -> int =
                let x = [2, 3, 4] in 5
        "#
    },

    Test{
        name: "array recursive iteration",
        ret: 9,
        debug: false,
        code: r#"
            sum(v: int[]) -> int =
                match v:
                    [] => 0,
                    [head | tail] => head + sum(tail)

            main() -> int =
                let x = [2, 3, 4] in sum(x)
        "#
    },

    Test{
        name: "array recursive iteration 2",
        ret: 49,
        debug: false,
        code: r#"
            sum(v: int[]) -> int =
                match v:
                    [] => 0,
                    [head | tail] => head + sum(tail)

            main() -> int =
                let y = 7, x = [y; 7] in sum(x)
        "#
    }
];


#[test]
fn test_all()
{
    for test in &ALL_TESTS
    {
        println!("#### start {} ####", test.name);
        assert_eq!(run(test.code, test.debug), Ok(test.ret));
        println!("#### end {} ####", test.name);
    }

    //assert!(false);
}
