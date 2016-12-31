use std::io::Cursor;
use typechecker::{type_check_module};
use ast::{TreePrinter};
use span::Span;
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
        println!("-----------------");
        md.print(0);
        println!("-----------------");
    }

    type_check_module(&mut md)?;

    if dump {
        println!("After type check");
        println!("-----------------");
        md.print(0);
        println!("-----------------");
    }

    let bc_mod = compile_to_byte_code(&md);
    if dump {
        println!("ByteCode");
        println!("-----------------");
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


#[test]
fn test_number()
{
    assert_eq!(run(r#"
main() -> int = 5
    "#, true), Ok(5));
}
