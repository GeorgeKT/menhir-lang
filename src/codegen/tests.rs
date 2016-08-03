use std::ptr;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::io::Cursor;
use llvm::prelude::*;
use llvm::core::*;
use llvm::execution_engine::*;
use compileerror::{ErrorCode, err, CompileResult, Pos};
use parser::{parse_module};
use codegen::{CodeGenOptions, codegen, cstr};
use passes::infer_and_check_types;
use ast::{TreePrinter};



fn run(prog: &str, dump: bool) -> CompileResult<i64>
{
    let mut cursor = Cursor::new(prog);
    let mut md = try!(parse_module(&mut cursor, "test"));
    if dump {
        md.print(0);
    }

    try!(infer_and_check_types(&mut md));

    let opts = CodeGenOptions{
        dump_ir: dump,
        build_dir: "build".into(),
        program_name: "test".into(),
        runtime_library: "libcobraruntime.a".into(),
        optimize: false,
    };

    let mut ctx = try!(codegen(&md, &opts));

    unsafe {
        LLVMLinkInInterpreter();

        let mut ee: LLVMExecutionEngineRef = ptr::null_mut();
        let mut error_message: *mut c_char = ptr::null_mut();
        if LLVMCreateInterpreterForModule(&mut ee, ctx.take_module_ref(), &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
            let e = format!("Unable to create interpreter: {}", msg);
            LLVMDisposeMessage(error_message);
            return err(Pos::zero(), ErrorCode::CodegenError, e);
        }

        let mut func: LLVMValueRef = ptr::null_mut();
        if LLVMFindFunction(ee, cstr("main"), &mut func) == 0 {
            let val = LLVMRunFunction(ee, func, 0, ptr::null_mut());
            let result = LLVMGenericValueToInt(val, 0) as i64;
            LLVMDisposeGenericValue(val);
            LLVMDisposeExecutionEngine(ee);
            println!("result {}", result);  
            Ok(result)
        } else {
            LLVMDisposeExecutionEngine(ee);
            err(Pos::zero(), ErrorCode::CodegenError, "No main function found".into())
        }
    }
}


#[test]
fn test_number()
{
    assert!(run(r#"
main() -> int = 5
    "#, false) == Ok(5));
}

#[test]
fn test_unary_sub()
{
    assert!(run(r#"
main() -> int = -5
    "#, false) == Ok(-5));
}

#[test]
fn test_unary_not()
{
    assert!(run(r#"
main() -> bool = !true
    "#, false) == Ok(0));
}


#[test]
fn test_binary_operators()
{
    assert!(run("main() -> int = 4 + 5 * 7 - 9 / 3 + 5 % 4", false) == Ok(4 + 35 - 3 + 1));
    assert!(run("main() -> bool = 4 < 5 * 7 && 9 / 3 > 5 % 4", false) == Ok(1));
}

#[test]
fn test_call()
{
    assert!(run(r#"
add(a: int, b: int) -> int = a + b
main() -> int = add(6, 7)
    "#, false) == Ok(13));
}

#[test]
fn test_match_int()
{
    assert!(run(r#"
foo(a: int) -> int =
    match a 
        0 => 100,
        1 => 299,
        _ => 0

main() -> int = foo(1)
    "#, false) == Ok(299));
}

#[test]
fn test_match_bool()
{
    assert!(run(r#"
foo(a: bool) -> int =
    match a 
        true => 100,
        false => 299

main() -> int = foo(true)
    "#, false) == Ok(100));
}

#[test]
fn test_let()
{
    assert!(run(r#"
foo(a: int, b: int, c: int) -> int =
    let x = a * b, y = b * c in
        x + y

main() -> int = foo(2, 3, 4)
    "#, false) == Ok(18));
}


#[test]
fn test_array()
{
    assert!(run(r#"
main() -> int = 
    let x = [2, 3, 4] in 5
    "#, false) == Ok(5));
}

#[test]
fn test_array_argument()
{
    assert!(run(r#"
foo(v: [int; 3]) -> int = 5

main() -> int = 
    let x = [2, 3, 4] in foo(x)
    "#, false) == Ok(5));
}