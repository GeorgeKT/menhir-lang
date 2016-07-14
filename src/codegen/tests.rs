#[cfg(test)] use std::ptr;
#[cfg(test)] use std::ffi::CStr;
#[cfg(test)] use std::os::raw::c_char;
#[cfg(test)] use std::io::Cursor;
#[cfg(test)] use llvm::prelude::*;
#[cfg(test)] use llvm::core::*;
#[cfg(test)] use llvm::execution_engine::*;
#[cfg(test)] use compileerror::{ErrorType, err, CompileError, Pos};
#[cfg(test)] use parser::{parse_module, ParseMode};
#[cfg(test)] use codegen::{CodeGenOptions, codegen, cstr};

#[cfg(test)]
fn run(prog: &str) -> Result<u64, CompileError>
{
    let mut cursor = Cursor::new(prog);
    let md = try!(parse_module(&mut cursor, "test", ParseMode::Module));

    let opts = CodeGenOptions{
        dump_ir: false,
        build_dir: "build".into(),
        program_name: "test".into(),
        runtime_library: "libcobraruntime.a".into(),
        optimize: true,
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
            return err(Pos::zero(), ErrorType::CodegenError(e));
        }

        let mut func: LLVMValueRef = ptr::null_mut();
        if LLVMFindFunction(ee, cstr("main"), &mut func) == 0 {
            let val = LLVMRunFunction(ee, func, 0, ptr::null_mut());
            let result = LLVMGenericValueToInt(val, 0) as u64;
            LLVMDisposeGenericValue(val);
            LLVMDisposeExecutionEngine(ee);
            Ok(result)
        } else {
            LLVMDisposeExecutionEngine(ee);
            err(Pos::zero(), ErrorType::CodegenError("No main function found".into()))
        }
    }
}

#[test]
fn test_number()
{
    assert!(run(r#"
func main() -> int:
    return 5
    ""#).unwrap() == 5);
}
