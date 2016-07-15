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
fn run(prog: &str, dump_ir: bool) -> Result<u64, CompileError>
{
    let mut cursor = Cursor::new(prog);
    let md = try!(parse_module(&mut cursor, "test", ParseMode::Module));

    let opts = CodeGenOptions{
        dump_ir: dump_ir,
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
    ""#, false).unwrap() == 5);
}

#[test]
fn test_call()
{
    assert!(run(r#"
func foo(a: int, b: int) -> int:
    return a + b * 2

func main() -> int:
    return foo(1, 2)
    ""#, false).unwrap() == 5);
}

#[test]
fn test_vars()
{
    assert!(run(r#"
func main() -> int:
    const x = 7
    var y = 6
    y += x
    return y
    ""#, false).unwrap() == 13);
}

#[test]
fn test_global_vars()
{
    assert!(run(r#"
const x = 7
var y = 6
func main() -> int:
    y += x
    return y
    ""#, false).unwrap() == 13);
}

#[test]
fn test_struct()
{
    assert!(run(r#"
struct Point:
    var x = 0, y = 0

    pub func sum(self) -> int:
        return self.x + self.y

func main() -> int:
    var p = Point{4, 9}
    return p.sum()
    ""#, false).unwrap() == 13);
}

#[test]
fn test_post_fix_increment()
{
    assert!(run(r#"
struct Foo:
    pub var z = 6

var bar = Foo{}

func main() -> int:
    var y = 6
    var foo = Foo{}
    foo.z++
    bar.z++
    y++
    return y + foo.z + bar.z
    ""#, false).unwrap() == 21);
}


#[test]
fn test_arrays()
{
    assert!(run(r#"
struct Point:
    var x = 0, y = 0

    pub func sum(self) -> int:
        return self.x + self.y

func main() -> int:
    var p = [Point{4, 9}, Point{3, 4}]
    return p[0].sum() + p[1].sum()
    ""#, false).unwrap() == 20);
}


#[test]
fn test_global_arrays()
{
    let v = run(r#"
struct Point:
    var x = 0, y = 0

    pub func sum(self) -> int:
        return self.x + self.y

var p = [Point{4, 9}, Point{3, 4}]

func main() -> int:
    return p[0].sum() + p[1].sum()
    ""#, false).unwrap();
    assert!(v == 20);
}


#[test]
fn test_struct_with_arrays()
{
    assert!(run(r#"
struct Point:
    var coords = [0, 0]

    pub func sum(self) -> int:
        return self.coords[0] + self.coords[1]

func main() -> int:
    var p = Point{[4, 10]}
    return p.sum()
    ""#, false).unwrap() == 14);
}
