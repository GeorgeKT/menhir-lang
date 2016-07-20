use std::ptr;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::io::Cursor;
use llvm::prelude::*;
use llvm::core::*;
use llvm::execution_engine::*;
use compileerror::{ErrorCode, err, CompileResult, Pos};
use parser::{parse_module, ParseMode};
use codegen::{CodeGenOptions, codegen, cstr};
use ast::{TreePrinter};


fn run(prog: &str, dump: bool) -> CompileResult<u64>
{
    let mut cursor = Cursor::new(prog);
    let md = try!(parse_module(&mut cursor, "test", ParseMode::Module));
    if dump {
        md.print(0);
    }

    let opts = CodeGenOptions{
        dump_ir: dump,
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
            return err(Pos::zero(), ErrorCode::CodegenError, e);
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
            err(Pos::zero(), ErrorCode::CodegenError, "No main function found".into())
        }
    }
}

#[test]
fn test_number()
{
    assert!(run(r#"
func main() -> int:
    return 5
    "#, false).unwrap() == 5);
}

#[test]
fn test_call()
{
    assert!(run(r#"
func foo(a: int, b: int) -> int:
    return a + b * 2

func main() -> int:
    return foo(1, 2)
    "#, false).unwrap() == 5);
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
    "#, false).unwrap() == 13);
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
    "#, false).unwrap() == 13);
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
    "#, false).unwrap() == 13);
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
    "#, false).unwrap() == 21);
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
    "#, false).unwrap() == 20);
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
    "#, false).unwrap();
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
    "#, false).unwrap() == 14);
}

#[test]
fn test_array_initializer()
{
    assert!(run(r#"
func sum(v: [int]) -> int:
    var i = 0, s = 0
    while i < v.len:
        s += v[i]
        i++
    return s

func main() -> int:
    const x = 20
    return sum([x; 4])
    "#, false).unwrap() == 80);
}

#[test]
fn test_if()
{
    assert!(run(r#"
struct Point:
    var x = 0, y = 0

    pub func sum(self) -> int:
        return self.x + self.y

func main() -> int:
    const x = 77
    if x > 10:
        var p = Point{7, 9}
        return p.sum()
    else:
        return 5
    "#, false).unwrap() == 16);
}

#[test]
fn test_else()
{
    assert!(run(r#"
struct Point:
    var x = 0, y = 0

    pub func sum(self) -> int:
        return self.x + self.y

func main() -> int:
    const x = 77
    if x < 10:
        var p = Point{7, 9}
        return p.sum()
    else:
        return 5
    "#, false).unwrap() == 5);
}

#[test]
fn test_while()
{
    assert!(run(r#"
func main() -> int:
    var i = 0
    var count = 0
    while i < 10:
        count += 2
        i++
    return count
    "#, false).unwrap() == 20);
}

#[test]
fn test_slice()
{
    assert!(run(r#"
func sum(v: [int]) -> int:
    var i = 0, s = 0
    while i < v.len:
        s += v[i]
        i++
    return s

func main() -> int:
    return sum([4, 5, 6, 7])
    "#, false).unwrap() == 22);
}

#[test]
fn test_object_passing()
{
    assert!(run(r#"
struct Point:
    pub var x = 0, y = 0

func test(var p: Point) -> int:
    p.x += 1
    p.y += 1
    return p.x + p.y


func test2(var p: *Point) -> int:
    p.x += 1
    p.y += 1
    return p.x + p.y

func main() -> int:
    var a = Point{10, 12}
    var b = Point{4, 5}
    return test(a) + test2(b) + (a.x + a.y) + (b.x + b.y)
    "#, false).unwrap() == (24 + 11 + 22 + 11));
}

#[test]
fn test_traits()
{
    assert!(run(r#"
trait Sum:
    func sum(self) -> int

struct Foo impl Sum:
    var a = 0, b = 0, c = 0

    pub func sum(self) -> int:
        return self.a + self.b + self.c

func main() -> int:
    const f = Foo{1, 2, 3}
    return f.sum()
    "#, false).unwrap() == 6);
}

#[test]
fn test_not_implemented_traits()
{
    assert!(run(r#"
trait Sum:
    func sum(self) -> int

struct Foo impl Sum:
    var a = 0, b = 0, c = 0

    pub func not_really_sum(self) -> int:
        return self.a + self.b + self.c

func main() -> int:
    const f = Foo{1, 2, 3}
    return f.not_really_sum()
    "#, false).unwrap_err().error == ErrorCode::TraitNotImplemented);
}


#[test]
fn test_generic_function()
{
    assert!(run(r#"
trait Sum:
    func sum(self) -> int

struct Foo impl Sum:
    var a = 0, b = 0, c = 0

    pub func sum(self) -> int:
        return self.a + self.b + self.c

func sum<T: Sum>(x: *T) -> int:
    return x.sum()

func main() -> int:
    const f = Foo{1, 2, 3}
    return sum(f)
    "#, false).unwrap() == 6);
}
