use std::ptr;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::io::Cursor;
use llvm::prelude::*;
use llvm::core::*;
use llvm::execution_engine::*;
use compileerror::{ErrorCode, err, CompileResult, Pos};
use parser::{ParserOptions, parse_module};
use codegen::{codegen, cstr, llvm_init};
use passes::{type_check_module};
use ast::{TreePrinter};



fn run(prog: &str, dump: bool) -> CompileResult<i64>
{
    let mut cursor = Cursor::new(prog);
    let parser_options = ParserOptions::default();
    let mut md = try!(parse_module(&parser_options, &mut cursor, "test"));
    if dump {
        println!("Before type check");
        println!("-----------------");
        md.print(0);
        println!("-----------------");
    }

    try!(type_check_module(&mut md));

    if dump {
        println!("After type check");
        println!("-----------------");
        md.print(0);
        println!("-----------------");
    }

    llvm_init();
    let mut ctx = try!(codegen(&md));

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
    let r = run(r#"
main() -> int =
    let x = [2, 3, 4] in 5
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(5));
}

#[test]
fn test_array_argument()
{
    let r = run(r#"
foo(v: [int]) -> int = 5

main() -> int =
    let x = [2, 3, 4] in foo(x)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(5));
}

#[test]
fn test_array_iteration()
{
    let r = run(r#"
sum(v: [int]) -> int =
    match v
        [] => 0,
        [head | tail] => head + sum(tail)

main() -> int =
    let x = [2, 3, 4] in sum(x)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(9));
}

#[test]
fn test_array_iteration2()
{
    let r = run(r#"
sum(v: [int]) -> int =
    match v
        [] => 0,
        [head | tail] => head + sum(tail)

main() -> int =
    let y = 7, x = [y; 7] in sum(x)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(49));
}

#[test]
fn test_generic()
{
    let r = run(r#"
add(x: $a, y: $a) -> $a = x + y

mul(x: $a, y: $a) -> $a = x * y

combine(x: $a, y: $a) -> $a =
    add(x, y) + mul(x, y)

main() -> int = combine(3, 2)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(11));
}

#[test]
fn test_lambda()
{
    let r = run(r#"
apply(x: int, fn: (int) -> int) -> int =
    fn(x)

main() -> int = apply(5, @(x) -> x * 2)
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(10));
}

#[test]
fn test_lambda_var()
{
    let r = run(r#"
apply(x: int, fn: (int) -> int) -> int =
    fn(x)

main() -> int =
    let triple = @(x) -> x * 3 in apply(5, triple)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(15));
}

#[test]
fn test_func_var()
{
    let r = run(r#"
triple(x: int) -> int = x * 3

main() -> int =
    let f = triple in f(5)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(15));
}

#[test]
fn test_generic_array_arguments()
{
    let r = run(r#"
fold(v: [$a], accu: $b, fn: ($b, $a) -> $b) -> $b =
    match v
        [] => accu,
        [hd | tail] => fold(tail, fn(accu, hd), fn)

sum(v: [int]) -> int =
    fold(v, 0, @(s, el) -> s + el)

main() -> int =
    sum([4, 5, 6, 7])
    ""#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(22));
}

#[test]
fn test_structs() {
    let r = run(r#"
type Vec2D = {x: int, y: int}

dot(a: Vec2D, b: Vec2D) -> int = a.x * b.x + a.y * b.y

main() -> int = dot(Vec2D{4, 5}, Vec2D{5, 6})
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(50));
}

#[test]
fn test_complex_return_types() {
    let r = run(r#"
type Vec2D = {x: int, y: int}

add(a: Vec2D, b: Vec2D) -> Vec2D = Vec2D{a.x + b.x, a.y + b.y}

main() -> int =
    let v = add(Vec2D{4, 5}, Vec2D{5, 6}) in v.x + v.y
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(20));
}

#[test]
fn test_sum_types() {
    let r = run(r#"
type Option = Some{int} | None

unwrap_or(opt: Option, default: int) -> int =
    match opt
        Some{i} => i,
        None => default

main() -> int =
    unwrap_or(Some{5}, 1) + unwrap_or(None, 1)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(6));
}

#[test]
fn test_enum_types() {
    let r = run(r#"
type Animal = Dog | Cat | Bird | Fish

number(a: Animal) -> int =
    match a
        Dog => 7,
        Cat => 8,
        Fish => 9,
        _ => 10

main() -> int =
    number(Dog) + number(Fish) + number(Cat) + number(Bird)
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(34));
}

#[test]
fn test_multiple_sum_types() {
    let r = run(r#"
type Animal = Dog | Cat | Bird | Fish
type Option = Some{int} | None

number(a: Animal) -> int =
    match a
        Dog => 7,
        Cat => 8,
        Bird => 22,
        Fish => 9

unwrap_or(opt: Option, def: int) -> int =
    match opt
        Some{i} => i,
        None => def

main() -> int =
    number(Dog) + number(Fish) + number(Cat) + number(Bird) + unwrap_or(Some{7}, 9)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(53));
}

#[test]
fn test_generic_sum_types() {
    let r = run(r#"
type Option = Some{$a} | None

unwrap_or(opt: Option<$a>, def: $a) -> $a =
    match opt
        Some{i} => i,
        None => def

main() -> int =
    unwrap_or(None, 9) + unwrap_or(Some{7}, 9)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(16));
}

#[test]
fn test_generic_struct_types() {
    let r = run(r#"
type Pair = {first: $a, second: $b}

add(p: Pair<$a, $a>) -> $a  =
    p.first + p.second

main() -> int = add(Pair{4, 14})
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(18));
}


/*
#[test]
fn test_strings() {
    let r = run(r#"

len(a: string) -> int = a.len

concat(a: string, b: string) -> string = a + b

main() -> int =
    let x = concat("Hello", " World") in len(x)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(11));
}
*/

#[test]
fn test_if() {
    let r = run(r#"

max(a: int, b: int) -> int =
    if a > b
    then a
    else b

main() -> int =
    max(100, 10)
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(100));
}


#[test]
fn test_block() {
    let r = run(r#"

max(a: int, b: int) -> int =
    if a > b
    then a
    else b

main() -> int = (let x = max(10, 11);  7 * x)
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(77));
}
