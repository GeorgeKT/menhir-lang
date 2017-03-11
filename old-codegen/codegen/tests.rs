use std::ptr;
use std::ffi::CStr;
use std::io::Cursor;
use libc;
use llvm::prelude::*;
use llvm::core::*;
use llvm::execution_engine::*;
use compileerror::{ErrorCode, err, CompileResult};
use parser::{ParserOptions, parse_module};
use codegen::{codegen, llvm_init};
use typechecker::{type_check_module};
use ast::{TreePrinter};
use span::Span;
use bytecode::*;

#[link(name="cobraruntime")]
extern {
    fn cobra_runtime_version() -> libc::c_uint;
}

fn run(prog: &str, dump: bool) -> CompileResult<i64>
{
    // Call this so, we force the compiler to link to the runtime library
    println!("COBRA runtime version {}", unsafe{cobra_runtime_version()});
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

    let llmod = compile_to_byte_code(&md);
    if dump {
        println!("LLREP");
        println!("-----------------");
        println!("{}", llmod);
        println!("-----------------");
    }

    llvm_init();
    let mut ctx = codegen(&llmod)?;

    unsafe {
        LLVMLinkInInterpreter();

        let mut ee: LLVMExecutionEngineRef = ptr::null_mut();
        let mut error_message: *mut libc::c_char = ptr::null_mut();
        if LLVMCreateInterpreterForModule(&mut ee, ctx.take_module_ref(), &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
            let e = format!("Unable to create interpreter: {}", msg);
            LLVMDisposeMessage(error_message);
            return err(&Span::default(), e);
        }

        let mut func: LLVMValueRef = ptr::null_mut();
        if LLVMFindFunction(ee, cstr!("main"), &mut func) == 0 {
            let val = LLVMRunFunction(ee, func, 0, ptr::null_mut());
            let result = LLVMGenericValueToInt(val, 0) as i64;
            LLVMDisposeGenericValue(val);
            LLVMDisposeExecutionEngine(ee);
            println!("result {}", result);
            Ok(result)
        } else {
            LLVMDisposeExecutionEngine(ee);
            err(&Span::default(), "No main function found".into())
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
    "#, true) == Ok(13));
}

#[test]
fn test_match_int()
{
    assert!(run(r#"
foo(a: int) -> int =
    match a:
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
    match a:
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
    "#, true) == Ok(18));
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
    match v:
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
    match v:
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
    match v:
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
fn test_anonymous_structs() {
    let r = run(r#"
make_pair(a: int, b: int) -> {int, int} = ${a, b}

main() -> int =
    let {left, right} = make_pair(4, 5) in left + right
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(9));
}

#[test]
fn test_sum_types() {
    let r = run(r#"
type Option = Some{int} | None

unwrap_or(opt: Option, default: int) -> int =
    match opt:
        Some{i} => i,
        None => default

main() -> int =
    unwrap_or(Some{5}, 1) + unwrap_or(None, 1)
    "#, false);
    println!("r: {:?}", r);
    assert!(r == Ok(6));
}


#[test]
fn test_sum_return_types() {
    let r = run(r#"
type PairOrSingle = Pair{int, int} | Single{int}

pair_or_single(a: int, b: int) -> PairOrSingle =
    if a != b: Pair{a, b} else Single{a}

value_of(v: PairOrSingle) -> int =
    match v:
        Pair{a, b} => a + b,
        Single{a} => a

main() -> int =
    value_of(pair_or_single(4, 5)) + value_of(pair_or_single(3, 3))
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(12));
}

#[test]
fn test_enum_types() {
    let r = run(r#"
type Animal = Dog | Cat | Bird | Fish

number(a: Animal) -> int =
    match a:
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
    match a:
        Dog => 7,
        Cat => 8,
        Bird => 22,
        Fish => 9

unwrap_or(opt: Option, def: int) -> int =
    match opt:
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
    match opt:
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


#[test]
fn test_if() {
    let r = run(r#"

max(a: int, b: int) -> int =
    if a > b: a
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
    if a > b: a
    else b

main() -> int {
    let x = max(10, 11);
    7 * x
}
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(77));
}

#[test]
fn test_char()
{
    let r = run(r#"
foo(c: char) -> int =
    match c:
        'b' => 5,
        '\n' => 7,
        _ => 3

main() -> int = foo('b') + foo('c')
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(8));
}

#[test]
fn test_var_escape()
{
let r = run(r#"
    type Foo = {
        bar: int
    }

    choose(num: int) -> Foo {
        let a = Foo{6};
        let b = Foo{7};
        if num > 6:
            a
        else
            b
    }

    main() -> int =
        let c = choose(10) in c.bar
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(6));
}



#[test]
fn test_member_function()
{
let r = run(r#"
    type Foo = {
        bar: int
    }

    Foo.add(self, num: int) -> int = self.bar + num

    main() -> int =
        let f = Foo{7} in f.add(4)
    "#, true);
    println!("r: {:?}", r);
    assert!(r == Ok(11));
}