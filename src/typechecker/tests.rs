use parser::{th_expr, th_mod};
use super::typecheck::{type_check_expression, type_check_module};
use super::typecheckercontext::TypeCheckerContext;
use ast::Type;
use compileerror::{CompileResult};

fn type_check(expr: &str) -> CompileResult<Type>
{
	let mut ctx = TypeCheckerContext::new();
	let mut e = th_expr(expr);
	let r = type_check_expression(&mut ctx, &mut e, &None);
	println!("result: {:?}", r);
	r
}


fn type_check_mod(expr: &str) -> CompileResult<()>
{
	let mut md = th_mod(expr);
	let r = type_check_module(&mut md);
	println!("result: {:?}", r);
	r
}

#[test]
fn test_unary_op()
{
	assert!(type_check("-true").is_err());
	assert!(type_check("-5").is_ok());
	assert!(type_check("!0").is_err());
	assert!(type_check("!false").is_ok());
}


#[test]
fn test_wrong_type_bin_op()
{
	assert!(type_check("4 + 6.3").is_err());
	assert!(type_check("4.4 % 6.3").is_err());
	assert!(type_check("4 - 7").is_ok());
	assert!(type_check("4.5 * 7.6").is_ok());
	assert!(type_check("true / 7").is_err());

	assert!(type_check("4 >= 6.3").is_err());
	assert!(type_check("4 > 7").is_ok());
	assert!(type_check("4.5 > 7.6").is_ok());
	assert!(type_check("true <= 7").is_err());

	assert!(type_check("(true && false) || true").is_ok());
	assert!(type_check("true && 5").is_err());
}

#[test]
fn test_arrays()
{
	assert!(type_check("[4, 5.7]").is_err());
	assert!(type_check("[4, 5, 7]").is_ok());
	assert!(type_check("[4; 10]").is_ok());
}

#[test]
fn test_function()
{
	assert!(type_check_mod("add(a: int, b: int) -> int = a + b").is_ok());
	assert!(type_check_mod("add(a: int, b: int) -> int = 7.5").is_err());
}

#[test]
fn test_match()
{
	assert!(type_check_mod(r#"
foo(x: int[]) -> int =
	match x:
		[] => 0,
		[head | tail] => head + foo(tail)
"#).is_ok());

	assert!(type_check_mod(r#"
foo(x: int[]) -> int =
	match x:
		7 => 0,
		[head | tail] => head + foo(tail)
"#).is_err());

	assert!(type_check_mod(r#"
foo(x: int) -> int =
	match x:
		7 => 8,
		6 => 7,
		_ => 9
"#).is_ok());
}

#[test]
fn test_let()
{
	assert!(type_check(r#"
let x = 6 in x + x
"#).is_ok());

	assert!(type_check(r#"
let x = 6 in x + y
"#).is_err());

	assert!(type_check(r#"
let x = [6, 7] in x.len
"#).is_ok());
}


#[test]
fn test_mutability()
{
    assert!(
        type_check_mod(r#"
            main() -> int {
                let x = 9;
                x = 5;
                x
            }
        "#).is_err()
    );

    assert!(
        type_check_mod(r#"
            foo(a: int) -> int {
                a = a + 2;
                a
            }
            main() -> int {
                foo(5)
            }
        "#).is_err()
    );

    assert!(
        type_check_mod(r#"
            main() -> int {
                var x = 9;
                x = 5;
                x
            }
            "#).is_ok()
    );
}
