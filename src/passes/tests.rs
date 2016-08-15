use parser::th_expr;
use passes::typechecker::{TypeCheckerContext, infer_and_check_expression};
use ast::Type;
use compileerror::{CompileResult, ErrorCode};

fn type_check(expr: &str) -> CompileResult<Type>
{
	let mut ctx = TypeCheckerContext::new();
	let mut e = th_expr(expr);
	let r = infer_and_check_expression(&mut ctx, &mut e);
	println!("result: {:?}", r);
	r
}

#[test]
fn test_unary_op()
{
	assert!(type_check("-true").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("-5").is_ok());
	assert!(type_check("!0").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("!false").is_ok());
}


#[test]
fn test_wrong_type_bin_op()
{
	assert!(type_check("4 + 6.3").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("4.4 % 6.3").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("4 - 7").is_ok());
	assert!(type_check("4.5 * 7.6").is_ok());
	assert!(type_check("true / 7").unwrap_err().error == ErrorCode::TypeError);

	assert!(type_check("4 >= 6.3").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("4 > 7").is_ok());
	assert!(type_check("4.5 > 7.6").is_ok());
	assert!(type_check("true <= 7").unwrap_err().error == ErrorCode::TypeError);

	assert!(type_check("(true && false) || true").is_ok());
	assert!(type_check("true && 5").unwrap_err().error == ErrorCode::TypeError);
}

#[test]
fn test_arrays()
{
	assert!(type_check("[4] ++ [5]").is_ok());
	assert!(type_check("4 ++ [5]").is_ok());
	assert!(type_check("[4] ++ 5").is_ok());
	assert!(type_check("4 ++ [5.7]").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("[4] ++ [5.7]").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("4 ++ []").is_ok());
	assert!(type_check("[4] ++ []").is_ok());
	assert!(type_check("[4, 5.7]").unwrap_err().error == ErrorCode::TypeError);
	assert!(type_check("[4, 5, 7]").is_ok());
	assert!(type_check("[4; 10]").is_ok());
	assert!(type_check("let v = [1, 2] in [x * 2 | x <- v]").is_ok());
	assert!(type_check("let v = [1.0, 2.0] in [x * 2 | x <- v]").unwrap_err().error == ErrorCode::TypeError);
}

#[test]
fn test_function()
{
	assert!(type_check("add(a: int, b: int) -> int = a + b").is_ok());
	assert!(type_check("add(a: int, b: int) -> int = 7.5").unwrap_err().error == ErrorCode::TypeError);
}

#[test]
fn test_match()
{
	assert!(type_check(r#"
foo(x: [int]) -> int =
	match x
		[] => 0,
		[head | tail] => head + foo(tail)
"#).is_ok());

	assert!(type_check(r#"
foo(x: [int]) -> int =
	match x
		7 => 0,
		[head | tail] => head + foo(tail)
"#).unwrap_err().error == ErrorCode::TypeError);

	assert!(type_check(r#"
foo(x: int) -> int =
	match x
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
"#).unwrap_err().error == ErrorCode::UnknownName);

	assert!(type_check(r#"
let x = [6, 7] in x + x
"#).unwrap_err().error == ErrorCode::TypeError);
}
