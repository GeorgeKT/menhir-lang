use parser::th_expr;
use passes::typechecker::{TypeCheckerContext, infer_and_check_expression};

#[test]
fn test_wrong_type_unary_op()
{
	let mut ctx = TypeCheckerContext::new();
	let mut e = th_expr("-true");
	assert!(infer_and_check_expression(&mut ctx, &mut e).is_err());
}

#[test]
fn test_wrong_type_unary_op2()
{
	let mut ctx = TypeCheckerContext::new();
	let mut e = th_expr("!0");
	assert!(infer_and_check_expression(&mut ctx, &mut e).is_err());
}

#[test]
fn test_wrong_type_bin_op()
{
	let mut ctx = TypeCheckerContext::new();
	let mut e = th_expr("a ");
	assert!(infer_and_check_expression(&mut ctx, &mut e).is_err());
}