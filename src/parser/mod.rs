mod statements;
mod expressions;

use std::io::Read;

use lexer::{Lexer};
use ast::*;
use compileerror::{Pos, CompileError};

use self::statements::*;

pub use self::expressions::parse_expression;

pub fn err<T: Sized>(pos: Pos, msg: String) -> Result<T, CompileError>
{
    Err(CompileError::new(pos, msg))
}

pub fn parse_program<Input: Read>(input: &mut Input) -> Result<Program, CompileError>
{
    let mut tq = try!(Lexer::new().read(input));
    let indent = try!(tq.expect_indent());
    let block = try!(parse_block(&mut tq, indent));
    Ok(Program::new(block))
}

#[cfg(test)]
use std::io::Cursor;

#[cfg(test)]
fn th_statement(data: &str) -> Statement
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new().read(&mut cursor).expect("Lexing failed");
    let lvl = tq.expect_indent().expect("Missing indentation");
    parse_statement(&mut tq, lvl).expect("Parsing failed")
}

#[test]
fn test_simple_var()
{
    let stmt = th_statement("var x = 7");
    if let Statement::VariableDeclaration(vars) = stmt
    {
        assert!(vars.len() == 1);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        if let Expression::Number(ref n) = v.init {
            assert!(n == "7");
        } else {
            assert!(false);
        }
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_simple_var_with_type()
{
    let stmt = th_statement("var x: int = 7");
    if let Statement::VariableDeclaration(vars) = stmt
    {
        assert!(vars.len() == 1);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ == Some(Type::Primitive("int".into())));
        assert!(!v.is_const);
        if let Expression::Number(ref n) = v.init {
            assert!(n == "7");
        } else {
            assert!(false);
        }
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_simple_const()
{
    let stmt = th_statement("const x = 7");
    if let Statement::VariableDeclaration(vars) = stmt
    {
        assert!(vars.len() == 1);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ.is_none());
        assert!(v.is_const);
        if let Expression::Number(ref n) = v.init {
            assert!(n == "7");
        } else {
            assert!(false);
        }
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_multiple_var()
{
    let stmt = th_statement("var x = 7, z = 888");

    if let Statement::VariableDeclaration(vars) = stmt
    {
        assert!(vars.len() == 2);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        if let Expression::Number(ref n) = v.init {
            assert!(n == "7");
        } else {
            assert!(false);
        }

        let v = &vars[1];
        assert!(v.name == "z");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        if let Expression::Number(ref n) = v.init {
            assert!(n == "888");
        } else {
            assert!(false);
        }
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_multiple_var_with_indentation()
{
    let stmt = th_statement(r#"
var
    x = 7
    z = 888"#);
    if let Statement::VariableDeclaration(vars) = stmt
    {
        assert!(vars.len() == 2);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        if let Expression::Number(ref n) = v.init {
            assert!(n == "7");
        } else {
            assert!(false);
        }

        let v = &vars[1];
        assert!(v.name == "z");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        if let Expression::Number(ref n) = v.init {
            assert!(n == "888");
        } else {
            assert!(false);
        }
    }
    else
    {
        assert!(false);
    }
}
