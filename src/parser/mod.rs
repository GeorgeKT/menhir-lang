mod statements;
mod expressions;

use std::io::Read;

use lexer::{Lexer};
use ast::*;
use compileerror::{Pos, CompileError, ErrorType};

use self::statements::*;

pub use self::expressions::{parse_function_call, parse_expression};

pub fn err<T: Sized>(pos: Pos, e: ErrorType) -> Result<T, CompileError>
{
    Err(CompileError::new(pos, e))
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
    if let Statement::Variable(vars) = stmt
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
    if let Statement::Variable(vars) = stmt
    {
        assert!(vars.len() == 1);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ == Some(Type::Primitive("int".into())));
        assert!(!v.is_const);
        assert!(!v.public);
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
    let stmt = th_statement("pub const x = 7");
    if let Statement::Variable(vars) = stmt
    {
        assert!(vars.len() == 1);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ.is_none());
        assert!(v.is_const);
        assert!(v.public);
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
    let stmt = th_statement("pub var x = 7, z = 888");

    if let Statement::Variable(vars) = stmt
    {
        assert!(vars.len() == 2);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        assert!(v.public);
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
    if let Statement::Variable(vars) = stmt
    {
        assert!(vars.len() == 2);
        let v = &vars[0];
        assert!(v.name == "x");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        assert!(!v.public);
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
fn test_while()
{
    let stmt = th_statement(r#"
while 1:
    print("true")
    print("something else")
    ""#);

    if let Statement::While(w) = stmt
    {
        assert!(w.cond == Expression::Number("1".into()));
        assert!(w.block.statements.len() == 2);

        let s = &w.block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));

        let s = &w.block.statements[1];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("something else".into())]
            )));
    }
    else
    {
        assert!(false);
    }
}


#[test]
fn test_while_single_line()
{
    let stmt = th_statement(r#"
while 1: print("true")
    ""#);

    if let Statement::While(w) = stmt
    {
        assert!(w.cond == Expression::Number("1".into()));
        assert!(w.block.statements.len() == 1);

        let s = &w.block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_if()
{
    let stmt = th_statement(r#"
if 1:
    print("true")
    ""#);

    if let Statement::If(w) = stmt
    {
        assert!(w.cond == Expression::Number("1".into()));
        assert!(w.if_block.statements.len() == 1);
        assert!(w.else_part == ElsePart::Empty);

        let s = &w.if_block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_if_else()
{
    let stmt = th_statement(r#"
if 1:
    print("true")
else:
    print("false")
    ""#);

    if let Statement::If(w) = stmt
    {
        assert!(w.cond == Expression::Number("1".into()));
        assert!(w.if_block.statements.len() == 1);

        let s = &w.if_block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));

        if let ElsePart::Block(eb) = w.else_part
        {
            assert!(eb.statements.len() == 1);
            let s = &eb.statements[0];
            assert!(*s == Statement::Call(
                Call::new(
                    "print".into(),
                    vec![Expression::StringLiteral("false".into())]
                )));
        }
        else
        {
            assert!(false);
        }
    }
    else
    {
        assert!(false);
    }
}


#[test]
fn test_else_if()
{
    let stmt = th_statement(r#"
if 1:
    print("true")
else if 0:
    print("nada")
    ""#);

    if let Statement::If(w) = stmt
    {
        assert!(w.cond == Expression::Number("1".into()));
        assert!(w.if_block.statements.len() == 1);

        let s = &w.if_block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));

        if let ElsePart::If(else_if) = w.else_part
        {
            assert!(else_if.cond == Expression::Number("0".into()));
            assert!(else_if.if_block.statements.len() == 1);
            let s = &else_if.if_block.statements[0];
            assert!(*s == Statement::Call(
                Call::new(
                    "print".into(),
                    vec![Expression::StringLiteral("nada".into())]
                )));

            assert!(else_if.else_part == ElsePart::Empty);
        }
        else
        {
            assert!(false);
        }
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_single_line_if()
{
    let stmt = th_statement(r#"
if 1: print("true")
    ""#);

    if let Statement::If(w) = stmt
    {
        assert!(w.cond == Expression::Number("1".into()));
        assert!(w.if_block.statements.len() == 1);
        assert!(w.else_part == ElsePart::Empty);

        let s = &w.if_block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_return()
{
    let stmt = th_statement(r#"
return 5
    ""#);

    if let Statement::Return(w) = stmt
    {
        assert!(w.expr == Expression::Number("5".into()));
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_func()
{
    let stmt = th_statement(r#"
func blaat():
    print("true")
    return 5
    ""#);

    if let Statement::Function(f) = stmt
    {
        assert!(f.name == "blaat");
        assert!(f.args.is_empty());
        assert!(f.return_type == Type::Void);
        assert!(!f.public);
        assert!(f.block.statements.len() == 2);
        let s = &f.block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));

        let s = &f.block.statements[1];
        assert!(*s == Statement::Return(
            Return::new(Expression::Number("5".into()))
        ));
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_func_with_args_and_return_type()
{
    let stmt = th_statement(r#"
pub func blaat(x: int, const y: int) -> int:
    print("true")
    return 5
    ""#);

    if let Statement::Function(f) = stmt
    {
        assert!(f.name == "blaat");
        assert!(f.args.len() == 2);
        assert!(f.args[0] == Argument::new("x".into(), Type::Primitive("int".into()), false));
        assert!(f.args[1] == Argument::new("y".into(), Type::Primitive("int".into()), true));
        assert!(f.return_type == Type::Primitive("int".into()));
        assert!(f.block.statements.len() == 2);
        assert!(f.public);

        let s = &f.block.statements[0];
        assert!(*s == Statement::Call(
            Call::new(
                "print".into(),
                vec![Expression::StringLiteral("true".into())]
            )));

        let s = &f.block.statements[1];
        assert!(*s == Statement::Return(
            Return::new(Expression::Number("5".into()))
        ));
    }
    else
    {
        assert!(false);
    }
}



#[test]
fn test_struct()
{
    let stmt = th_statement(r#"
pub struct Blaat:
    var x = 7, y = 9
    pub const z = 99

    pub func foo(self):
        print("foo")

    func bar(self):
        print("bar")
    ""#);

    if let Statement::Struct(s) = stmt
    {
        println!("Struct: {:?}", s);
        assert!(s.name == "Blaat");
        assert!(s.functions.len() == 2);

        assert!(s.variables == vec![
            Variable::new("x".into(), None, false, false, Expression::Number("7".into())),
            Variable::new("y".into(), None, false, false, Expression::Number("9".into())),
            Variable::new("z".into(), None, true, true, Expression::Number("99".into())),
        ]);

        assert!(s.functions == vec![
            Function::new(
                "foo".into(),
                Type::Void,
                vec![
                    Argument::new("self".into(), Type::Struct("Blaat".into()), false),
                ],
                true,
                Block::new(vec![
                    Statement::Call(
                        Call::new(
                            "print".into(),
                            vec![Expression::StringLiteral("foo".into())]
                        )
                    )
                ])
            ),
            Function::new(
                "bar".into(),
                Type::Void,
                vec![
                    Argument::new("self".into(), Type::Struct("Blaat".into()), false),
                ],
                false,
                Block::new(vec![
                    Statement::Call(
                        Call::new(
                            "print".into(),
                            vec![Expression::StringLiteral("bar".into())]
                        )
                    )
                ])
            ),
        ]);
    }
    else
    {
        assert!(false);
    }
}
