mod statements;
mod expressions;

use std::io::Read;

use lexer::{Lexer};
use ast::*;
use compileerror::*;

use self::statements::*;

pub use self::expressions::*;



pub fn parse_program<Input: Read>(input: &mut Input, name: &str) -> Result<Program, CompileError>
{
    let mut tq = try!(Lexer::new().read(input));
    let block = try!(parse_block(&mut tq, 0));
    Ok(Program::new(name, block))
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

#[cfg(test)]
fn type_struct(typ: &str, pos: Pos) -> Type
{
    Type::Struct(pos, typ.into())
}

#[cfg(test)]
fn type_primitve(typ: &str, pos: Pos) -> Type
{
    Type::Primitive(pos, typ.into())
}

#[cfg(test)]
fn arg(name: &str, typ: &str, tp: Pos, constant: bool, span: Span) -> Argument
{
    Argument::new(name.into(), type_primitve(typ, tp), constant, span)
}

#[cfg(test)]
fn sig(name: &str, ret: Type, args: Vec<Argument>) -> FunctionSignature
{
    FunctionSignature{
        name: name.into(),
        return_type: ret,
        args: args,
    }
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
        if let Expression::Number(_, ref n) = v.init {
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
        assert!(v.typ == Some(Type::Primitive(Pos::new(1, 8), "int".into())));
        assert!(!v.is_const);
        assert!(!v.public);
        if let Expression::Number(_, ref n) = v.init {
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
        if let Expression::Number(_, ref n) = v.init {
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
        if let Expression::Number(_, ref n) = v.init {
            assert!(n == "7");
        } else {
            assert!(false);
        }

        let v = &vars[1];
        assert!(v.name == "z");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        if let Expression::Number(_, ref n) = v.init {
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
        if let Expression::Number(_, ref n) = v.init {
            assert!(n == "7");
        } else {
            assert!(false);
        }

        let v = &vars[1];
        assert!(v.name == "z");
        assert!(v.typ.is_none());
        assert!(!v.is_const);
        if let Expression::Number(_, ref n) = v.init {
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

#[cfg(test)]
fn call(name: &str, args: Vec<Expression>, span: Span) -> Statement
{
    Statement::Expression(Expression::Call(Call::new(name.into(), args, span)))
}

#[cfg(test)]
fn str_lit(s: &str, span: Span) -> Expression
{
    Expression::StringLiteral(span, s.into())
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
        w.print(0);
        assert!(w.cond == number("1", span(2, 7, 2, 7)));
        assert!(w.block.statements.len() == 2);

        let s = &w.block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        let s = &w.block.statements[1];
        assert!(*s == call("print", vec![str_lit("something else", span(4, 11, 4, 26))], span(4, 5, 4, 27)));
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
        w.print(0);
        assert!(w.cond == number("1", span(2, 7, 2, 7)));
        assert!(w.block.statements.len() == 1);

        let s = &w.block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(2, 16, 2, 21))], span(2, 10, 2, 22)));
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
        assert!(w.cond == number("1", span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);
        assert!(w.else_part == ElsePart::Empty);

        let s = &w.if_block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));
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
        w.print(0);
        assert!(w.cond == number("1", span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);

        let s = &w.if_block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        if let ElsePart::Block(eb) = w.else_part
        {
            assert!(eb.statements.len() == 1);
            let s = &eb.statements[0];
            assert!(*s == call("print", vec![str_lit("false", span(5, 11, 5, 17))], span(5, 5, 5, 18)));
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
        w.print(0);
        assert!(w.cond == number("1", span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);

        let s = &w.if_block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        if let ElsePart::If(else_if) = w.else_part
        {
            else_if.print(1);
            assert!(else_if.cond == number("0", span(4, 9, 4, 9)));
            assert!(else_if.if_block.statements.len() == 1);
            let s = &else_if.if_block.statements[0];
            assert!(*s == call("print", vec![str_lit("nada", span(5, 11, 5, 16))], span(5, 5, 5, 17)));
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
        w.print(0);
        assert!(w.cond == number("1", span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);
        assert!(w.else_part == ElsePart::Empty);

        let s = &w.if_block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(2, 13, 2, 18))], span(2, 7, 2, 19)));
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
        assert!(w.expr == number("5", span(2, 8, 2, 8)));
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
        f.print(0);
        assert!(f.sig.name == "blaat");
        assert!(f.sig.args.is_empty());
        assert!(f.sig.return_type == Type::Void);
        assert!(!f.public);
        assert!(f.block.statements.len() == 2);
        let s = &f.block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        let s = &f.block.statements[1];
        assert!(*s == Statement::Return(
            Return::new(number("5", span(4, 12, 4, 12)), span(4, 5, 4, 12))
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
        f.print(0);
        assert!(f.sig.name == "blaat");
        assert!(f.sig.args.len() == 2);
        assert!(f.sig.args[0] == arg("x", "int", Pos::new(2, 19), false, span(2, 16, 2, 21)));
        assert!(f.sig.args[1] == arg("y", "int", Pos::new(2, 33), true, span(2, 30, 2, 35)));
        assert!(f.sig.return_type == type_primitve("int", Pos::new(2, 41)));
        assert!(f.block.statements.len() == 2);
        assert!(f.public);

        let s = &f.block.statements[0];
        assert!(*s == call("print", vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        let s = &f.block.statements[1];
        assert!(*s == Statement::Return(
            Return::new(number("5", span(4, 12, 4, 12)), span(4, 5, 4, 12))
        ));
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_external_func()
{
    let stmt = th_statement(r#"
extern func blaat()
    ""#);

    if let Statement::ExternalFunction(f) = stmt
    {
        f.print(0);
        assert!(f.sig.name == "blaat");
        assert!(f.sig.args.is_empty());
        assert!(f.sig.return_type == Type::Void);
        assert!(f.span == span(2, 1, 2, 19));
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
        s.print(0);
        assert!(s.name == "Blaat");
        assert!(s.functions.len() == 2);

        assert!(s.variables == vec![
            Variable::new("x".into(), None, false, false, number("7", span(3, 13, 3, 13)), span(3, 9, 3, 13)),
            Variable::new("y".into(), None, false, false, number("9", span(3, 20, 3, 20)), span(3, 16, 3, 20)),
            Variable::new("z".into(), None, true, true, number("99", span(4, 19, 4, 20)), span(4, 15, 4, 20)),
        ]);

        assert!(s.functions == vec![
            Function::new(
                sig("foo", Type::Void, vec![
                    Argument::new("self".into(), type_struct("Blaat", Pos::new(6, 9)), false, span(6, 18, 6, 18)),
                ]),
                true,
                Block::new(vec![
                    call("print", vec![str_lit("foo", span(7, 15, 7, 19))], span(7, 9, 7, 20))
                ]),
                span(6, 9, 7, 20),
            ),
            Function::new(
                sig("bar", Type::Void, vec![
                    Argument::new("self".into(), type_struct("Blaat", Pos::new(9, 5)), false, span(9, 14, 9, 14)),
                ]),
                false,
                Block::new(vec![
                    call("print", vec![str_lit("bar", span(10, 15, 10, 19))], span(10, 9, 10, 20))
                ]),
                span(9, 5, 10, 20),
            ),
        ]);
    }
    else
    {
        assert!(false);
    }
}


#[test]
fn test_union()
{
    let stmt = th_statement(r#"
pub union Blaat:
    Foo(x: int, y: int)
    Bar, Baz

    pub func foo(self):
        print("foo")
    ""#);

    if let Statement::Union(u) = stmt
    {
        u.print(0);
        assert!(u.name == "Blaat");
        assert!(u.public);

        assert!(u.cases == vec![
            UnionCase{
                name: "Foo".into(),
                vars: vec![
                    arg("x", "int", Pos::new(3, 12), false, span(3, 9, 3, 14)),
                    arg("y", "int", Pos::new(3, 20), false, span(3, 17, 3, 22)),
                ],
                span: span(3, 5, 3, 23),
            },
            UnionCase::new("Bar".into(), span(4, 5, 4, 8)),
            UnionCase::new("Baz".into(), span(4, 10, 4, 12)),
        ]);

        assert!(u.functions == vec![
            Function::new(
                sig("foo",Type::Void, vec![
                    Argument::new("self".into(), Type::Union(Pos::new(6, 5), "Blaat".into()), false, span(6, 18, 6, 18)),
                ]),
                true,
                Block::new(vec![
                    call("print", vec![str_lit("foo", span(7, 15, 7, 19))], span(7, 9, 7, 20))
                ]),
                span(6, 9, 7, 20),
            ),
        ]);
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_single_line_union()
{
    let stmt = th_statement(r#"
union Blaat: Bar, Baz, Foo
    ""#);

    if let Statement::Union(u) = stmt
    {
        u.print(0);
        assert!(u.name == "Blaat");
        assert!(!u.public);

        assert!(u.cases == vec![
            UnionCase::new("Bar".into(), span(2, 14, 2, 17)),
            UnionCase::new("Baz".into(), span(2, 19, 2, 22)),
            UnionCase::new("Foo".into(), span(2, 24, 2, 26)),
        ]);
    }
    else
    {
        assert!(false);
    }
}



#[test]
fn test_match()
{
    let stmt = th_statement(r#"
match bla:
    Foo(x, y): print("foo")
    Bar:
        print("bar")
    Baz:
        print("baz")
    ""#);

    if let Statement::Match(m) = stmt
    {
        m.print(0);
        assert!(m.expr == Expression::NameRef(span(2, 7, 2, 9), "bla".into()));
        assert!(m.cases == vec![
            MatchCase::new(
                "Foo".into(),
                vec!["x".into(), "y".into()],
                Block::new(
                    vec![
                        call("print", vec![str_lit("foo", span(3, 22, 3, 26))], span(3, 16, 3, 27))
                    ]
                ),
                span(3, 5, 3, 27),
            ),
            MatchCase::new(
                "Bar".into(),
                Vec::new(),
                Block::new(
                    vec![
                        call("print", vec![str_lit("bar", span(5, 15, 5, 19))], span(5, 9, 5, 20))
                    ]
                ),
                span(4, 5, 5, 20),
            ),
            MatchCase::new(
                "Baz".into(),
                Vec::new(),
                Block::new(
                    vec![
                        call("print", vec![str_lit("baz", span(7, 15, 7, 19))], span(7, 9, 7, 20))
                    ]
                ),
                span(6, 5, 7, 20),
            ),
        ]);
    }
    else
    {
        assert!(false);
    }
}
