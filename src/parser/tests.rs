use std::io::Cursor;
use ast::*;
use compileerror::*;
use parser::expressions::*;
use parser::lexer::*;
use parser::tokens::*;
use parser::statements::*;
use parser::ParseMode;

fn th_expr(data: &str) -> Expression
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new().read(&mut cursor).expect("Lexing failed");
    let lvl = tq.expect_indent().expect("Missing indentation");
    let e = parse_expression(&mut tq, lvl).expect("Parsing failed");
    println!("AST dump:");
    e.print(0);
    e
}


pub fn number(v: u64, span: Span) -> Expression
{
    Expression::IntLiteral(span, v)
}


fn enclosed(span: Span, left: Expression) -> Expression
{
    Expression::Enclosed(span, Box::new(left))
}

pub fn name_ref(name: &str, span: Span) -> Expression
{
    Expression::NameRef(NameRef{
        name: name.into(),
        span: span,
    })
}

pub fn name_ref2(name: &str, span: Span) -> NameRef
{
    NameRef::new(name.into(), span)
}

#[test]
fn test_basic_expressions()
{
    assert!(th_expr("1000") == number(1000, span(1, 1, 1, 4)));
    assert!(th_expr("id") == name_ref("id", span(1, 1, 1, 2)));
    assert!(th_expr("-1000") == unary_op(Operator::Sub, number(1000, span(1, 2, 1, 5)), span(1, 1, 1, 5)));
    assert!(th_expr("!id") == unary_op(Operator::Not, name_ref("id", span(1, 2, 1, 3)), span(1, 1, 1, 3)));
    assert!(th_expr("++id") == unary_op(Operator::Increment, name_ref("id", span(1, 3, 1, 4)), span(1, 1, 1, 4)));
    assert!(th_expr("--id") == unary_op(Operator::Decrement, name_ref("id", span(1, 3, 1, 4)), span(1, 1, 1, 4)));
}

#[test]
fn test_binary_ops()
{
    let ops = [
        (Operator::Add, "+"),
        (Operator::Sub, "-"),
        (Operator::Div, "/"),
        (Operator::Mul, "*"),
        (Operator::Mod, "%"),
        (Operator::Equals, "=="),
        (Operator::NotEquals, "!="),
        (Operator::GreaterThan, ">"),
        (Operator::GreaterThanEquals, ">="),
        (Operator::LessThan, "<"),
        (Operator::LessThanEquals, "<="),
        (Operator::Or, "||"),
        (Operator::And, "&&"),
    ];

    for &(op, op_txt) in &ops
    {
        let e_txt = format!("a {} b", op_txt);
        let e = th_expr(&e_txt);
        assert!(e == bin_op(
            op,
            name_ref("a", span(1, 1, 1, 1)),
            name_ref("b", span(1, e_txt.len(), 1, e_txt.len())),
            span(1, 1, 1, e_txt.len())
        ));
    }
}

#[test]
fn test_precedence()
{
    let e = th_expr("a + b * c");
    assert!(e == bin_op(
        Operator::Add,
        name_ref("a", span(1, 1, 1, 1)),
        bin_op(Operator::Mul, name_ref("b", span(1, 5, 1, 5)), name_ref("c", span(1, 9, 1, 9)), span(1, 5, 1, 9)),
        span(1, 1, 1, 9),
    ));
}


#[test]
fn test_precedence_2()
{
    let e = th_expr("a * b + c ");
    assert!(e == bin_op(
        Operator::Add,
        bin_op(Operator::Mul, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 5, 1, 5)), span(1, 1, 1, 5)),
        name_ref("c", span(1, 9, 1, 9)),
        span(1, 1, 1, 9),
    ));
}

#[test]
fn test_precedence_3()
{
    let e = th_expr("a * b + c / d ");
    assert!(e == bin_op(
        Operator::Add,
        bin_op(Operator::Mul, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 5, 1, 5)), span(1, 1, 1, 5)),
        bin_op(Operator::Div, name_ref("c", span(1, 9, 1, 9)), name_ref("d", span(1, 13, 1, 13)), span(1, 9, 1, 13)),
        span(1, 1, 1, 13),
    ));
}

#[test]
fn test_precedence_4()
{
    let e = th_expr("a && b || c && d ");
    assert!(e == bin_op(
        Operator::Or,
        bin_op(Operator::And, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 6, 1, 6)), span(1, 1, 1, 6)),
        bin_op(Operator::And, name_ref("c", span(1, 11, 1, 11)), name_ref("d", span(1, 16, 1, 16)), span(1, 11, 1, 16)),
        span(1, 1, 1, 16),
    ));
}

#[test]
fn test_precedence_5()
{
    let e = th_expr("a >= b && c < d");
    assert!(e == bin_op(
        Operator::And,
        bin_op(Operator::GreaterThanEquals, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 6, 1, 6)), span(1, 1, 1, 6)),
        bin_op(Operator::LessThan, name_ref("c", span(1, 11, 1, 11)), name_ref("d", span(1, 15, 1, 15)), span(1, 11, 1, 15)),
        span(1, 1, 1, 15),
    ));
}

#[test]
fn test_precedence_6()
{
    let e = th_expr("a * (b + c)");
    assert!(e == bin_op(
        Operator::Mul,
        name_ref("a", span(1, 1, 1, 1)),
        enclosed(
            span(1, 5, 1, 11),
            bin_op(Operator::Add, name_ref("b", span(1, 6, 1, 6)), name_ref("c", span(1, 10, 1, 10)), span(1, 6, 1, 10))),
        span(1, 1, 1, 11),
    ));
}

#[test]
fn test_precedence_7()
{
    let e = th_expr("b + -c");
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b", span(1, 1, 1, 1)),
        unary_op(Operator::Sub, name_ref("c", span(1, 6, 1, 6)), span(1, 5, 1, 6)),
        span(1, 1, 1, 6),
    ));
}

#[test]
fn test_precedence_8()
{
    let e = th_expr("b + c++");
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b", span(1, 1, 1, 1)),
        pf_unary_op(
            Operator::Increment,
            name_ref("c", span(1, 5, 1, 5)),
            span(1, 5, 1, 7),
        ),
        span(1, 1, 1, 7),
    ));
}

#[test]
fn test_precedence_9()
{
    let e = th_expr("(b + c)++");
    assert!(e == pf_unary_op(
        Operator::Increment,
        enclosed(
            span(1, 1, 1, 7),
            bin_op(
                Operator::Add,
                name_ref("b", span(1, 2, 1, 2)),
                name_ref("c", span(1, 6, 1, 6)),
                span(1, 2, 1, 6),
            )
        ),
        span(1, 1, 1, 9),
    ));
}

#[test]
fn test_precedence_10()
{
    let e = th_expr("b + c(6)");
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b", span(1, 1, 1, 1)),
        Expression::Call(Call::new(
            name_ref("c", span(1, 5, 1, 5)),
            vec![number(6, span(1, 7, 1, 7))],
            span(1, 5, 1, 8)
        )),
        span(1, 1, 1, 8),
    ));
}

#[test]
fn test_precedence_11()
{
    let e = th_expr("c(6) + b");
    assert!(e == bin_op(
        Operator::Add,
        Expression::Call(Call::new(
            name_ref("c", span(1, 1, 1, 1)),
            vec![number(6, span(1, 3, 1, 3))],
            span(1, 1, 1, 4)
        )),
        name_ref("b", span(1, 8, 1, 8)),
        span(1, 1, 1, 8),
    ));
}

#[test]
fn test_assign()
{
    let e = th_expr("a = 6 + 9");
    assert!(e == assignment(
        Operator::Assign,
        name_ref("a", span(1, 1, 1, 1)),
        bin_op(Operator::Add,
            number(6, span(1, 5, 1, 5)),
            number(9, span(1, 9, 1, 9)),
            span(1, 5, 1, 9),
        ),
        span(1, 1, 1, 9),
    ));
}

#[test]
fn test_object_construction()
{
    let e = th_expr("Foo{7, 8}");
    assert!(e == object_construction(
        NameRef::new("Foo".into(), span(1, 1, 1, 3)),
        vec![
            number(7, span(1, 5, 1, 5)),
            number(8, span(1, 8, 1, 8))
        ],
        span(1, 1, 1, 9),
    ));
}

#[test]
fn test_member_accesss()
{
    let e = th_expr("b.d + a.c(6)");
    assert!(e == bin_op(
        Operator::Add,
        member_access(
            name_ref2("b", span(1, 1, 1, 1)),
            Member::Var(NameRef::new("d".into(), span(1, 3, 1, 3))),
            span(1, 1, 1, 3)
        ),
        member_access(
            name_ref2("a", span(1, 7, 1, 7)),
            Member::Call(Call::new(
                name_ref("c", span(1, 9, 1, 9)),
                vec![number(6, span(1, 11, 1, 11))],
                span(1, 9, 1, 12),
            )),
            span(1, 7, 1, 12)
        ),
        span(1, 1, 1, 12),
    ));
}


#[test]
fn test_nested_member_accesss()
{
    let e = th_expr("a.b.c.d");

    let c = MemberAccess::new(name_ref("c", span(1, 5, 1, 5)), Member::Var(NameRef::new("d".into(), span(1, 7, 1, 7))), span(1, 5, 1, 7));
    let b = MemberAccess::new(name_ref("b", span(1, 3, 1, 3)), Member::Nested(Box::new(c)), span(1, 3, 1, 7));
    let a = MemberAccess::new(name_ref("a", span(1, 1, 1, 1)), Member::Nested(Box::new(b)), span(1, 1, 1, 7));
    assert!(e == Expression::MemberAccess(a));
}

#[test]
fn test_member_assignment()
{
    let e = th_expr("b.d = 6");
    let ma = MemberAccess::new(name_ref("b", span(1, 1, 1, 1)), Member::Var(NameRef::new("d".into(), span(1, 3, 1, 3))), span(1, 1, 1, 3));
    assert!(e == assignment(
        Operator::Assign,
        Expression::MemberAccess(ma),
        number(6, span(1, 7, 1, 7)),
        span(1, 1, 1, 7)));
}

#[test]
fn test_namespacing()
{
    let e = th_expr("foo::bar = 7");
    assert!(e == assignment(
        Operator::Assign,
        name_ref("foo::bar", span(1, 1, 1, 8)),
        number(7, span(1, 12, 1, 12)),
        span(1, 1, 1, 12)));
}

#[test]
fn test_namespaced_call()
{
    let e = th_expr("foo::bar(7)");
    assert!(e == Expression::Call(
        Call::new(
            name_ref("foo::bar", span(1, 1, 1, 8)),
            vec![number(7, span(1, 10, 1, 10))],
            span(1, 1, 1, 11),
        )));
}

#[test]
fn test_array_literal()
{
    let e = th_expr("[1, 2, 3]");
    assert!(e == array_lit(
        vec![
            number(1, span(1, 2, 1, 2)),
            number(2, span(1, 5, 1, 5)),
            number(3, span(1, 8, 1, 8)),
        ],
        span(1, 1, 1, 9)));

}

#[test]
fn test_multi_line_array_literal()
{
    let e = th_expr(r#"[
    1,
    2,
    3
]"#);
    assert!(e == array_lit(
        vec![
            number(1, span(2, 5, 2, 5)),
            number(2, span(3, 5, 3, 5)),
            number(3, span(4, 5, 4, 5)),
        ],
        span(1, 1, 5, 1)));
}

#[test]
fn test_index_op()
{
    let e = th_expr("foo[5]");
    assert!(e == index_op(
        name_ref("foo", span(1, 1, 1, 3)),
        number(5, span(1, 5, 1, 5)),
        span(1, 1, 1, 6)));
}

#[test]
fn test_nested_index_op()
{
    let e = th_expr("foo[5][5]");
    assert!(e == index_op(
        index_op(
            name_ref("foo", span(1, 1, 1, 3)),
            number(5, span(1, 5, 1, 5)),
            span(1, 1, 1, 6)),
        number(5, span(1, 8, 1, 8)),
        span(1, 1, 1, 9)
    ));
}


fn th_statement(data: &str) -> Statement
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new().read(&mut cursor).expect("Lexing failed");
    let lvl = tq.expect_indent().expect("Missing indentation");
    parse_statement(&mut tq, lvl, ParseMode::Block).expect("Parsing failed")
}


fn type_complex(typ: &str) -> Type
{
    Type::ptr(Type::Complex(typ.into()))
}


fn type_trait(typ: &str) -> Type
{
    Type::ptr(Type::Trait(typ.into()))
}


fn type_primitve(typ: &str) -> Type
{
    Type::Primitive(typ.into())
}


fn arg(name: &str, typ: &str, constant: bool, span: Span) -> Argument
{
    Argument::new(name.into(), type_primitve(typ), constant, span)
}


fn sig(name: &str, ret: Type, args: Vec<Argument>, span: Span) -> FunctionSignature
{
    FunctionSignature{
        name: name.into(),
        return_type: ret,
        args: args,
        generic_args: Vec::new(),
        span: span,
    }
}


fn generic_sig(name: &str, ret: Type, args: Vec<Argument>, generic_args: Vec<GenericArgument>, span: Span) -> FunctionSignature
{
    FunctionSignature{
        name: name.into(),
        return_type: ret,
        args: args,
        generic_args: generic_args,
        span: span,
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
        assert!(v.typ == Type::Unknown);
        assert!(!v.is_const);
        assert!(v.init == number(7, span(1, 9, 1, 9)));
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
        assert!(v.typ == Type::Primitive("int".into()));
        assert!(!v.is_const);
        assert!(!v.public);
        assert!(v.init == number(7, span(1, 14, 1, 14)));
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
        assert!(v.typ == Type::Unknown);
        assert!(v.is_const);
        assert!(v.public);
        assert!(v.init == number(7, span(1, 15, 1, 15)));
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
        assert!(v.typ == Type::Unknown);
        assert!(!v.is_const);
        assert!(v.public);
        assert!(v.init == number(7, span(1, 13, 1, 13)));

        let v = &vars[1];
        assert!(v.name == "z");
        assert!(v.typ == Type::Unknown);
        assert!(!v.is_const);
        assert!(v.init == number(888, span(1, 20, 1, 22)));
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
        v.print(0);
        assert!(v.name == "x");
        assert!(v.typ == Type::Unknown);
        assert!(!v.is_const);
        assert!(!v.public);
        assert!(v.init == number(7, span(3, 9, 3, 9)));

        let v = &vars[1];
        v.print(0);
        assert!(v.name == "z");
        assert!(v.typ == Type::Unknown);
        assert!(!v.is_const);
        assert!(v.init == number(888, span(4, 9, 4, 11)));
    }
    else
    {
        assert!(false);
    }
}


#[test]
fn test_var_with_pointer_type()
{
    let stmt = th_statement("var x: *char = 0");
    if let Statement::Variable(vars) = stmt
    {
        assert!(vars.len() == 1);
        let v = &vars[0];
        v.print(0);
        assert!(v.name == "x");
        assert!(v.typ == Type::Pointer(Box::new(Type::Primitive("char".into()))));
        assert!(!v.is_const);
        assert!(!v.public);
        assert!(v.init == number(0, span(1, 16, 1, 16)));
    }
    else
    {
        assert!(false);
    }
}

#[test]
fn test_var_with_array()
{
    let stmt = th_statement("var x: [int, 2] = [4, 4]");
    if let Statement::Variable(vars) = stmt
    {
        assert!(vars.len() == 1);
        let v = &vars[0];
        v.print(0);
        assert!(v.name == "x");
        assert!(v.typ == Type::Array(Box::new(Type::Primitive("int".into())), 2));
        assert!(!v.is_const);
        assert!(!v.public);
        assert!(v.init == array_lit(vec![
                number(4, span(1, 20, 1, 20)),
                number(4, span(1, 23, 1, 23)),
            ],
            span(1, 19, 1, 24)));
    }
    else
    {
        assert!(false);
    }
}




fn call(name: Expression, args: Vec<Expression>, span: Span) -> Statement
{
    Statement::Expression(Expression::Call(
        Call::new(name, args, span)))
}


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
        assert!(w.cond == number(1, span(2, 7, 2, 7)));
        assert!(w.block.statements.len() == 2);

        let s = &w.block.statements[0];
        assert!(*s == call(name_ref("print", span(3, 5, 3, 9)), vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        let s = &w.block.statements[1];
        assert!(*s == call(name_ref("print", span(4, 5, 4, 9)), vec![str_lit("something else", span(4, 11, 4, 26))], span(4, 5, 4, 27)));
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
        assert!(w.cond == number(1, span(2, 7, 2, 7)));
        assert!(w.block.statements.len() == 1);

        let s = &w.block.statements[0];
        assert!(*s == call(name_ref("print", span(2, 10, 2, 14)), vec![str_lit("true", span(2, 16, 2, 21))], span(2, 10, 2, 22)));
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
        assert!(w.cond == number(1, span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);
        assert!(w.else_part == ElsePart::Empty);

        let s = &w.if_block.statements[0];
        assert!(*s == call(name_ref("print", span(3, 5, 3, 9)), vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));
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
        assert!(w.cond == number(1, span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);

        let s = &w.if_block.statements[0];
        assert!(*s == call(name_ref("print", span(3, 5, 3, 9)), vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        if let ElsePart::Block(eb) = w.else_part
        {
            assert!(eb.statements.len() == 1);
            let s = &eb.statements[0];
            assert!(*s == call(name_ref("print", span(5, 5, 5, 9)), vec![str_lit("false", span(5, 11, 5, 17))], span(5, 5, 5, 18)));
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
        assert!(w.cond == number(1, span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);

        let s = &w.if_block.statements[0];
        assert!(*s == call(name_ref("print", span(3, 5, 3, 9)), vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        if let ElsePart::If(else_if) = w.else_part
        {
            else_if.print(1);
            assert!(else_if.cond == number(0, span(4, 9, 4, 9)));
            assert!(else_if.if_block.statements.len() == 1);
            let s = &else_if.if_block.statements[0];
            assert!(*s == call(name_ref("print", span(5, 5, 5, 9)), vec![str_lit("nada", span(5, 11, 5, 16))], span(5, 5, 5, 17)));
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
        assert!(w.cond == number(1, span(2, 4, 2, 4)));
        assert!(w.if_block.statements.len() == 1);
        assert!(w.else_part == ElsePart::Empty);

        let s = &w.if_block.statements[0];
        assert!(*s == call(name_ref("print", span(2, 7, 2, 11)), vec![str_lit("true", span(2, 13, 2, 18))], span(2, 7, 2, 19)));
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
        assert!(w.expr == number(5, span(2, 8, 2, 8)));
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
        assert!(*s == call(name_ref("print", span(3, 5, 3, 9)), vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        let s = &f.block.statements[1];
        assert!(*s == Statement::Return(
            Return::new(number(5, span(4, 12, 4, 12)), span(4, 5, 4, 12))
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
pub func blaat(var x: int, const y: int) -> int:
    print("true")
    return 5
    ""#);

    if let Statement::Function(f) = stmt
    {
        f.print(0);
        assert!(f.sig.name == "blaat");
        assert!(f.sig.args.len() == 2);
        assert!(f.sig.args[0] == arg("x", "int", false, span(2, 20, 2, 25)));
        assert!(f.sig.args[1] == arg("y", "int", true, span(2, 34, 2, 39)));
        assert!(f.sig.return_type == type_primitve("int"));
        assert!(f.block.statements.len() == 2);
        assert!(f.public);

        let s = &f.block.statements[0];
        assert!(*s == call(name_ref("print", span(3, 5, 3, 9)), vec![str_lit("true", span(3, 11, 3, 16))], span(3, 5, 3, 17)));

        let s = &f.block.statements[1];
        assert!(*s == Statement::Return(
            Return::new(number(5, span(4, 12, 4, 12)), span(4, 5, 4, 12))
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
pub struct Blaat impl Foo, Bar:
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
        assert!(s.impls == vec![
            Type::Trait("Foo".into()),
            Type::Trait("Bar".into()),
        ]);

        assert!(s.variables == vec![
            Variable::new("x".into(), Type::Unknown, false, false, number(7, span(3, 13, 3, 13)), span(3, 9, 3, 13)),
            Variable::new("y".into(), Type::Unknown, false, false, number(9, span(3, 20, 3, 20)), span(3, 16, 3, 20)),
            Variable::new("z".into(), Type::Unknown, true, true, number(99, span(4, 19, 4, 20)), span(4, 15, 4, 20)),
        ]);

        assert!(s.functions == vec![
            Function::new(
                sig(
                    "Blaat::foo",
                    Type::Void,
                    vec![
                        Argument::new("self".into(), type_complex("Blaat"), true, span(6, 18, 6, 21)),
                    ],
                    span(6, 9, 6, 22)
                ),
                true,
                Block::new(vec![
                    call(name_ref("print", span(7, 9, 7, 13)), vec![str_lit("foo", span(7, 15, 7, 19))], span(7, 9, 7, 20))
                ]),
                span(6, 9, 7, 20),
            ),
            Function::new(
                sig(
                    "Blaat::bar",
                    Type::Void,
                    vec![
                        Argument::new("self".into(), type_complex("Blaat"), true, span(9, 14, 9, 17)),
                    ],
                    span(9, 5, 9, 18),
                ),
                false,
                Block::new(vec![
                    call(name_ref("print", span(10, 9, 10, 13)), vec![str_lit("bar", span(10, 15, 10, 19))], span(10, 9, 10, 20))
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
                    arg("x", "int", false, span(3, 9, 3, 14)),
                    arg("y", "int", false, span(3, 17, 3, 22)),
                ],
                span: span(3, 5, 3, 23),
            },
            UnionCase::new("Bar".into(), span(4, 5, 4, 8)),
            UnionCase::new("Baz".into(), span(4, 10, 4, 12)),
        ]);

        let foo = Function::new(
            sig(
                "Blaat::foo",
                Type::Void,
                vec![
                    Argument::new("self".into(), type_complex("Blaat"), true, span(6, 18, 6, 21)),
                ],
                span(6, 9, 6, 22)
            ),
            true,
            Block::new(vec![
                call(name_ref("print", span(7, 9, 7, 13)), vec![str_lit("foo", span(7, 15, 7, 19))], span(7, 9, 7, 20))
            ]),
            span(6, 9, 7, 20),
        );
        foo.print(0);
        assert!(u.functions == vec![foo]);
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
        assert!(m.expr == name_ref("bla", span(2, 7, 2, 9)));
        assert!(m.cases == vec![
            MatchCase::new(
                "Foo".into(),
                vec!["x".into(), "y".into()],
                Block::new(
                    vec![
                        call(name_ref("print", span(3, 16, 3, 20)), vec![str_lit("foo", span(3, 22, 3, 26))], span(3, 16, 3, 27))
                    ]
                ),
                span(3, 5, 3, 27),
            ),
            MatchCase::new(
                "Bar".into(),
                Vec::new(),
                Block::new(
                    vec![
                        call(name_ref("print", span(5, 9, 5, 13)), vec![str_lit("bar", span(5, 15, 5, 19))], span(5, 9, 5, 20))
                    ]
                ),
                span(4, 5, 5, 20),
            ),
            MatchCase::new(
                "Baz".into(),
                Vec::new(),
                Block::new(
                    vec![
                        call(name_ref("print", span(7, 9, 7, 13)), vec![str_lit("baz", span(7, 15, 7, 19))], span(7, 9, 7, 20))
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

#[test]
fn test_import()
{
    let stmt = th_statement(r#"
import cobra::syscalls, foo
    ""#);

    if let Statement::Import(m) = stmt
    {
        m.print(0);
        assert!(m.modules == vec![
            ModuleName::new(vec!["cobra".into(), "syscalls".into()], span(2, 8, 2, 22)),
            ModuleName::new(vec!["foo".into()], span(2, 25, 2, 27)),
        ]);
        assert!(m.span == span(2, 1, 2, 27));
    }
    else
    {
        assert!(false);
    }
}


#[test]
fn test_trait()
{
    let stmt = th_statement(r#"
trait Bar:
    func bar(self) -> int
    func foo(self) -> double
    ""#);

    if let Statement::Trait(t) = stmt
    {
        t.print(0);
        let expected = Trait::new(
            "Bar".into(),
            false,
            vec![
                sig(
                    "Bar::bar",
                    Type::Primitive("int".into()),
                    vec![
                        Argument::new("self".into(), type_trait("Bar"), true, span(3, 14, 3, 17)),
                    ],
                    span(3, 5, 3, 25)
                ),
                sig(
                    "Bar::foo",
                    Type::Primitive("double".into()),
                    vec![
                        Argument::new("self".into(), type_trait("Bar"), true, span(4, 14, 4, 17)),
                    ],
                    span(4, 5, 4, 28)
                ),
            ],
            span(2, 1, 5, 5)
        );
        expected.print(0);
        assert!(t == expected);
    }
    else
    {
        assert!(false);
    }
}


#[test]
fn test_generic_arguments()
{
    let stmt = th_statement(r#"
func sum<T: Sum>(x: *T) -> int:
    return x.sum()
    ""#);

    if let Statement::Function(f) = stmt
    {
        f.print(0);
        let expected = Function::new(
            generic_sig(
                "sum",
                Type::Primitive("int".into()),
                vec![
                    Argument::new("x".into(), type_complex("T"), true, span(2, 18, 2, 22)),
                ],
                vec![
                    GenericArgument::new("T".into(), Type::Trait("Sum".into())),
                ],
                span(2, 1, 2, 30)
            ),
            false,
            Block::new(
                vec![
                    Statement::Return(
                        Return::new(
                            member_access(
                                name_ref2("x", span(3, 12, 3, 12)),
                                Member::Call(Call::new(
                                    name_ref("sum", span(3, 14, 3, 16)),
                                    Vec::new(),
                                    span(3, 14, 3, 18),
                                )),
                                span(3, 12, 3, 18)
                            ),
                            span(3, 5, 3, 18)
                        )
                    )
                ]
            ),
            span(2, 1, 4, 5)
        );
        expected.print(0);
        assert!(f == expected);
    }
    else
    {
        assert!(false);
    }
}
