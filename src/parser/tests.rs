use std::io::Cursor;
use ast::{Expression, NameRef, TreePrinter, Function, Argument, Call, Type,
    sig, unary_op, bin_op, array_lit, match_expression, match_case,
    array_pattern, lambda};
use compileerror::{Span, span};
use parser::{Lexer, Operator, parse_expression};

pub fn th_expr(data: &str) -> Expression
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new().read(&mut cursor).expect("Lexing failed");
    let e = parse_expression(&mut tq).expect("Parsing failed");
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
        typ: Type::Unknown,
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
    assert!(th_expr("true") == Expression::BoolLiteral(span(1, 1, 1, 4), true));
    assert!(th_expr("false") == Expression::BoolLiteral(span(1, 1, 1, 5), false));
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
    let e = th_expr("b + c(6)");
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b", span(1, 1, 1, 1)),
        Expression::Call(Call::new(
            name_ref2("c", span(1, 5, 1, 5)),
            vec![number(6, span(1, 7, 1, 7))],
            span(1, 5, 1, 8)
        )),
        span(1, 1, 1, 8),
    ));
}

#[test]
fn test_precedence_9()
{
    let e = th_expr("c(6) + b");
    assert!(e == bin_op(
        Operator::Add,
        Expression::Call(Call::new(
            name_ref2("c", span(1, 1, 1, 1)),
            vec![number(6, span(1, 3, 1, 3))],
            span(1, 1, 1, 4)
        )),
        name_ref("b", span(1, 8, 1, 8)),
        span(1, 1, 1, 8),
    ));
}

#[test]
fn test_namespaced_call()
{
    let e = th_expr("foo::bar(7)");
    assert!(e == Expression::Call(
        Call::new(
            name_ref2("foo::bar", span(1, 1, 1, 8)),
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
fn test_array_pattern()
{
    let e = th_expr("[head | tail]");
    assert!(e == array_pattern("head", "tail", span(1, 1, 1, 13)));
}

#[test]
fn test_array_concat()
{
    let e = th_expr("a ++ [1, 2]");
    assert!(e == bin_op(
        Operator::Concat,
        name_ref("a", span(1, 1, 1, 1)),
        array_lit(
            vec![
                number(1, span(1, 7, 1, 7)),
                number(2, span(1, 10, 1, 10)),
            ],
            span(1, 6, 1, 11)
        ),
        span(1, 1, 1, 11))
    );
}


fn arg(name: &str, typ: Type, span: Span) -> Argument
{
    Argument::new(name.into(), typ, span)
}

#[test]
fn test_function_with_args()
{
    let e = th_expr("foo(a: int, b: int) -> int = 7");
    assert!(e == Expression::Function(Function::new(
        sig(
            "foo",
            Type::Int,
            vec![
                arg("a", Type::Int, span(1, 5, 1, 10)),
                arg("b", Type::Int, span(1, 13, 1, 18)),
            ],
            span(1, 1, 1, 26)
        ),
        true,
        number(7, span(1, 30, 1, 30)),
        span(1, 1, 1, 30))
    ))
}

#[test]
fn test_function_with_no_args()
{
    let e = th_expr("foo() -> int = 7");
    assert!(e == Expression::Function(Function::new(
        sig(
            "foo",
            Type::Int,
            Vec::new(),
            span(1, 1, 1, 12)
        ),
        true,
        number(7, span(1, 16, 1, 16)),
        span(1, 1, 1, 16))
    ))
}

#[test]
fn test_function_with_no_return_type()
{
    let e = th_expr("foo() = 7");
    assert!(e == Expression::Function(Function::new(
        sig(
            "foo",
            Type::Void,
            Vec::new(),
            span(1, 1, 1, 5)
        ),
        true,
        number(7, span(1, 9, 1, 9)),
        span(1, 1, 1, 9))
    ))
}

#[test]
fn test_function_with_func_type()
{
    let e = th_expr("foo(a: (int, int) -> int) = 7");
    assert!(e == Expression::Function(Function::new(
        sig(
            "foo",
            Type::Void,
            vec![
                Argument::new(
                    "a".into(),
                    Type::Func(
                        vec![
                            Type::Int,
                            Type::Int,
                        ],
                        Box::new(Type::Int),
                    ),
                    span(1, 5, 1, 24)
                ),
            ],
            span(1, 1, 1, 25)
        ),
        true,
        number(7, span(1, 29, 1, 29)),
        span(1, 1, 1, 29))
    ))
}


#[test]
fn test_lambda()
{
    let e = th_expr("@(a, b) -> a + b");
    assert!(e == Expression::Lambda(lambda(
        vec![
            Argument::new("a".into(), Type::Unknown, span(1, 3, 1, 3)),
            Argument::new("b".into(), Type::Unknown, span(1, 6, 1, 6)),
        ],
        bin_op(
            Operator::Add,
            name_ref("a", span(1, 12, 1, 12)),
            name_ref("b", span(1, 16, 1, 16)),
            span(1, 12, 1, 16)
        ),
        span(1, 1, 1, 16)
    )))
}

#[test]
fn test_match()
{
    let e = th_expr(r#"
match a
    0 => 1,
    1 => 2,
    2 => 3
"#);
    assert!(e == Expression::Match(match_expression(
        name_ref("a", span(2, 7, 2, 7)),
        vec![
            match_case(number(0, span(3, 5, 3, 5)), number(1, span(3, 10, 3, 10))),
            match_case(number(1, span(4, 5, 4, 5)), number(2, span(4, 10, 4, 10))),
            match_case(number(2, span(5, 5, 5, 5)), number(3, span(5, 10, 5, 10))),
        ],
        span(2, 1, 5, 10))
    ))
}
