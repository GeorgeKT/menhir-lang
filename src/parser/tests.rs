use std::io::Cursor;
use ast::*;
use parser::*;
use span::{Pos, Span};

fn span(sl: usize, so: usize, el: usize, eo: usize) -> Span
{
    Span::new("", Pos::new(sl, so), Pos::new(el, eo))
}

pub fn th_expr(data: &str) -> Expression
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new("").read(&mut cursor).expect("Lexing failed");
    let e = parse_expression(&mut tq).expect("Parsing failed");
    println!("AST dump:");
    e.print(0);
    e
}

pub fn th_pattern(data: &str) -> Pattern
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new("").read(&mut cursor).expect("Lexing failed");
    let e = parse_pattern(&mut tq).expect("Parsing failed");
    println!("AST dump:");
    e.print(0);
    e
}

pub fn th_mod(data: &str) -> Module
{
    let mut cursor = Cursor::new(data);
    let parser_options = ParserOptions::default();
    let md = parse_module(&parser_options, &mut cursor, "test", "").expect("Parsing failed");
    println!("AST dump:");
    md.print(0);
    md
}


pub fn number(v: u64, span: Span) -> Expression
{
    Expression::Literal(Literal::Int(span, v))
}

pub fn number_pattern(v: u64, span: Span) -> Pattern
{
    Pattern::Literal(Literal::Int(span, v))
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
    assert!(th_expr("true") == Expression::Literal(Literal::Bool(span(1, 1, 1, 4), true)));
    assert!(th_expr("false") == Expression::Literal(Literal::Bool(span(1, 1, 1, 5), false)));
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
        bin_op(Operator::Add, name_ref("b", span(1, 6, 1, 6)), name_ref("c", span(1, 10, 1, 10)), span(1, 6, 1, 10)),
        span(1, 1, 1, 10),
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
fn test_precedence_10()
{
    let e = th_expr("4 + 5 * 7 - 9 / 3 + 5 % 4");

    let mul = bin_op(
        Operator::Mul,
        number(5, span(1, 5, 1, 5)),
        number(7, span(1, 9, 1, 9)),
        span(1, 5, 1, 9));

    let s1 = bin_op(Operator::Add, number(4, span(1, 1, 1, 1)), mul, span(1, 1, 1, 9));

    let div = bin_op(
        Operator::Div,
        number(9, span(1, 13, 1, 13)),
        number(3, span(1, 17, 1, 17)),
        span(1, 13, 1, 17));

    let s2 = bin_op(Operator::Sub, s1, div, span(1, 1, 1, 17));

    let rem = bin_op(
        Operator::Mod,
        number(5, span(1, 21, 1, 21)),
        number(4, span(1, 25, 1, 25)),
        span(1, 21, 1, 25));

    let s3 = bin_op(Operator::Add, s2, rem,span(1, 1, 1, 25));

    println!("s3:");
    s3.print(0);
    assert!(e == s3);
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
    assert!(e == Expression::Literal(array_lit(
        vec![
            number(1, span(1, 2, 1, 2)),
            number(2, span(1, 5, 1, 5)),
            number(3, span(1, 8, 1, 8)),
        ],
        span(1, 1, 1, 9))));

}

/*
#[test]
fn test_array_generator()
{
    let e = th_expr("[x * x | x <- v]");
    assert!(e == array_generator(
        bin_op(
            Operator::Mul,
            name_ref("x", span(1, 2, 1, 2)),
            name_ref("x", span(1, 6, 1, 6)),
            span(1, 2, 1, 6)
        ),
        "x".into(),
        name_ref("v", span(1, 15, 1, 15)),
        span(1, 1, 1, 16))
    );
}
*/

#[test]
fn test_array_pattern()
{
    let e = th_pattern("[head | tail]");
    assert!(e == array_pattern("head", "tail", span(1, 1, 1, 13)));
}

#[test]
fn test_array_concat()
{
    let e = th_expr("a + [1, 2]");
    assert!(e == bin_op(
        Operator::Add,
        name_ref("a", span(1, 1, 1, 1)),
        Expression::Literal(array_lit(
            vec![
                number(1, span(1, 6, 1, 6)),
                number(2, span(1, 9, 1, 9)),
            ],
            span(1, 5, 1, 10)
        )),
        span(1, 1, 1, 10))
    );
}


fn arg(name: &str, typ: Type, span: Span) -> Argument
{
    Argument::new(name.into(), typ, span)
}

#[test]
fn test_function_with_args()
{
    let md = th_mod("foo(a: int, b: int) -> int = 7");
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
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
    )
}

#[test]
fn test_function_with_no_args()
{
    let md = th_mod("foo() -> int = 7");
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            Type::Int,
            Vec::new(),
            span(1, 1, 1, 12)
        ),
        true,
        number(7, span(1, 16, 1, 16)),
        span(1, 1, 1, 16))
    )
}

#[test]
fn test_function_with_no_return_type()
{
    let md = th_mod("foo() = 7");
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            Type::Void,
            Vec::new(),
            span(1, 1, 1, 5)
        ),
        true,
        number(7, span(1, 9, 1, 9)),
        span(1, 1, 1, 9))
    )
}

#[test]
fn test_function_with_func_type()
{
    let md = th_mod("foo(a: (int, int) -> int) = 7");
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            Type::Void,
            vec![
                Argument::new(
                    "a".into(),
                    func_type(
                        vec![
                            Type::Int,
                            Type::Int,
                        ],
                        Type::Int,
                    ),
                    span(1, 5, 1, 24)
                ),
            ],
            span(1, 1, 1, 25)
        ),
        true,
        number(7, span(1, 29, 1, 29)),
        span(1, 1, 1, 29))
    )
}

#[test]
fn test_external_function()
{
    let md = th_mod("extern foo() -> int");
    assert!(*md.externals.get("foo").unwrap() == ExternalFunction::new(
        sig(
            "foo",
            Type::Int,
            Vec::new(),
            span(1, 8, 1, 19)
        ),
        span(1, 1, 1, 19))
    )
}

#[test]
fn test_lambda()
{
    let e = th_expr("@(a, b) -> a + b");
    assert!(e == lambda(
        vec![
            Argument::new("a".into(), Type::Generic("a".into()), span(1, 3, 1, 3)),
            Argument::new("b".into(), Type::Generic("b".into()), span(1, 6, 1, 6)),
        ],
        bin_op(
            Operator::Add,
            name_ref("a", span(1, 12, 1, 12)),
            name_ref("b", span(1, 16, 1, 16)),
            span(1, 12, 1, 16)
        ),
        span(1, 1, 1, 16)
    ))
}

#[test]
fn test_match()
{
    let e = th_expr(r#"
match a:
    0 => 1,
    1 => 2,
    2 => 3
"#);
    assert!(e == match_expression(
        name_ref("a", span(2, 7, 2, 7)),
        vec![
            match_case(number_pattern(0, span(3, 5, 3, 5)), number(1, span(3, 10, 3, 10)), span(3, 5, 3, 10)),
            match_case(number_pattern(1, span(4, 5, 4, 5)), number(2, span(4, 10, 4, 10)), span(4, 5, 4, 10)),
            match_case(number_pattern(2, span(5, 5, 5, 5)), number(3, span(5, 10, 5, 10)), span(5, 5, 5, 10)),
        ],
        span(2, 1, 5, 10))
    )
}

#[test]
fn test_let()
{
    let e = th_expr(r#"
let x = 5, y = 7 in x * y
"#);
    assert!(e == let_expression(
        vec![
            let_name_binding("x".into(), number(5, span(2, 9, 2, 9)), span(2, 5, 2, 9)),
            let_name_binding("y".into(), number(7, span(2, 16, 2, 16)), span(2, 12, 2, 16)),
        ],
        bin_op(Operator::Mul, name_ref("x", span(2, 21, 2, 21)), name_ref("y", span(2, 25, 2, 25)), span(2, 21, 2, 25)),
        span(2, 1, 2, 25))
    )
}

#[test]
fn test_struct()
{
    let md = th_mod(r#"
type Point = {x: int, y: int}
"#);
    assert!(*md.types.get("test::Point").unwrap() == TypeDeclaration::Struct(struct_declaration(
        "test::Point",
        vec![
            struct_member_declaration("x", Type::Int, span(2, 15, 2, 20)),
            struct_member_declaration("y", Type::Int, span(2, 23, 2, 28)),
        ],
        span(2, 1, 2, 29))
    ))
}

#[test]
fn test_generic_struct()
{
    let md = th_mod(r#"
type Point = {x: $a, y: $b}
"#);
    assert!(*md.types.get("test::Point").unwrap() == TypeDeclaration::Struct(struct_declaration(
        "test::Point",
        vec![
            struct_member_declaration("x", Type::Generic("a".into()), span(2, 15, 2, 19)),
            struct_member_declaration("y", Type::Generic("b".into()), span(2, 22, 2, 26)),
        ],
        span(2, 1, 2, 27))
    ))
}

#[test]
fn test_struct_initializer()
{
    let e = th_expr(r#"
Point{6, 7}
"#);
    assert!(e == Expression::StructInitializer(struct_initializer(
        "Point",
        vec![
            number(6, span(2, 7, 2, 7)),
            number(7, span(2, 10, 2, 10)),
        ],
        span(2, 1, 2, 11))
    ))
}

#[test]
fn test_member_access()
{
    let e = th_expr(r#"
a.b.c.d
"#);
    assert!(e ==
        member_access(
            member_access(
                member_access(
                    name_ref("a", span(2, 1, 2, 1)),
                    MemberAccessType::Name(field("b", 0)),
                    span(2, 1, 2, 3)
                ),
                MemberAccessType::Name(field("c", 0)),
                span(2, 1, 2, 5)
            ),
            MemberAccessType::Name(field("d", 0)),
            span(2, 1, 2, 7)
        )
    )
}

#[test]
fn test_member_access_call()
{
    let e = th_expr(r#"
a.b()
"#);
    assert!(e ==
        member_access(
            name_ref("a", span(2, 1, 2, 1)),
            MemberAccessType::Call(
                Call::new(
                    name_ref2("b", span(2, 3, 2, 3)),
                    Vec::new(),
                    span(2, 3, 2, 5)
                ),
            ),
            span(2, 1, 2, 5)
        )
    )
}

#[test]
fn test_sum_types()
{
    let md = th_mod(r#"
type Option = Some | None
"#);
    assert!(*md.types.get("test::Option").unwrap() == TypeDeclaration::Sum(sum_type_decl(
        "test::Option",
        vec![
            sum_type_case_decl("test::Some", None, span(2, 15, 2, 18)),
            sum_type_case_decl("test::None", None, span(2, 22, 2, 25)),
        ],
        span(2, 1, 2, 25))
    ))
}

#[test]
fn test_sum_types_with_data()
{
    let md = th_mod(r#"
type Foo = Bar{int, int} | Foo | Baz{bla: bool}
"#);
    assert!(*md.types.get("test::Foo").unwrap() == TypeDeclaration::Sum(sum_type_decl(
        "test::Foo",
        vec![
            sum_type_case_decl(
                "test::Bar",
                Some(
                    struct_declaration(
                        "test::Bar",
                        vec![
                            struct_member_declaration("", Type::Int, span(2, 15, 2, 18)),
                            struct_member_declaration("", Type::Int, span(2, 19, 2, 23)),
                        ],
                        span(2, 12, 2, 24)
                    )
                ),
                span(2, 12, 2, 24)
            ),
            sum_type_case_decl("test::Foo", None, span(2, 28, 2, 30)),
            sum_type_case_decl(
                "test::Baz",
                Some(
                    struct_declaration(
                        "test::Baz",
                        vec![
                            struct_member_declaration("bla", Type::Bool, span(2, 38, 2, 46)),
                        ],
                        span(2, 34, 2, 47)
                    )
                ),
                span(2, 34, 2, 47)
            ),
        ],
        span(2, 1, 2, 47))
    ))
}

#[test]
fn test_generic_type_declaration()
{
    let md = th_mod(r#"
type Point = {x: $a, y: $b}

foo(p: Point<int>) -> int = 7
"#);
    assert!(*md.types.get("test::Point").unwrap() == TypeDeclaration::Struct(struct_declaration(
        "test::Point",
        vec![
            struct_member_declaration("x", Type::Generic("a".into()), span(2, 15, 2, 19)),
            struct_member_declaration("y", Type::Generic("b".into()), span(2, 22, 2, 26)),
        ],
        span(2, 1, 2, 27))
    ));

    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            Type::Int,
            vec![
                arg("p", unresolved_type("Point", vec![Type::Int]), span(4, 5, 4, 17)),
            ],
            span(4, 1, 4, 25)
        ),
        true,
        number(7, span(4, 29, 4, 29)),
        span(4, 1, 4, 29))
    )
}

#[test]
fn test_if()
{
    let e = th_expr(r#"
if true: 5 else 10"#);
    assert!(e == if_expression(
        Expression::Literal(Literal::Bool(span(2, 4, 2, 7), true)),
        number(5, span(2, 10, 2, 10)),
        number(10, span(2, 17, 2, 18)),
        span(2, 1, 2, 18)
    ))
}

#[test]
fn test_block()
{
    let e = th_expr(r#"
{a; b; c; 7}"#);
    assert!(e == block(
        vec![
            name_ref("a", span(2, 2, 2, 2)),
            name_ref("b", span(2, 5, 2, 5)),
            name_ref("c", span(2, 8, 2, 8)),
            number(7, span(2, 11, 2, 11)),
        ],
        span(2, 1, 2, 12)
    ))
}
