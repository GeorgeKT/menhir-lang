use std::io::Cursor;
use ast::*;
use parser::*;
use super::lexer::Lexer;
use target::Target;
use span::{Pos, Span};

fn span(sl: usize, so: usize, el: usize, eo: usize) -> Span
{
    Span::new("", Pos::new(sl, so), Pos::new(el, eo))
}

pub fn th_expr(data: &str, target: &Target) -> Expression
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new("").read(&mut cursor).expect("Lexing failed");
    let (level, _) = tq.pop_indent().unwrap().unwrap();
    let e = parse_expression(&mut tq, level, target).expect("Parsing failed");
    println!("AST dump:");
    e.print(0);
    e
}

pub fn th_pattern(data: &str, target: &Target) -> Pattern
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new("").read(&mut cursor).expect("Lexing failed");
    let (level, _) = tq.pop_indent().unwrap().unwrap();
    let e = parse_pattern(&mut tq, level, target).expect("Parsing failed");
    println!("AST dump:");
    e.print(0);
    e
}

pub fn th_mod(data: &str, target: &Target) -> Module
{
    let mut pkg = parse_str(data, "test", target).expect("Parsing failed");
    println!("AST dump:");
    pkg.print(0);
    pkg.modules.remove("test").expect("No module named test")
}


pub fn number(v: i64, span: Span, target: &Target) -> Expression
{
    Expression::Literal(Literal::Int(span, v, target.int_size))
}

pub fn number_pattern(v: i64, span: Span, target: &Target) -> Pattern
{
    Pattern::Literal(Literal::Int(span, v, target.int_size))
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
    let target = Target::new(IntSize::I32, "");
    assert!(th_expr("1000", &target) == number(1000, span(1, 1, 1, 4), &target));
    assert!(th_expr("id", &target) == name_ref("id", span(1, 1, 1, 2)));
    assert!(th_expr("-1000", &target) == unary_op(UnaryOperator::Sub, number(1000, span(1, 2, 1, 5), &target), span(1, 1, 1, 5)));
    assert!(th_expr("!id", &target) == unary_op(UnaryOperator::Not, name_ref("id", span(1, 2, 1, 3)), span(1, 1, 1, 3)));
    assert!(th_expr("true", &target) == Expression::Literal(Literal::Bool(span(1, 1, 1, 4), true)));
    assert!(th_expr("false", &target) == Expression::Literal(Literal::Bool(span(1, 1, 1, 5), false)));
}

#[test]
fn test_binary_ops()
{
    let target = Target::new(IntSize::I32, "");
    let ops = [
        (BinaryOperator::Add, "+"),
        (BinaryOperator::Sub, "-"),
        (BinaryOperator::Div, "/"),
        (BinaryOperator::Mul, "*"),
        (BinaryOperator::Mod, "%"),
        (BinaryOperator::Equals, "=="),
        (BinaryOperator::NotEquals, "!="),
        (BinaryOperator::GreaterThan, ">"),
        (BinaryOperator::GreaterThanEquals, ">="),
        (BinaryOperator::LessThan, "<"),
        (BinaryOperator::LessThanEquals, "<="),
        (BinaryOperator::Or, "||"),
        (BinaryOperator::And, "&&"),
    ];

    for &(op, op_txt) in &ops
    {
        let e_txt = format!("a {} b", op_txt);
        let e = th_expr(&e_txt, &target);
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
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("a + b * c", &target);
    assert!(e == bin_op(
        BinaryOperator::Add,
        name_ref("a", span(1, 1, 1, 1)),
        bin_op(BinaryOperator::Mul, name_ref("b", span(1, 5, 1, 5)), name_ref("c", span(1, 9, 1, 9)), span(1, 5, 1, 9)),
        span(1, 1, 1, 9),
    ));
}


#[test]
fn test_precedence_2()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("a * b + c ", &target);
    assert!(e == bin_op(
        BinaryOperator::Add,
        bin_op(BinaryOperator::Mul, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 5, 1, 5)), span(1, 1, 1, 5)),
        name_ref("c", span(1, 9, 1, 9)),
        span(1, 1, 1, 9),
    ));
}

#[test]
fn test_precedence_3()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("a * b + c / d ", &target);
    assert!(e == bin_op(
        BinaryOperator::Add,
        bin_op(BinaryOperator::Mul, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 5, 1, 5)), span(1, 1, 1, 5)),
        bin_op(BinaryOperator::Div, name_ref("c", span(1, 9, 1, 9)), name_ref("d", span(1, 13, 1, 13)), span(1, 9, 1, 13)),
        span(1, 1, 1, 13),
    ));
}

#[test]
fn test_precedence_4()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("a && b || c && d ", &target);
    assert!(e == bin_op(
        BinaryOperator::Or,
        bin_op(BinaryOperator::And, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 6, 1, 6)), span(1, 1, 1, 6)),
        bin_op(BinaryOperator::And, name_ref("c", span(1, 11, 1, 11)), name_ref("d", span(1, 16, 1, 16)), span(1, 11, 1, 16)),
        span(1, 1, 1, 16),
    ));
}

#[test]
fn test_precedence_5()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("a >= b && c < d", &target);
    assert!(e == bin_op(
        BinaryOperator::And,
        bin_op(BinaryOperator::GreaterThanEquals, name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 6, 1, 6)), span(1, 1, 1, 6)),
        bin_op(BinaryOperator::LessThan, name_ref("c", span(1, 11, 1, 11)), name_ref("d", span(1, 15, 1, 15)), span(1, 11, 1, 15)),
        span(1, 1, 1, 15),
    ));
}

#[test]
fn test_precedence_6()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("a * (b + c)", &target);
    assert!(e == bin_op(
        BinaryOperator::Mul,
        name_ref("a", span(1, 1, 1, 1)),
        bin_op_with_precedence(
            BinaryOperator::Add,
            name_ref("b", span(1, 6, 1, 6)),
            name_ref("c", span(1, 10, 1, 10)),
            span(1, 6, 1, 10),
            TOP_PRECEDENCE
        ),
        span(1, 1, 1, 10),
    ));
}

#[test]
fn test_precedence_7()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("b + -c", &target);
    assert!(e == bin_op(
        BinaryOperator::Add,
        name_ref("b", span(1, 1, 1, 1)),
        unary_op(UnaryOperator::Sub, name_ref("c", span(1, 6, 1, 6)), span(1, 5, 1, 6)),
        span(1, 1, 1, 6),
    ));
}

#[test]
fn test_precedence_8()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("b + c(6)", &target);
    assert!(e == bin_op(
        BinaryOperator::Add,
        name_ref("b", span(1, 1, 1, 1)),
        Expression::Call(Box::new(Call::new(
            name_ref2("c", span(1, 5, 1, 5)),
            vec![number(6, span(1, 7, 1, 7), &target)],
            span(1, 5, 1, 8)
        ))),
        span(1, 1, 1, 8),
    ));
}

#[test]
fn test_precedence_9()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("c(6) + b", &target);
    assert!(e == bin_op(
        BinaryOperator::Add,
        Expression::Call(Box::new(Call::new(
            name_ref2("c", span(1, 1, 1, 1)),
            vec![number(6, span(1, 3, 1, 3), &target)],
            span(1, 1, 1, 4)
        ))),
        name_ref("b", span(1, 8, 1, 8)),
        span(1, 1, 1, 8),
    ));
}

#[test]
fn test_precedence_10()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("4 + 5 * 7 - 9 / 3 + 5 % 4", &target);

    let mul = bin_op(
        BinaryOperator::Mul,
        number(5, span(1, 5, 1, 5), &target),
        number(7, span(1, 9, 1, 9), &target),
        span(1, 5, 1, 9));

    let s1 = bin_op(BinaryOperator::Add, number(4, span(1, 1, 1, 1), &target), mul, span(1, 1, 1, 9));

    let div = bin_op(
        BinaryOperator::Div,
        number(9, span(1, 13, 1, 13), &target),
        number(3, span(1, 17, 1, 17), &target),
        span(1, 13, 1, 17));

    let s2 = bin_op(BinaryOperator::Sub, s1, div, span(1, 1, 1, 17));

    let rem = bin_op(
        BinaryOperator::Mod,
        number(5, span(1, 21, 1, 21), &target),
        number(4, span(1, 25, 1, 25), &target),
        span(1, 21, 1, 25));

    let s3 = bin_op(BinaryOperator::Add, s2, rem,span(1, 1, 1, 25));

    println!("s3:");
    s3.print(0);
    assert!(e == s3);
}

#[test]
fn test_namespaced_call()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("foo::bar(7)", &target);
    assert!(e == Expression::Call(
        Box::new(Call::new(
            name_ref2("foo::bar", span(1, 1, 1, 8)),
            vec![number(7, span(1, 10, 1, 10), &target)],
            span(1, 1, 1, 11),
        )))
    );
}

#[test]
fn test_array_literal()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("[1, 2, 3]", &target);
    assert!(e == Expression::Literal(array_lit(
        vec![
            number(1, span(1, 2, 1, 2), &target),
            number(2, span(1, 5, 1, 5), &target),
            number(3, span(1, 8, 1, 8), &target),
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
            BinaryOperator::Mul,
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
    let target = Target::new(IntSize::I32, "");
    let e = th_pattern("[head | tail]", &target);
    assert!(e == array_pattern("head", "tail", span(1, 1, 1, 13)));
}

#[test]
fn test_array_concat()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("a + [1, 2]", &target);
    assert!(e == bin_op(
        BinaryOperator::Add,
        name_ref("a", span(1, 1, 1, 1)),
        Expression::Literal(array_lit(
            vec![
                number(1, span(1, 6, 1, 6), &target),
                number(2, span(1, 9, 1, 9), &target),
            ],
            span(1, 5, 1, 10)
        )),
        span(1, 1, 1, 10))
    );
}


fn arg(name: &str, typ: Type, span: Span) -> Argument
{
    Argument::new(name, typ, false, span)
}

#[test]
fn test_function_with_args()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod("fn foo(a: int, b: int) -> int: 7", &target);
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            target.native_int_type.clone(),
            vec![
                arg("a", target.native_int_type.clone(), span(1, 8, 1, 13)),
                arg("b", target.native_int_type.clone(), span(1, 16, 1, 21)),
            ],
            span(1, 1, 1, 29)
        ),
        true,
        number(7, span(1, 32, 1, 32), &target),
        span(1, 1, 1, 32))
    )
}

#[test]
fn test_function_with_no_args()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod("fn foo() -> int: 7", &target);
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            target.native_int_type.clone(),
            Vec::new(),
            span(1, 1, 1, 15)
        ),
        true,
        number(7, span(1, 18, 1, 18), &target),
        span(1, 1, 1, 18))
    )
}

#[test]
fn test_function_with_no_return_type()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod("fn foo(): 7", &target);
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            Type::Void,
            Vec::new(),
            span(1, 1, 1, 8)
        ),
        true,
        number(7, span(1, 11, 1, 11), &target),
        span(1, 1, 1, 11))
    )
}

#[test]
fn test_function_with_func_type()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod("fn foo(a: fn(int, int) -> int) -> int: 7", &target);
    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            target.native_int_type.clone(),
            vec![
                Argument::new(
                    "a",
                    func_type(
                        vec![
                            target.native_int_type.clone(),
                            target.native_int_type.clone(),
                        ],
                        target.native_int_type.clone(),
                    ),
                    false,
                    span(1, 8, 1, 29)
                ),
            ],
            span(1, 1, 1, 37)
        ),
        true,
        number(7, span(1, 40, 1, 40), &target),
        span(1, 1, 1, 40))
    )
}

#[test]
fn test_external_function()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod("extern fn foo() -> int", &target);
    assert!(*md.externals.get("foo").unwrap() == ExternalFunction::new(
        sig(
            "foo",
            target.native_int_type.clone(),
            Vec::new(),
            span(1, 11, 1, 22)
        ),
        span(1, 1, 1, 22))
    )
}

#[test]
fn test_lambda()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr("fn(a, b) -> a + b", &target);
    assert!(e == lambda(
        vec![
            Argument::new("a", generic_type("a"), false, span(1, 4, 1, 4)),
            Argument::new("b", generic_type("b"), false, span(1, 7, 1, 7)),
        ],
        bin_op(
            BinaryOperator::Add,
            name_ref("a", span(1, 13, 1, 13)),
            name_ref("b", span(1, 17, 1, 17)),
            span(1, 13, 1, 17)
        ),
        span(1, 1, 1, 17)
    ))
}

#[test]
fn test_match()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr(r#"
match a:
    0 => 1
    1 => 2
    2 => 3
"#, &target);
    assert!(e == match_expression(
        name_ref("a", span(2, 7, 2, 7)),
        vec![
            match_case(number_pattern(0, span(3, 5, 3, 5), &target), number(1, span(3, 10, 3, 10), &target), span(3, 5, 3, 10)),
            match_case(number_pattern(1, span(4, 5, 4, 5), &target), number(2, span(4, 10, 4, 10), &target), span(4, 5, 4, 10)),
            match_case(number_pattern(2, span(5, 5, 5, 5), &target), number(3, span(5, 10, 5, 10), &target), span(5, 5, 5, 10)),
        ],
        span(2, 1, 5, 10))
    )
}

#[test]
fn test_struct()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod(r#"
struct Point:
    x: int
    y: int
"#, &target);
    assert!(*md.types.get("test::Point").unwrap() == TypeDeclaration::Struct(struct_declaration(
        "test::Point",
        vec![
            struct_member_declaration("x", target.native_int_type.clone(), span(3, 5, 3, 10)),
            struct_member_declaration("y", target.native_int_type.clone(), span(4, 5, 4, 10)),
        ],
        span(2, 1, 4, 10))
    ))
}

#[test]
fn test_generic_struct()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod(r#"
struct Point:
    x: $a
    y: $b
"#, &target);
    assert!(*md.types.get("test::Point").unwrap() == TypeDeclaration::Struct(struct_declaration(
        "test::Point",
        vec![
            struct_member_declaration("x", generic_type("a"), span(3, 5, 3, 9)),
            struct_member_declaration("y", generic_type("b"), span(4, 5, 4, 9)),
        ],
        span(2, 1, 4, 9))
    ))
}

#[test]
fn test_struct_initializer()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr(r#"
Point{6, 7}
"#, &target);
    assert!(e == Expression::StructInitializer(struct_initializer(
        "Point",
        vec![
            number(6, span(2, 7, 2, 7), &target),
            number(7, span(2, 10, 2, 10), &target),
        ],
        span(2, 1, 2, 11))
    ))
}

#[test]
fn test_anonymous_struct_initializer()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr(r#"
{6, 7}
"#, &target);
    assert!(e == Expression::StructInitializer(struct_initializer(
        "",
        vec![
            number(6, span(2, 2, 2, 2), &target),
            number(7, span(2, 5, 2, 5), &target),
        ],
        span(2, 1, 2, 6))
    ))
}

#[test]
fn test_member_access()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr(r#"
a.b.c.d
"#, &target);
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
    let target = Target::new(IntSize::I32, "");
    let e = th_expr(r#"
a.b()
"#, &target);
    assert!(e ==
        member_access(
            name_ref("a", span(2, 1, 2, 1)),
            MemberAccessType::Call(
                Box::new(Call::new(
                    name_ref2("b", span(2, 3, 2, 3)),
                    Vec::new(),
                    span(2, 3, 2, 5)
                )),
            ),
            span(2, 1, 2, 5)
        )
    )
}

#[test]
fn test_sum_types()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod(r#"
enum Option:
    Some
    None
"#, &target);
    assert!(*md.types.get("test::Option").unwrap() == TypeDeclaration::Sum(sum_type_decl(
        "test::Option",
        vec![
            sum_type_case_decl("test::Option::Some", None, span(3, 5, 3, 8)),
            sum_type_case_decl("test::Option::None", None, span(4, 5, 4, 8)),
        ],
        span(2, 1, 4, 8))
    ))
}

#[test]
fn test_sum_types_with_data()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod(r#"
enum Foo:
    Bar{x: int, y: int}
    Foo
    Baz{bla: bool}
"#, &target);
    let result = md.types.get("test::Foo").unwrap();
    result.print(1);
    assert!(*result == TypeDeclaration::Sum(sum_type_decl(
        "test::Foo",
        vec![
            sum_type_case_decl(
                "test::Foo::Bar",
                Some(
                    struct_declaration(
                        "Foo::Bar",
                        vec![
                            struct_member_declaration("x", target.native_int_type.clone(), span(3, 9, 3, 14)),
                            struct_member_declaration("y", target.native_int_type.clone(), span(3, 17, 3, 22)),
                        ],
                        span(3, 5, 3, 23)
                    )
                ),
                span(3, 5, 3, 23)
            ),
            sum_type_case_decl("test::Foo::Foo", None, span(4, 5, 4, 7)),
            sum_type_case_decl(
                "test::Foo::Baz",
                Some(
                    struct_declaration(
                        "Foo::Baz",
                        vec![
                            struct_member_declaration("bla", Type::Bool, span(5, 9, 5, 17)),
                        ],
                        span(5, 5, 5, 18)
                    )
                ),
                span(5, 5, 5, 18)
            ),
        ],
        span(2, 1, 5, 18))
    ))
}

#[test]
fn test_generic_type_declaration()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod(r#"
struct Point:
    x: $a
    y: $b

fn foo(p: Point<int>) -> int: 7
"#, &target);
    assert!(*md.types.get("test::Point").unwrap() == TypeDeclaration::Struct(struct_declaration(
        "test::Point",
        vec![
            struct_member_declaration("x", generic_type("a"), span(3, 5, 3, 9)),
            struct_member_declaration("y", generic_type("b"), span(4, 5, 4, 9)),
        ],
        span(2, 1, 4, 9))
    ));

    assert!(*md.functions.get("test::foo").unwrap() == Function::new(
        sig(
            "test::foo",
            target.native_int_type.clone(),
            vec![
                arg("p", unresolved_type("Point", vec![target.native_int_type.clone()]), span(6, 8, 6, 20)),
            ],
            span(6, 1, 6, 28)
        ),
        true,
        number(7, span(6, 31, 6, 31), &target),
        span(6, 1, 6, 31))
    )
}

#[test]
fn test_if()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr(r#"
if true: 5 else 10"#, &target);
    assert!(e == if_expression(
        Expression::Literal(Literal::Bool(span(2, 4, 2, 7), true)),
        number(5, span(2, 10, 2, 10), &target),
        number(10, span(2, 17, 2, 18), &target),
        span(2, 1, 2, 18)
    ))
}

#[test]
fn test_block()
{
    let target = Target::new(IntSize::I32, "");
    let e = th_expr(r#"
(a; b; c; 7)"#, &target);
    assert!(e == block(
        vec![
            name_ref("a", span(2, 2, 2, 2)),
            name_ref("b", span(2, 5, 2, 5)),
            name_ref("c", span(2, 8, 2, 8)),
            number(7, span(2, 11, 2, 11), &target),
        ],
        span(2, 2, 2, 11)
    ))
}


#[test]
fn test_interface()
{
    let target = Target::new(IntSize::I32, "");
    let md = th_mod(r#"
interface Foo:
    fn bar(self, x: int) -> int
"#, &target);
    let result = md.types.get("test::Foo").unwrap();
    println!("{:?}", result);
    assert!(*result == TypeDeclaration::Interface(interface(
        "test::Foo".into(),
        vec![
            sig("bar",
                target.native_int_type.clone(),
                vec![
                    arg("self", ptr_type(Type::SelfType), span(3, 12, 3, 15)),
                    arg("x", target.native_int_type.clone(), span(3, 18, 3, 23))
                ],
                span(3, 8, 3, 31)
            )
        ],
        span(2, 1, 3, 31)
    )))
}
