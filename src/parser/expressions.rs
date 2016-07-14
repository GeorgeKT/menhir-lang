use ast::*;
use compileerror::*;
use parser::*;


fn is_end_of_expression(tok: &Token) -> bool
{
    match tok.kind
    {
        TokenKind::Operator(_) |
        TokenKind::Number(_) |
        TokenKind::Identifier(_) |
        TokenKind::StringLiteral(_) |
        TokenKind::OpenParen |
        TokenKind::OpenBracket |
        TokenKind::OpenCurly => false,
        _ => true,
    }
}

fn eat_comma(tq: &mut TokenQueue) -> Result<(), CompileError>
{
    if tq.is_next(TokenKind::Comma) {
        try!(tq.pop());
    }
    Ok(())
}

fn parse_unary_expression(tq: &mut TokenQueue, indent_level: usize, op: Operator, op_pos: Pos) -> Result<Expression, CompileError>
{
    if op == Operator::Not || op == Operator::Sub || op == Operator::Increment || op == Operator::Decrement {
        let se = try!(parse_expression(tq, indent_level));
        Ok(unary_op(op, se, Span::new(op_pos, tq.pos())))
    } else {
        err(op_pos, ErrorType::InvalidUnaryOperator(op))
    }
}

fn parse_binary_op_rhs(tq: &mut TokenQueue, indent_level: usize, mut lhs: Expression) -> Result<Expression, CompileError>
{
    loop
    {
        if tq.peek().map(|tok| is_end_of_expression(tok)).unwrap_or(false) {
            return Ok(lhs);
        }

        let prec = tq.peek().map(|tok| {
            match tok.kind
            {
                TokenKind::Operator(op) => op.precedence(),
                _ => 0,
            }
        }).unwrap_or(0);

        if prec < lhs.precedence() {
            return Ok(lhs);
        }

        let op = try!(tq.expect_operator());
        let rhs = try!(parse_expression(tq, indent_level));
        match rhs
        {
            //Expression::BinaryOp(span, rhs_op, left, right) => {
            Expression::BinaryOp(bop) => {
                if bop.operator.precedence() <= prec {
                    let span = Span::merge(&lhs.span(), &bop.left.span());
                    let e = bin_op2(op, lhs, bop.left, span);
                    let span = Span::merge(&span, &bop.right.span());
                    lhs = bin_op2(bop.operator, e, bop.right, span);
                } else {
                    let lhs_span = Span::merge(&bop.span, &lhs.span());
                    let e = Expression::BinaryOp(bop);
                    lhs = bin_op(op, lhs, e, lhs_span);
                }
            },
            _ => {
                let span = Span::merge(&lhs.span(), &rhs.span());
                lhs = bin_op(op, lhs, rhs, span);
            },
        }
    }
}

fn parse_function_call(tq: &mut TokenQueue, indent_level: usize, name: Expression) -> Result<Call, CompileError>
{
    let mut args = Vec::new();
    while !tq.is_next(TokenKind::CloseParen)
    {
        let expr = try!(parse_expression(tq, indent_level));
        args.push(expr);
        try!(eat_comma(tq));
    }

    try!(tq.pop());
    let pos = name.span().start;
    Ok(Call::new(name, args, Span::new(pos, tq.pos())))
}

fn parse_assignment(tq: &mut TokenQueue, indent_level: usize, lhs: Expression, op: Operator, pos: Pos) -> Result<Expression, CompileError>
{
    try!(tq.pop()); // pop operator
    let e = try!(parse_expression(tq, indent_level));
    Ok(assignment(op, lhs, e, Span::new(pos, tq.pos())))
}

fn parse_number(num: &str, span: Span) -> Result<Expression, CompileError>
{
    if num.find('.').is_some() || num.find('e').is_some() {
        match num.parse::<f64>() {
            Ok(_) => Ok(Expression::FloatLiteral(span, num.into())),
            Err(_) => err(span.start, ErrorType::InvalidFloatingPoint)
        }
    } else {
        // Should be an integer
        match num.parse::<u64>() {
            Ok(i) => Ok(Expression::IntLiteral(span, i)),
            Err(_) => err(span.start, ErrorType::InvalidInteger)
        }
    }
}

fn parse_object_construction(tq: &mut TokenQueue, indent_level: usize, name: NameRef) -> Result<Expression, CompileError>
{
    let mut params = Vec::new();

    while !tq.is_next(TokenKind::CloseCurly) {
        let e = try!(parse_expression(tq, indent_level));
        params.push(e);
        try!(eat_comma(tq));
    }

    try!(tq.expect(TokenKind::CloseCurly));
    let pos = name.span.start;
    Ok(object_construction(name, params, Span::new(pos, tq.pos())))
}

fn parse_member_access(tq: &mut TokenQueue, indent_level: usize, name: NameRef) -> Result<MemberAccess, CompileError>
{
    let (next_name, next_name_span) = try!(tq.expect_identifier());
    let next_nr = NameRef::new(next_name, next_name_span);
    let member = if tq.is_next(TokenKind::OpenParen)
    {
        try!(tq.pop());
        let call = try!(parse_function_call(tq, indent_level, Expression::NameRef(next_nr)));
        Member::Call(call)
    }
    else if tq.is_next(TokenKind::Operator(Operator::Dot))
    {
        try!(tq.pop());
        let next = try!(parse_member_access(tq, indent_level, next_nr));
        Member::Nested(Box::new(next))
    }
    else
    {
        Member::Var(next_nr)
    };

    let pos = name.span.start;
    Ok(MemberAccess::new(name.name, member, Span::new(pos, tq.pos())))
}

fn parse_name(tq: &mut TokenQueue, id: String, pos: Pos) -> Result<NameRef, CompileError>
{
    let mut name = id;
    while tq.is_next(TokenKind::DoubleColon)
    {
        try!(tq.pop());
        let (next, _) = try!(tq.expect_identifier());
        name.push_str("::");
        name.push_str(&next);
    }

    Ok(NameRef::new(name, Span::new(pos, tq.pos())))
}

fn parse_array_literal(tq: &mut TokenQueue, indent_level: usize, pos: Pos) -> Result<Expression, CompileError>
{
    let mut expressions = Vec::new();
    loop
    {
        if tq.is_next(TokenKind::CloseBracket) {
            break;
        }

        if tq.next_indent().is_some() {
            try!(tq.pop());
            continue;
        }

        let e = try!(parse_expression(tq, indent_level));
        expressions.push(e);
        try!(eat_comma(tq));
    }

    try!(tq.expect(TokenKind::CloseBracket));
    Ok(array_lit(expressions, Span::new(pos, tq.pos())))
}

fn parse_index_operation(tq: &mut TokenQueue, indent_level: usize, target: Expression) -> Result<Expression, CompileError>
{
    let index_expr = try!(parse_expression(tq, indent_level));
    try!(tq.expect(TokenKind::CloseBracket));
    let span =  Span::new(target.span().start, tq.pos());
    Ok(index_op(target, index_expr, span))
}

fn parse_lhs(tq: &mut TokenQueue, indent_level: usize, tok: Token) -> Result<Expression, CompileError>
{
    match tok.kind
    {
        TokenKind::OpenParen => {
            let expr = try!(parse_expression(tq, indent_level));
            try!(tq.expect(TokenKind::CloseParen));
            Ok(Expression::Enclosed(Span::new(tok.span.start, tq.pos()), Box::new(expr)))
        },

        TokenKind::OpenBracket => {
            parse_array_literal(tq, indent_level, tok.span.start)
        },

        TokenKind::Identifier(id) => {
            let nr = try!(parse_name(tq, id, tok.span.start));
            Ok(Expression::NameRef(nr))
        },

        TokenKind::StringLiteral(s) => {
            Ok(Expression::StringLiteral(tok.span, s))
        },

        TokenKind::Number(n) => {
            Ok(try!(parse_number(&n, tok.span)))
        },

        TokenKind::Operator(op) => parse_unary_expression(tq, indent_level, op, tok.span.start),

        _ => err(tok.span.start, ErrorType::UnexpectedToken(tok)),
    }
}

fn parse_rhs(tq: &mut TokenQueue, indent_level: usize, lhs: Expression) -> Result<Expression, CompileError>
{
    if is_end_of_expression(tq.peek().expect("Unexpected EOF")) {
        return Ok(lhs);
    }

    let next = try!(tq.pop());
    let nlhs = match next.kind
    {
        TokenKind::OpenParen => {
            try!(parse_function_call(tq, indent_level, lhs).map(|c| Expression::Call(c)))
        },
        TokenKind::OpenCurly => {
            let nr = try!(lhs.to_name_ref());
            try!(parse_object_construction(tq, indent_level, nr))
        },
        TokenKind::Operator(op) if op == Operator::Dot => {
            let nr = try!(lhs.to_name_ref());
            try!(parse_member_access(tq, indent_level, nr).map(|m| Expression::MemberAccess(m)))
        },
        /*
        Some(TokenKind::Operator(op)) if op == Operator::Increment || op == Operator::Decrement => {
            // Turn x++ in x += 1, and x-- in x -= 1
            let new_op = if op == Operator::Increment {Operator::AddAssign} else {Operator::SubAssign};
            Ok(assignment(new_op, lhs, Expression::IntLiteral(tok.span, 1), tok.span))
        },
        */
        TokenKind::Operator(op) if op == Operator::Increment || op == Operator::Decrement => {
            let start = lhs.span().start;
            pf_unary_op(op, lhs, Span::new(start, next.span.end))
        },
        TokenKind::Operator(op) if op.is_assignment() => {
            tq.push_front(next);
            let start = lhs.span().start;
            try!(parse_assignment(tq, indent_level, lhs, op, start))
        },
        TokenKind::Operator(op) if op.is_binary_operator() => {
            tq.push_front(next);
            try!(parse_binary_op_rhs(tq, indent_level, lhs))
        },
        TokenKind::OpenBracket => {
            try!(parse_index_operation(tq, indent_level, lhs))
        },
        _ => {
            return err(tq.pos(), ErrorType::UnexpectedToken(next));
        },
    };

    parse_rhs(tq, indent_level, nlhs)
}


pub fn parse_expression(tq: &mut TokenQueue, indent_level: usize) -> Result<Expression, CompileError>
{
    let tok = try!(tq.pop());
    if is_end_of_expression(&tok) {
        return err(tq.pos(), ErrorType::ExpectedStartOfExpression);
    }
    
    let lhs = try!(parse_lhs(tq, indent_level, tok));
    parse_rhs(tq, indent_level, lhs)
}

#[cfg(test)]
use std::io::Cursor;


#[cfg(test)]
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

#[cfg(test)]
pub fn number(v: u64, span: Span) -> Expression
{
    Expression::IntLiteral(span, v)
}

#[cfg(test)]
fn enclosed(span: Span, left: Expression) -> Expression
{
    Expression::Enclosed(span, Box::new(left))
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
            "b".into(),
            Member::Var(NameRef::new("d".into(), span(1, 3, 1, 3))),
            span(1, 1, 1, 3)
        ),
        member_access(
            "a".into(),
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

    let c = MemberAccess::new("c".into(), Member::Var(NameRef::new("d".into(), span(1, 7, 1, 7))), span(1, 5, 1, 7));
    let b = MemberAccess::new("b".into(), Member::Nested(Box::new(c)), span(1, 3, 1, 7));
    let a = member_access("a".into(), Member::Nested(Box::new(b)), span(1, 1, 1, 7));
    assert!(e == a);
}

#[test]
fn test_member_assignment()
{
    let e = th_expr("b.d = 6");
    assert!(e == assignment(
        Operator::Assign,
        member_access("b".into(), Member::Var(NameRef::new("d".into(), span(1, 3, 1, 3))), span(1, 1, 1, 3)),
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
