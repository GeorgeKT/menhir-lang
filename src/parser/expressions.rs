use ast::*;
use compileerror::*;
use parser::*;


fn is_end_of_expression(tok: &Token, indent_level: usize) -> bool
{
    match tok.kind
    {
        TokenKind::Operator(_) |
        TokenKind::Number(_) |
        TokenKind::Identifier(_) |
        TokenKind::StringLiteral(_) |
        TokenKind::OpenParen => false,
        TokenKind::Indent(level) => level < indent_level,
        _ => true,
    }
}

fn parse_unary_expression(tq: &mut TokenQueue, indent_level: usize, op: Operator, op_pos: Pos) -> Result<Expression, CompileError>
{
    if op == Operator::Not || op == Operator::Sub || op == Operator::Increment || op == Operator::Decrement {
        let se = try!(parse_expression(tq, indent_level));
        Ok(Expression::UnaryOp(Span::new(op_pos, tq.pos()), op, Box::new(se)))
    } else {
        err(op_pos, ErrorType::InvalidUnaryOperator(op))
    }
}

fn parse_binary_op_rhs(tq: &mut TokenQueue, indent_level: usize, mut lhs: Expression) -> Result<Expression, CompileError>
{
    loop
    {
        if tq.peek().map(|tok| is_end_of_expression(tok, indent_level)).unwrap_or(false) {
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
            Expression::BinaryOp(span, rhs_op, left, right) => {

                if rhs_op.precedence() <= prec {
                    let span = Span::merge(&lhs.span(), &left.span());
                    let e = Expression::BinaryOp(span, op, Box::new(lhs), left);
                    let span = Span::merge(&span, &right.span());
                    lhs = Expression::BinaryOp(span, rhs_op, Box::new(e), right);
                } else {
                    let lhs_span = Span::merge(&span, &lhs.span());
                    let e = Expression::BinaryOp(span, rhs_op, left, right);
                    lhs = Expression::BinaryOp(lhs_span, op, Box::new(lhs), Box::new(e));
                }
            },
            _ => {
                let span = Span::merge(&lhs.span(), &rhs.span());
                lhs = Expression::BinaryOp(span, op, Box::new(lhs), Box::new(rhs));
            },
        }
    }
}

fn parse_function_call(tq: &mut TokenQueue, indent_level: usize, name: String, pos: Pos) -> Result<Call, CompileError>
{
    try!(tq.expect(TokenKind::OpenParen));

    let mut args = Vec::new();
    while !tq.is_next(TokenKind::CloseParen)
    {
        let expr = try!(parse_expression(tq, indent_level));
        args.push(expr);
        if tq.is_next(TokenKind::Comma) {
            try!(tq.pop());
            continue;
        }
    }

    try!(tq.pop());
    Ok(Call::new(name, args, Span::new(pos, tq.pos())))
}

fn parse_assignemnt(tq: &mut TokenQueue, indent_level: usize, lhs: String, op: Operator, pos: Pos) -> Result<Expression, CompileError>
{
    try!(tq.pop()); // pop operator
    let e = try!(parse_expression(tq, indent_level));

    Ok(Expression::Assignment(
        Span::new(pos, tq.pos()),
        op,
        lhs,
        Box::new(e),
    ))
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

fn parse_object_construction(tq: &mut TokenQueue, indent_level: usize, name: &str, pos: Pos) -> Result<Expression, CompileError>
{
    try!(tq.expect(TokenKind::OpenCurly));
    let mut params = Vec::new();

    while !tq.is_next(TokenKind::CloseCurly) {
        let e = try!(parse_expression(tq, indent_level));
        params.push(e);
        if tq.is_next(TokenKind::Comma) {
            try!(tq.pop());
        }
    }

    try!(tq.expect(TokenKind::CloseCurly));
    Ok(Expression::ObjectConstruction(
        Span::new(pos, tq.pos()),
        name.into(),
        params,
    ))
}

fn parse_primary_expression(tq: &mut TokenQueue, indent_level: usize, tok: Token) -> Result<Expression, CompileError>
{
    match tok.kind
    {
        TokenKind::OpenParen => {
            let expr = try!(parse_expression(tq, indent_level));
            try!(tq.expect(TokenKind::CloseParen));
            Ok(Expression::Enclosed(Span::new(tok.span.start, tq.pos()), Box::new(expr)))
        },

        TokenKind::Identifier(id) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::OpenParen) => parse_function_call(tq, indent_level, id, tok.span.start).map(|c| Expression::Call(c)),
                Some(TokenKind::OpenCurly) => parse_object_construction(tq, indent_level, &id, tok.span.start),
                Some(TokenKind::Operator(op)) if op == Operator::Increment || op == Operator::Decrement => {
                    // Turn x++ in x += 1, and x-- in x -= 1
                    try!(tq.pop());
                    let new_op = if op == Operator::Increment {Operator::AddAssign} else {Operator::SubAssign};
                    Ok(Expression::Assignment(tok.span, new_op, id, Box::new(Expression::IntLiteral(tok.span, 1))))
                },
                Some(TokenKind::Operator(op)) if op.is_assignment() => {
                    parse_assignemnt(tq, indent_level, id, op, tok.span.start)
                },
                Some(TokenKind::Operator(op)) if op.is_binary_operator() => {
                    parse_binary_op_rhs(tq, indent_level, Expression::NameRef(tok.span, id))
                },
                _ => Ok(Expression::NameRef(tok.span, id)),
            }
        },

        TokenKind::StringLiteral(s) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::Operator(op)) if op.is_binary_operator() => {
                    parse_binary_op_rhs(tq, indent_level, Expression::StringLiteral(tok.span, s))
                },
                _ => Ok(Expression::StringLiteral(tok.span, s)),
            }
        },

        TokenKind::Number(n) => {
            let num = try!(parse_number(&n, tok.span));
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::Operator(op)) => {
                    if op.is_binary_operator() {
                        parse_binary_op_rhs(tq, indent_level, num)
                    } else {
                        Ok(num)
                    }
                },
                _ => Ok(num),
            }
        },

        TokenKind::Operator(op) => parse_unary_expression(tq, indent_level, op, tok.span.start),

        _ => err(tok.span.start, ErrorType::UnexpectedToken(tok)),
    }
}

pub fn parse_expression(tq: &mut TokenQueue, indent_level: usize) -> Result<Expression, CompileError>
{
    if let Some(tok) = try!(tq.pop_if(|tok| !is_end_of_expression(tok, indent_level)))
    {
        let tok_pos = tok.span.start;
        let e = try!(parse_primary_expression(tq, indent_level, tok));
        if tq.is_next(TokenKind::Operator(Operator::Increment)) || tq.is_next(TokenKind::Operator(Operator::Decrement)) {
            let op = try!(tq.expect_operator());
            Ok(Expression::PostFixUnaryOp(Span::new(tok_pos, tq.pos()), op, Box::new(e)))
        } else {
            Ok(e)
        }
    }
    else
    {
        err(tq.pos(), ErrorType::UnexpectedEOF)
    }
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
    e.print(0);
    e
}

#[cfg(test)]
fn bin_op(op: Operator, span: Span, left: Expression, right: Expression) -> Expression
{
    Expression::BinaryOp(
        span,
        op,
        Box::new(left),
        Box::new(right),
    )
}

#[cfg(test)]
fn unary_op(op: Operator, span: Span, left: Expression) -> Expression
{
    Expression::UnaryOp(span, op, Box::new(left))
}

#[cfg(test)]
fn pf_unary_op(op: Operator, span: Span, left: Expression) -> Expression
{
    Expression::PostFixUnaryOp(span, op, Box::new(left))
}

#[cfg(test)]
fn name_ref(left: &str, span: Span) -> Expression
{
    Expression::NameRef(span, left.into())
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
    assert!(th_expr("-1000") == unary_op(Operator::Sub, span(1, 1, 1, 5), number(1000, span(1, 2, 1, 5))));
    assert!(th_expr("!id") == unary_op(Operator::Not, span(1, 1, 1, 3), name_ref("id", span(1, 2, 1, 3))));
    assert!(th_expr("++id") == unary_op(Operator::Increment, span(1, 1, 1, 4), name_ref("id", span(1, 3, 1, 4))));
    assert!(th_expr("--id") == unary_op(Operator::Decrement, span(1, 1, 1, 4), name_ref("id", span(1, 3, 1, 4))));
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
        assert!(e == bin_op(op, span(1, 1, 1, e_txt.len()), name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, e_txt.len(), 1, e_txt.len()))));
    }
}

#[test]
fn test_precedence()
{
    let e = th_expr("a + b * c");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 9),
        name_ref("a", span(1, 1, 1, 1)),
        bin_op(Operator::Mul, span(1, 5, 1, 9), name_ref("b", span(1, 5, 1, 5)), name_ref("c", span(1, 9, 1, 9))),
    ));
}


#[test]
fn test_precedence_2()
{
    let e = th_expr("a * b + c ");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 9),
        bin_op(Operator::Mul, span(1, 1, 1, 5), name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 5, 1, 5))),
        name_ref("c", span(1, 9, 1, 9)),
    ));
}

#[test]
fn test_precedence_3()
{
    let e = th_expr("a * b + c / d ");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 13),
        bin_op(Operator::Mul, span(1, 1, 1, 5), name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 5, 1, 5))),
        bin_op(Operator::Div, span(1, 9, 1, 13), name_ref("c", span(1, 9, 1, 9)), name_ref("d", span(1, 13, 1, 13))),
    ));
}

#[test]
fn test_precedence_4()
{
    let e = th_expr("a && b || c && d ");
    assert!(e == bin_op(
        Operator::Or,
        span(1, 1, 1, 16),
        bin_op(Operator::And, span(1, 1, 1, 6), name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 6, 1, 6))),
        bin_op(Operator::And, span(1, 11, 1, 16), name_ref("c", span(1, 11, 1, 11)), name_ref("d", span(1, 16, 1, 16))),
    ));
}

#[test]
fn test_precedence_5()
{
    let e = th_expr("a >= b && c < d");
    assert!(e == bin_op(
        Operator::And,
        span(1, 1, 1, 15),
        bin_op(Operator::GreaterThanEquals, span(1, 1, 1, 6), name_ref("a", span(1, 1, 1, 1)), name_ref("b", span(1, 6, 1, 6))),
        bin_op(Operator::LessThan, span(1, 11, 1, 15), name_ref("c", span(1, 11, 1, 11)), name_ref("d", span(1, 15, 1, 15))),
    ));
}

#[test]
fn test_precedence_6()
{
    let e = th_expr("a * (b + c)");
    assert!(e == bin_op(
        Operator::Mul,
        span(1, 1, 1, 11),
        name_ref("a", span(1, 1, 1, 1)),
        enclosed(
            span(1, 5, 1, 11),
            bin_op(Operator::Add, span(1, 6, 1, 10), name_ref("b", span(1, 6, 1, 6)), name_ref("c", span(1, 10, 1, 10)))),
    ));
}

#[test]
fn test_precedence_7()
{
    let e = th_expr("b + -c");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 6),
        name_ref("b", span(1, 1, 1, 1)),
        unary_op(Operator::Sub, span(1, 5, 1, 6), name_ref("c", span(1, 6, 1, 6))),
    ));
}

#[test]
fn test_precedence_8()
{
    let e = th_expr("b + c++");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 5),
        name_ref("b", span(1, 1, 1, 1)),
        Expression::Assignment(
            span(1, 5, 1, 5),
            Operator::AddAssign,
            "c".into(),
            Box::new(number(1, span(1, 5, 1, 5))),
        )
    ));
}

#[test]
fn test_precedence_9()
{
    let e = th_expr("(b + c)++");
    assert!(e == pf_unary_op(
        Operator::Increment,
        span(1, 1, 1, 9),
        enclosed(
            span(1, 1, 1, 7),
            bin_op(
                Operator::Add,
                span(1, 2, 1, 6),
                name_ref("b", span(1, 2, 1, 2)),
                name_ref("c", span(1, 6, 1, 6)),
            )
        )
    ));
}

#[test]
fn test_precedence_10()
{
    let e = th_expr("b + c++");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 5),
        name_ref("b", span(1, 1, 1, 1)),
        Expression::Assignment(
            span(1, 5, 1, 5),
            Operator::AddAssign,
            "c".into(),
            Box::new(number(1, span(1, 5, 1, 5))),
        )
    ));
}

#[test]
fn test_precedence_11()
{
    let e = th_expr("b + c(6)");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 8),
        name_ref("b", span(1, 1, 1, 1)),
        Expression::Call(Call::new(
            "c".into(),
            vec![number(6, span(1, 7, 1, 7))],
            span(1, 5, 1, 8)
        ))
    ));
}

#[test]
fn test_precedence_12()
{
    let e = th_expr("b.d + a.c(6)");
    assert!(e == bin_op(
        Operator::Add,
        span(1, 1, 1, 12),
        bin_op(Operator::Dot, span(1, 1, 1, 3), name_ref("b", span(1, 1, 1, 1)), name_ref("d", span(1, 3, 1, 3))),
        bin_op(Operator::Dot, span(1, 7, 1, 12), name_ref("a", span(1, 7, 1, 7)),
            Expression::Call(Call::new(
                "c".into(),
                vec![number(6, span(1, 11, 1, 11))],
                span(1, 9, 1, 12),
            ))
        )
    ));
}

#[test]
fn test_assign()
{
    let e = th_expr("a = 6 + 9");
    assert!(e == Expression::Assignment(
        span(1, 1, 1, 9),
        Operator::Assign,
        "a".into(),
        Box::new(bin_op(Operator::Add, span(1, 5, 1, 9),
            number(6, span(1, 5, 1, 5)),
            number(9, span(1, 9, 1, 9))
        )),
    ));
}

#[test]
fn test_object_construction()
{
    let e = th_expr("Foo{7, 8}");
    assert!(e == Expression::ObjectConstruction(
        span(1, 1, 1, 9),
        "Foo".into(),
        vec![
            number(7, span(1, 5, 1, 5)),
            number(8, span(1, 8, 1, 8))
        ],
    ));
}
