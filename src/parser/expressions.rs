use ast::*;
use tokenqueue::TokenQueue;
use compileerror::*;
use tokens::*;
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
        Ok(Expression::UnaryOp((op, Box::new(se))))
    } else {
        err(op_pos, ErrorType::InvalidUnaryOperator(op))
    }
}

fn parse_binary_op_rhs(tq: &mut TokenQueue, indent_level: usize, mut lhs: Expression) -> Result<Expression, CompileError>
{
    loop
    {
        println!("parse_binary_op_rhs: lhs {:?}", lhs);

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

        println!("parse_binary_op_rhs: p {} {}", prec, lhs.precedence());
        if prec < lhs.precedence() {
            return Ok(lhs);
        }

        let op = try!(tq.expect_operator());
        println!("parse_binary_op_rhs: op {:?}", op);

        let rhs = try!(parse_expression(tq, indent_level));
        println!("parse_binary_op_rhs: rhs {:?}", rhs);

        match rhs
        {
            Expression::BinaryOp((rhs_op, left, right)) => {
                if rhs_op.precedence() <= prec {
                    let e = Expression::BinaryOp((op, Box::new(lhs), left));
                    lhs = Expression::BinaryOp((rhs_op, Box::new(e), right));
                } else {
                    let e = Expression::BinaryOp((rhs_op, left, right));
                    lhs = Expression::BinaryOp((op, Box::new(lhs), Box::new(e)));
                }
            },
            _ => {
                lhs = Expression::BinaryOp((op, Box::new(lhs), Box::new(rhs)));
            },
        }
    }
}

pub fn parse_function_call(tq: &mut TokenQueue, indent_level: usize, name: String) -> Result<Call, CompileError>
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
    Ok(Call::new(name, args))
}


fn parse_primary_expression(tq: &mut TokenQueue, indent_level: usize, tok: Token) -> Result<Expression, CompileError>
{
    match tok.kind
    {
        TokenKind::OpenParen => {
            let expr = try!(parse_expression(tq, indent_level));
            try!(tq.expect(TokenKind::CloseParen));
            Ok(Expression::Enclosed(Box::new(expr)))
        },

        TokenKind::Identifier(id) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::OpenParen) => parse_function_call(tq, indent_level, id).map(|c| Expression::Call(c)),
                Some(TokenKind::Operator(op)) =>
                    if op.is_binary_operator() {
                        parse_binary_op_rhs(tq, indent_level, Expression::NameRef(id))
                    } else {
                         Ok(Expression::NameRef(id))
                    },
                _ => Ok(Expression::NameRef(id)),
            }
        },

        TokenKind::StringLiteral(s) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::Operator(op)) =>
                    if op.is_binary_operator() {
                        parse_binary_op_rhs(tq, indent_level, Expression::StringLiteral(s))
                    } else {
                        Ok(Expression::StringLiteral(s))
                    },
                _ => Ok(Expression::StringLiteral(s)),
            }
        },

        TokenKind::Number(n) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::Operator(op)) =>
                    if op.is_binary_operator() {
                        parse_binary_op_rhs(tq, indent_level, Expression::Number(n))
                    } else {
                        Ok(Expression::Number(n))
                    },
                _ => Ok(Expression::Number(n)),
            }
        },

        TokenKind::Operator(op) => parse_unary_expression(tq, indent_level, op, tok.pos),

        _ => err(tok.pos, ErrorType::UnexpectedToken(tok)),
    }
}

pub fn parse_expression(tq: &mut TokenQueue, indent_level: usize) -> Result<Expression, CompileError>
{
    if let Some(tok) = try!(tq.pop_if(|tok| !is_end_of_expression(tok, indent_level)))
    {
        let e = try!(parse_primary_expression(tq, indent_level, tok));
        if tq.is_next(TokenKind::Operator(Operator::Increment)) || tq.is_next(TokenKind::Operator(Operator::Decrement)) {
            let op = try!(tq.expect_operator());
            Ok(Expression::PostFixUnaryOp((op, Box::new(e))))
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
use lexer::Lexer;

#[cfg(test)]
fn th_expr(data: &str) -> Expression
{
    let mut cursor = Cursor::new(data);
    let mut tq = Lexer::new().read(&mut cursor).expect("Lexing failed");
    let lvl = tq.expect_indent().expect("Missing indentation");
    parse_expression(&mut tq, lvl).expect("Parsing failed")
}

#[cfg(test)]
fn bin_op(op: Operator, left: Expression, right: Expression) -> Expression
{
    Expression::BinaryOp((op,
        Box::new(left),
        Box::new(right),
    ))
}

#[cfg(test)]
fn unary_op(op: Operator, left: Expression) -> Expression
{
    Expression::UnaryOp((op, Box::new(left)))
}

#[cfg(test)]
fn pf_unary_op(op: Operator, left: Expression) -> Expression
{
    Expression::PostFixUnaryOp((op, Box::new(left)))
}

#[cfg(test)]
fn name_ref(left: &str) -> Expression
{
    Expression::NameRef(left.into())
}

#[cfg(test)]
fn number(left: &str) -> Expression
{
    Expression::Number(left.into())
}

#[cfg(test)]
fn enclosed(left: Expression) -> Expression
{
    Expression::Enclosed(Box::new(left))
}

#[test]
fn test_basic_expressions()
{
    assert!(th_expr("1000") == number("1000"));
    assert!(th_expr("id") == name_ref("id"));
    assert!(th_expr("-1000") == unary_op(Operator::Sub, number("1000")));
    assert!(th_expr("!id") == unary_op(Operator::Not, name_ref("id")));
    assert!(th_expr("++id") == unary_op(Operator::Increment, name_ref("id")));
    assert!(th_expr("--id") == unary_op(Operator::Decrement, name_ref("id")));
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
        let e = th_expr(&format!("a {} b", op_txt));
        println!("Expr: {:?}", e);
        assert!(e == bin_op(op, name_ref("a"), name_ref("b")));
    }
}

#[test]
fn test_precedence()
{
    let e = th_expr("a + b * c");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Add,
        name_ref("a"),
        bin_op(Operator::Mul, name_ref("b"), name_ref("c")),
    ));
}


#[test]
fn test_precedence_2()
{
    let e = th_expr("a * b + c ");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Add,
        bin_op(Operator::Mul, name_ref("a"), name_ref("b")),
        name_ref("c"),
    ));
}

#[test]
fn test_precedence_3()
{
    let e = th_expr("a * b + c / d ");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Add,
        bin_op(Operator::Mul, name_ref("a"), name_ref("b")),
        bin_op(Operator::Div, name_ref("c"), name_ref("d")),
    ));
}

#[test]
fn test_precedence_4()
{
    let e = th_expr("a && b || c && d ");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Or,
        bin_op(Operator::And, name_ref("a"), name_ref("b")),
        bin_op(Operator::And, name_ref("c"), name_ref("d")),
    ));
}

#[test]
fn test_precedence_5()
{
    let e = th_expr("a >= b && c < d");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::And,
        bin_op(Operator::GreaterThanEquals, name_ref("a"), name_ref("b")),
        bin_op(Operator::LessThan, name_ref("c"), name_ref("d")),
    ));
}

#[test]
fn test_precedence_6()
{
    let e = th_expr("a * (b + c)");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Mul,
        name_ref("a"),
        enclosed(bin_op(Operator::Add, name_ref("b"), name_ref("c"))),
    ));
}

#[test]
fn test_precedence_7()
{
    let e = th_expr("b + -c");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b"),
        unary_op(Operator::Sub, name_ref("c")),
    ));
}

#[test]
fn test_precedence_8()
{
    let e = th_expr("b + c++");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b"),
        pf_unary_op(Operator::Increment, name_ref("c")),
    ));
}

#[test]
fn test_precedence_9()
{
    let e = th_expr("(b + c)++");
    println!("Expr: {:?}", e);
    assert!(e == pf_unary_op(
        Operator::Increment,
        enclosed(
            bin_op(
                Operator::Add,
                name_ref("b"),
                name_ref("c"),
            )
        )
    ));
}

#[test]
fn test_precedence_10()
{
    let e = th_expr("b + c++");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b"),
        pf_unary_op(Operator::Increment, name_ref("c"))
    ));
}

#[test]
fn test_precedence_11()
{
    let e = th_expr("b + c(6)");
    println!("Expr: {:?}", e);
    assert!(e == bin_op(
        Operator::Add,
        name_ref("b"),
        Expression::Call(Call::new(
            "c".into(),
            vec![number("6")],
        ))
    ));
}
