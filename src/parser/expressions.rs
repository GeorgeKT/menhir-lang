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
        TokenKind::StringLiteral(_) => false,
        TokenKind::Indent(level) => level < indent_level,
        _ => true,
    }
}

/*
fn is_operator(tok: &Token) -> bool
{
    match tok.kind
    {
        TokenKind::Operator(_) => true,
        _ => false,
    }
}*/

fn parse_unary_expression(tq: &mut TokenQueue, indent_level: usize, op: Operator, op_pos: Pos) -> Result<Expression, CompileError>
{
    if op == Operator::Not || op == Operator::Sub {
        let se = try!(parse_expression(tq, indent_level));
        Ok(Expression::UnaryOp((op, Box::new(se))))
    } else {
        err(op_pos, format!("Invalid unary operator {}", op))
    }
}

fn parse_binary_expression(tq: &mut TokenQueue, lhs: Expression) -> Result<Expression, CompileError>
{
    err(tq.pos(), format!("NYI"))
}

fn parse_function_call(tq: &mut TokenQueue, indent_level: usize, name: String) -> Result<Expression, CompileError>
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
    Ok(Expression::Call((name, args)))
}

fn parse_primary_expression(tq: &mut TokenQueue, indent_level: usize, tok: Token) -> Result<Expression, CompileError>
{
    match tok.kind
    {
        TokenKind::OpenParen => {
            let expr = try!(parse_expression(tq, indent_level));
            try!(tq.expect(TokenKind::CloseParen));
            Ok(expr)
        },

        TokenKind::Identifier(id) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::OpenParen) => parse_function_call(tq, indent_level, id),
                Some(TokenKind::Operator(_)) => parse_binary_expression(tq, Expression::NameRef(id)),
                _ => Ok(Expression::NameRef(id)),
            }
        },

        TokenKind::StringLiteral(s) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::Operator(_)) => parse_binary_expression(tq, Expression::StringLiteral(s)),
                _ => Ok(Expression::StringLiteral(s)),
            }
        },

        TokenKind::Number(n) => {
            let next_kind = tq.peek().map(|tok| tok.kind.clone());
            match next_kind
            {
                Some(TokenKind::Operator(op)) => parse_binary_expression(tq, Expression::Number(n)),
                _ => Ok(Expression::Number(n)),
            }
        },

        TokenKind::Operator(op) => parse_unary_expression(tq, indent_level, op, tok.pos),

        _ => err(tok.pos, format!("Unexpected token {}", tok.kind)),
    }
}

pub fn parse_expression(tq: &mut TokenQueue, indent_level: usize) -> Result<Expression, CompileError>
{
    if let Some(tok) = try!(tq.pop_if(|tok| !is_end_of_expression(tok, indent_level)))
    {
        parse_primary_expression(tq, indent_level, tok)
    }
    else
    {
        err(tq.pos(), format!("Unexpected end of input, expected an expression"))
    }
}
