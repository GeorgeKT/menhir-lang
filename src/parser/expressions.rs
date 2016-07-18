use ast::{Expression, Call, NameRef, MemberAccess, Member, array_lit, array_init, index_op, object_construction, assignment, unary_op, pf_unary_op, bin_op, bin_op2};
use compileerror::{CompileError, Pos, Span, ErrorType, err};
use parser::{Token, TokenKind, TokenQueue, Operator};


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

fn parse_member_access(tq: &mut TokenQueue, indent_level: usize, target: Expression) -> Result<MemberAccess, CompileError>
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
        let next = try!(parse_member_access(tq, indent_level, Expression::NameRef(next_nr)));
        Member::Nested(Box::new(next))
    }
    else
    {
        Member::Var(next_nr)
    };

    let pos = target.span().start;
    Ok(MemberAccess::new(target, member, Span::new(pos, tq.pos())))
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
        if expressions.is_empty() && tq.is_next(TokenKind::SemiColon)
        {
            // [x ; 4]
            try!(tq.pop());
            let (times, _) = try!(tq.expect_int());
            try!(tq.expect(TokenKind::CloseBracket));
            return Ok(array_init(e, times, Span::new(pos, tq.pos())))
        }
        else
        {
            expressions.push(e);
            try!(eat_comma(tq));
        }
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
            try!(parse_member_access(tq, indent_level, lhs).map(|m| Expression::MemberAccess(m)))
        },
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
