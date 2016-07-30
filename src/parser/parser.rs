use std::fs;
use std::io::Read;
use ast::{Expression, Function, Call, NameRef, Type, Argument, Module,
    array_init, array_lit, array_pattern, unary_op, bin_op, sig, to_primitive,
    match_expression, match_case, lambda};
use compileerror::{CompileResult, ErrorCode, Span, Pos, err};
use parser::{TokenQueue, Token, TokenKind, Operator, Lexer};

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

fn eat_comma(tq: &mut TokenQueue) -> CompileResult<()>
{
    if tq.is_next(TokenKind::Comma) {
        try!(tq.pop());
    }
    Ok(())
}

fn parse_number(num: &str, span: Span) -> CompileResult<Expression>
{
    if num.find('.').is_some() || num.find('e').is_some() {
        match num.parse::<f64>() {
            Ok(_) => Ok(Expression::FloatLiteral(span, num.into())),
            Err(_) => err(span.start, ErrorCode::InvalidFloatingPoint, format!("{} is not a valid floating point number", num))
        }
    } else {
        // Should be an integer
        match num.parse::<u64>() {
            Ok(i) => Ok(Expression::IntLiteral(span, i)),
            Err(_) => err(span.start, ErrorCode::InvalidInteger, format!("{} is not a valid integer", num))
        }
    }
}

fn parse_array_literal(tq: &mut TokenQueue, pos: Pos) -> CompileResult<Expression>
{
    let mut expressions = Vec::new();
    while !tq.is_next(TokenKind::CloseBracket)
    {
        let e = try!(parse_expression(tq));
        if expressions.is_empty() && tq.is_next(TokenKind::SemiColon)
        {
            // [x ; 4]
            try!(tq.pop());
            let (times, _) = try!(tq.expect_int());
            try!(tq.expect(TokenKind::CloseBracket));
            return Ok(array_init(e, times, Span::new(pos, tq.pos())));
        }
        else if expressions.is_empty() && tq.is_next(TokenKind::Pipe)
        {
            // array pattern [head | tail]
            let head = try!(e.to_name_ref());
            try!(tq.expect(TokenKind::Pipe));
            let (tail, _) = try!(tq.expect_identifier());
            try!(tq.expect(TokenKind::CloseBracket));
            return Ok(array_pattern(&head.name, &tail, Span::new(pos, tq.pos())));
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

fn parse_name(tq: &mut TokenQueue, id: String, pos: Pos) -> CompileResult<NameRef>
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

fn parse_unary_expression(tq: &mut TokenQueue, op: Operator, op_pos: Pos) -> CompileResult<Expression>
{
    if op == Operator::Not || op == Operator::Sub {
        let se = try!(parse_expression(tq));
        Ok(unary_op(op, se, Span::new(op_pos, tq.pos())))
    } else {
        err(op_pos, ErrorCode::InvalidUnaryOperator, format!("Invalid unary operator {}", op))
    }
}

fn combine_binary_op(op: Operator, lhs: Expression, rhs: Expression) -> Expression
{
    if lhs.is_binary_op() && lhs.precedence() < op.precedence()
    {
        let bop = lhs.to_binary_op().expect("Not a binary op");
        let nrhs = combine_binary_op(op, bop.right.clone(), rhs);
        let span = Span::merge(&bop.left.span(), &nrhs.span());
        bin_op(bop.operator, bop.left, nrhs, span)
    }
    else 
    {
        let span = Span::merge(&lhs.span(), &rhs.span());
        bin_op(op, lhs, rhs, span)
    }
}

fn parse_binary_op_rhs(tq: &mut TokenQueue, mut lhs: Expression) -> CompileResult<Expression>
{
    //use ast::TreePrinter;

    loop
    {
        if tq.peek().map(|tok| is_end_of_expression(tok)).unwrap_or(false) {
            return Ok(lhs);
        }

        let op = try!(tq.expect_operator());
        let next_tok = try!(tq.pop());
        let rhs = try!(parse_expression_start(tq, next_tok));

        /*
        let prec = op.precedence();
        println!("operator {} prec {}", op, prec);
        println!("rhs: {}", rhs.precedence());
        rhs.print(0);
        println!("lhs: {}", lhs.precedence());
        lhs.print(0);
*/
        lhs = combine_binary_op(op, lhs, rhs);
/*
        println!("new lhs: {}", lhs.precedence());
        lhs.print(0);
        println!("----------------------");
        */
    }
}

fn parse_function_call(tq: &mut TokenQueue, name: NameRef) -> CompileResult<Call>
{
    let mut args = Vec::new();
    while !tq.is_next(TokenKind::CloseParen)
    {
        let expr = try!(parse_expression(tq));
        args.push(expr);
        try!(eat_comma(tq));
    }

    try!(tq.pop());
    let pos = name.span.start;
    Ok(Call::new(name, args, Span::new(pos, tq.pos())))
}



fn parse_type(tq: &mut TokenQueue) -> CompileResult<Type>
{
    if tq.is_next(TokenKind::Dollar)
    {
        try!(tq.pop());
        let (name, _pos) = try!(tq.expect_identifier());
        Ok(Type::Generic(name))
    }
    else if tq.is_next(TokenKind::OpenBracket)
    {
        try!(tq.pop());
        let at = try!(parse_type(tq));
        try!(tq.expect(TokenKind::CloseBracket));
        Ok(Type::Array(Box::new(at)))
    }
    else if tq.is_next(TokenKind::OpenParen)
    {
        // Function signature: (a, b) -> c
        try!(tq.pop());
        let mut args = Vec::new();
        while !tq.is_next(TokenKind::CloseParen)
        {
            args.push(try!(parse_type(tq)));
            try!(eat_comma(tq));
        }
        try!(tq.expect(TokenKind::CloseParen));
        try!(tq.expect(TokenKind::Arrow));
        let ret = try!(parse_type(tq));
        Ok(Type::Func(args, Box::new(ret)))
    }
    else
    {
        let (name, _pos) = try!(tq.expect_identifier());
        Ok(to_primitive(&name).unwrap_or(Type::Complex(name)))
    }
}

fn parse_function_argument(tq: &mut TokenQueue, type_is_optional: bool) -> CompileResult<Argument>
{
    let (name, span) = try!(tq.expect_identifier());
    let typ = if tq.is_next(TokenKind::Colon) {
        try!(tq.expect(TokenKind::Colon));
        try!(parse_type(tq))
    } else if type_is_optional{
        Type::Unknown
    } else {
        return err(span.start, ErrorCode::MissingType, format!("Type not specified of function argument {}", name));
    };

    Ok(Argument::new(name, typ, Span::new(span.start, tq.pos())))
}

fn parse_function_arguments(tq: &mut TokenQueue, type_is_optional: bool) -> CompileResult<Vec<Argument>>
{
    let mut args = Vec::new();
    while !tq.is_next(TokenKind::CloseParen)
    {
        let arg = try!(parse_function_argument(tq, type_is_optional));
        args.push(arg);
        try!(eat_comma(tq));
    }

    try!(tq.expect(TokenKind::CloseParen));
    Ok(args)
}

fn parse_function_definition(tq: &mut TokenQueue, name: NameRef) -> CompileResult<Function>
{
    let args = try!(parse_function_arguments(tq, false));

    let ret_type = if tq.is_next(TokenKind::Arrow) {
        try!(tq.pop());
        try!(parse_type(tq))
    } else {
        Type::Void
    };

    let sig_span_end = tq.pos();
    try!(tq.expect(TokenKind::Assign));
    let expr = try!(parse_expression(tq));

    Ok(Function::new(
        sig(&name.name, ret_type, args, Span::new(name.span.start, sig_span_end)),
        true,
        expr,
        Span::new(name.span.start, tq.pos())))
}

fn parse_match(tq: &mut TokenQueue, start: Pos) -> CompileResult<Expression>
{
    let target = try!(parse_expression(tq));
    let mut cases = Vec::new();
    loop
    {
        let c = try!(parse_expression(tq));
        try!(tq.expect(TokenKind::FatArrow));
        let t = try!(parse_expression(tq));
        cases.push(match_case(c, t));
        if tq.is_next(TokenKind::Comma) { // Continue, while we see a comman
            try!(tq.pop());
        } else {
            break;
        }
    }
    Ok(Expression::Match(match_expression(target, cases, Span::new(start, tq.pos()))))
}

fn parse_lambda(tq: &mut TokenQueue, pos: Pos) -> CompileResult<Expression>
{
    try!(tq.expect(TokenKind::OpenParen));
    let args = try!(parse_function_arguments(tq, true));
    try!(tq.expect(TokenKind::Arrow));
    let expr = try!(parse_expression(tq));
    Ok(Expression::Lambda(lambda(args, expr, Span::new(pos, tq.pos()))))
}

fn parse_expression_start(tq: &mut TokenQueue, tok: Token) -> CompileResult<Expression>
{
    match tok.kind
    {
        TokenKind::True => {
            Ok(Expression::BoolLiteral(tok.span, true))
        },

        TokenKind::False => {
            Ok(Expression::BoolLiteral(tok.span, false))
        },

        TokenKind::Lambda => {
            parse_lambda(tq, tok.span.start)
        },

        TokenKind::Match => {
            parse_match(tq, tok.span.start)
        },

        TokenKind::OpenParen => {
            let expr = try!(parse_expression(tq));
            try!(tq.expect(TokenKind::CloseParen));
            Ok(Expression::Enclosed(Span::new(tok.span.start, tq.pos()), Box::new(expr)))
        },

        TokenKind::OpenBracket => {
            parse_array_literal(tq, tok.span.start)
        },

        TokenKind::Identifier(id) => {
            let nr = try!(parse_name(tq, id, tok.span.start));
            if tq.is_next(TokenKind::OpenParen) 
            {
                try!(tq.pop());
                if tq.is_next_at(1, TokenKind::Colon) || tq.is_next_at(1, TokenKind::Arrow) || tq.is_next_at(1, TokenKind::Assign)
                {
                    println!("parse_function_decl");
                    parse_function_definition(tq, nr).map(|f| Expression::Function(f))
                }
                else
                {
                    println!("parse_function_call");
                    parse_function_call(tq, nr).map(|c| Expression::Call(c))
                }
            } 
            else 
            {
                Ok(Expression::NameRef(nr))    
            }       
        },

        TokenKind::StringLiteral(s) => {
            Ok(Expression::StringLiteral(tok.span, s))
        },

        TokenKind::Number(n) => {
            Ok(try!(parse_number(&n, tok.span)))
        },

        TokenKind::Operator(op) => parse_unary_expression(tq, op, tok.span.start),

        _ => err(tok.span.start, ErrorCode::UnexpectedToken, format!("Unexpected token {}", tok)),
    }
}

fn parse_expression_continued(tq: &mut TokenQueue, lhs: Expression) -> CompileResult<Expression>
{
    if is_end_of_expression(tq.peek().expect("Unexpected EOF")) {
        return Ok(lhs);
    }

    let next = try!(tq.pop());
    match next.kind
    {
        TokenKind::Operator(op) if op.is_binary_operator() => {
            tq.push_front(next);
            parse_binary_op_rhs(tq, lhs)
        },
        _ => {
            tq.push_front(next);
            Ok(lhs)
        },
    }
}

pub fn parse_expression(tq: &mut TokenQueue) -> CompileResult<Expression>
{
    let tok = try!(tq.pop());
    let lhs = try!(parse_expression_start(tq, tok));
    parse_expression_continued(tq, lhs)
}

pub fn parse_expression_list(tq: &mut TokenQueue) -> CompileResult<Vec<Expression>>
{
    let mut expressions = Vec::new();
    while !tq.is_next(TokenKind::EOF)
    {
        expressions.push(try!(parse_expression(tq)));
    }

    Ok(expressions)
}


pub fn parse_file(file_path: &str) -> CompileResult<Module>
{
    use std::path::Path;
    use std::ffi::OsStr;

    let mut file = try!(fs::File::open(file_path));
    let path = Path::new(file_path);
    let module_name: &OsStr = path.file_stem().expect("Invalid filename");
    parse_module(&mut file, module_name.to_str().expect("Invalid UTF8 filename"))
}

pub fn parse_module<Input: Read>(input: &mut Input, name: &str) -> CompileResult<Module>
{
    let mut tq = try!(Lexer::new().read(input));   
    let expressions = try!(parse_expression_list(&mut tq));
     Ok(Module{
        name: name.into(),
        expressions: expressions,
    })
}
