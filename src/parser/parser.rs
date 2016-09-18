use std::fs;
use std::io::Read;
use ast::*;
use compileerror::{CompileResult, ErrorCode, err};
use parser::{TokenQueue, Token, TokenKind, Operator, Lexer, ParserOptions};
use span::{Span};

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

fn parse_number(num: &str, span: &Span) -> CompileResult<Literal>
{
    if num.find('.').is_some() || num.find('e').is_some() {
        match num.parse::<f64>() {
            Ok(_) => Ok(Literal::Float(span.clone(), num.into())),
            Err(_) => err(span, ErrorCode::InvalidFloatingPoint, format!("{} is not a valid floating point number", num))
        }
    } else {
        // Should be an integer
        match num.parse::<u64>() {
            Ok(i) => Ok(Literal::Int(span.clone(), i)),
            Err(_) => err(span, ErrorCode::InvalidInteger, format!("{} is not a valid integer", num))
        }
    }
}

fn parse_array_literal(tq: &mut TokenQueue, span: &Span) -> CompileResult<Literal>
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
            return Ok(array_lit(vec![e; times as usize], span.expanded(tq.pos())));
        }
        else
        {
            expressions.push(e);
            try!(eat_comma(tq));
        }
    }

    try!(tq.expect(TokenKind::CloseBracket));
    Ok(array_lit(expressions, span.expanded(tq.pos())))
}

fn parse_name(tq: &mut TokenQueue, id: String, span: &Span) -> CompileResult<NameRef>
{
    let mut name = id;
    while tq.is_next(TokenKind::DoubleColon)
    {
        try!(tq.pop());
        let (next, _) = try!(tq.expect_identifier());
        name.push_str("::");
        name.push_str(&next);
    }

    Ok(NameRef::new(name, span.expanded(tq.pos())))
}

fn parse_unary_expression(tq: &mut TokenQueue, op: Operator, op_span: &Span) -> CompileResult<Expression>
{
    if op == Operator::Not || op == Operator::Sub {
        let se = try!(parse_expression(tq));
        Ok(unary_op(op, se, op_span.expanded(tq.pos())))
    } else {
        err(op_span, ErrorCode::InvalidUnaryOperator, format!("Invalid unary operator {}", op))
    }
}

fn combine_binary_op(op: Operator, lhs: Expression, rhs: Expression) -> Expression
{
    if lhs.is_binary_op() && lhs.precedence() < op.precedence()
    {
        let bop = lhs.to_binary_op().expect("Not a binary op");
        let nrhs = combine_binary_op(op, bop.right.clone(), rhs);
        let span = Span::merge(&bop.left.span(), &nrhs.span());
        bin_op(bop.operator, bop.left.clone(), nrhs, span)
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

        if !tq.is_next_operator() {
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

fn parse_list<T, P>(tq: &mut TokenQueue, separator: TokenKind, end_token: TokenKind, parse_element: P) -> CompileResult<Vec<T>>
    where P: Fn(&mut TokenQueue) -> CompileResult<T>
{
    let mut elements = Vec::new();
    while !tq.is_next(end_token.clone())
    {
        let e = try!(parse_element(tq));
        elements.push(e);
        if !tq.is_next(separator.clone()) {
            break;
        } else {
            try!(tq.pop());
        }
    }

    try!(tq.expect(end_token));
    Ok(elements)
}

fn parse_comma_separated_list<T, P>(tq: &mut TokenQueue, end_token: TokenKind, parse_element: P) -> CompileResult<Vec<T>>
    where P: Fn(&mut TokenQueue) -> CompileResult<T>
{
    parse_list(tq, TokenKind::Comma, end_token, parse_element)
}

fn parse_function_call(tq: &mut TokenQueue, name: NameRef) -> CompileResult<Call>
{
    let args = try!(parse_comma_separated_list(tq, TokenKind::CloseParen, parse_expression));
    let span = name.span.expanded(tq.pos());
    Ok(Call::new(name, args, span))
}

fn parse_generic_arg_list(tq: &mut TokenQueue) -> CompileResult<Vec<Type>>
{
    if !tq.is_next(TokenKind::Operator(Operator::LessThan)) {
        return Ok(Vec::new());
    }
    try!(tq.pop());
    parse_comma_separated_list(tq, TokenKind::Operator(Operator::GreaterThan), parse_type)
}

fn parse_type(tq: &mut TokenQueue) -> CompileResult<Type>
{
    if tq.is_next(TokenKind::Dollar)
    {
        try!(tq.pop());
        let (name, _span) = try!(tq.expect_identifier());
        Ok(Type::Generic(name))
    }
    else if tq.is_next(TokenKind::OpenBracket)
    {
        try!(tq.pop());
        let at = try!(parse_type(tq));
        try!(tq.expect(TokenKind::CloseBracket));
        Ok(array_type(at))
    }
    else if tq.is_next(TokenKind::OpenParen)
    {
        // Function signature: (a, b) -> c
        try!(tq.pop());
        let args = try!(parse_comma_separated_list(tq, TokenKind::CloseParen, parse_type));
        try!(tq.expect(TokenKind::Arrow));
        let ret = try!(parse_type(tq));
        Ok(func_type(args, ret))
    }
    else if tq.is_next(TokenKind::OpenCurly)
    {
        // Anonymous struct
        let members = try!(parse_struct_members(tq));
        Ok(struct_type(
            members.into_iter().map(|m| StructMember{
                name: m.name,
                typ: m.typ
            }).collect()
        ))
    }
    else
    {
        let (name, _pos) = try!(tq.expect_identifier());
        match to_primitive(&name)
        {
            Some(t) => Ok(t),
            None => {
                let generic_args = try!(parse_generic_arg_list(tq));
                Ok(unresolved_type(&name, generic_args))
            },
        }
    }
}

fn parse_function_argument(tq: &mut TokenQueue, type_is_optional: bool) -> CompileResult<Argument>
{
    let (name, span) = try!(tq.expect_identifier());
    let typ = if tq.is_next(TokenKind::Colon) {
        try!(tq.expect(TokenKind::Colon));
        try!(parse_type(tq))
    } else if type_is_optional{
        Type::Generic(name.clone()) // If the type is not known threat it as generic arg
    } else {
        return err(&span, ErrorCode::MissingType, format!("Type not specified of function argument {}", name));
    };

    Ok(Argument::new(name, typ, span.expanded(tq.pos())))
}

fn parse_function_arguments(tq: &mut TokenQueue, type_is_optional: bool) -> CompileResult<Vec<Argument>>
{
    parse_comma_separated_list(tq, TokenKind::CloseParen, |tq| parse_function_argument(tq, type_is_optional))
}

fn parse_external_function(tq: &mut TokenQueue, span: &Span) -> CompileResult<ExternalFunction>
{
    let (name, name_span) = try!(tq.expect_identifier());

    try!(tq.expect(TokenKind::OpenParen));
    let args = try!(parse_function_arguments(tq, false));
    let ret_type = if tq.is_next(TokenKind::Arrow) {
        try!(tq.pop());
        try!(parse_type(tq))
    } else {
        Type::Void
    };

    let sig_span_end = tq.pos();
    Ok(ExternalFunction::new(
        sig(&name, ret_type, args, name_span.expanded(sig_span_end)),
        span.expanded(tq.pos()),
    ))
}

fn parse_function_declaration(tq: &mut TokenQueue, namespace: &str, name: &str, span: &Span) -> CompileResult<Function>
{
    try!(tq.expect(TokenKind::OpenParen));
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

    let full_name = if name != "main" {
        namespaced(namespace, name)
    } else {
        name.into()
    };

    let func_span = span.expanded(expr.span().end);
    Ok(Function::new(
        sig(&full_name, ret_type, args, span.expanded(sig_span_end)),
        true,
        expr,
        func_span
    ))
}

fn parse_struct_pattern(tq: &mut TokenQueue, name: &str, span: &Span) -> CompileResult<Pattern>
{
    let bindings = try!(parse_comma_separated_list(tq, TokenKind::CloseCurly, |tq| {
        let (name, _) = try!(tq.expect_identifier());
        Ok(name)
    }));
    Ok(Pattern::Struct(struct_pattern(name, bindings, Vec::new(), Type::Unknown, span.expanded(tq.pos()))))
}

pub fn parse_pattern(tq: &mut TokenQueue) -> CompileResult<Pattern>
{
    let tok = try!(tq.pop());
    match tok.kind
    {
        TokenKind::Number(ref num) => parse_number(num, &tok.span).map(|lit| Pattern::Literal(lit)),
        TokenKind::True => Ok(Pattern::Literal(Literal::Bool(tok.span, true))),
        TokenKind::False => Ok(Pattern::Literal(Literal::Bool(tok.span, false))),
        TokenKind::CharLiteral(c) => Ok(Pattern::Literal(Literal::Char(tok.span, c as u8))),
        TokenKind::StringLiteral(s) => Ok(Pattern::Literal(Literal::String(tok.span, s))),

        TokenKind::OpenBracket => {
            if tq.is_next(TokenKind::CloseBracket)
            {
                try!(tq.pop());
                Ok(empty_array_pattern(tok.span.expanded(tq.pos())))
            }
            else if tq.is_next_at(1, TokenKind::Pipe)
            {
                let (head, _head_span) = try!(tq.expect_identifier());
                try!(tq.expect(TokenKind::Pipe));
                let (tail, _) = try!(tq.expect_identifier());
                try!(tq.expect(TokenKind::CloseBracket));
                Ok(array_pattern(&head, &tail, tok.span.expanded(tq.pos())))
            }
            else
            {
                let al = try!(parse_array_literal(tq, &tok.span));
                Ok(Pattern::Literal(al))
            }
        },

        TokenKind::OpenCurly => {
            parse_struct_pattern(tq, "", &tok.span)
        },

        TokenKind::Identifier(id) => {
            if id == "_" {
                Ok(Pattern::Any(tok.span))
            }
            else if tq.is_next(TokenKind::OpenCurly) {
                try!(tq.pop());
                parse_struct_pattern(tq, &id, &tok.span)
            } else {
                Ok(Pattern::Name(NameRef::new(id, tok.span)))
            }
        },
        _ => err(&tok.span, ErrorCode::UnexpectedToken, format!("Unexpected token '{}'", tok)),
    }
}

fn parse_match(tq: &mut TokenQueue, span: &Span) -> CompileResult<Expression>
{
    let target = try!(parse_expression(tq));
    try!(tq.expect(TokenKind::Colon));
    let mut cases = Vec::new();
    loop
    {
        let pattern = try!(parse_pattern(tq));
        try!(tq.expect(TokenKind::FatArrow));
        let t = try!(parse_expression(tq));
        let case_span = pattern.span().expanded(tq.pos());
        cases.push(match_case(pattern, t, case_span));
        if tq.is_next(TokenKind::Comma) { // Continue, while we see a comman
            try!(tq.pop());
        } else {
            break;
        }
    }
    Ok(match_expression(target, cases, span.expanded(tq.pos())))
}

fn parse_lambda(tq: &mut TokenQueue, span: &Span) -> CompileResult<Expression>
{
    try!(tq.expect(TokenKind::OpenParen));
    let args = try!(parse_function_arguments(tq, true));
    try!(tq.expect(TokenKind::Arrow));
    let expr = try!(parse_expression(tq));
    Ok(lambda(args, expr, span.expanded(tq.pos())))
}

fn parse_let(tq: &mut TokenQueue, span: &Span) -> CompileResult<Expression>
{
    let mut bindings = Vec::new();
    while !tq.is_next(TokenKind::In) && !tq.is_next(TokenKind::SemiColon) && !tq.is_next(TokenKind::CloseParen)
    {
        let (name, span) = try!(tq.expect_identifier());
        try!(tq.expect(TokenKind::Assign));
        let init = try!(parse_expression(tq));
        bindings.push(let_binding(name, init, span.expanded(tq.pos())));
        try!(eat_comma(tq));
    }

    if tq.is_next(TokenKind::In)
    {
        try!(tq.expect(TokenKind::In));
        let e = try!(parse_expression(tq));
        Ok(let_expression(bindings, e, span.expanded(tq.pos())))
    }
    else
    {
        Ok(let_bindings(bindings, span.expanded(tq.pos())))
    }

}

fn parse_if(tq: &mut TokenQueue, span: &Span) -> CompileResult<Expression>
{
    let cond = try!(parse_expression(tq));
    try!(tq.expect(TokenKind::Colon));
    let on_true = try!(parse_expression(tq));
    try!(tq.expect(TokenKind::Else));
    let on_false = try!(parse_expression(tq));
    Ok(if_expression(cond, on_true, on_false, span.expanded(tq.pos())))
}

fn parse_type_declaration(tq: &mut TokenQueue, namespace: &str, span: &Span) -> CompileResult<TypeDeclaration>
{
    let (name, _) = try!(tq.expect_identifier());
    try!(tq.expect(TokenKind::Assign));
    if tq.is_next(TokenKind::OpenCurly)
    {
        let sd = try!(parse_struct_type(tq, namespace, &name, span));
        Ok(TypeDeclaration::Struct(sd))
    }
    else if tq.is_next_at(1, TokenKind::Pipe) || tq.is_next_at(1, TokenKind::OpenCurly)
    {
        let st = try!(parse_sum_type(tq, namespace, &name, span));
        Ok(TypeDeclaration::Sum(st))
    }
    else
    {
        let typ = try!(parse_type(tq));
        Ok(TypeDeclaration::Alias(type_alias(&namespaced(namespace, &name), typ, span.expanded(tq.pos()))))
    }
}


fn parse_sum_type(tq: &mut TokenQueue, namespace: &str, name: &str, span: &Span) -> CompileResult<SumTypeDeclaration>
{
    let mut cases = Vec::new();
    loop
    {
        let (case_name, case_name_span) = try!(tq.expect_identifier());
        if tq.is_next(TokenKind::Pipe)
        {
            cases.push(sum_type_case_decl(&namespaced(namespace, &case_name), None, case_name_span));
            try!(tq.pop());
        }
        else if tq.is_next(TokenKind::OpenCurly)
        {
            let sd = try!(parse_struct_type(tq, namespace, &case_name, &case_name_span));
            cases.push(sum_type_case_decl(&namespaced(namespace, &case_name), Some(sd), case_name_span.expanded(tq.pos())));
            if tq.is_next(TokenKind::Pipe)
            {
                try!(tq.pop());
            }
            else
            {
                break;
            }
        }
        else
        {
            cases.push(sum_type_case_decl(&namespaced(namespace, &case_name), None, case_name_span));
            break;
        }
    }

    Ok(sum_type_decl(&namespaced(namespace, &name), cases, span.expanded(tq.pos())))
}

fn namespaced(namespace: &str, name: &str) -> String
{
    format!("{}::{}", namespace, name)
}

fn parse_struct_member(tq: &mut TokenQueue, file_name: &str) -> CompileResult<StructMemberDeclaration>
{
    if tq.is_next_at(1, TokenKind::Colon) // Named struct member
    {
        let (member_name, member_name_span) = try!(tq.expect_identifier());
        try!(tq.expect(TokenKind::Colon));
        let typ = try!(parse_type(tq));
        Ok(struct_member_declaration(&member_name, typ, member_name_span.expanded(tq.pos())))
    }
    else // Anonymous struct member
    {
        let span_start = tq.pos();
        let typ = try!(parse_type(tq));
        Ok(struct_member_declaration("", typ, Span::new(file_name, span_start, tq.pos())))
    }
}

fn parse_struct_members(tq: &mut TokenQueue) -> CompileResult<Vec<StructMemberDeclaration>>
{
    let span = try!(tq.expect(TokenKind::OpenCurly)).span;
    parse_comma_separated_list(
        tq,
        TokenKind::CloseCurly,
        |tq| parse_struct_member(tq, &span.file)
    )
}

fn parse_struct_type(tq: &mut TokenQueue, namespace: &str, name: &str, span: &Span) -> CompileResult<StructDeclaration>
{
    let members = try!(parse_struct_members(tq));
    Ok(struct_declaration(&namespaced(namespace, name), members, span.expanded(tq.pos())))
}

fn parse_struct_initializer(tq: &mut TokenQueue, name: &str, name_span: &Span) -> CompileResult<Expression>
{
    let expressions = try!(parse_comma_separated_list(tq, TokenKind::CloseCurly, parse_expression));
    Ok(Expression::StructInitializer(
        struct_initializer(name, expressions, name_span.expanded(tq.pos()))
    ))
}

fn parse_struct_member_access(tq: &mut TokenQueue, name: NameRef) -> CompileResult<Expression>
{
    let mut members = Vec::new();
    while tq.is_next(TokenKind::Operator(Operator::Dot))
    {
        try!(tq.pop());
        let next = try!(tq.pop());
        match next.kind
        {
            TokenKind::Identifier(name) => {
                members.push(MemberAccessType::ByName(name));
            },

            TokenKind::Number(idx) => {
                let index = if idx.ends_with(".") {
                    // a.5.b
                    // We have something mistaken as a float number
                    // This is really a problem in the lexer,
                    // but the lexer hasn't got enough information to disambiguate between a float number
                    // and a struct member access with a number
                    tq.push_front(Token::new(TokenKind::Operator(Operator::Dot), next.span.clone()));
                    let real_idx = &idx[0..idx.len() - 1];
                    real_idx.parse::<usize>().expect("Internal Compiler Error: Parsed number is not valid")
                } else {
                    idx.parse::<usize>().expect("Internal Compiler Error: Parsed number is not valid")
                };
                members.push(MemberAccessType::ByIndex(index));
            }

            _ => return err(&next.span, ErrorCode::UnexpectedToken, format!("Expecting integer or an identifier")),
        }
    }

    Ok(Expression::StructMemberAccess(struct_member_access(&name.name, members, name.span.expanded(tq.pos()))))
}

fn parse_block(tq: &mut TokenQueue, start: &Span) -> CompileResult<Expression>
{
    let expressions = try!(parse_list(tq, TokenKind::SemiColon, TokenKind::CloseParen, parse_expression));
    Ok(block(expressions, start.expanded(tq.pos())))
}

fn parse_expression_start(tq: &mut TokenQueue, tok: Token) -> CompileResult<Expression>
{
    match tok.kind
    {
        TokenKind::True => {
            Ok(Expression::Literal(Literal::Bool(tok.span, true)))
        },

        TokenKind::False => {
            Ok(Expression::Literal(Literal::Bool(tok.span, false)))
        },

        TokenKind::CharLiteral(c) => {
            Ok(Expression::Literal(Literal::Char(tok.span, c as u8)))
        },

        TokenKind::Lambda => {
            parse_lambda(tq, &tok.span)
        },

        TokenKind::Match => {
            parse_match(tq, &tok.span)
        },

        TokenKind::Let => {
            parse_let(tq, &tok.span)
        },

        TokenKind::If => {
            parse_if(tq, &tok.span)
        },

        TokenKind::OpenParen => {
            parse_block(tq, &tok.span)
        },

        TokenKind::OpenBracket => {
            parse_array_literal(tq, &tok.span).map(|al| Expression::Literal(al))
        },

        TokenKind::OpenCurly => {
            parse_struct_initializer(tq, "", &tok.span)
        },

        TokenKind::Identifier(id) => {
            let nr = try!(parse_name(tq, id, &tok.span));
            if tq.is_next(TokenKind::OpenParen)
            {
                try!(tq.pop());
                parse_function_call(tq, nr).map(|c| Expression::Call(c))
            }
            else if tq.is_next(TokenKind::OpenCurly)
            {
                try!(tq.expect(TokenKind::OpenCurly));
                parse_struct_initializer(tq, &nr.name, &nr.span)
            }
            else if tq.is_next(TokenKind::Operator(Operator::Dot))
            {
                parse_struct_member_access(tq, nr)
            }
            else
            {
                Ok(Expression::NameRef(nr))
            }
        },

        TokenKind::StringLiteral(s) => {
            Ok(Expression::Literal(Literal::String(tok.span, s)))
        },

        TokenKind::Number(n) => {
            parse_number(&n, &tok.span).map(|n| Expression::Literal(n))
        },

        TokenKind::Operator(op) => parse_unary_expression(tq, op, &tok.span),

        _ => err(&tok.span, ErrorCode::UnexpectedToken, format!("Unexpected token '{}'", tok)),
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

pub fn parse_file(options: &ParserOptions, file_path: &str) -> CompileResult<Module>
{
    use std::path::Path;
    use std::ffi::OsStr;

    let mut file = try!(fs::File::open(file_path));
    let path = Path::new(file_path);
    let module_name: &OsStr = path.file_stem().expect("Invalid filename");
    parse_module(options, &mut file, module_name.to_str().expect("Invalid UTF8 filename"), file_path)
}

fn parse_import(options: &ParserOptions, import_name: &str) -> CompileResult<Module>
{
    let file_name = format!("{}.cobra", import_name);
    for dir in &options.import_dirs {
        let dwf = dir.join(&file_name);
        if let Ok(mut file) = fs::File::open(&dwf) {
            return parse_module(options, &mut file, import_name, dwf.to_str().expect("Invalid file name"));
        }
    }

    err(&Span::default(), ErrorCode::FileNotFound, format!("Unable to find file for import {}", import_name))
}

pub fn parse_module<Input: Read>(options: &ParserOptions, input: &mut Input, name: &str, file_name: &str) -> CompileResult<Module>
{
    let mut tq = try!(Lexer::new(file_name).read(input));
    let mut module = Module::new(name);
    let namespace = name;

    while !tq.is_next(TokenKind::EOF)
    {
        let tok = try!(tq.pop());
        match tok.kind
        {
            TokenKind::Type => {
                let sd = try!(parse_type_declaration(&mut tq, namespace, &tok.span));
                if module.types.contains_key(sd.name()) {
                    return err(&sd.span(), ErrorCode::RedefinitionOfStruct, format!("Type {} redefined", sd.name()));
                }
                module.types.insert(sd.name().into(), sd);
            },

            TokenKind::Extern => {
                let ext_func = try!(parse_external_function(&mut tq, &tok.span));
                if module.externals.contains_key(&ext_func.sig.name) {
                    return err(&ext_func.span, ErrorCode::RedefinitionOfFunction, format!("External function {} redefined", ext_func.sig.name));
                }
                module.externals.insert(ext_func.sig.name.clone(), ext_func);
            },

            TokenKind::Import => {
                loop
                {
                    let (name, _) = try!(tq.expect_identifier());
                    if !module.imports.contains(&name) {
                        let m = try!(parse_import(options, &name));
                        module.import(&m);
                    }

                    if tq.is_next(TokenKind::Comma) {
                        try!(tq.pop());
                    } else {
                        break;
                    }
                }
            },

            TokenKind::Identifier(ref id) => {
                let func = try!(parse_function_declaration(&mut tq, namespace, &id, &tok.span));
                if module.functions.contains_key(&func.sig.name) {
                    return err(&func.span, ErrorCode::RedefinitionOfFunction, format!("Function {} redefined", func.sig.name));
                }
                module.functions.insert(func.sig.name.clone(), func);
            }
            _ => {
                println!("tok: {:?}", tok);
                return err(&tok.span, ErrorCode::ExpressionNotAllowedAtTopLevel, format!("Expression is not allowed at toplevel"));
            }
        }
    }

    Ok(module)
}
