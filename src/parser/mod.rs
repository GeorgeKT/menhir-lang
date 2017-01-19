mod lexer;
mod tokens;
mod tokenqueue;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub use self::tests::{th_expr, th_mod};

use std::path::PathBuf;
use std::fs;
use std::io::Read;
use std::rc::Rc;
use ast::*;
use compileerror::{CompileResult, ErrorCode, err};
use span::{Span};
use self::tokenqueue::{TokenQueue};
use self::lexer::{Lexer};
use self::tokens::{Token, TokenKind};

pub struct ParserOptions
{
    pub import_dirs: Vec<PathBuf>,
}

impl Default for ParserOptions
{
    fn default() -> Self
    {
        ParserOptions{
            import_dirs: Vec::new(),
        }
    }
}

fn is_end_of_expression(tok: &Token) -> bool
{
    match tok.kind
    {
        TokenKind::Operator(_) |
        TokenKind::Number(_) |
        TokenKind::Identifier(_) |
        TokenKind::StringLiteral(_) |
        TokenKind::Assign |
        TokenKind::OpenParen |
        TokenKind::OpenBracket |
        TokenKind::OpenCurly => false,
        _ => true,
    }
}

fn eat_comma(tq: &mut TokenQueue) -> CompileResult<()>
{
    if tq.is_next(TokenKind::Comma) {
        tq.pop()?;
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
        let e = parse_expression(tq)?;
        if expressions.is_empty() && tq.is_next(TokenKind::SemiColon)
        {
            // [x ; 4]
            tq.pop()?;
            let (times, _) = tq.expect_int()?;
            tq.expect(TokenKind::CloseBracket)?;
            return Ok(array_lit(vec![e; times as usize], span.expanded(tq.pos())));
        }
        else
        {
            expressions.push(e);
            eat_comma(tq)?;
        }
    }

    tq.expect(TokenKind::CloseBracket)?;
    Ok(array_lit(expressions, span.expanded(tq.pos())))
}

fn parse_name(tq: &mut TokenQueue, id: String, span: &Span) -> CompileResult<NameRef>
{
    let mut name = id;
    while tq.is_next(TokenKind::DoubleColon)
    {
        tq.pop()?;
        let (next, _) = tq.expect_identifier()?;
        name.push_str("::");
        name.push_str(&next);
    }

    Ok(NameRef::new(name, span.expanded(tq.pos())))
}

fn parse_unary_expression(tq: &mut TokenQueue, op: Operator, op_span: &Span) -> CompileResult<Expression>
{
    if op == Operator::Not || op == Operator::Sub {
        let se = parse_expression(tq)?;
        Ok(unary_op(op, se, op_span.expanded(tq.pos())))
    } else {
        err(op_span, ErrorCode::InvalidUnaryOperator, format!("Invalid unary operator {}", op))
    }
}

fn combine_binary_op(op: Operator, lhs: Expression, rhs: Expression) -> Expression
{
    if lhs.is_binary_op() && lhs.precedence() < op.precedence()
    {
        let bop = lhs.extract_binary_op().expect("Not a binary op");
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

        let op = tq.expect_operator()?;
        let next_tok = tq.pop()?;
        let rhs = parse_expression_start(tq, next_tok)?;

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

#[derive(Clone, Copy, Eq, PartialEq)]
enum ListEnd
{
    Separator,
    EndToken,
}

fn parse_list<T, P>(tq: &mut TokenQueue, separator: TokenKind, end_token: TokenKind, parse_element: P) -> CompileResult<(Vec<T>, ListEnd)>
    where P: Fn(&mut TokenQueue) -> CompileResult<T>
{
    let mut list_end = ListEnd::Separator;
    let mut elements = Vec::new();
    while !tq.is_next(end_token.clone())
    {
        let e = parse_element(tq)?;
        elements.push(e);
        if !tq.is_next(separator.clone()) {
            list_end = ListEnd::EndToken;
            break;
        } else {
            tq.pop()?;
        }
    }

    tq.expect(end_token)?;
    Ok((elements, list_end))
}

fn parse_comma_separated_list<T, P>(tq: &mut TokenQueue, end_token: TokenKind, parse_element: P) -> CompileResult<(Vec<T>, ListEnd)>
    where P: Fn(&mut TokenQueue) -> CompileResult<T>
{
    parse_list(tq, TokenKind::Comma, end_token, parse_element)
}

fn parse_function_call(tq: &mut TokenQueue, name: NameRef) -> CompileResult<Call>
{
    let (args, _) = parse_comma_separated_list(tq, TokenKind::CloseParen, parse_expression)?;
    let span = name.span.expanded(tq.pos());
    Ok(Call::new(name, args, span))
}

fn parse_generic_arg_list(tq: &mut TokenQueue) -> CompileResult<Vec<Type>>
{
    if !tq.is_next(TokenKind::Operator(Operator::LessThan)) {
        return Ok(Vec::new());
    }
    tq.pop()?;
    let (args, _) = parse_comma_separated_list(tq, TokenKind::Operator(Operator::GreaterThan), parse_type)?;
    Ok(args)
}

fn parse_start_of_type(tq: &mut TokenQueue) -> CompileResult<Type>
{
    if tq.is_next(TokenKind::Operator(Operator::Mul))
    {
        tq.pop()?;
        let inner = parse_type(tq)?;
        Ok(Type::Pointer(Rc::new(inner)))
    }
    else if tq.is_next(TokenKind::Dollar)
    {
        tq.pop()?;
        let (name, _span) = tq.expect_identifier()?;
        Ok(Type::Generic(name))
    }
    else if tq.is_next(TokenKind::QuestionMark)
    {
        tq.pop()?;
        let inner = parse_type(tq)?;
        Ok(Type::Optional(Rc::new(inner)))
    }
    else if tq.is_next(TokenKind::OpenParen)
    {
        // Function signature: (a, b) -> c
        tq.pop()?;
        let (args, _) = parse_comma_separated_list(tq, TokenKind::CloseParen, parse_type)?;
        tq.expect(TokenKind::Arrow)?;
        let ret = parse_type(tq)?;
        Ok(func_type(args, ret))
    }
    else if tq.is_next(TokenKind::OpenCurly)
    {
        // Anonymous struct
        let members = parse_struct_members(tq)?;
        Ok(struct_type(
            "",
            members.into_iter().map(|m| StructMember{
                name: m.name,
                typ: m.typ
            }).collect()
        ))
    }
    else
    {
        let (name, _pos) = tq.expect_identifier()?;
        match to_primitive(&name)
        {
            Some(t) => Ok(t),
            None => {
                let generic_args = parse_generic_arg_list(tq)?;
                Ok(unresolved_type(&name, generic_args))
            },
        }
    }
}

fn parse_type(tq: &mut TokenQueue) -> CompileResult<Type>
{
    let mut typ = parse_start_of_type(tq)?;
    while tq.is_next(TokenKind::OpenBracket)
    {
        tq.pop()?;
        if tq.is_next(TokenKind::CloseBracket) {
            tq.pop()?;
            typ = slice_type(typ);
        } else {
            let (len, _span) = tq.expect_int()?;
            typ = array_type(typ, len as usize);
        }
    }

    Ok(typ)
}

fn parse_function_argument(tq: &mut TokenQueue, type_is_optional: bool, self_type: &Option<Type>) -> CompileResult<Argument>
{
    let mutable = if tq.is_next(TokenKind::Var) {
        tq.pop()?;
        true
    } else {
        false
    };

    let (name, span) = tq.expect_identifier()?;
    let typ = if tq.is_next(TokenKind::Colon) {
        tq.expect(TokenKind::Colon)?;
        parse_type(tq)?
    } else if name == "self" {
        if let Some(ref t) = *self_type {
            t.clone()
        } else {
            return err(&span, ErrorCode::SelfTypeUnknown, "Cannot determine type of self argument");
        }
    } else if type_is_optional {
        Type::Generic(name.clone()) // If the type is not known threat it as generic arg
    } else {
        return err(&span, ErrorCode::MissingType, format!("Type not specified of function argument {}", name));
    };

    Ok(Argument::new(name, typ, mutable, span.expanded(tq.pos())))
}

fn parse_function_arguments(tq: &mut TokenQueue, type_is_optional: bool, self_type: &Option<Type>) -> CompileResult<Vec<Argument>>
{
    let (args, _) = parse_comma_separated_list(tq, TokenKind::CloseParen, |tq| parse_function_argument(tq, type_is_optional, self_type))?;
    Ok(args)
}

fn parse_external_function(tq: &mut TokenQueue, span: &Span) -> CompileResult<ExternalFunction>
{
    let (name, name_span) = tq.expect_identifier()?;

    tq.expect(TokenKind::OpenParen)?;
    let args = parse_function_arguments(tq, false, &None)?;
    let ret_type = if tq.is_next(TokenKind::Arrow) {
        tq.pop()?;
        parse_type(tq)?
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
    let (full_name, self_type) = match name
    {
        "main" => (name.into(), None),
        _ if name.starts_with('~') => {
            let self_type = ptr_type(unresolved_type(&name[1..], Vec::new()));
            (namespaced(namespace, name), Some(self_type))
        },
        _ => {
            if tq.is_next(TokenKind::Operator(Operator::Dot)) {
                tq.pop()?;
                let (member_function_name, _) = tq.expect_identifier()?;
                let self_type = ptr_type(unresolved_type(name, Vec::new()));
                (namespaced(namespace, &format!("{}.{}", name, member_function_name)), Some(self_type))
            } else {
                (namespaced(namespace, name), None)
            }
        },
    };

    tq.expect(TokenKind::OpenParen)?;
    let args = parse_function_arguments(tq, false, &self_type)?;

    let ret_type = if tq.is_next(TokenKind::Arrow) {
        tq.pop()?;
        parse_type(tq)?
    } else {
        Type::Void
    };

    let sig_span_end = tq.pos();

    let expr = if tq.is_next(TokenKind::OpenCurly) {
        let tok = tq.pop()?;
        parse_block(tq, &tok.span)?
    } else {
        tq.expect(TokenKind::Assign)?;
        parse_expression(tq)?
    };

    let func_span = span.expanded(expr.span().end);
    Ok(Function::new(
        sig(&full_name, ret_type, args, span.expanded(sig_span_end)),
        true,
        expr,
        func_span
    ))
}

fn parse_struct_pattern(tq: &mut TokenQueue, name: &str, span: &Span) -> CompileResult<StructPattern>
{
    let (bindings, _) = parse_comma_separated_list(tq, TokenKind::CloseCurly, |tq| {
        let (name, _) = tq.expect_identifier()?;
        Ok(name)
    })?;
    Ok(struct_pattern(name, bindings, Vec::new(), Type::Unknown, span.expanded(tq.pos())))
}

pub fn parse_pattern(tq: &mut TokenQueue) -> CompileResult<Pattern>
{
    let tok = tq.pop()?;
    match tok.kind
    {
        TokenKind::Number(ref num) => parse_number(num, &tok.span).map(Pattern::Literal),
        TokenKind::True => Ok(Pattern::Literal(Literal::Bool(tok.span, true))),
        TokenKind::False => Ok(Pattern::Literal(Literal::Bool(tok.span, false))),
        TokenKind::CharLiteral(c) => Ok(Pattern::Literal(Literal::Char(tok.span, c as u8))),
        TokenKind::StringLiteral(s) => Ok(Pattern::Literal(Literal::String(tok.span, s))),

        TokenKind::OpenBracket => {
            if tq.is_next(TokenKind::CloseBracket)
            {
                tq.pop()?;
                Ok(empty_array_pattern(tok.span.expanded(tq.pos())))
            }
            else if tq.is_next_at(1, TokenKind::Pipe)
            {
                let (head, _head_span) = tq.expect_identifier()?;
                tq.expect(TokenKind::Pipe)?;
                let (tail, _) = tq.expect_identifier()?;
                tq.expect(TokenKind::CloseBracket)?;
                Ok(array_pattern(&head, &tail, tok.span.expanded(tq.pos())))
            }
            else
            {
                let al = parse_array_literal(tq, &tok.span)?;
                Ok(Pattern::Literal(al))
            }
        },

        TokenKind::OpenCurly => {
            parse_struct_pattern(tq, "", &tok.span).map(Pattern::Struct)
        },

        TokenKind::Identifier(id) => {
            if id == "_" {
                Ok(Pattern::Any(tok.span))
            }
            else if tq.is_next(TokenKind::OpenCurly) {
                tq.pop()?;
                parse_struct_pattern(tq, &id, &tok.span).map(Pattern::Struct)
            } else {
                Ok(Pattern::Name(NameRef::new(id, tok.span)))
            }
        },

        TokenKind::QuestionMark => {
            let (name, name_span) = tq.expect_identifier()?;
            Ok(optional_pattern(name, tok.span.expanded(name_span.end)))
        },

        TokenKind::Nil => {
            Ok(Pattern::Nil(tok.span))
        },

        _ => err(&tok.span, ErrorCode::UnexpectedToken, format!("Unexpected token '{}'", tok)),
    }
}

fn parse_match(tq: &mut TokenQueue, span: &Span) -> CompileResult<Expression>
{
    let target = parse_expression(tq)?;
    tq.expect(TokenKind::Colon)?;

    let mut cases = Vec::new();
    loop
    {
        let pattern = parse_pattern(tq)?;
        tq.expect(TokenKind::FatArrow)?;
        let t = parse_expression(tq)?;
        let case_span = pattern.span().expanded(tq.pos());
        cases.push(match_case(pattern, t, case_span));
        if tq.is_next(TokenKind::Comma) { // Continue, while we see a comman
            tq.pop()?;
        } else {
            break;
        }
    }

    Ok(match_expression(target, cases, span.expanded(tq.pos())))
}

fn parse_lambda(tq: &mut TokenQueue, span: &Span) -> CompileResult<Expression>
{
    tq.expect(TokenKind::OpenParen)?;
    let args = parse_function_arguments(tq, true, &None)?;
    tq.expect(TokenKind::Arrow)?;
    let expr = parse_expression(tq)?;
    Ok(lambda(args, expr, span.expanded(tq.pos())))
}

fn parse_bindings(tq: &mut TokenQueue, mutable: bool) -> CompileResult<Vec<Binding>>
{
    let mut bindings = Vec::new();
    while !tq.is_next(TokenKind::In) && !tq.is_next(TokenKind::SemiColon) && !tq.is_next(TokenKind::CloseParen)
    {
        let (binding_type, span) = if tq.is_next(TokenKind::OpenCurly) {
            let tok = tq.pop()?;
            let pattern = parse_struct_pattern(tq, "", &tok.span)?;
            let span = pattern.span.clone();
            (BindingType::Struct(pattern), span)
        } else {
            let (name, span) = tq.expect_identifier()?;
            (BindingType::Name(name), span)
        };

        tq.expect(TokenKind::Assign)?;
        let init = parse_expression(tq)?;
        bindings.push(binding(binding_type, init, mutable, span.expanded(tq.pos())));
        eat_comma(tq)?;
    }

    Ok(bindings)
}

fn parse_binding(tq: &mut TokenQueue, mutable: bool, span: &Span) -> CompileResult<Expression>
{
    let b = parse_bindings(tq, mutable)?;
    if tq.is_next(TokenKind::In)
    {
        tq.expect(TokenKind::In)?;
        let e = parse_expression(tq)?;
        Ok(binding_expression(b, e, span.expanded(tq.pos())))
    }
    else
    {
        Ok(bindings(b, span.expanded(tq.pos())))
    }

}

fn parse_if(tq: &mut TokenQueue, span: &Span) -> CompileResult<Expression>
{
    let cond = parse_expression(tq)?;
    tq.expect(TokenKind::Colon)?;
    let on_true = parse_expression(tq)?;
    tq.expect(TokenKind::Else)?;
    let on_false = parse_expression(tq)?;
    Ok(if_expression(cond, on_true, on_false, span.expanded(tq.pos())))
}

fn parse_type_declaration(tq: &mut TokenQueue, namespace: &str, span: &Span) -> CompileResult<TypeDeclaration>
{
    let (name, _) = tq.expect_identifier()?;
    if tq.is_next(TokenKind::OpenCurly) {
        let sd = parse_struct_type(tq, namespace, &name, span)?;
        return Ok(TypeDeclaration::Struct(sd))
    }

    tq.expect(TokenKind::Assign)?;
    if tq.is_next(TokenKind::OpenCurly)
    {
        let sd = parse_struct_type(tq, namespace, &name, span)?;
        Ok(TypeDeclaration::Struct(sd))
    }
    else if tq.is_next_at(1, TokenKind::Pipe) || tq.is_next_at(1, TokenKind::OpenCurly)
    {
        let st = parse_sum_type(tq, namespace, &name, span)?;
        Ok(TypeDeclaration::Sum(st))
    }
    else
    {
        let typ = parse_type(tq)?;
        Ok(TypeDeclaration::Alias(type_alias(&namespaced(namespace, &name), typ, span.expanded(tq.pos()))))
    }
}


fn parse_sum_type(tq: &mut TokenQueue, namespace: &str, name: &str, span: &Span) -> CompileResult<SumTypeDeclaration>
{
    let mut cases = Vec::new();
    loop
    {
        let (case_name, case_name_span) = tq.expect_identifier()?;
        if tq.is_next(TokenKind::Pipe)
        {
            cases.push(sum_type_case_decl(&namespaced(namespace, &case_name), None, case_name_span));
            tq.pop()?;
        }
        else if tq.is_next(TokenKind::OpenCurly)
        {
            let sd = parse_struct_type(tq, namespace, &case_name, &case_name_span)?;
            cases.push(sum_type_case_decl(&namespaced(namespace, &case_name), Some(sd), case_name_span.expanded(tq.pos())));
            if tq.is_next(TokenKind::Pipe)
            {
                tq.pop()?;
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

    Ok(sum_type_decl(&namespaced(namespace, name), cases, span.expanded(tq.pos())))
}

fn namespaced(namespace: &str, name: &str) -> String
{
    format!("{}::{}", namespace, name)
}

fn parse_struct_member(tq: &mut TokenQueue, file_name: &str) -> CompileResult<StructMemberDeclaration>
{
    if tq.is_next_at(1, TokenKind::Colon) // Named struct member
    {
        let (member_name, member_name_span) = tq.expect_identifier()?;
        tq.expect(TokenKind::Colon)?;
        let typ = parse_type(tq)?;
        Ok(struct_member_declaration(&member_name, typ, member_name_span.expanded(tq.pos())))
    }
    else // Anonymous struct member
    {
        let span_start = tq.pos();
        let typ = parse_type(tq)?;
        Ok(struct_member_declaration("", typ, Span::new(file_name, span_start, tq.pos())))
    }
}

fn parse_struct_members(tq: &mut TokenQueue) -> CompileResult<Vec<StructMemberDeclaration>>
{
    let span = tq.expect(TokenKind::OpenCurly)?.span;
    let (members, _) = parse_comma_separated_list(
        tq,
        TokenKind::CloseCurly,
        |tq| parse_struct_member(tq, &span.file)
    )?;
    Ok(members)
}

fn parse_struct_type(tq: &mut TokenQueue, namespace: &str, name: &str, span: &Span) -> CompileResult<StructDeclaration>
{
    let members = parse_struct_members(tq)?;
    Ok(struct_declaration(&namespaced(namespace, name), members, span.expanded(tq.pos())))
}

fn parse_struct_initializer(tq: &mut TokenQueue, name: &str, name_span: &Span) -> CompileResult<Expression>
{
    tq.expect(TokenKind::OpenCurly)?;
    let (expressions, _) = parse_comma_separated_list(tq, TokenKind::CloseCurly, parse_expression)?;
    Ok(Expression::StructInitializer(
        struct_initializer(name, expressions, name_span.expanded(tq.pos()))
    ))
}

fn parse_member_access(tq: &mut TokenQueue, left_expr: Expression) -> CompileResult<Expression>
{
    let mut left = left_expr;
    while tq.is_next(TokenKind::Operator(Operator::Dot))
    {
        tq.pop()?;
        let (name, name_span) = tq.expect_identifier()?;

        let (ma, span) = if tq.is_next(TokenKind::OpenParen) {
            tq.pop()?;
            let call = parse_function_call(tq, NameRef::new(name, name_span))?;
            let span = left.span().expanded(call.span.end);
            (MemberAccessType::Call(call), span)
        } else {
            (MemberAccessType::Name(field(&name, 0)), left.span().expanded(name_span.end))
        };

        left = member_access(left, ma, span);
    }

    Ok(left)
}

fn parse_block(tq: &mut TokenQueue, start: &Span) -> CompileResult<Expression>
{
    let mut ends_with_semicolon = false;
    let mut expressions = Vec::new();
    while !tq.is_next(TokenKind::CloseCurly)
    {
        let e = parse_expression(tq)?;
        expressions.push(e);
        ends_with_semicolon = false;
        while tq.is_next(TokenKind::SemiColon) {
            tq.pop()?;
            ends_with_semicolon = true;
        }
    }

    tq.expect(TokenKind::CloseCurly)?;
    if ends_with_semicolon {
        expressions.push(Expression::Void);
    }
    Ok(block(expressions, start.expanded(tq.pos())))
}

fn parse_while(tq: &mut TokenQueue, start: &Span) -> CompileResult<Expression>
{
    let cond = parse_expression(tq)?;
    tq.expect(TokenKind::Colon)?;
    let body = parse_expression(tq)?;
    Ok(while_loop(cond, body, start.expanded(tq.pos())))
}


fn parse_for(tq: &mut TokenQueue, start: &Span) -> CompileResult<Expression>
{
    let (loop_variable, _) = tq.expect_identifier()?;
    tq.expect(TokenKind::In)?;

    let iterable = parse_expression(tq)?;
    tq.expect(TokenKind::Colon)?;
    
    let body = parse_expression(tq)?;
    Ok(for_loop(&loop_variable, iterable, body, start.expanded(tq.pos())))
}

fn parse_expression_start(tq: &mut TokenQueue, tok: Token) -> CompileResult<Expression>
{
    match tok.kind
    {
        TokenKind::Nil => {
            Ok(Expression::Nil(tok.span))
        },

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
            parse_binding(tq, false, &tok.span)
        },

        TokenKind::Var => {
            parse_binding(tq, true, &tok.span)
        },

        TokenKind::If => {
            parse_if(tq, &tok.span)
        },

        TokenKind::While => {
            parse_while(tq, &tok.span)
        },

        TokenKind::For => {
            parse_for(tq, &tok.span)
        },

        TokenKind::OpenCurly => {
            parse_block(tq, &tok.span)
        },

        TokenKind::OpenBracket => {
            parse_array_literal(tq, &tok.span).map(Expression::Literal)
        },

        TokenKind::OpenParen => {
            let inner = parse_expression(tq)?;
            tq.expect(TokenKind::CloseParen)?;
            Ok(inner)
        },

        TokenKind::Dollar => {
            parse_struct_initializer(tq, "", &tok.span)
        },

        TokenKind::Identifier(id) => {
            let nr = parse_name(tq, id, &tok.span)?;
            if tq.is_next(TokenKind::OpenParen)
            {
                tq.pop()?;
                let call = parse_function_call(tq, nr).map(Expression::Call)?;
                if tq.is_next(TokenKind::Operator(Operator::Dot)) {
                    parse_member_access(tq, call)
                } else {
                    Ok(call)
                }
            }
            else if tq.is_next(TokenKind::OpenCurly)
            {
                parse_struct_initializer(tq, &nr.name, &nr.span)
            }
            else if tq.is_next(TokenKind::Operator(Operator::Dot))
            {
                parse_member_access(tq, Expression::NameRef(nr))
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
            parse_number(&n, &tok.span).map(Expression::Literal)
        },

        TokenKind::New => {
            let inner = parse_expression(tq)?;
            Ok(new(inner, tok.span.expanded(tq.pos())))
        },

        TokenKind::Delete => {
            let inner = parse_expression(tq)?;
            Ok(delete(inner, tok.span.expanded(tq.pos())))
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

    let next = tq.pop()?;
    match next.kind
    {
        TokenKind::Assign => {
            let rhs = parse_expression(tq)?;
            let span = lhs.span().expanded(tq.pos());
            Ok(assign(lhs, rhs, span))
        },

        TokenKind::Operator(Operator::Dot) => {
            tq.push_front(next);
            parse_member_access(tq, lhs)
        },

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
    let tok = tq.pop()?;
    let lhs = parse_expression_start(tq, tok)?;
    parse_expression_continued(tq, lhs)
}

pub fn parse_file(options: &ParserOptions, file_path: &str) -> CompileResult<Module>
{
    use std::path::Path;
    use std::ffi::OsStr;

    let mut file = fs::File::open(file_path)?;
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

fn parse_global_bindings(module: &mut Module, tq: &mut TokenQueue, mutable: bool) -> CompileResult<()>
{
    while !tq.is_next(TokenKind::SemiColon)
    {
        let (name, span) = tq.expect_identifier()?;
        tq.expect(TokenKind::Assign)?;
        let init = parse_expression(tq)?;

        if module.globals.contains_key(&name) {
            return err(&span, ErrorCode::RedefinitionOfVariable, format!("Global {} already defined in this module", name));
        }

        module.globals.insert(name.clone(), global_binding(name, init, mutable, span.expanded(tq.pos())));
        eat_comma(tq)?;
    }

    tq.expect(TokenKind::SemiColon)?;
    Ok(())
}

pub fn parse_module<Input: Read>(options: &ParserOptions, input: &mut Input, name: &str, file_name: &str) -> CompileResult<Module>
{
    let mut tq = Lexer::new(file_name).read(input)?;
    let mut module = Module::new(name);
    let namespace = name;

    let add_function = |module: &mut Module, func: Function| -> CompileResult<()> {
        if module.functions.contains_key(&func.sig.name) {
            return err(&func.span, ErrorCode::RedefinitionOfFunction, format!("Function {} redefined", func.sig.name));
        }
        module.functions.insert(func.sig.name.clone(), func);
        Ok(())
    };

    while !tq.is_next(TokenKind::EOF)
    {
        let tok = tq.pop()?;
        match tok.kind
        {
            TokenKind::Let => {
                parse_global_bindings(&mut module, &mut tq, false)?;
            },

            TokenKind::Var => {
                parse_global_bindings(&mut module, &mut tq, true)?;
            },

            TokenKind::Type => {
                let sd = parse_type_declaration(&mut tq, namespace, &tok.span)?;
                if module.types.contains_key(sd.name()) {
                    return err(&sd.span(), ErrorCode::RedefinitionOfStruct, format!("Type {} redefined", sd.name()));
                }
                module.types.insert(sd.name().into(), sd);
            },

            TokenKind::Extern => {
                let ext_func = parse_external_function(&mut tq, &tok.span)?;
                if module.externals.contains_key(&ext_func.sig.name) {
                    return err(&ext_func.span, ErrorCode::RedefinitionOfFunction, format!("External function {} redefined", ext_func.sig.name));
                }
                module.externals.insert(ext_func.sig.name.clone(), ext_func);
            },

            TokenKind::Import => {
                loop
                {
                    let (name, _) = tq.expect_identifier()?;
                    if !module.imports.contains(&name) {
                        let m = parse_import(options, &name)?;
                        module.import(&m);
                    }

                    if tq.is_next(TokenKind::Comma) {
                        tq.pop()?;
                    } else {
                        break;
                    }
                }
            },

            TokenKind::Tilde => {
                let (type_name, span) = tq.expect_identifier()?;
                let func_name = format!("~{}", type_name);
                let func = parse_function_declaration(&mut tq, namespace, &func_name, &span)?;
                add_function(&mut module, func)?;
            },

            TokenKind::Identifier(ref id) => {
                let func = parse_function_declaration(&mut tq, namespace, id, &tok.span)?;
                add_function(&mut module, func)?;
            }
            _ => {
                return err(&tok.span, ErrorCode::ExpressionNotAllowedAtTopLevel,
                    format!("Expected a function declaration, import statement, extern function declaration or type declaration, found token {}", tok));
            }
        }
    }

    Ok(module)
}
