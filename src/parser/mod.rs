mod lexer;
mod tokenqueue;
mod tokens;

#[cfg(test)]
mod tests;

use litrs::{FloatLit, IntegerLit};

#[cfg(test)]
pub use self::tests::{th_expr, th_mod};

use std::fs;
use std::io::Read;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

use crate::ast::*;
use crate::compileerror::{parse_error_result, CompileError, CompileResult};
use crate::span::Span;
use crate::target::Target;
use crate::timer::time_operation;

use self::lexer::Lexer;
use self::tokenqueue::TokenQueue;
use self::tokens::{Token, TokenKind};

fn is_end_of_expression(tok: &Token) -> bool {
    matches!(
        tok.kind,
        TokenKind::Colon
            | TokenKind::SemiColon
            | TokenKind::Assign(_)
            | TokenKind::CloseParen
            | TokenKind::CloseBracket
            | TokenKind::CloseCurly
            | TokenKind::EOF
    )
}

fn eat_comma(tq: &mut TokenQueue) -> CompileResult<()> {
    if tq.is_next(&TokenKind::Comma) {
        tq.pop()?;
    }
    Ok(())
}

fn number_to_literal(
    number: u64,
    force_unsigned: bool,
    span: &Span,
    native_int_size: IntSize,
) -> CompileResult<Literal> {
    let int_sizes = [IntSize::I8, IntSize::I16, IntSize::I32, IntSize::I64];
    let mut selected_int_size = IntSize::I8;
    for int_size in &int_sizes {
        let lim = if force_unsigned {
            int_size.max_unsigned()
        } else {
            int_size.max_signed() as u64
        };

        if number <= lim {
            selected_int_size = *int_size;
            break;
        }
    }

    if selected_int_size.size_in_bits() < native_int_size.size_in_bits() {
        selected_int_size = native_int_size;
    }

    if force_unsigned {
        Ok(Literal::UInt(span.clone(), number, selected_int_size))
    } else {
        Ok(Literal::Int(span.clone(), number as i64, selected_int_size))
    }
}

fn parse_number(tq: &mut TokenQueue, num: &str, span: &Span, target: &Target) -> CompileResult<Literal> {
    if num.find('.').is_some() || num.find('e').is_some() {
        match FloatLit::parse(num) {
            Ok(_) => Ok(Literal::Float(span.clone(), num.into(), FloatSize::F64)),
            Err(_) => parse_error_result(span, format!("{} is not a valid floating point number", num)),
        }
    } else {
        let force_unsigned = if tq.is_next_identifier("u") {
            tq.pop()?;
            true
        } else {
            false
        };

        // Should be an integer
        match IntegerLit::parse(num) {
            Ok(i) => {
                if let Some(v) = i.value() {
                    number_to_literal(v, force_unsigned, span, target.int_size)
                } else {
                    parse_error_result(span, format!("{num} overflows a u64"))
                }
            }
            Err(_) => parse_error_result(span, format!("{num} is not a valid integer")),
        }
    }
}

fn parse_array_literal(
    tq: &mut TokenQueue,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Literal> {
    let mut expressions = Vec::new();
    while !tq.is_next(&TokenKind::CloseBracket) {
        let e = parse_expression(tq, indent_level, target)?;
        if expressions.is_empty() && tq.is_next(&TokenKind::SemiColon) {
            // [x ; 4]
            tq.pop()?;
            let (times, _) = tq.expect_int()?;
            tq.expect(&TokenKind::CloseBracket)?;
            return Ok(array_lit(vec![e; times as usize], span.expanded(tq.pos())));
        } else {
            expressions.push(e);
            eat_comma(tq)?;
        }
    }

    tq.expect(&TokenKind::CloseBracket)?;
    Ok(array_lit(expressions, span.expanded(tq.pos())))
}

fn parse_name(tq: &mut TokenQueue, id: String, span: &Span) -> CompileResult<NameRef> {
    let mut name = id;
    while tq.is_next(&TokenKind::DoubleColon) {
        tq.pop()?;
        let (next, _) = tq.expect_identifier()?;
        name.push_str("::");
        name.push_str(&next);
    }

    Ok(NameRef::new(name, span.expanded(tq.pos())))
}

fn parse_unary_expression(
    tq: &mut TokenQueue,
    op: UnaryOperator,
    op_span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    let se = parse_expression(tq, indent_level, target)?;
    Ok(unary_op(op, se, op_span.expanded(tq.pos())))
}

fn combine_binary_op(op: BinaryOperator, lhs: Expression, rhs: Expression) -> Expression {
    if lhs.is_binary_op() && lhs.precedence() < op.precedence() {
        let bop = lhs.extract_binary_op().expect("Not a binary op");
        let nrhs = combine_binary_op(op, bop.right.clone(), rhs);
        let span = Span::merge(&bop.left.span(), &nrhs.span());
        bin_op(bop.operator, bop.left.clone(), nrhs, span)
    } else {
        let span = Span::merge(&lhs.span(), &rhs.span());
        bin_op(op, lhs, rhs, span)
    }
}

fn combine_type_cast(lhs: Expression, destination_type: Type, span: Span) -> Expression {
    if lhs.is_binary_op() {
        let bop = lhs.extract_binary_op().expect("Not a binary op");
        let nrhs = combine_type_cast(bop.right.clone(), destination_type, span);
        let span = Span::merge(&bop.left.span(), &nrhs.span());
        bin_op(bop.operator, bop.left, nrhs, span)
    } else {
        type_cast(lhs, destination_type, span)
    }
}

fn parse_binary_op_rhs(
    tq: &mut TokenQueue,
    mut lhs: Expression,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    //use ast::TreePrinter;

    loop {
        if !tq.is_next_binary_operator() {
            return Ok(lhs);
        }

        let op = tq.expect_binary_operator()?;
        if op == BinaryOperator::As {
            let (typ, _) = parse_type(tq, indent_level, target)?;
            let span = lhs.span().expanded(tq.pos());
            lhs = combine_type_cast(lhs, typ, span);
            continue;
        }

        let next_tok = tq.pop()?;
        let rhs = parse_expression_start(tq, next_tok, indent_level, target)?;
        lhs = combine_binary_op(op, lhs, rhs);
    }
}

fn parse_list<T, P>(
    tq: &mut TokenQueue,
    separator: &TokenKind,
    end_token: &TokenKind,
    parse_element: P,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Vec<T>>
where
    P: Fn(&mut TokenQueue, usize, &Target) -> CompileResult<T>,
{
    let mut elements = Vec::new();
    while !tq.is_next(end_token) {
        if !tq.is_in_same_block(indent_level + 1) {
            break;
        } else {
            tq.pop_indent()?;
        }

        let e = parse_element(tq, indent_level, target)?;
        elements.push(e);
        if !tq.is_next(separator) {
            break;
        } else {
            tq.pop()?;
        }
    }

    tq.expect(end_token)?;
    Ok(elements)
}

fn parse_comma_separated_list<T, P>(
    tq: &mut TokenQueue,
    end_token: &TokenKind,
    parse_element: P,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Vec<T>>
where
    P: Fn(&mut TokenQueue, usize, &Target) -> CompileResult<T>,
{
    parse_list(tq, &TokenKind::Comma, end_token, parse_element, indent_level, target)
}

fn parse_function_call(
    tq: &mut TokenQueue,
    name: NameRef,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Call> {
    tq.expect(&TokenKind::OpenParen)?;
    let args = parse_comma_separated_list(tq, &TokenKind::CloseParen, parse_expression, indent_level, target)?;
    let span = name.span.expanded(tq.pos());
    Ok(Call::new(name, args, span))
}

fn parse_generic_arg_list(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<Vec<Type>> {
    if !tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::LessThan)) {
        return Ok(Vec::new());
    }
    tq.pop()?;
    let args = parse_comma_separated_list(
        tq,
        &TokenKind::BinaryOperator(BinaryOperator::GreaterThan),
        parse_type,
        indent_level,
        target,
    )?;
    Ok(args.into_iter().map(|(t, _)| t).collect())
}

fn to_primitive(name: &str, target: &Target) -> Option<Type> {
    match name {
        "int8" | "i8" => Some(Type::Int(IntSize::I8)),
        "int16" | "i16" => Some(Type::Int(IntSize::I16)),
        "int32" | "i32" => Some(Type::Int(IntSize::I32)),
        "int64" | "i64" => Some(Type::Int(IntSize::I64)),
        "int" => Some(target.native_int_type.clone()),
        "uint8" | "u8" => Some(Type::UInt(IntSize::I8)),
        "uint16" | "u16" => Some(Type::UInt(IntSize::I16)),
        "uint32" | "u32" => Some(Type::UInt(IntSize::I32)),
        "uint64" | "u64" => Some(Type::UInt(IntSize::I64)),
        "uint" => Some(target.native_uint_type.clone()),
        "float" | "float32" | "f32" => Some(Type::Float(FloatSize::F32)),
        "double" | "float64" | "f64" => Some(Type::Float(FloatSize::F64)),
        "string" => Some(Type::String),
        "bool" => Some(Type::Bool),
        "char" => Some(Type::Char),
        "Self" => Some(Type::SelfType),
        "void" => Some(Type::Void),
        _ => None,
    }
}

fn parse_generic_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    let start = tq.expect(&TokenKind::Dollar)?.span;
    if tq.is_next(&TokenKind::OpenParen) {
        tq.pop()?;
        let constraints = parse_list(
            tq,
            &TokenKind::BinaryOperator(BinaryOperator::Add),
            &TokenKind::CloseParen,
            parse_type,
            indent_level,
            target,
        )?;
        let span = start.expanded(tq.pos());
        if constraints.is_empty() {
            return parse_error_result(&span, "Empty generic constraints list");
        }
        let ci = constraints.into_iter().map(|(t, _)| t);
        Ok((generic_type_with_constraints(ci.collect()), span))
    } else {
        let (name, span) = tq.expect_identifier()?;
        Ok((generic_type(&name), Span::merge(&start, &span)))
    }
}

fn parse_function_type_arg(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<FuncArg> {
    let mutable = if tq.is_next(&TokenKind::Var) {
        tq.expect(&TokenKind::Var)?;
        true
    } else {
        false
    };

    let (typ, _) = parse_type(tq, indent_level, target)?;
    Ok(FuncArg { typ, mutable })
}

fn parse_function_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    // Function signature: fn(a, b) -> c
    let start = tq.expect(&TokenKind::Func)?.span;
    tq.expect(&TokenKind::OpenParen)?;
    let args = parse_comma_separated_list(
        tq,
        &TokenKind::CloseParen,
        parse_function_type_arg,
        indent_level,
        target,
    )?;
    tq.expect(&TokenKind::Arrow)?;
    let (ret, span) = parse_type(tq, indent_level, target)?;
    Ok((func_type(args, ret), Span::merge(&start, &span)))
}

fn parse_pointer_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    let start = tq
        .expect(&TokenKind::BinaryOperator(BinaryOperator::Mul))?
        .span;
    let (inner, span) = parse_type(tq, indent_level, target)?;
    Ok((Type::Pointer(Rc::new(inner)), Span::merge(&start, &span)))
}

fn parse_optional_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    let start = tq.expect(&TokenKind::QuestionMark)?.span;
    let (inner, span) = parse_type(tq, indent_level, target)?;
    Ok((Type::Optional(Rc::new(inner)), Span::merge(&start, &span)))
}

fn parse_struct_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    let start = tq.expect(&TokenKind::OpenCurly)?.span;
    let mut members = Vec::new();
    while !tq.is_next(&TokenKind::CloseCurly) {
        let (name, _) = tq.expect_identifier()?;
        tq.expect(&TokenKind::Colon)?;
        let (t, _) = parse_type(tq, indent_level, target)?;
        members.push(struct_member(name, t));

        if !tq.is_next(&TokenKind::CloseCurly) {
            tq.expect(&TokenKind::Comma)?;
        }
    }
    tq.expect(&TokenKind::CloseCurly)?;
    let span = start.expanded(tq.pos());
    Ok((struct_type(None, members, Vec::new()), span))
}

fn parse_primitive_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    let (name, pos) = tq.expect_identifier()?;
    match to_primitive(&name, target) {
        Some(t) => Ok((t, pos)),
        None => {
            let generic_args = parse_generic_arg_list(tq, indent_level, target)?;
            Ok((unresolved_type(&name, generic_args), pos.expanded(tq.pos())))
        }
    }
}

fn parse_start_of_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    if tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::Mul)) {
        parse_pointer_type(tq, indent_level, target)
    } else if tq.is_next(&TokenKind::Dollar) {
        parse_generic_type(tq, indent_level, target)
    } else if tq.is_next(&TokenKind::QuestionMark) {
        parse_optional_type(tq, indent_level, target)
    } else if tq.is_next(&TokenKind::Func) {
        parse_function_type(tq, indent_level, target)
    } else if tq.is_next(&TokenKind::OpenCurly) {
        parse_struct_type(tq, indent_level, target)
    } else {
        parse_primitive_type(tq, indent_level, target)
    }
}

fn parse_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<(Type, Span)> {
    let (mut typ, mut span) = parse_start_of_type(tq, indent_level, target)?;
    loop {
        if tq.is_next(&TokenKind::OpenBracket) {
            tq.pop()?;
            if tq.is_next(&TokenKind::CloseBracket) {
                span = Span::merge(&span, &tq.pop()?.span);
                typ = slice_type(typ);
            } else {
                let (len, _span) = tq.expect_int()?;
                typ = array_type(typ, len as usize);
                span = Span::merge(&span, &tq.expect(&TokenKind::CloseBracket)?.span);
            }
        } else if tq.is_next(&TokenKind::UnaryOperator(UnaryOperator::Not)) {
            tq.pop()?;
            let (err_typ, err_span) = parse_type(tq, indent_level, target)?;
            span = Span::merge(&span, &err_span);
            typ = result_type(typ, err_typ);
        } else {
            break;
        }
    }

    Ok((typ, span))
}

fn parse_function_argument(
    tq: &mut TokenQueue,
    self_type: &Type,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Argument> {
    let mutable = if tq.is_next(&TokenKind::Var) {
        tq.pop()?;
        true
    } else {
        false
    };

    let (name, span) = tq.expect_identifier()?;
    let typ = if tq.is_next(&TokenKind::Colon) {
        tq.expect(&TokenKind::Colon)?;
        parse_type(tq, indent_level, target)?.0
    } else if name == "self" {
        if *self_type != Type::Unknown {
            self_type.clone()
        } else {
            return parse_error_result(&span, "Cannot determine type of self argument");
        }
    } else {
        generic_type(&name) // If the type is not known threat it as generic arg
    };

    Ok(Argument::new(name, typ, mutable, span.expanded(tq.pos())))
}

fn parse_function_arguments(
    tq: &mut TokenQueue,
    self_type: &Type,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Vec<Argument>> {
    tq.expect(&TokenKind::OpenParen)?;
    let parse_arg = |tq: &mut TokenQueue, indent_level: usize, target: &Target| {
        parse_function_argument(tq, self_type, indent_level, target)
    };
    let args = parse_comma_separated_list(tq, &TokenKind::CloseParen, parse_arg, indent_level, target)?;
    Ok(args)
}

fn parse_function_signature(
    tq: &mut TokenQueue,
    self_type: &Type,
    indent_level: usize,
    target: &Target,
) -> CompileResult<FunctionSignature> {
    let (name, name_span) = tq.expect_identifier()?;
    let args = parse_function_arguments(tq, self_type, indent_level, target)?;
    let ret_type = if tq.is_next(&TokenKind::Arrow) {
        tq.pop()?;
        parse_type(tq, indent_level, target)?.0
    } else {
        Type::Void
    };

    let sig_span_end = tq.pos();
    Ok(sig(&name, ret_type, args, name_span.expanded(sig_span_end)))
}

fn parse_external_function(
    tq: &mut TokenQueue,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<External> {
    tq.expect(&TokenKind::Func)?;
    Ok(External::Function {
        sig: parse_function_signature(tq, &Type::Unknown, indent_level, target)?,
        span: span.expanded(tq.pos()),
    })
}

fn parse_function_declaration(
    tq: &mut TokenQueue,
    namespace: &str,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Function> {
    let name = if tq.is_next(&TokenKind::Tilde) {
        tq.pop()?;
        let (name, _) = tq.expect_identifier()?;
        format!("~{}", name)
    } else {
        let (name, _) = tq.expect_identifier()?;
        name
    };

    let (full_name, self_type) = match &name[..] {
        _ if name.starts_with('~') => {
            let self_type = ptr_type(unresolved_type(&name[1..], Vec::new()));
            (namespaced(namespace, &name), self_type)
        }
        _ => {
            if tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::Dot)) {
                tq.pop()?;
                let (member_function_name, _) = tq.expect_identifier()?;
                let self_type = ptr_type(unresolved_type(&name, Vec::new()));
                (
                    namespaced(namespace, &format!("{}.{}", name, member_function_name)),
                    self_type,
                )
            } else {
                (namespaced(namespace, &name), Type::Unknown)
            }
        }
    };

    let args = parse_function_arguments(tq, &self_type, indent_level, target)?;
    let ret_type = if tq.is_next(&TokenKind::Arrow) {
        tq.pop()?;
        parse_type(tq, indent_level, target)?.0
    } else {
        Type::Void
    };

    let signature = sig(&full_name, ret_type, args, span.expanded(tq.pos()));
    let colon = tq.expect(&TokenKind::Colon)?;

    let expr = parse_block(tq, &colon.span, indent_level, target)?;
    let func_span = Span::merge(span, &expr.span());
    Ok(Function::new(signature, true, expr, func_span))
}

fn parse_struct_pattern(
    tq: &mut TokenQueue,
    name: &str,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<StructPattern> {
    tq.expect(&TokenKind::OpenCurly)?;
    let parse_binding = |tq: &mut TokenQueue, _indent_level: usize, _target: &Target| {
        let mode = if tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::Mul)) {
            tq.pop()?;
            StructPatternBindingMode::Pointer
        } else {
            StructPatternBindingMode::Value
        };

        let (name, _) = tq.expect_identifier()?;
        Ok(StructPatternBinding {
            name,
            typ: Type::Unknown,
            mode,
        })
    };
    let bindings = parse_comma_separated_list(tq, &TokenKind::CloseCurly, parse_binding, indent_level, target)?;
    Ok(struct_pattern(name, bindings, Type::Unknown, span.expanded(tq.pos())))
}

pub fn parse_pattern(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<Pattern> {
    let tok = tq.pop()?;
    match tok.kind {
        TokenKind::Number(ref num) => parse_number(tq, num, &tok.span, target).map(Pattern::Literal),
        TokenKind::True => Ok(Pattern::Literal(Literal::Bool(tok.span, true))),
        TokenKind::False => Ok(Pattern::Literal(Literal::Bool(tok.span, false))),
        TokenKind::CharLiteral(c) => Ok(Pattern::Literal(Literal::Char(tok.span, c))),
        TokenKind::StringLiteral(s) => Ok(Pattern::Literal(Literal::String(tok.span, s))),

        TokenKind::OpenBracket => {
            if tq.is_next(&TokenKind::CloseBracket) {
                tq.pop()?;
                Ok(empty_array_pattern(tok.span.expanded(tq.pos())))
            } else if tq.is_next_at(1, &TokenKind::BinaryOperator(BinaryOperator::BitwiseOr)) {
                let (head, _head_span) = tq.expect_identifier()?;
                tq.expect(&TokenKind::BinaryOperator(BinaryOperator::BitwiseOr))?;
                let (tail, _) = tq.expect_identifier()?;
                tq.expect(&TokenKind::CloseBracket)?;
                Ok(array_pattern(&head, &tail, tok.span.expanded(tq.pos())))
            } else {
                let al = parse_array_literal(tq, &tok.span, indent_level, target)?;
                Ok(Pattern::Literal(al))
            }
        }

        TokenKind::Identifier(id) => {
            let name = parse_name(tq, id, &tok.span)?;
            if name.name == "_" {
                Ok(Pattern::Any(tok.span))
            } else if tq.is_next(&TokenKind::OpenCurly) {
                parse_struct_pattern(tq, &name.name, &tok.span, indent_level, target).map(Pattern::Struct)
            } else {
                Ok(Pattern::Name(name))
            }
        }

        TokenKind::QuestionMark => {
            let (name, name_span) = tq.expect_identifier()?;
            Ok(optional_pattern(name, Span::merge(&tok.span, &name_span)))
        }

        TokenKind::Nil => Ok(Pattern::Nil(tok.span)),

        TokenKind::Ok => {
            let inner = parse_pattern(tq, indent_level, target)?;
            let span = Span::merge(&tok.span, &inner.span());
            Ok(ok_pattern(inner, span))
        }

        TokenKind::Error => {
            let inner = parse_pattern(tq, indent_level, target)?;
            let span = Span::merge(&tok.span, &inner.span());
            Ok(error_pattern(inner, span))
        }

        _ => parse_error_result(&tok.span, format!("Unexpected token '{}'", tok)),
    }
}

fn parse_indented_block<T, P>(
    tq: &mut TokenQueue,
    indent_level: usize,
    parse_element: P,
    target: &Target,
) -> CompileResult<Vec<T>>
where
    P: Fn(&mut TokenQueue, usize, &Target) -> CompileResult<T>,
{
    let block_indent_level = check_indent_level(tq, indent_level)?;
    let mut elements = Vec::new();
    while tq.is_in_same_block(block_indent_level) {
        tq.pop_indent()?;
        let element = parse_element(tq, block_indent_level, target)?;
        elements.push(element);
    }

    Ok(elements)
}

fn parse_match(tq: &mut TokenQueue, span: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let target_expr = parse_expression(tq, indent_level, target)?;
    tq.expect(&TokenKind::Colon)?;

    let parse_match_case = |tq: &mut TokenQueue, indent_level: usize, target: &Target| {
        let pattern = parse_pattern(tq, indent_level, target)?;
        let tok = tq.expect(&TokenKind::FatArrow)?;
        let t = parse_block(tq, &tok.span, indent_level, target)?;
        let case_span = Span::merge(&pattern.span(), &t.span());
        Ok(match_case(pattern, t, case_span))
    };

    let cases = parse_indented_block(tq, indent_level, parse_match_case, target)?;

    Ok(match_expression(target_expr, cases, span.expanded(tq.pos())))
}

fn parse_lambda(tq: &mut TokenQueue, span: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let args = parse_function_arguments(tq, &Type::Unknown, indent_level, target)?;
    tq.expect(&TokenKind::Arrow)?;
    let expr = parse_expression(tq, indent_level, target)?;
    Ok(lambda(args, expr, span.expanded(tq.pos())))
}

fn is_end_of_bindings(tq: &mut TokenQueue, indent_level: usize) -> bool {
    tq.is_next(&TokenKind::In)
        || tq.is_next(&TokenKind::SemiColon)
        || tq.is_next(&TokenKind::CloseParen)
        || tq.is_next(&TokenKind::CloseCurly)
        || tq.is_next(&TokenKind::CloseBracket)
        || !tq.is_in_same_block(indent_level + 1)
}

fn parse_bindings(
    tq: &mut TokenQueue,
    mutable: bool,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Vec<Binding>> {
    let mut bindings = Vec::new();
    while !is_end_of_bindings(tq, indent_level) {
        tq.pop_indent()?;

        let (binding_type, span) = if tq.is_next(&TokenKind::OpenCurly) {
            let span = tq.peek().expect("Unexpected EOF").span.clone();
            let pattern = parse_struct_pattern(tq, "", &span, indent_level, target)?;
            let span = pattern.span.clone();
            (BindingType::Struct(pattern), span)
        } else {
            let (name, span) = tq.expect_identifier()?;
            (BindingType::Name(name), span)
        };

        let typ = parse_type_of_binding(tq, indent_level, target)?;
        tq.expect(&TokenKind::Assign(AssignOperator::Assign))?;
        let init = parse_expression(tq, indent_level, target)?;
        bindings.push(binding(binding_type, init, mutable, typ, span.expanded(tq.pos())));
        eat_comma(tq)?;
    }

    Ok(bindings)
}

fn parse_binding(
    tq: &mut TokenQueue,
    mutable: bool,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    let b = parse_bindings(tq, mutable, indent_level, target)?;
    Ok(bindings(b, span.expanded(tq.pos())))
}

fn parse_if(tq: &mut TokenQueue, span: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let cond = parse_expression(tq, indent_level, target)?;
    let colon = tq.expect(&TokenKind::Colon)?;
    let on_true = parse_block(tq, &colon.span, indent_level, target)?;
    if tq.is_next(&TokenKind::Indent(indent_level)) {
        tq.pop_indent()?;
    }

    if let Some(else_tok) = tq.pop_if(|t| t.kind == TokenKind::Else)? {
        let on_false = if let Some(tok) = tq.pop_if(|t| t.kind == TokenKind::If)? {
            parse_if(tq, &tok.span, indent_level, target)?
        } else {
            parse_block(tq, &else_tok.span, indent_level, target)?
        };

        Ok(if_expression(cond, on_true, on_false, span.expanded(tq.pos())))
    } else {
        Ok(single_if_expression(cond, on_true, span.expanded(tq.pos())))
    }
}

fn parse_sum_type(
    tq: &mut TokenQueue,
    namespace: &str,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<SumTypeDeclaration> {
    let (sum_type_name, _) = tq.expect_identifier()?;
    let implements = if (tq.pop_if(|t| t.kind == TokenKind::Implements)?).is_some() {
        parse_implements_list(tq, indent_level, target)?
    } else {
        tq.expect(&TokenKind::Colon)?;
        Vec::new()
    };

    let parse_sum_type_case = |tq: &mut TokenQueue, indent_level: usize, target: &Target| {
        if tq.is_next_at(1, &TokenKind::OpenCurly) {
            let sd = parse_struct_declaration(tq, &sum_type_name, indent_level, target)?;
            let span = sd.span.clone();
            let name = namespaced(namespace, &sd.name);
            Ok(sum_type_case_decl(&name, Some(sd), span))
        } else {
            let (case_name, case_name_span) = tq.expect_identifier()?;
            let name = format!("{}::{}::{}", namespace, sum_type_name, case_name);
            Ok(sum_type_case_decl(&name, None, case_name_span))
        }
    };
    let cases = parse_indented_block(tq, indent_level, parse_sum_type_case, target)?;

    Ok(sum_type_decl(
        &namespaced(namespace, &sum_type_name),
        cases,
        implements,
        span.expanded(tq.pos()),
    ))
}

fn namespaced(namespace: &str, name: &str) -> String {
    if namespace.is_empty() {
        name.into()
    } else {
        format!("{}::{}", namespace, name)
    }
}

fn parse_implements_list(
    tq: &mut TokenQueue,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Vec<(Type, Span)>> {
    parse_comma_separated_list(tq, &TokenKind::Colon, parse_type, indent_level, target)
}

fn parse_struct_declaration(
    tq: &mut TokenQueue,
    namespace: &str,
    indent_level: usize,
    target: &Target,
) -> CompileResult<StructDeclaration> {
    let (name, span) = tq.expect_identifier()?;

    let parse_struct_member = |tq: &mut TokenQueue, indent_level: usize, target: &Target| {
        let (member_name, member_name_span) = tq.expect_identifier()?;
        tq.expect(&TokenKind::Colon)?;
        let (typ, _) = parse_type(tq, indent_level, target)?;
        Ok(struct_member_declaration(
            &member_name,
            typ,
            member_name_span.expanded(tq.pos()),
        ))
    };

    let (members, implements) = if tq.is_next(&TokenKind::OpenCurly) {
        tq.expect(&TokenKind::OpenCurly)?;
        (
            parse_comma_separated_list(tq, &TokenKind::CloseCurly, parse_struct_member, indent_level, target)?,
            Vec::new(),
        )
    } else {
        let implements = if (tq.pop_if(|t| t.kind == TokenKind::Implements)?).is_some() {
            parse_implements_list(tq, indent_level, target)?
        } else {
            tq.expect(&TokenKind::Colon)?;
            Vec::new()
        };
        (
            parse_indented_block(tq, indent_level, parse_struct_member, target)?,
            implements,
        )
    };

    Ok(struct_declaration(
        &namespaced(namespace, &name),
        members,
        implements,
        span.expanded(tq.pos()),
    ))
}

fn with_ignore_indents<T>(
    tq: &mut TokenQueue,
    mut op: impl FnMut(&mut TokenQueue) -> CompileResult<T>,
) -> CompileResult<T> {
    tq.set_ignore_indents(true);
    let ret = op(tq);
    tq.set_ignore_indents(false);
    ret
}

fn parse_struct_initializer(
    tq: &mut TokenQueue,
    name: Option<NameRef>,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    let open_curly = tq.expect(&TokenKind::OpenCurly)?;
    let span = if let Some(n) = &name {
        n.span.clone()
    } else {
        open_curly.span
    };

    let expressions = with_ignore_indents(tq, |tq: &mut TokenQueue| {
        let mut expressions = Vec::new();
        while !tq.is_next(&TokenKind::CloseCurly) {
            let (name, _) = tq.expect_identifier()?;
            tq.expect(&TokenKind::Colon)?;
            let e = parse_expression(tq, indent_level, target)?;
            expressions.push(StructMemberInitializer {
                name,
                initializer: e,
                member_idx: 0,
            });

            if !tq.is_next(&TokenKind::CloseCurly) {
                tq.expect(&TokenKind::Comma)?;
            }
        }
        tq.expect(&TokenKind::CloseCurly)?;
        Ok(expressions)
    })?;

    Ok(Expression::StructInitializer(struct_initializer(
        name.map(|nr| nr.name),
        expressions,
        span.expanded(tq.pos()),
    )))
}

fn parse_member_access(
    tq: &mut TokenQueue,
    left_expr: Expression,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    let mut left = left_expr;
    while tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::Dot)) {
        tq.pop()?;
        let (name, name_span) = tq.expect_identifier()?;

        if tq.is_next(&TokenKind::OpenParen) {
            let call = Box::new(parse_function_call(
                tq,
                NameRef::new(name, name_span),
                indent_level,
                target,
            )?);
            let span = Span::merge(&left.span(), &call.span);
            left = member_access(left, MemberAccessType::Call(call), span);
        } else {
            let ma = MemberAccessType::Name(field(&name, 0));
            let span = Span::merge(&left.span(), &name_span);
            left = member_access(left, ma, span);
        }
    }

    Ok(left)
}

fn check_indent_level(tq: &mut TokenQueue, indent_level: usize) -> CompileResult<usize> {
    if let Some((level, indent_span)) = tq.pop_indent()? {
        if level <= indent_level {
            return parse_error_result(&indent_span, "Expecting an indented block");
        }

        Ok(level)
    } else {
        Ok(indent_level + 1)
    }
}

fn is_end_of_block(tq: &mut TokenQueue) -> bool {
    tq.peek()
        .map(|tok| {
            matches!(
                tok.kind,
                TokenKind::CloseParen | TokenKind::CloseBracket | TokenKind::Else | TokenKind::EOF
            )
        })
        .unwrap_or(true)
}

fn parse_block(tq: &mut TokenQueue, start: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let mut ends_with_semicolon = false;
    let mut expressions = Vec::new();
    let block_indent_level = check_indent_level(tq, indent_level)?;

    while tq.is_in_same_block(block_indent_level) {
        tq.pop_indent()?;
        if is_end_of_block(tq) {
            break;
        }

        if tq.is_next(&TokenKind::Indent(block_indent_level)) {
            continue;
        }

        let e = parse_expression(tq, block_indent_level, target)?;
        if let Some(op) = tq.is_next_assign_operator() {
            tq.pop()?;

            let rhs = parse_expression(tq, block_indent_level, target)?;
            let span = e.span().expanded(tq.pos());

            let assign_expr = match e {
                Expression::NameRef(nr) => assign(op, AssignTarget::Var(nr), rhs, span),
                Expression::MemberAccess(ma) => assign(op, AssignTarget::MemberAccess(*ma), rhs, span),
                Expression::Dereference(d) => assign(op, AssignTarget::Dereference(*d), rhs, span),
                Expression::IndexOperation(iop) => assign(op, AssignTarget::IndexOperation(*iop), rhs, span),
                _ => {
                    return parse_error_result(
                        &e.span(),
                        "Expression not allowed on the left hand side of an assignment",
                    )
                }
            };
            expressions.push(assign_expr);
        } else {
            expressions.push(e);
        }

        ends_with_semicolon = false;
        while tq.is_next(&TokenKind::SemiColon) {
            tq.pop()?;
            ends_with_semicolon = true;
        }
    }

    if ends_with_semicolon {
        expressions.push(Expression::Void);
    }

    if expressions.len() == 1 {
        let mut e = expressions.remove(0);
        // Increase it's precedence, because it is actually a block
        e.set_precedence(TOP_PRECEDENCE);
        Ok(e)
    } else {
        Ok(block_expr(expressions, start.expanded(tq.pos())))
    }
}

fn parse_while(tq: &mut TokenQueue, start: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let cond = parse_expression(tq, indent_level, target)?;
    let colon = tq.expect(&TokenKind::Colon)?;
    let body = parse_block(tq, &colon.span, indent_level, target)?;
    Ok(while_loop(cond, body, start.expanded(tq.pos())))
}

fn parse_for(tq: &mut TokenQueue, start: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let (loop_variable, _) = tq.expect_identifier()?;
    tq.expect(&TokenKind::In)?;

    let iterable = parse_expression(tq, indent_level, target)?;
    let colon = tq.expect(&TokenKind::Colon)?;

    let body = parse_block(tq, &colon.span, indent_level, target)?;
    Ok(for_loop(&loop_variable, iterable, body, start.expanded(tq.pos())))
}

fn parse_compiler_call(
    tq: &mut TokenQueue,
    start: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    let (name, name_span) = tq.expect_identifier()?;
    match &name[..] {
        "size" => {
            tq.expect(&TokenKind::OpenParen)?;
            let (typ, _) = parse_type(tq, indent_level, target)?;
            tq.expect(&TokenKind::CloseParen)?;

            Ok(cc_size_of(typ, target.int_size, start.expanded(tq.pos())))
        }

        "drop" => {
            tq.expect(&TokenKind::OpenParen)?;
            let obj = parse_expression(tq, indent_level, target)?;
            tq.expect(&TokenKind::CloseParen)?;
            Ok(cc_drop(obj, start.expanded(tq.pos()), None, None))
        }

        _ => parse_error_result(&name_span, format!("Unknown compiler call {}", name)),
    }
}

fn parse_return(tq: &mut TokenQueue, start: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    if !tq.is_in_same_block(indent_level) {
        Ok(return_expr(Expression::Void, start.clone()))
    } else {
        let expr = parse_expression(tq, indent_level, target)?;
        Ok(return_expr(expr, start.expanded(tq.pos())))
    }
}

fn parse_range(
    tq: &mut TokenQueue,
    lhs: Option<Expression>,
    start: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    tq.expect(&TokenKind::DotDot)?;
    if tq.peek().map(is_end_of_expression).unwrap_or(false) {
        Ok(range(lhs, None, Type::Unknown, start.expanded(tq.pos())))
    } else {
        let rhs = parse_expression(tq, indent_level, target)?;
        let span = Span::merge(start, &rhs.span());
        Ok(range(lhs, Some(rhs), Type::Unknown, span))
    }
}

fn parse_optional_constructor(
    tq: &mut TokenQueue,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    let tok = tq.pop()?;
    let e = parse_expression_start(tq, tok, indent_level, target)?;
    Ok(to_optional(e, Type::Unknown, span.expanded(tq.pos())))
}

fn parse_expression_start(
    tq: &mut TokenQueue,
    tok: Token,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    let mut lhs = match tok.kind {
        TokenKind::Nil => nil_expr(tok.span),

        TokenKind::Null => Expression::Literal(Literal::NullPtr(tok.span, Type::Unknown)),

        TokenKind::True => Expression::Literal(Literal::Bool(tok.span, true)),

        TokenKind::False => Expression::Literal(Literal::Bool(tok.span, false)),

        TokenKind::CharLiteral(c) => Expression::Literal(Literal::Char(tok.span, c)),

        TokenKind::Func => parse_lambda(tq, &tok.span, indent_level, target)?,

        TokenKind::Match => parse_match(tq, &tok.span, indent_level, target)?,

        TokenKind::Let => parse_binding(tq, false, &tok.span, indent_level, target)?,

        TokenKind::Var => parse_binding(tq, true, &tok.span, indent_level, target)?,

        TokenKind::If => parse_if(tq, &tok.span, indent_level, target)?,

        TokenKind::While => parse_while(tq, &tok.span, indent_level, target)?,

        TokenKind::For => parse_for(tq, &tok.span, indent_level, target)?,

        TokenKind::OpenBracket => parse_array_literal(tq, &tok.span, indent_level, target).map(Expression::Literal)?,

        TokenKind::QuestionMark => parse_optional_constructor(tq, &tok.span, indent_level, target)?,

        TokenKind::OpenParen => {
            let inner = parse_block(tq, &tok.span, indent_level, target)?;
            tq.expect(&TokenKind::CloseParen)?;
            Expression::Enclosed(Box::new(inner))
        }

        TokenKind::OpenCurly => {
            tq.push_front(tok.clone());
            parse_struct_initializer(tq, None, indent_level, target)?
        }

        TokenKind::Identifier(id) => {
            let nr = parse_name(tq, id, &tok.span)?;
            if tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::Dot)) {
                parse_member_access(tq, Expression::NameRef(nr), indent_level, target)?
            } else if tq.is_next(&TokenKind::OpenParen) {
                Expression::Call(Box::new(parse_function_call(tq, nr, indent_level, target)?))
            } else if tq.is_next(&TokenKind::OpenCurly) {
                parse_struct_initializer(tq, Some(nr), indent_level, target)?
            } else {
                Expression::NameRef(nr)
            }
        }

        TokenKind::StringLiteral(s) => Expression::Literal(Literal::String(tok.span, s)),

        TokenKind::Number(n) => parse_number(tq, &n, &tok.span, target).map(Expression::Literal)?,

        TokenKind::New => {
            let inner = parse_expression(tq, indent_level, target)?;
            new(inner, tok.span.expanded(tq.pos()))
        }

        TokenKind::Delete => {
            let inner = parse_expression(tq, indent_level, target)?;
            delete(inner, tok.span.expanded(tq.pos()))
        }

        TokenKind::UnaryOperator(op) => parse_unary_expression(tq, op, &tok.span, indent_level, target)?,

        TokenKind::BinaryOperator(BinaryOperator::Sub) => {
            parse_unary_expression(tq, UnaryOperator::Sub, &tok.span, indent_level, target)?
        }

        TokenKind::BinaryOperator(BinaryOperator::BitwiseAnd) => {
            let inner = parse_expression(tq, indent_level, target)?;
            address_of(inner, tok.span.expanded(tq.pos()))
        }

        TokenKind::BinaryOperator(BinaryOperator::Mul) => {
            let next_tok = tq.pop()?;
            let inner = parse_expression_start(tq, next_tok, indent_level, target)?;
            dereference(inner, tok.span.expanded(tq.pos()))
        }

        TokenKind::At => parse_compiler_call(tq, &tok.span, indent_level, target)?,

        TokenKind::Return => parse_return(tq, &tok.span, indent_level, target)?,

        TokenKind::Ok => {
            let inner = parse_expression(tq, indent_level, target)?;
            let span = Span::merge(&tok.span, &inner.span());
            to_ok_result(inner, Type::Unknown, span)
        }

        TokenKind::Error => {
            let inner = parse_expression(tq, indent_level, target)?;
            let span = Span::merge(&tok.span, &inner.span());
            to_err_result(inner, Type::Unknown, span)
        }

        _ => return parse_error_result(&tok.span, format!("Unexpected token '{}'", tok)),
    };

    while let Some(next) = tq.pop_if(|t| !is_end_of_expression(t))? {
        match next.kind {
            TokenKind::OpenBracket => {
                let index_expr = if tq.is_next(&TokenKind::DotDot) {
                    parse_range(tq, None, &next.span, indent_level, target)?
                } else {
                    parse_expression(tq, indent_level, target)?
                };

                tq.expect(&TokenKind::CloseBracket)?;
                let span = lhs.span().expanded(tq.pos());
                lhs = index_op(lhs, index_expr, span);
            }

            TokenKind::BinaryOperator(BinaryOperator::Dot) => {
                tq.push_front(next);
                lhs = parse_member_access(tq, lhs, indent_level, target)?;
            }

            TokenKind::QuestionMark => {
                let span = Span::merge(&lhs.span(), &next.span);
                lhs = unary_op(UnaryOperator::TryOptional, lhs, span);
            }

            TokenKind::UnaryOperator(UnaryOperator::Not) => {
                let span = Span::merge(&lhs.span(), &next.span);
                lhs = unary_op(UnaryOperator::TryResult, lhs, span)
            }

            _ => {
                tq.push_front(next);
                break;
            }
        }
    }

    Ok(lhs)
}

pub fn parse_expression(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let tok = tq.pop()?;
    let e_start = parse_expression_start(tq, tok, indent_level, target)?;
    if tq.is_next_binary_operator() {
        parse_binary_op_rhs(tq, e_start, indent_level, target)
    } else if tq.is_next(&TokenKind::DotDot) {
        let start = e_start.span();
        parse_range(tq, Some(e_start), &start, indent_level, target)
    } else {
        Ok(e_start)
    }
}

fn parse_type_of_binding(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<Type> {
    if (tq.pop_if(|t| t.kind == TokenKind::Colon)?).is_some() {
        parse_type(tq, indent_level, target).map(|(t, _)| t)
    } else {
        Ok(Type::Unknown)
    }
}

fn parse_global_bindings(
    module: &mut Module,
    tq: &mut TokenQueue,
    mutable: bool,
    thread_local: bool,
    indent_level: usize,
    namespace: &str,
    target: &Target,
) -> CompileResult<()> {
    while !is_end_of_bindings(tq, indent_level) {
        let (name, span) = tq.expect_identifier()?;
        let typ = parse_type_of_binding(tq, indent_level, target)?;

        tq.expect(&TokenKind::Assign(AssignOperator::Assign))?;
        let init = parse_expression(tq, indent_level, target)?;

        if module.globals.contains_key(&name) {
            return parse_error_result(&span, format!("Global {} already defined in this module", name));
        }

        let full_name = namespaced(namespace, &name);
        module.globals.insert(
            full_name.clone(),
            global_binding(full_name, init, mutable, thread_local, typ, span.expanded(tq.pos())),
        );
        eat_comma(tq)?;
    }

    Ok(())
}

fn parse_interface(
    module: &mut Module,
    tq: &mut TokenQueue,
    namespace: &str,
    span: &Span,
    indent_level: usize,
    target: &Target,
) -> CompileResult<()> {
    let (name, _) = tq.expect_identifier()?;
    if module.types.contains_key(&name) {
        return parse_error_result(span, format!("Type {} already defined in this module", name));
    }

    tq.expect(&TokenKind::Colon)?;
    let self_type = ptr_type(Type::SelfType);

    let parse_interface_function = |tq: &mut TokenQueue, indent_level: usize, target: &Target| {
        tq.expect(&TokenKind::Func)?;
        parse_function_signature(tq, &self_type, indent_level, target)
    };

    let functions = parse_indented_block(tq, indent_level, parse_interface_function, target)?;

    let name = namespaced(namespace, &name);
    module.types.insert(
        name.clone(),
        TypeDeclaration::Interface(interface(name, functions, span.expanded(tq.pos()))),
    );
    Ok(())
}

fn parse_import_name(tq: &mut TokenQueue) -> CompileResult<ImportName> {
    let mut namespace = Vec::new();
    let mut span = Span::default();
    loop {
        let (name, name_span) = tq.expect_identifier()?;
        if namespace.is_empty() {
            span = name_span;
        } else {
            span = Span::merge(&span, &name_span);
        }

        namespace.push(name);
        if tq.is_next(&TokenKind::DoubleColon) {
            tq.pop()?;
        } else {
            break;
        }
    }

    Ok(ImportName::new(namespace, span))
}

fn add_function(module: &mut Module, func: Function) -> CompileResult<()> {
    if module.functions.contains_key(&func.sig.name) {
        return parse_error_result(&func.span, format!("Function {} redefined", func.sig.name));
    }
    module.functions.insert(func.sig.name.clone(), func);
    Ok(())
}

fn parse_external_var(
    tq: &mut TokenQueue,
    module: &mut Module,
    indent_level: &mut usize,
    target: &Target,
    start: &Span,
    thread_local: bool,
) -> CompileResult<()> {
    let mutable = tq.pop()?.kind == TokenKind::Var;
    let (name, span) = tq.expect_identifier()?;
    if module.externals.contains_key(&name) {
        return parse_error_result(&span, format!("External {} redefined", name));
    }

    tq.expect(&TokenKind::Colon)?;
    let (typ, typ_span) = parse_type(tq, *indent_level, target)?;
    let span = start.expanded(typ_span.end());
    module.externals.insert(
        name.clone(),
        External::Variable {
            name,
            span,
            typ,
            mutable,
            thread_local,
        },
    );
    Ok(())
}

fn parse_extern(
    tq: &mut TokenQueue,
    module: &mut Module,
    indent_level: &mut usize,
    target: &Target,
    start: &Span,
) -> CompileResult<()> {
    if tq.is_next(&TokenKind::Func) {
        let ext_func = parse_external_function(tq, start, *indent_level, target)?;
        if module.externals.contains_key(ext_func.name()) {
            return parse_error_result(ext_func.span(), format!("External {} redefined", ext_func.name()));
        }
        module
            .externals
            .insert(ext_func.name().to_owned(), ext_func);
        Ok(())
    } else if tq.is_next(&TokenKind::Let) || tq.is_next(&TokenKind::Var) {
        parse_external_var(tq, module, indent_level, target, start, false)
    } else if tq.is_next(&TokenKind::ThreadLocal) {
        tq.pop()?;
        parse_external_var(tq, module, indent_level, target, start, true)
    } else {
        let tok = tq.pop()?;
        return parse_error_result(
            &tok.span,
            format!("Expecting fn, let or var after extern got {}", tok.kind),
        );
    }
}

fn parse_top_level(
    tq: &mut TokenQueue,
    module: &mut Module,
    indent_level: &mut usize,
    namespace: &str,
    target: &Target,
) -> CompileResult<()> {
    let tok = tq.pop()?;
    match tok.kind {
        TokenKind::Indent(level) => {
            *indent_level = level;
        }

        TokenKind::Interface => {
            parse_interface(module, tq, namespace, &tok.span, *indent_level, target)?;
        }

        TokenKind::ThreadLocal => {
            let mode = tq.pop()?;
            match mode.kind {
                TokenKind::Let => {
                    parse_global_bindings(module, tq, false, true, *indent_level, namespace, target)?;
                }

                TokenKind::Var => {
                    parse_global_bindings(module, tq, true, true, *indent_level, namespace, target)?;
                }
                _ => {
                    return parse_error_result(&mode.span, format!("Expected let or var found token {}", mode));
                }
            }
        }

        TokenKind::Let => {
            parse_global_bindings(module, tq, false, false, *indent_level, namespace, target)?;
        }

        TokenKind::Var => {
            parse_global_bindings(module, tq, true, false, *indent_level, namespace, target)?;
        }

        TokenKind::Struct => {
            let mut sd = parse_struct_declaration(tq, namespace, *indent_level, target)?;
            sd.span = Span::merge(&tok.span, &sd.span);
            if module.types.contains_key(&sd.name) {
                return parse_error_result(&sd.span, format!("Type {} redefined", sd.name));
            }
            module
                .types
                .insert(sd.name.clone(), TypeDeclaration::Struct(sd));
        }

        TokenKind::Enum => {
            let st = parse_sum_type(tq, namespace, &tok.span, *indent_level, target)?;
            if module.types.contains_key(&st.name) {
                return parse_error_result(&st.span, format!("Type {} redefined", st.name));
            }
            module
                .types
                .insert(st.name.clone(), TypeDeclaration::Sum(st));
        }

        TokenKind::Type => {
            panic!("NYI");
        }

        TokenKind::Extern => parse_extern(tq, module, indent_level, target, &tok.span)?,

        TokenKind::Import => loop {
            let import = parse_import_name(tq)?;
            module.import_names.insert(import);
            if tq.is_next(&TokenKind::Comma) {
                tq.pop()?;
            } else {
                break;
            }
        },

        TokenKind::Func => {
            let func = parse_function_declaration(tq, namespace, &tok.span, *indent_level, target)?;
            add_function(module, func)?;
        }

        _ => {
            return parse_error_result(
                &tok.span,
                format!(
                    "Expected import, fn, let, var, extern, type, struct, enum or interface found token {}",
                    tok
                ),
            );
        }
    }
    Ok(())
}

fn skip_until_next_top_level_declaration(tq: &mut TokenQueue) -> CompileResult<()> {
    while !tq.is_next(&TokenKind::EOF) {
        if let Some(tok) = tq.peek() {
            if let TokenKind::Indent(0) = &tok.kind {
                break;
            }
        } else {
            break;
        }

        tq.pop()?;
    }

    Ok(())
}

fn parse_module<Input: Read>(
    module: &mut Module,
    input: &mut Input,
    namespace: &str,
    file_name: &str,
    target: &Target,
) -> CompileResult<()> {
    let mut tq = Lexer::new(file_name).read(input)?;
    let mut errors = Vec::new();
    let mut indent_level = 0;
    while !tq.is_next(&TokenKind::EOF) {
        if let Err(e) = parse_top_level(&mut tq, module, &mut indent_level, namespace, target) {
            errors.push(e);
            if let Err(e) = skip_until_next_top_level_declaration(&mut tq) {
                errors.push(e);
                break;
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        CompileResult::Err(CompileError::Many(errors))
    }
}

pub fn parse_file(file_path: &Path, namespace: &str, target: &Target, show_timing: bool) -> CompileResult<Module> {
    let op_name = format!("Parsing {}", file_path.to_string_lossy());
    time_operation(show_timing, 2, &op_name, || {
        let mut module = Module::new(namespace);
        let mut file = fs::File::open(file_path)?;
        parse_module(
            &mut module,
            &mut file,
            namespace,
            file_path.to_string_lossy().deref(),
            target,
        )?;
        Ok(module)
    })
}

#[cfg(test)]
use crate::build::Package;

#[cfg(test)]
pub fn parse_str(code: &str, root_namespace: &str, target: &Target) -> CompileResult<Package> {
    use crate::build::PackageDescription;
    use crate::llvmbackend::OutputType;
    use std::io::Cursor;

    let mut pkg = Package::new(root_namespace, OutputType::Binary, &PackageDescription::default());
    let mut module = Module::new(root_namespace);
    let mut cursor = Cursor::new(code);
    parse_module(&mut module, &mut cursor, root_namespace, "", target)?;
    pkg.modules.insert(root_namespace.into(), module);
    Ok(pkg)
}
