mod lexer;
mod tokenqueue;
mod tokens;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub use self::tests::{th_expr, th_mod};

use std::fs;
use std::io::Read;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;

use crate::ast::*;
use crate::compileerror::{parse_error_result, CompileResult};
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
            if int_size.size_in_bits() == 64 {
                u64::max_value()
            } else {
                2u64.pow(int_size.size_in_bits()) - 1
            }
        } else {
            2u64.pow(int_size.size_in_bits() - 1) - 1
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
        match num.parse::<f64>() {
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
        match num.parse::<u64>() {
            Ok(i) => number_to_literal(i, force_unsigned, span, target.int_size),
            Err(_) => parse_error_result(span, format!("{} is not a valid integer", num)),
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
            let typ = parse_type(tq, indent_level, target)?;
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
    Ok(args)
}

fn to_primitive(name: &str, target: &Target) -> Option<Type> {
    match name {
        "int8" => Some(Type::Int(IntSize::I8)),
        "int16" => Some(Type::Int(IntSize::I16)),
        "int32" => Some(Type::Int(IntSize::I32)),
        "int64" => Some(Type::Int(IntSize::I64)),
        "int" => Some(target.native_int_type.clone()),
        "uint8" => Some(Type::UInt(IntSize::I8)),
        "uint16" => Some(Type::UInt(IntSize::I16)),
        "uint32" => Some(Type::UInt(IntSize::I32)),
        "uint64" => Some(Type::UInt(IntSize::I64)),
        "uint" => Some(target.native_uint_type.clone()),
        "float" | "float32" => Some(Type::Float(FloatSize::F32)),
        "double" | "float64" => Some(Type::Float(FloatSize::F64)),
        "string" => Some(Type::String),
        "bool" => Some(Type::Bool),
        "char" => Some(Type::Char),
        "Self" => Some(Type::SelfType),
        "void" => Some(Type::Void),
        _ => None,
    }
}

fn parse_start_of_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<Type> {
    if tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::Mul)) {
        tq.pop()?;
        let inner = parse_type(tq, indent_level, target)?;
        Ok(Type::Pointer(Rc::new(inner)))
    } else if tq.is_next(&TokenKind::Dollar) {
        tq.pop()?;
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
            Ok(generic_type_with_constraints(constraints))
        } else {
            let (name, _span) = tq.expect_identifier()?;
            Ok(generic_type(&name))
        }
    } else if tq.is_next(&TokenKind::QuestionMark) {
        tq.pop()?;
        let inner = parse_type(tq, indent_level, target)?;
        Ok(Type::Optional(Rc::new(inner)))
    } else if tq.is_next(&TokenKind::Func) {
        // Function signature: fn(a, b) -> c
        tq.pop()?;
        tq.expect(&TokenKind::OpenParen)?;
        let args = parse_comma_separated_list(tq, &TokenKind::CloseParen, parse_type, indent_level, target)?;
        tq.expect(&TokenKind::Arrow)?;
        let ret = parse_type(tq, indent_level, target)?;
        Ok(func_type(args, ret))
    } else if tq.is_next(&TokenKind::OpenCurly) {
        tq.pop()?;
        let member_types = parse_comma_separated_list(tq, &TokenKind::CloseCurly, parse_type, indent_level, target)?;
        Ok(struct_type(
            "",
            member_types
                .into_iter()
                .map(|member_type| struct_member("", member_type))
                .collect(),
        ))
    } else {
        let (name, _pos) = tq.expect_identifier()?;
        match to_primitive(&name, target) {
            Some(t) => Ok(t),
            None => {
                let generic_args = parse_generic_arg_list(tq, indent_level, target)?;
                Ok(unresolved_type(&name, generic_args))
            }
        }
    }
}

fn parse_type(tq: &mut TokenQueue, indent_level: usize, target: &Target) -> CompileResult<Type> {
    let mut typ = parse_start_of_type(tq, indent_level, target)?;
    while tq.is_next(&TokenKind::OpenBracket) {
        tq.pop()?;
        if tq.is_next(&TokenKind::CloseBracket) {
            tq.pop()?;
            typ = slice_type(typ);
        } else {
            let (len, _span) = tq.expect_int()?;
            typ = array_type(typ, len as usize);
            tq.expect(&TokenKind::CloseBracket)?;
        }
    }

    Ok(typ)
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
        parse_type(tq, indent_level, target)?
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
        parse_type(tq, indent_level, target)?
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
) -> CompileResult<ExternalFunction> {
    tq.expect(&TokenKind::Func)?;
    Ok(ExternalFunction::new(
        parse_function_signature(tq, &Type::Unknown, indent_level, target)?,
        span.expanded(tq.pos()),
    ))
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
        parse_type(tq, indent_level, target)?
    } else {
        Type::Void
    };

    let signature = sig(&full_name, ret_type, args, span.expanded(tq.pos()));
    tq.expect(&TokenKind::Colon)?;

    let expr = parse_block(tq, &span.file, indent_level, target)?;
    let func_span = span.expanded(expr.span().end);
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
            } else if tq.is_next_at(1, &TokenKind::Pipe) {
                let (head, _head_span) = tq.expect_identifier()?;
                tq.expect(&TokenKind::Pipe)?;
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
            Ok(optional_pattern(name, tok.span.expanded(name_span.end)))
        }

        TokenKind::Nil => Ok(Pattern::Nil(tok.span)),

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
        let t = parse_block(tq, &tok.span.file, indent_level, target)?;
        let case_span = pattern.span().expanded(t.span().end);
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

        tq.expect(&TokenKind::Assign(AssignOperator::Assign))?;
        let init = parse_expression(tq, indent_level, target)?;
        bindings.push(binding(binding_type, init, mutable, span.expanded(tq.pos())));
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
    tq.expect(&TokenKind::Colon)?;
    let on_true = parse_block(tq, &span.file, indent_level, target)?;
    if tq.is_next(&TokenKind::Indent(indent_level)) {
        tq.pop_indent()?;
    }

    if tq.is_next(&TokenKind::Else) {
        tq.expect(&TokenKind::Else)?;
        let on_false = if tq.is_next(&TokenKind::If) {
            let tok = tq.expect(&TokenKind::If)?;
            parse_if(tq, &tok.span, indent_level, target)?
        } else {
            parse_block(tq, &span.file, indent_level, target)?
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
    tq.expect(&TokenKind::Colon)?;

    let parse_sum_type_case = |tq: &mut TokenQueue, indent_level: usize, target: &Target| {
        if tq.is_next_at(1, &TokenKind::OpenCurly) {
            let sd = parse_struct_type(tq, &sum_type_name, indent_level, target)?;
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

fn parse_struct_type(
    tq: &mut TokenQueue,
    namespace: &str,
    indent_level: usize,
    target: &Target,
) -> CompileResult<StructDeclaration> {
    let (name, span) = tq.expect_identifier()?;

    let parse_struct_member = |tq: &mut TokenQueue, indent_level: usize, target: &Target| {
        let (member_name, member_name_span) = tq.expect_identifier()?;
        tq.expect(&TokenKind::Colon)?;
        let typ = parse_type(tq, indent_level, target)?;
        Ok(struct_member_declaration(
            &member_name,
            typ,
            member_name_span.expanded(tq.pos()),
        ))
    };

    let members = if tq.is_next(&TokenKind::OpenCurly) {
        tq.expect(&TokenKind::OpenCurly)?;
        parse_comma_separated_list(tq, &TokenKind::CloseCurly, parse_struct_member, indent_level, target)?
    } else {
        tq.expect(&TokenKind::Colon)?;
        parse_indented_block(tq, indent_level, parse_struct_member, target)?
    };

    Ok(struct_declaration(
        &namespaced(namespace, &name),
        members,
        span.expanded(tq.pos()),
    ))
}

fn parse_struct_initializer(
    tq: &mut TokenQueue,
    name: &NameRef,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
    tq.expect(&TokenKind::OpenCurly)?;
    let expressions = parse_comma_separated_list(tq, &TokenKind::CloseCurly, parse_expression, indent_level, target)?;
    Ok(Expression::StructInitializer(struct_initializer(
        &name.name,
        expressions,
        name.span.expanded(tq.pos()),
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
            let span = left.span().expanded(call.span.end);
            left = member_access(left, MemberAccessType::Call(call), span);
        } else {
            let ma = MemberAccessType::Name(field(&name, 0));
            let span = left.span().expanded(name_span.end);
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

fn parse_block(
    tq: &mut TokenQueue,
    current_file: &str,
    indent_level: usize,
    target: &Target,
) -> CompileResult<Expression> {
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
        let start = expressions
            .get(0)
            .map(|e| e.span())
            .unwrap_or_else(|| Span::single(current_file, tq.pos()));
        Ok(block(expressions, start.expanded(tq.pos())))
    }
}

fn parse_while(tq: &mut TokenQueue, start: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let cond = parse_expression(tq, indent_level, target)?;
    tq.expect(&TokenKind::Colon)?;
    let body = parse_block(tq, &start.file, indent_level, target)?;
    Ok(while_loop(cond, body, start.expanded(tq.pos())))
}

fn parse_for(tq: &mut TokenQueue, start: &Span, indent_level: usize, target: &Target) -> CompileResult<Expression> {
    let (loop_variable, _) = tq.expect_identifier()?;
    tq.expect(&TokenKind::In)?;

    let iterable = parse_expression(tq, indent_level, target)?;
    tq.expect(&TokenKind::Colon)?;

    let body = parse_block(tq, &start.file, indent_level, target)?;
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
            let typ = parse_type(tq, indent_level, target)?;
            tq.expect(&TokenKind::CloseParen)?;

            Ok(Expression::CompilerCall(CompilerCall::SizeOf(
                typ,
                start.expanded(tq.pos()),
            )))
        }

        "slice" => {
            tq.expect(&TokenKind::OpenParen)?;
            let arguments =
                parse_comma_separated_list(tq, &TokenKind::CloseParen, parse_expression, indent_level, target)?;
            let span = start.expanded(tq.pos());
            if arguments.len() != 2 {
                return parse_error_result(&span, "@slice expects two arguments");
            }

            Ok(Expression::CompilerCall(CompilerCall::Slice {
                data: Box::new(arguments[0].clone()),
                len: Box::new(arguments[1].clone()),
                typ: Type::Unknown,
                span,
            }))
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

        TokenKind::OpenParen => {
            let inner = parse_block(tq, &tok.span.file, indent_level, target)?;
            tq.expect(&TokenKind::CloseParen)?;
            inner
        }

        TokenKind::OpenCurly => {
            tq.push_front(tok.clone());
            parse_struct_initializer(tq, &NameRef::new("".into(), tok.span), indent_level, target)?
        }

        TokenKind::Identifier(id) => {
            let nr = parse_name(tq, id, &tok.span)?;
            if tq.is_next(&TokenKind::BinaryOperator(BinaryOperator::Dot)) {
                parse_member_access(tq, Expression::NameRef(nr), indent_level, target)?
            } else if tq.is_next(&TokenKind::OpenParen) {
                Expression::Call(Box::new(parse_function_call(tq, nr, indent_level, target)?))
            } else if tq.is_next(&TokenKind::OpenCurly) {
                parse_struct_initializer(tq, &nr, indent_level, target)?
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

        TokenKind::Ampersand => {
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

        _ => return parse_error_result(&tok.span, format!("Unexpected token '{}'", tok)),
    };

    while !is_end_of_expression(tq.peek().expect("Unexpected EOF")) {
        let next = tq.pop()?;
        match next.kind {
            TokenKind::OpenBracket => {
                let index_expr = parse_expression(tq, indent_level, target)?;
                tq.expect(&TokenKind::CloseBracket)?;
                let span = lhs.span().expanded(tq.pos());
                lhs = index_op(lhs, index_expr, span);
            }

            TokenKind::BinaryOperator(BinaryOperator::Dot) => {
                tq.push_front(next);
                lhs = parse_member_access(tq, lhs, indent_level, target)?;
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
    } else {
        Ok(e_start)
    }
}

fn parse_global_bindings(
    module: &mut Module,
    tq: &mut TokenQueue,
    mutable: bool,
    indent_level: usize,
    namespace: &str,
    target: &Target,
) -> CompileResult<()> {
    while !is_end_of_bindings(tq, indent_level) {
        let (name, span) = tq.expect_identifier()?;
        tq.expect(&TokenKind::Assign(AssignOperator::Assign))?;
        let init = parse_expression(tq, indent_level, target)?;

        if module.globals.contains_key(&name) {
            return parse_error_result(&span, format!("Global {} already defined in this module", name));
        }

        let full_name = namespaced(namespace, &name);
        module.globals.insert(
            full_name.clone(),
            global_binding(full_name, init, mutable, span.expanded(tq.pos())),
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
            span = span.expanded(name_span.end);
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

fn parse_module<Input: Read>(
    module: &mut Module,
    input: &mut Input,
    namespace: &str,
    file_name: &str,
    target: &Target,
) -> CompileResult<()> {
    let mut tq = Lexer::new(file_name).read(input)?;

    let add_function = |module: &mut Module, func: Function| -> CompileResult<()> {
        if module.functions.contains_key(&func.sig.name) {
            return parse_error_result(&func.span, format!("Function {} redefined", func.sig.name));
        }
        module.functions.insert(func.sig.name.clone(), func);
        Ok(())
    };

    let mut indent_level = 0;
    while !tq.is_next(&TokenKind::EOF) {
        let tok = tq.pop()?;
        match tok.kind {
            TokenKind::Indent(level) => {
                indent_level = level;
            }

            TokenKind::Interface => {
                parse_interface(module, &mut tq, namespace, &tok.span, indent_level, target)?;
            }

            TokenKind::Let => {
                parse_global_bindings(module, &mut tq, false, indent_level, namespace, target)?;
            }

            TokenKind::Var => {
                parse_global_bindings(module, &mut tq, true, indent_level, namespace, target)?;
            }

            TokenKind::Struct => {
                let mut sd = parse_struct_type(&mut tq, namespace, indent_level, target)?;
                sd.span = tok.span.expanded(sd.span.end);
                if module.types.contains_key(&sd.name) {
                    return parse_error_result(&sd.span, format!("Type {} redefined", sd.name));
                }
                module
                    .types
                    .insert(sd.name.clone(), TypeDeclaration::Struct(sd));
            }

            TokenKind::Enum => {
                let st = parse_sum_type(&mut tq, namespace, &tok.span, indent_level, target)?;
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

            TokenKind::Extern => {
                let ext_func = parse_external_function(&mut tq, &tok.span, indent_level, target)?;
                if module.externals.contains_key(&ext_func.sig.name) {
                    return parse_error_result(
                        &ext_func.span,
                        format!("External function {} redefined", ext_func.sig.name),
                    );
                }
                module.externals.insert(ext_func.sig.name.clone(), ext_func);
            }

            TokenKind::Import => loop {
                let import = parse_import_name(&mut tq)?;
                module.import_names.insert(import);
                if tq.is_next(&TokenKind::Comma) {
                    tq.pop()?;
                } else {
                    break;
                }
            },

            TokenKind::Func => {
                let func = parse_function_declaration(&mut tq, namespace, &tok.span, indent_level, target)?;
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
    }

    Ok(())
}

pub fn parse_file(file_path: &Path, namespace: &str, target: &Target) -> CompileResult<Module> {
    let op_name = format!("Parsing {}", file_path.to_string_lossy());
    time_operation(2, &op_name, || {
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
use crate::package::Package;

#[cfg(test)]
pub fn parse_str(code: &str, root_namespace: &str, target: &Target) -> CompileResult<Package> {
    use std::io::Cursor;

    let mut pkg = Package::new(root_namespace);
    let mut module = Module::new(root_namespace);
    let mut cursor = Cursor::new(code);
    parse_module(&mut module, &mut cursor, root_namespace, "", target)?;
    pkg.modules.insert(root_namespace.into(), module);
    Ok(pkg)
}
