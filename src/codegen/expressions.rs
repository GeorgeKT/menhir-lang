use llvm::core::*;
use llvm::prelude::*;

use ast::*;
use codegen::*;
use compileerror::*;
use tokens::Operator;

unsafe fn gen_number(ctx: &mut Context, num: &str, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    if num.find('.').is_some() || num.find('e').is_some() {
        match num.parse::<f64>() {
            Ok(f) => Ok(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f)),
            Err(_) => err(span.start, ErrorType::InvalidFloatingPoint)
        }
    } else {
        // Should be an integer
        match num.parse::<u64>() {
            Ok(i) => Ok(LLVMConstInt(LLVMInt64TypeInContext(ctx.context), i, 0)),
            Err(_) => err(span.start, ErrorType::InvalidInteger)
        }
    }
}

#[allow(unused_variables)]
fn gen_string_literal(ctx: &mut Context, s: &str, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    err(span.start, ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_unary(ctx: &mut Context, op: Operator, e: &Box<Expression>, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    err(span.start, ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_pf_unary(ctx: &mut Context, op: Operator, e: &Box<Expression>, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    err(span.start, ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_binary(ctx: &mut Context, op: Operator, left: &Expression, right: &Expression, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    err(span.start, ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_enclosed(ctx: &mut Context, e: &Expression, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    err(span.start, ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_call(ctx: &mut Context, c: &Call) -> Result<LLVMValueRef, CompileError>
{
    err(c.span.start, ErrorType::UnexpectedEOF)
}

#[allow(unused_variables)]
fn gen_name_ref(ctx: &mut Context, nr: &str, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    err(span.start, ErrorType::UnexpectedEOF)
}

pub unsafe fn gen_expression(ctx: &mut Context, e: &Expression) -> Result<LLVMValueRef, CompileError>
{
    match *e
    {
        Expression::Number(ref span, ref s) => gen_number(ctx, s, span),
        Expression::StringLiteral(ref span, ref s) => gen_string_literal(ctx, s, span),
        Expression::UnaryOp(ref span, ref op, ref e) => gen_unary(ctx, *op, e, span),
        Expression::PostFixUnaryOp(ref span, ref op, ref e) => gen_pf_unary(ctx, *op, e, span),
        Expression::BinaryOp(ref span, ref op, ref left, ref right) => gen_binary(ctx, *op, left, right, span),
        Expression::Enclosed(ref span, ref e) => gen_enclosed(ctx, e, span),
        Expression::Call(ref c) => gen_call(ctx, c),
        Expression::NameRef(ref span, ref s) => gen_name_ref(ctx, s, span),
    }
}
