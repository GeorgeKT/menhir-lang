use llvm::core::*;
use llvm::prelude::*;
use libc;

use ast::*;
use codegen::*;
use compileerror::*;
use parser::Operator;

unsafe fn is_integer(ctx: LLVMContextRef, tr: LLVMTypeRef) -> bool
{
    tr == LLVMInt64TypeInContext(ctx)
}

unsafe fn is_floating_point(ctx: LLVMContextRef, tr: LLVMTypeRef) -> bool
{
    tr == LLVMDoubleTypeInContext(ctx)
}

unsafe fn is_numeric(ctx: LLVMContextRef, tr: LLVMTypeRef) -> bool
{
    is_integer(ctx, tr) || is_floating_point(ctx, tr)
}

unsafe fn const_int(ctx: &mut Context, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0)
}

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
            Ok(i) => Ok(const_int(ctx, i)),
            Err(_) => err(span.start, ErrorType::InvalidInteger)
        }
    }
}

unsafe fn gen_string_literal(_ctx: &mut Context, s: &str, _span: &Span) -> Result<LLVMValueRef, CompileError>
{
    Ok(LLVMConstString(cstr(s), s.len() as u32, 0))
}

unsafe fn gen_unary(ctx: &mut Context, op: Operator, e: &Box<Expression>, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    let e_val = try!(gen_expression(ctx, e));
    let e_type = LLVMTypeOf(e_val);
    match op {
        Operator::Sub => {
            if !is_numeric(ctx.context, e_type) {
                err(span.start, ErrorType::TypeError("Operator '-', expects and integer or floating point expression as argument".into()))
            } else {
                Ok(LLVMBuildNeg(ctx.builder, e_val, cstr("neg")))
            }
        },
        Operator::Not => {
            if !is_integer(ctx.context, e_type) {
                err(span.start, ErrorType::TypeError("Operator '!', expects an integer or boolean expression".into()))
            } else {
                Ok(LLVMBuildNot(ctx.builder, e_val, cstr("not")))
            }
        },
        Operator::Increment => {
            if !is_integer(ctx.context, e_type) {
                err(span.start, ErrorType::TypeError("Operator '++', expects an integer expression".into()))
            } else {
                Ok(LLVMBuildAdd(ctx.builder, e_val, const_int(ctx, 1), cstr("inc")))
            }
        },
        Operator::Decrement => {
            if !is_integer(ctx.context, e_type) {
                err(span.start, ErrorType::TypeError("Operator '--', expects an integer expression".into()))
            } else {
                Ok(LLVMBuildSub(ctx.builder, e_val, const_int(ctx, 1), cstr("dec")))
            }
        },
        _ => err(span.start, ErrorType::InvalidUnaryOperator(op)),
    }
}

unsafe fn gen_pf_unary(ctx: &mut Context, op: Operator, e: &Box<Expression>, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    match op {
        Operator::Increment | Operator::Decrement => gen_unary(ctx, op, e, span),
        _ => err(span.start, ErrorType::InvalidUnaryOperator(op)),
    }
}

unsafe fn check_numeric_operands(ctx: &mut Context, op: Operator, left_type: LLVMTypeRef, right_type: LLVMTypeRef, pos: Pos) -> Result<(), CompileError>
{
    if left_type != right_type {
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects both operands to be of the same type", op)))
    } else if !is_numeric(ctx.context, left_type) || !is_numeric(ctx.context, right_type){
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects integer or floating point expression as operands", op)))
    } else {
        Ok(())
    }
}

unsafe fn check_bool_operands(ctx: &mut Context, op: Operator, left_type: LLVMTypeRef, right_type: LLVMTypeRef, pos: Pos) -> Result<(), CompileError>
{
    if left_type != right_type {
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects both operands to be of the same type", op, )))
    } else if !is_integer(ctx.context, left_type) || !is_integer(ctx.context, right_type){
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects integer or boolean point expression as operands", op)))
    } else {
        Ok(())
    }
}


unsafe fn gen_binary(ctx: &mut Context, op: Operator, left: &Expression, right: &Expression, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    let left_val = try!(gen_expression(ctx, left));
    let right_val = try!(gen_expression(ctx, right));
    let left_type = LLVMTypeOf(left_val);
    let right_type = LLVMTypeOf(right_val);

    match op {
        Operator::Add => {
            try!(check_numeric_operands(ctx, op, left_type, right_type, span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFAdd(ctx.builder, left_val, right_val, cstr("add")))
            } else {
                Ok(LLVMBuildAdd(ctx.builder, left_val, right_val, cstr("add")))
            }
        },
        Operator::Sub => {
            try!(check_numeric_operands(ctx, op, left_type, right_type, span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFSub(ctx.builder, left_val, right_val, cstr("sub")))
            } else {
                Ok(LLVMBuildSub(ctx.builder, left_val, right_val, cstr("sub")))
            }
        },
        Operator::Div => {
            try!(check_numeric_operands(ctx, op, left_type, right_type, span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFDiv(ctx.builder, left_val, right_val, cstr("div")))
            } else {
                Ok(LLVMBuildUDiv(ctx.builder, left_val, right_val, cstr("div")))
            }
        },
        Operator::Mod => {
            try!(check_numeric_operands(ctx, op, left_type, right_type, span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFRem(ctx.builder, left_val, right_val, cstr("mod")))
            } else {
                Ok(LLVMBuildURem(ctx.builder, left_val, right_val, cstr("mod")))
            }
        },
        Operator::Mul => {
            try!(check_numeric_operands(ctx, op, left_type, right_type, span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFMul(ctx.builder, left_val, right_val, cstr("mul")))
            } else {
                Ok(LLVMBuildMul(ctx.builder, left_val, right_val, cstr("mul")))
            }
        },
        Operator::And => {
            try!(check_bool_operands(ctx, op, left_type, right_type, span.start));
            Ok(LLVMBuildAnd(ctx.builder, left_val, right_val, cstr("and")))
        },
        Operator::Or => {
            try!(check_bool_operands(ctx, op, left_type, right_type, span.start));
            Ok(LLVMBuildOr(ctx.builder, left_val, right_val, cstr("or")))
        },
        _ => err(span.start, ErrorType::InvalidBinaryOperator(op)),
    }

}

unsafe fn gen_enclosed(ctx: &mut Context, e: &Expression, _span: &Span) -> Result<LLVMValueRef, CompileError>
{
    gen_expression(ctx, e)
}


unsafe fn gen_call(ctx: &mut Context, c: &Call) -> Result<LLVMValueRef, CompileError>
{
    let mut arg_vals = Vec::with_capacity(c.args.len());
    for arg in &c.args {
        arg_vals.push(try!(gen_expression(ctx, arg)));
    }

    let func: &FunctionInstance = try!(ctx
        .get_function(&c.name)
        .ok_or(CompileError::new(c.span.start, ErrorType::UnknownFunction(c.name.clone()))));

    if c.args.len() != func.args.len() {
        return err(c.span.start, ErrorType::ArgumentCountMismatch(
            format!("Function '{}', expects {} arguments, {} are provided",
                c.name, func.args.len(), c.args.len())));
    }

    for (i, arg) in c.args.iter().enumerate() {
        let val_type = LLVMTypeOf(arg_vals[i]);
        if val_type != func.args[i] {
            return err(arg.span().start, ErrorType::TypeError(format!("Argument {} of function has the wrong type", i)));
        }
    }

    Ok(LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr("call")))
}

unsafe fn gen_name_ref(ctx: &mut Context, nr: &str, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    if let Some(ref v) = ctx.get_variable(nr) {
        Ok(LLVMBuildLoad(ctx.builder, v.value, cstr("load")))
    } else {
        err(span.start, ErrorType::UnknownVariable(nr.into()))
    }
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
