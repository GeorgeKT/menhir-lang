use std::ptr;
use std::rc::Rc;

use libc;
use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use ast::{Expression, UnaryOp, BinaryOp, Function, FunctionSignature, Call, NameRef, MatchExpression, MatchCase};
use parser::Operator;
use codegen::{type_name, cstr};
use codegen::context::Context;
use codegen::valueref::ValueRef;
use codegen::symboltable::{FunctionInstance, VariableInstance};
use compileerror::{Span, CompileResult, CompileError, ErrorCode, err};


unsafe fn is_floating_point(ctx: LLVMContextRef, tr: LLVMTypeRef) -> bool
{
    tr == LLVMDoubleTypeInContext(ctx)
}

unsafe fn gen_integer(ctx: &mut Context, v: u64) -> CompileResult<ValueRef>
{
    Ok(ValueRef::new(LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0), true, ctx.builder))
}

unsafe fn gen_bool(ctx: &mut Context, v: bool) -> CompileResult<ValueRef>
{
    Ok(ValueRef::new(LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0), true, ctx.builder))
}

unsafe fn gen_float(ctx: &Context, num: &str, span: &Span) -> CompileResult<ValueRef>
{
    match num.parse::<f64>() 
    {
        Ok(f) => Ok(ValueRef::new(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f), true, ctx.builder)),
        Err(_) => err(span.start, ErrorCode::InvalidFloatingPoint, format!("{} is not a valid floating point number", num))
    }
}

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> CompileResult<ValueRef>
{
    Ok(ValueRef::new(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1), true, ctx.builder))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str, _span: &Span) -> CompileResult<ValueRef>
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as u32), cstr("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);

    // Initialize with string:
    LLVMSetInitializer(glob, try!(gen_const_string_literal(ctx, s)).load());
    Ok(ValueRef::new(glob, true, ctx.builder))
}

unsafe fn gen_unary_op(ctx: &mut Context, op: &UnaryOp) -> CompileResult<ValueRef>
{
    let e_val = try!(gen_expression(ctx, &op.expression));
    match op.operator 
    {
        Operator::Sub => {      
            Ok(ValueRef::new(LLVMBuildNeg(ctx.builder, e_val.load(), cstr("neg")), true, ctx.builder))
        },
        Operator::Not => {
            Ok(ValueRef::new(LLVMBuildNot(ctx.builder, e_val.load(), cstr("not")), true, ctx.builder))
        },
        _ => err(op.span.start, ErrorCode::InvalidUnaryOperator, format!("Operator {} is not a unary operator", op.operator)),
    }
}

unsafe fn gen_binary_op(ctx: &mut Context, op: &BinaryOp) -> CompileResult<ValueRef>
{
    let left_val = try!(gen_expression(ctx, &op.left)).load();
    let right_val = try!(gen_expression(ctx, &op.right)).load();
    let left_type = LLVMTypeOf(left_val);

    let v = match op.operator {
        Operator::Add => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFAdd(ctx.builder, left_val, right_val, cstr("add")))
            } else {
                Ok(LLVMBuildAdd(ctx.builder, left_val, right_val, cstr("add")))
            }
        },
        Operator::Sub => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFSub(ctx.builder, left_val, right_val, cstr("sub")))
            } else {
                Ok(LLVMBuildSub(ctx.builder, left_val, right_val, cstr("sub")))
            }
        },
        Operator::Div => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFDiv(ctx.builder, left_val, right_val, cstr("div")))
            } else {
                Ok(LLVMBuildUDiv(ctx.builder, left_val, right_val, cstr("div")))
            }
        },
        Operator::Mod => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFRem(ctx.builder, left_val, right_val, cstr("mod")))
            } else {
                Ok(LLVMBuildURem(ctx.builder, left_val, right_val, cstr("mod")))
            }
        },
        Operator::Mul => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFMul(ctx.builder, left_val, right_val, cstr("mul")))
            } else {
                Ok(LLVMBuildMul(ctx.builder, left_val, right_val, cstr("mul")))
            }
        },
        Operator::And => {
            Ok(LLVMBuildAnd(ctx.builder, left_val, right_val, cstr("and")))
        },
        Operator::Or => {
            Ok(LLVMBuildOr(ctx.builder, left_val, right_val, cstr("or")))
        },
        Operator::LessThan => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLT, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::LessThanEquals => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLE, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::GreaterThan => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGT, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::GreaterThanEquals => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGE, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::Equals => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::NotEquals => {
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealONE, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left_val, right_val, cstr("cmp")))
            }
        },
        _ => err(op.span.start, ErrorCode::InvalidBinaryOperator, format!("Operator {} is not a binary operator", op.operator)),
    };

    v.map(|val| ValueRef::new(val, true, ctx.builder))
}


unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature, span: &Span) -> CompileResult<FunctionInstance>
{
    let ret_type = try!(ctx
        .resolve_type(&sig.return_type)
        .ok_or(CompileError::new(span.start, ErrorCode::TypeError, format!("Cannot resolve the return type of function '{}'", sig.name))));

    let mut arg_types = Vec::new();
    for arg in &sig.args {
        let arg_type = try!(ctx
            .resolve_type(&arg.typ)
            .ok_or(CompileError::new(arg.span.start, ErrorCode::TypeError, format!("Cannot resolve the type of argument '{}'", arg.name))));

        println!("arg {} {}", type_name(arg_type), arg.typ);
        arg_types.push(arg_type);
    }

    let name = sig.name.clone();

    let function_type = LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
    let function = LLVMAddFunction(ctx.module, cstr(&name), function_type);

    Ok(FunctionInstance{
        name: name,
        args: arg_types,
        return_type: ret_type,
        function: function,
        sig: sig.clone(),
    })
}

/*
unsafe fn is_block_terminated(bb: LLVMBasicBlockRef) -> bool
{
    if bb == ptr::null_mut() {
        return true;
    }
    let last = LLVMGetLastInstruction(bb);
    last != ptr::null_mut() && LLVMIsATerminatorInst(last) != ptr::null_mut()
}
*/

unsafe fn gen_function(ctx: &mut Context, f: &Function) -> CompileResult<ValueRef>
{
    let fi = Rc::new(try!(gen_function_sig(ctx, &f.sig, &f.span)));
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr("entry"));
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.add_function(fi.clone());
    ctx.push_stack(fi.function);

    for (i, arg) in f.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        let llvm_arg = fi.args[i];
        let alloc = LLVMBuildAlloca(ctx.builder, llvm_arg, cstr("argtmp"));
        LLVMBuildStore(ctx.builder, var, alloc);

        let var = Rc::new(VariableInstance{
            value: alloc,
            name: arg.name.clone(),
            typ: arg.typ.clone(),
        });

        ctx.add_variable(var);
    }


    let ret = try!(gen_expression(ctx, &f.expression));
    LLVMBuildRet(ctx.builder, ret.load());
    ctx.pop_stack();

    if current_bb != ptr::null_mut() {
        LLVMPositionBuilderAtEnd(ctx.builder, current_bb);
    }

    Ok(ret)
}

unsafe fn gen_call(ctx: &mut Context, c: &Call) -> CompileResult<ValueRef>
{
    let func = try!(
        ctx.get_function(&c.callee.name).ok_or(
            CompileError::new(c.span.start, ErrorCode::UnknownName, format!("Unknown function {}", c.callee.name)))
    );

    let mut arg_vals = Vec::with_capacity(c.args.len());
    for arg in &c.args 
    {
        let a = try!(gen_expression(ctx, arg));
        arg_vals.push(a.load());
    }

    Ok(ValueRef::new(
        LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr("")),
        true,
        ctx.builder)
    )
}

unsafe fn gen_name_ref(ctx: &mut Context, nr: &NameRef) -> CompileResult<ValueRef>
{
    if let Some(vi) = ctx.get_variable(&nr.name) {
        Ok(ValueRef::new(LLVMBuildLoad(ctx.builder, vi.value, cstr("load")), true, ctx.builder))
    } else if let Some(fi) = ctx.get_function(&nr.name) {
        Ok(ValueRef::new(fi.function, true, ctx.builder))
    } else {
        err(nr.span.start, ErrorCode::UnknownName, format!("Unknown name {}", nr.name))
    }
}

unsafe fn gen_match_case(ctx: &mut Context, mc: &MatchCase, target: &ValueRef, 
    func: LLVMValueRef, match_end_bb: LLVMBasicBlockRef, dst: &ValueRef) -> CompileResult<()>
{
    let match_case_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("match_case_bb"));
    let next_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("next_bb"));

    match mc.match_expr
    {
        Expression::IntLiteral(_, v) => {
            let iv = try!(gen_integer(ctx, v));
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(), iv.load(), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
        },
        Expression::FloatLiteral(ref span, ref v) => {
            let iv = try!(gen_float(ctx, &v, span));
            let cond = LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, target.load(), iv.load(), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
        },
        Expression::BoolLiteral(_, v) => {
            let iv = try!(gen_bool(ctx, v));
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(), iv.load(), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
        },
        Expression::NameRef(ref nr) if nr.name == "_"  => {
            LLVMBuildBr(ctx.builder, match_case_bb);
        },

        _ => return err(mc.span.start, ErrorCode::TypeError, format!("Expression is not a valid match pattern")),

    }
    
    LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
    let ret = try!(gen_expression(ctx, &mc.to_execute));
    try!(dst.store(ctx, ret, mc.span.start));
    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, next_bb);
    
    Ok(()) 
}

unsafe fn gen_match(ctx: &mut Context, m: &MatchExpression)-> CompileResult<ValueRef>
{
    let target = try!(gen_expression(ctx, &m.target));
    let func = ctx.get_current_function();

    let match_end_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("match_end_bb"));

    let ret_type = try!(ctx
        .resolve_type(&m.typ)
        .ok_or(CompileError::new(m.span.start, ErrorCode::TypeError, format!("Cannot resolve the type of this match statement"))));

    let dst = ValueRef::local(ctx.builder, ret_type);
    for mc in &m.cases 
    {
        try!(gen_match_case(ctx, mc, &target, func, match_end_bb, &dst));
    }

    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, match_end_bb); 
    Ok(dst)
}

pub fn gen_expression(ctx: &mut Context, e: &Expression) -> CompileResult<ValueRef>
{
    unsafe 
    {
        match *e
        {
            Expression::UnaryOp(ref u) => gen_unary_op(ctx, u),
            Expression::BinaryOp(ref op) => gen_binary_op(ctx, op),
            Expression::ArrayLiteral(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::ArrayInitializer(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::ArrayPattern(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")), 
            Expression::Call(ref c) => gen_call(ctx, c),
            Expression::NameRef(ref nr) => gen_name_ref(ctx, nr),
            Expression::Function(ref f) => gen_function(ctx, f),
            Expression::Match(ref m) => gen_match(ctx, m),
            Expression::Lambda(ref l) => err(l.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::Enclosed(_, ref inner) => gen_expression(ctx, inner),
            Expression::IntLiteral(_, v) => gen_integer(ctx, v),
            Expression::FloatLiteral(ref span, ref v_str) => gen_float(ctx, &v_str, span),
            Expression::StringLiteral(ref span, ref s)  => gen_string_literal(ctx, s, span),
            Expression::BoolLiteral(_, v) => gen_bool(ctx, v),
        }
    }
}