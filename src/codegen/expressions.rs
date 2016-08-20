use std::ptr;

use libc;
use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use ast::{Expression, UnaryOp, BinaryOp, Function, FunctionSignature, Call, NameRef, MatchExpression, MatchCase, LetExpression,
    ArrayLiteral, ArgumentPassingMode, ArrayPattern, Type};
use parser::Operator;
use codegen::{cstr, Context, ValueRef, Slice, Sequence, Array};
use codegen::symboltable::{FunctionInstance};
use compileerror::{Span, CompileResult, CompileError, ErrorCode, Pos, err};


unsafe fn is_floating_point(ctx: LLVMContextRef, tr: LLVMTypeRef) -> bool
{
    tr == LLVMDoubleTypeInContext(ctx)
}

pub unsafe fn const_int(ctx: &Context, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0)
}

unsafe fn gen_integer(ctx: &mut Context, v: u64) -> CompileResult<ValueRef>
{
    Ok(ValueRef::const_value(const_int(ctx, v)))
}

unsafe fn gen_bool(ctx: &mut Context, v: bool) -> CompileResult<ValueRef>
{
    Ok(ValueRef::const_value(LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0)))
}

unsafe fn gen_float(ctx: &Context, num: &str, span: &Span) -> CompileResult<ValueRef>
{
    match num.parse::<f64>()
    {
        Ok(f) => Ok(ValueRef::const_value(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f))),
        Err(_) => err(span.start, ErrorCode::InvalidFloatingPoint, format!("{} is not a valid floating point number", num))
    }
}

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> CompileResult<ValueRef>
{
    Ok(ValueRef::const_value(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1)))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str, _span: &Span) -> CompileResult<ValueRef>
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as u32), cstr("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);

    // Initialize with string:
    LLVMSetInitializer(glob, try!(gen_const_string_literal(ctx, s)).load(ctx.builder));
    Ok(ValueRef::Global(glob))
}

unsafe fn gen_unary_op(ctx: &mut Context, op: &UnaryOp) -> CompileResult<ValueRef>
{
    let e_val = try!(gen_expression(ctx, &op.expression));
    match op.operator
    {
        Operator::Sub => {
            Ok(ValueRef::const_value(LLVMBuildNeg(ctx.builder, e_val.load(ctx.builder), cstr("neg"))))
        },
        Operator::Not => {
            Ok(ValueRef::const_value(LLVMBuildNot(ctx.builder, e_val.load(ctx.builder), cstr("not"))))
        },
        _ => err(op.span.start, ErrorCode::InvalidUnaryOperator, format!("Operator {} is not a unary operator", op.operator)),
    }
}

unsafe fn gen_binary_op(ctx: &mut Context, op: &BinaryOp) -> CompileResult<ValueRef>
{
    let left_val = try!(gen_expression(ctx, &op.left)).load(ctx.builder);
    let right_val = try!(gen_expression(ctx, &op.right)).load(ctx.builder);
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

    v.map(|val| ValueRef::const_value(val))
}


pub unsafe fn gen_function_sig(ctx: &mut Context, sig: &FunctionSignature, span: &Span) -> CompileResult<FunctionInstance>
{
    let ret_type = try!(ctx
        .resolve_type(&sig.return_type)
        .ok_or(CompileError::new(span.start, ErrorCode::TypeError, format!("Cannot resolve the return type of function '{}'", sig.name))));

    let mut arg_types = Vec::new();
    let mut arg_types_with_passing_mode = Vec::new();
    for arg in &sig.args {
        let arg_type = try!(ctx
            .resolve_type(&arg.typ)
            .ok_or(CompileError::new(arg.span.start, ErrorCode::TypeError, format!("Cannot resolve the type of argument '{}'", arg.name))));

        match arg.passing_mode
        {
            ArgumentPassingMode::ByValue => {
                arg_types.push(arg_type);
                arg_types_with_passing_mode.push((arg_type, arg.passing_mode));
            },
            ArgumentPassingMode::ByPtr => {
                let arg_ptr_type = LLVMPointerType(arg_type, 0);
                arg_types.push(arg_ptr_type);
                arg_types_with_passing_mode.push((arg_ptr_type, arg.passing_mode));
            }
        }
    }

    let name = sig.name.clone();

    let function_type = LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), arg_types.len() as libc::c_uint, 0);
    let function = LLVMAddFunction(ctx.module, cstr(&name), function_type);

    Ok(FunctionInstance{
        name: name,
        args: arg_types_with_passing_mode,
        return_type: ret_type,
        function: function,
        sig: sig.clone(),
    })
}

pub unsafe fn gen_function(ctx: &mut Context, f: &Function) -> CompileResult<ValueRef>
{
    let fi = try!(ctx.get_function(&f.sig.name).ok_or(
        CompileError::new(f.span.start, ErrorCode::UnknownName, format!("Unknown function {}", f.sig.name))));
    let bb = LLVMAppendBasicBlockInContext(ctx.context, fi.function, cstr("entry"));
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    ctx.push_stack(fi.function);

    for (i, arg) in f.sig.args.iter().enumerate() {
        let var = LLVMGetParam(fi.function, i as libc::c_uint);
        let (llvm_arg, mode) = fi.args[i];

        match mode
        {
            ArgumentPassingMode::ByPtr => {
                match arg.typ
                {
                    Type::Array(_, _) => ctx.add_variable(&arg.name, ValueRef::Array(Array::new(var))),
                    Type::Slice(_) => {
                        let builder = ctx.builder;
                        ctx.add_variable(&arg.name, ValueRef::Slice(Slice::new(builder, var)));
                    },
                    _ => ctx.add_variable(&arg.name, ValueRef::Ptr(var)),
                }
            },
            ArgumentPassingMode::ByValue => {
                let alloc = ValueRef::alloc(ctx, llvm_arg);
                LLVMBuildStore(ctx.builder, var, alloc.get());
                ctx.add_variable(&arg.name, alloc);
            },
        }
    }


    let ret = try!(gen_expression(ctx, &f.expression));
    LLVMBuildRet(ctx.builder, ret.load(ctx.builder));
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
    for (ref arg, passing_mode) in c.args.iter().zip(func.args.iter().map(|&(_, passing_mode)| passing_mode.clone()))
    {
        let a = try!(gen_expression(ctx, &arg));
        match passing_mode
        {
            ArgumentPassingMode::ByValue => arg_vals.push(a.load(ctx.builder)),
            ArgumentPassingMode::ByPtr => arg_vals.push(a.get()),
        }

    }

    let call = LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr(""));
    if c.tail_call {
        LLVMSetTailCall(call, 1);
    }

    Ok(ValueRef::const_value(call))
}

unsafe fn gen_name_ref(ctx: &mut Context, nr: &NameRef) -> CompileResult<ValueRef>
{
    if let Some(vi) = ctx.get_variable(&nr.name) {
        Ok(vi.value.clone())
    } else if let Some(fi) = ctx.get_function(&nr.name) {
        Ok(ValueRef::const_value(fi.function))
    } else {
        err(nr.span.start, ErrorCode::UnknownName, format!("Unknown name {}", nr.name))
    }
}

unsafe fn gen_match_case_to_execute(
    ctx: &mut Context,
    mc: &MatchCase,
    dst: &ValueRef,
    match_case_bb: LLVMBasicBlockRef,
    match_end_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    LLVMPositionBuilderAtEnd(ctx.builder, match_case_bb);
    let ret = try!(gen_expression(ctx, &mc.to_execute));
    try!(dst.store(ctx, ret, mc.span.start));
    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, next_bb);
    Ok(())
}

unsafe fn gen_sequence_match<Seq: Sequence>(
    ctx: &mut Context,
    ap: &ArrayPattern,
    seq: &Seq,
    pos: Pos,
    match_case_bb: LLVMBasicBlockRef,
    next_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    let head = seq.head(ctx);
    ctx.add_variable(&ap.head, head);
    let tail = try!(seq.tail(ctx, pos));
    ctx.add_variable(&ap.tail, tail);

    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, seq.gen_length(ctx).get(), const_int(ctx, 0), cstr("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
    Ok(())
}

unsafe fn gen_llvm_equals(
    ctx: &Context,
    left: LLVMValueRef,
    right: LLVMValueRef,
    on_equals_bb: LLVMBasicBlockRef,
    on_not_equals_bb: LLVMBasicBlockRef)
{
    let left_type = LLVMTypeOf(left);
    let cmp = if is_floating_point(ctx.context, left_type) {
        LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, left, right, cstr("cmp"))
    } else {
        LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr("cmp"))
    };

    LLVMBuildCondBr(ctx.builder, cmp, on_equals_bb, on_not_equals_bb);
}

unsafe fn gen_equals(
    ctx: &Context,
    left: &ValueRef,
    right: &ValueRef,
    on_equals_bb: LLVMBasicBlockRef,
    on_not_equals_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    match (left, right)
    {
        (&ValueRef::Const(lv), &ValueRef::Const(rv)) => Ok(gen_llvm_equals(ctx, lv, rv, on_equals_bb, on_not_equals_bb)),
        (&ValueRef::Ptr(_), &ValueRef::Ptr(_)) => Ok(gen_llvm_equals(ctx, left.load(ctx.builder), right.load(ctx.builder), on_equals_bb, on_not_equals_bb)),
        (&ValueRef::Global(_), &ValueRef::Ptr(_)) => Ok(gen_llvm_equals(ctx, left.load(ctx.builder), right.load(ctx.builder), on_equals_bb, on_not_equals_bb)),
        (&ValueRef::Global(_), &ValueRef::Global(_)) => Ok(gen_llvm_equals(ctx, left.load(ctx.builder), right.load(ctx.builder), on_equals_bb, on_not_equals_bb)),
        (&ValueRef::Ptr(_), &ValueRef::Global(_)) => Ok(gen_llvm_equals(ctx, left.load(ctx.builder), right.load(ctx.builder), on_equals_bb, on_not_equals_bb)),
        (&ValueRef::Array(ref l), &ValueRef::Array(ref r)) => gen_equals_seq(ctx, l, r, on_equals_bb, on_not_equals_bb),
        (&ValueRef::Array(ref l), &ValueRef::Slice(ref r)) => gen_equals_seq(ctx, l, r, on_equals_bb, on_not_equals_bb),
        (&ValueRef::Slice(ref l), &ValueRef::Slice(ref r)) => gen_equals_seq(ctx, l, r, on_equals_bb, on_not_equals_bb),
        (&ValueRef::Slice(ref l), &ValueRef::Array(ref r)) => gen_equals_seq(ctx, l, r, on_equals_bb, on_not_equals_bb),
        _ => err(Pos::zero(), ErrorCode::TypeError, format!("Unsupported comparison"))
    }
}

unsafe fn gen_equals_seq<ASeq: Sequence, BSeq: Sequence>(
    ctx: &Context,
    a: &ASeq,
    b: &BSeq,
    on_equals_bb: LLVMBasicBlockRef,
    on_not_equals_bb: LLVMBasicBlockRef) -> CompileResult<()>
{
    let func = ctx.get_current_function();
    let after_equal_length_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_equal_length_bb"));
    let for_cond_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("for_cond_bb"));
    let for_body_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("for_body_bb"));
    let after_element_cmp_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("after_element_cmp_bb"));

    // First check if sequences have the same length
    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, a.gen_length(ctx).get(), b.gen_length(ctx).get(), cstr("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, after_equal_length_bb, on_not_equals_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, after_equal_length_bb);

    // Then loop over each element and check equality
    let counter = ValueRef::alloc(ctx, LLVMInt64TypeInContext(ctx.context));
    try!(counter.store_direct(ctx, const_int(ctx, 0), Pos::zero()));
    LLVMBuildBr(ctx.builder, for_cond_bb);

    LLVMPositionBuilderAtEnd(ctx.builder, for_cond_bb);
    let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, counter.load(ctx.builder), a.gen_length(ctx).get(), cstr("cmp"));
    LLVMBuildCondBr(ctx.builder, cond, for_body_bb, on_equals_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, for_body_bb);

    let index = counter.load(ctx.builder);
    let a_element = a.get_element(ctx, index);
    let b_element = b.get_element(ctx, index);
    try!(gen_equals(ctx, &a_element, &b_element, after_element_cmp_bb, on_not_equals_bb));

    LLVMPositionBuilderAtEnd(ctx.builder, after_element_cmp_bb);

    // Increment counter, and jump back to for_cond_bb
    let index = LLVMBuildAdd(ctx.builder, index, const_int(ctx, 1), cstr("inc_index"));
    try!(counter.store_direct(ctx, index, Pos::zero()));
    LLVMBuildBr(ctx.builder, for_cond_bb);
    Ok(())
}

unsafe fn gen_match_case(
    ctx: &mut Context,
    mc: &MatchCase,
    target: &ValueRef,
    func: LLVMValueRef,
    match_end_bb: LLVMBasicBlockRef,
    dst: &ValueRef) -> CompileResult<()>
{
    let match_case_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("match_case_bb"));
    let next_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("next_bb"));

    match mc.match_expr
    {
        Expression::IntLiteral(_, v) => {
            let iv = try!(gen_integer(ctx, v));
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },
        Expression::FloatLiteral(ref span, ref v) => {
            let iv = try!(gen_float(ctx, &v, span));
            let cond = LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },
        Expression::BoolLiteral(_, v) => {
            let iv = try!(gen_bool(ctx, v));
            let cond = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, target.load(ctx.builder), iv.load(ctx.builder), cstr("cmp"));
            LLVMBuildCondBr(ctx.builder, cond, match_case_bb, next_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },
        Expression::NameRef(ref nr) if nr.name == "_"  => {
            LLVMBuildBr(ctx.builder, match_case_bb);
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },
        Expression::ArrayPattern(ref ap) => {
            ctx.push_stack(ptr::null_mut());
            match target
            {
                &ValueRef::Array(ref arr) => try!(gen_sequence_match(ctx, ap, arr, mc.span.start, match_case_bb, next_bb)),
                &ValueRef::Slice(ref slice) => try!(gen_sequence_match(ctx, ap, slice, mc.span.start, match_case_bb, next_bb)),
                _ => return err(mc.span.start, ErrorCode::TypeError, format!("Match expression cannot be matched with an array pattern")),
            }

            try!(gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb));
            ctx.pop_stack();
            Ok(())
        },
        Expression::ArrayLiteral(ref a) => {
            let arr = try!(gen_array_literal(ctx, a));
            try!(gen_equals(ctx, target, &arr, match_case_bb, next_bb));
            gen_match_case_to_execute(ctx, mc, dst, match_case_bb, match_end_bb, next_bb)
        },
        _ => err(mc.span.start, ErrorCode::TypeError, format!("Expression is not a valid match pattern")),

    }
}

unsafe fn gen_match(ctx: &mut Context, m: &MatchExpression) -> CompileResult<ValueRef>
{
    let target = try!(gen_expression(ctx, &m.target));
    let func = ctx.get_current_function();

    let match_end_bb = LLVMAppendBasicBlockInContext(ctx.context, func, cstr("match_end_bb"));

    let ret_type = try!(ctx
        .resolve_type(&m.typ)
        .ok_or(CompileError::new(m.span.start, ErrorCode::TypeError, format!("Cannot resolve the type of this match statement"))));

    let dst = ValueRef::alloc(ctx, ret_type);
    for mc in &m.cases
    {
        try!(gen_match_case(ctx, mc, &target, func, match_end_bb, &dst));
    }

    LLVMBuildBr(ctx.builder, match_end_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, match_end_bb);
    Ok(dst)
}

unsafe fn gen_let(ctx: &mut Context, l: &LetExpression) -> CompileResult<ValueRef>
{
    ctx.push_stack(ptr::null_mut());

    for b in &l.bindings
    {
        let b_type = try!(ctx
            .resolve_type(&b.typ)
            .ok_or(CompileError::new(b.span.start, ErrorCode::TypeError, format!("Cannot resolve the type of the {} binding", b.name))));

        let vr = ValueRef::alloc(ctx, b_type);
        try!(gen_expression_store(ctx, &b.init, &vr));
        ctx.add_variable(&b.name, vr);
    }

    let result = try!(gen_expression(ctx, &l.expression));
    ctx.pop_stack();
    Ok(result)
}

unsafe fn gen_const_array_literal(ctx: &mut Context, a: &ArrayLiteral) -> CompileResult<ValueRef>
{
    let element_type = a.array_type.get_element_type().expect("Invalid array type");
    let llvm_type = try!(ctx.resolve_type(&element_type).ok_or(
        CompileError::new(a.span.start, ErrorCode::TypeError, format!("Unknown type {}", element_type))
    ));

    let mut vals = Vec::new();
    for element in &a.elements
    {
        let element_val = try!(gen_const_expression(ctx, element));
        vals.push(element_val.load(ctx.builder));
    }

    Ok(ValueRef::const_array(LLVMConstArray(llvm_type, vals.as_mut_ptr(), vals.len() as u32)))
}


unsafe fn gen_array_literal_store(ctx: &mut Context, a: &ArrayLiteral, ptr: &ValueRef) -> CompileResult<()>
{
    if ctx.in_global_context()
    {
        let v = try!(gen_const_array_literal(ctx, a));
        try!(ptr.store(ctx, v, a.span.start));
    }
    else
    {
        for (idx, element) in a.elements.iter().enumerate()
        {
            let index = const_int(ctx, idx as u64);
            let el_ptr = try!(ptr.index(ctx, index, a.span.start));
            let e_val = try!(gen_expression(ctx, element));
            try!(el_ptr.store(ctx, e_val, a.span.start));
        }
    }

    Ok(())
}

unsafe fn gen_array_literal(ctx: &mut Context, a: &ArrayLiteral) -> CompileResult<ValueRef>
{
    if a.elements.len() == 0 {
        let slice = try!(Slice::empty(ctx, a.span.start));
        return Ok(ValueRef::Slice(slice));
    }

    let element_type = a.array_type.get_element_type().expect("Invalid array type");
    let llvm_type = try!(ctx.resolve_type(&element_type)
        .ok_or(CompileError::new(a.span.start, ErrorCode::TypeError, format!("Unknown type '{}'", element_type))));
    let var = ValueRef::alloc_array(ctx, llvm_type, a.elements.len());
    try!(gen_array_literal_store(ctx, a, &var));
    Ok(var)
}

unsafe fn gen_const_expression(ctx: &mut Context, e: &Expression) -> CompileResult<ValueRef>
{
    match *e
    {
        Expression::IntLiteral(_, integer) => gen_integer(ctx, integer),
        Expression::FloatLiteral(ref span, ref s) => gen_float(ctx, s, span),
        Expression::StringLiteral(_, ref s) => gen_const_string_literal(ctx, s),
        Expression::ArrayLiteral(ref a) => gen_const_array_literal(ctx, a),
        _ => err(e.span().start, ErrorCode::ExpectedConstExpr, format!("Expected a constant expression")),
    }
}


unsafe fn store(ctx: &mut Context, e: &Expression, ptr: &ValueRef) -> CompileResult<()>
{
    let v = try!(gen_expression(ctx, e));
    try!(ptr.store(ctx, v, e.span().start));
    Ok(())
}

pub unsafe fn gen_expression_store(ctx: &mut Context, e: &Expression, ptr: &ValueRef) -> CompileResult<()>
{
    match *e
    {
        Expression::ArrayLiteral(ref a) => gen_array_literal_store(ctx, a, ptr),
        _ => store(ctx, e, &ptr),
    }
}

pub unsafe fn gen_array_to_slice_conversion(ctx: &mut Context, e: &Expression) -> CompileResult<ValueRef>
{
    let v = try!(gen_expression(ctx, e));
    match v
    {
        ValueRef::Array(arr) => {
            let element_type = arr.get_element_type();
            let slice_type = ctx.get_slice_type(element_type);
            let slice = try!(Slice::from_array(ctx, slice_type, &arr, 0, e.span().start));
            Ok(ValueRef::slice(slice))
        },
        _ => err(e.span().start, ErrorCode::TypeError, format!("Array to slice conversion, requires an array")),
    }
}


pub fn gen_expression(ctx: &mut Context, e: &Expression) -> CompileResult<ValueRef>
{
    unsafe
    {
        match *e
        {
            Expression::UnaryOp(ref u) => gen_unary_op(ctx, u),
            Expression::BinaryOp(ref op) => gen_binary_op(ctx, op),
            Expression::ArrayLiteral(ref a) => gen_array_literal(ctx, a),
            Expression::ArrayPattern(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::ArrayGenerator(ref a) => err(a.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::Call(ref c) => gen_call(ctx, c),
            Expression::NameRef(ref nr) => gen_name_ref(ctx, nr),
            Expression::Function(ref f) => gen_function(ctx, f),
            Expression::Match(ref m) => gen_match(ctx, m),
            Expression::Lambda(ref l) => err(l.span.start, ErrorCode::UnexpectedEOF, format!("NYI")),
            Expression::Let(ref l) => gen_let(ctx, l),
            Expression::Enclosed(_, ref inner) => gen_expression(ctx, inner),
            Expression::IntLiteral(_, v) => gen_integer(ctx, v),
            Expression::FloatLiteral(ref span, ref v_str) => gen_float(ctx, &v_str, span),
            Expression::StringLiteral(ref span, ref s)  => gen_string_literal(ctx, s, span),
            Expression::BoolLiteral(_, v) => gen_bool(ctx, v),
            Expression::ArrayToSliceConversion(ref e) => gen_array_to_slice_conversion(ctx, e),
        }
    }
}
