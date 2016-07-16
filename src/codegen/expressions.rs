use std::rc::Rc;
use std::ops::Deref;

use llvm::*;
use llvm::core::*;
use llvm::prelude::*;
use libc;

use ast::*;
use codegen::*;
use codegen::context::*;
use codegen::symbols::*;
use codegen::conversions::*;
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

pub unsafe fn const_int(ctx: LLVMContextRef, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx), v, 0)
}

unsafe fn gen_float(ctx: &Context, num: &str, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    match num.parse::<f64>() {
        Ok(f) => Ok(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f)),
        Err(_) => err(span.start, ErrorType::InvalidFloatingPoint)
    }
}

unsafe fn gen_integer(ctx: &Context, i: u64, _span: &Span) -> Result<LLVMValueRef, CompileError>
{
    Ok(const_int(ctx.context, i))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str, _span: &Span) -> Result<LLVMValueRef, CompileError>
{
    let glob = LLVMAddGlobal(ctx.get_module_ref(), LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as u32), cstr("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);

    // Initialize with string:
    LLVMSetInitializer(glob, try!(gen_const_string_literal(ctx, s)));
    Ok(glob)
}

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> Result<LLVMValueRef, CompileError>
{
    Ok(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1))
}

unsafe fn gen_unary(ctx: &mut Context, op: &UnaryOp) -> Result<LLVMValueRef, CompileError>
{
    let e_val = try!(gen_expression(ctx, &op.expression));
    let e_type = LLVMTypeOf(e_val);
    match op.operator {
        Operator::Sub => {
            if !is_numeric(ctx.context, e_type) {
                err(op.span.start, ErrorType::TypeError("Operator '-', expects and integer or floating point expression as argument".into()))
            } else {
                Ok(LLVMBuildNeg(ctx.builder, e_val, cstr("neg")))
            }
        },
        Operator::Not => {
            if !is_integer(ctx.context, e_type) {
                err(op.span.start, ErrorType::TypeError("Operator '!', expects an integer or boolean expression".into()))
            } else {
                Ok(LLVMBuildNot(ctx.builder, e_val, cstr("not")))
            }
        },
        Operator::Increment => {
            if !is_integer(ctx.context, e_type) {
                err(op.span.start, ErrorType::TypeError("Operator '++', expects an integer expression".into()))
            } else {
                Ok(LLVMBuildAdd(ctx.builder, e_val, const_int(ctx.context, 1), cstr("inc")))
            }
        },
        Operator::Decrement => {
            if !is_integer(ctx.context, e_type) {
                err(op.span.start, ErrorType::TypeError("Operator '--', expects an integer expression".into()))
            } else {
                Ok(LLVMBuildSub(ctx.builder, e_val, const_int(ctx.context, 1), cstr("dec")))
            }
        },
        _ => err(op.span.start, ErrorType::InvalidUnaryOperator(op.operator)),
    }
}

unsafe fn gen_pf_unary(ctx: &mut Context, op: &UnaryOp) -> Result<LLVMValueRef, CompileError>
{
    match op.operator {
        Operator::Increment | Operator::Decrement =>
        {
            if op.expression.is_assignable()
            {
                let ptr = try!(gen_target(ctx, &op.expression));
                let val = LLVMBuildLoad(ctx.builder, ptr, cstr("val"));
                if !is_integer(ctx.context, LLVMTypeOf(val)) {
                    return err(op.span.start, ErrorType::InvalidUnaryOperator(op.operator));
                }

                let nval = if op.operator == Operator::Increment {
                    LLVMBuildAdd(ctx.builder, val, const_int(ctx.context, 1), cstr("inc"))
                } else {
                    LLVMBuildSub(ctx.builder, val, const_int(ctx.context, 1), cstr("dec"))
                };
                LLVMBuildStore(ctx.builder, nval, ptr);
                Ok(val)
            }
            else
            {
                gen_unary(ctx, op)
            }

        },
        _ => err(op.span.start, ErrorType::InvalidUnaryOperator(op.operator)),
    }
}

unsafe fn check_numeric_operands(ctx: &Context, op: Operator, left_type: LLVMTypeRef, right_type: LLVMTypeRef, pos: Pos) -> Result<(), CompileError>
{
    if left_type != right_type {
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects both operands to be of the same type", op)))
    } else if !is_numeric(ctx.context, left_type) || !is_numeric(ctx.context, right_type){
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects integer or floating point expression as operands", op)))
    } else {
        Ok(())
    }
}

unsafe fn check_bool_operands(ctx: &Context, op: Operator, left_type: LLVMTypeRef, right_type: LLVMTypeRef, pos: Pos) -> Result<(), CompileError>
{
    if left_type != right_type {
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects both operands to be of the same type", op, )))
    } else if !is_integer(ctx.context, left_type) || !is_integer(ctx.context, right_type){
        err(pos, ErrorType::TypeError(format!("Operator '{}', expects integer or boolean point expression as operands", op)))
    } else {
        Ok(())
    }
}


unsafe fn gen_binary(ctx: &mut Context, op: &BinaryOp) -> Result<LLVMValueRef, CompileError>
{
    let left_val = try!(gen_expression(ctx, &op.left));
    let right_val = try!(gen_expression(ctx, &op.right));
    let left_type = LLVMTypeOf(left_val);
    let right_type = LLVMTypeOf(right_val);

    match op.operator {
        Operator::Add => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFAdd(ctx.builder, left_val, right_val, cstr("add")))
            } else {
                Ok(LLVMBuildAdd(ctx.builder, left_val, right_val, cstr("add")))
            }
        },
        Operator::Sub => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFSub(ctx.builder, left_val, right_val, cstr("sub")))
            } else {
                Ok(LLVMBuildSub(ctx.builder, left_val, right_val, cstr("sub")))
            }
        },
        Operator::Div => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFDiv(ctx.builder, left_val, right_val, cstr("div")))
            } else {
                Ok(LLVMBuildUDiv(ctx.builder, left_val, right_val, cstr("div")))
            }
        },
        Operator::Mod => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFRem(ctx.builder, left_val, right_val, cstr("mod")))
            } else {
                Ok(LLVMBuildURem(ctx.builder, left_val, right_val, cstr("mod")))
            }
        },
        Operator::Mul => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFMul(ctx.builder, left_val, right_val, cstr("mul")))
            } else {
                Ok(LLVMBuildMul(ctx.builder, left_val, right_val, cstr("mul")))
            }
        },
        Operator::And => {
            try!(check_bool_operands(ctx, op.operator, left_type, right_type, op.span.start));
            Ok(LLVMBuildAnd(ctx.builder, left_val, right_val, cstr("and")))
        },
        Operator::Or => {
            try!(check_bool_operands(ctx, op.operator, left_type, right_type, op.span.start));
            Ok(LLVMBuildOr(ctx.builder, left_val, right_val, cstr("or")))
        },
        Operator::LessThan => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLT, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::LessThanEquals => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLE, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::GreaterThan => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGT, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::GreaterThanEquals => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGE, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::Equals => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left_val, right_val, cstr("cmp")))
            }
        },
        Operator::NotEquals => {
            try!(check_numeric_operands(ctx, op.operator, left_type, right_type, op.span.start));
            if is_floating_point(ctx.context, left_type) {
                Ok(LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealONE, left_val, right_val, cstr("cmp")))
            } else {
                Ok(LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left_val, right_val, cstr("cmp")))
            }
        },
        _ => err(op.span.start, ErrorType::InvalidBinaryOperator(op.operator)),
    }

}

unsafe fn gen_enclosed(ctx: &mut Context, e: &Expression, _span: &Span) -> Result<LLVMValueRef, CompileError>
{
    gen_expression(ctx, e)
}

unsafe fn gen_member_call(ctx: &mut Context, c: &Call, st: &StructType, self_ptr: LLVMValueRef, private_allowed: bool) -> Result<LLVMValueRef, CompileError>
{
    let func_name = try!(c.get_function_name());
    let full_name = format!("{}::{}", st.name, func_name);
    let func = try!(ctx
        .get_function(&full_name)
        .ok_or(CompileError::new(c.span.start, ErrorType::UnknownFunction(full_name))));

    if !private_allowed && !func.public {
        return err(c.span.start, ErrorType::PrivateMemberAccess(func_name));
    }

    let mut arg_vals = Vec::with_capacity(c.args.len() + 1);

    // Add self argument
    let expected_self_type = *func.args.first().expect("Self argument missing");
    if LLVMTypeOf(self_ptr) == LLVMPointerType(expected_self_type, 0) {
        arg_vals.push(LLVMBuildLoad(ctx.builder, self_ptr, cstr("self")));
    } else if LLVMTypeOf(self_ptr) == expected_self_type {
        arg_vals.push(self_ptr);
    } else {
        return Err(type_error(c.span.start, format!("Self type mismatch (got {}, expected {})", type_name(LLVMTypeOf(self_ptr)), type_name(expected_self_type))));
    }

    for arg in &c.args {
        arg_vals.push(try!(gen_expression(ctx, arg)));
    }

    gen_call_common(ctx, c, &func, arg_vals)
}

unsafe fn gen_call_common(ctx: &Context, c: &Call, func: &FunctionInstance, mut arg_vals: Vec<LLVMValueRef>)  -> Result<LLVMValueRef, CompileError>
{
    let func_name = try!(c.get_function_name());
    if arg_vals.len() != func.args.len() {
        return err(c.span.start, ErrorType::ArgumentCountMismatch(
            format!("Function '{}', expects {} arguments, {} are provided",
                func_name, func.args.len(), c.args.len())));
    }

    for (i, arg) in c.args.iter().enumerate() {
        let nval = convert(ctx, arg_vals[i], func.args[i]);
        match nval {
            Some(val) => {
                arg_vals[i] = val;
            },
            None => {
                let val_type = LLVMTypeOf(arg_vals[i]);
                let msg = format!("Argument {} of function '{}' has the wrong type\n  Expecting {}, got {}",
                                i, func_name, type_name(func.args[i]), type_name(val_type));
                return err(arg.span().start, ErrorType::TypeError(msg));
            },
        }
    }

    Ok(LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr("")))
}

unsafe fn gen_call(ctx: &mut Context, c: &Call) -> Result<LLVMValueRef, CompileError>
{
    let mut arg_vals = Vec::with_capacity(c.args.len());
    for arg in &c.args {
        arg_vals.push(try!(gen_expression(ctx, arg)));
    }

    let func_name = try!(c.get_function_name());
    let func = try!(ctx
        .get_function(&func_name)
        .ok_or(CompileError::new(c.span.start, ErrorType::UnknownFunction(func_name))));

    gen_call_common(ctx, c, &func, arg_vals)
}

unsafe fn gen_name_ref(ctx: &Context, nr: &NameRef, store: bool) -> Result<LLVMValueRef, CompileError>
{
    if let Some(ref v) = ctx.get_variable(&nr.name) {
        if !store {
            return Ok(LLVMBuildLoad(ctx.builder, v.value, cstr("load")));
        }

        if v.constant {
            return err(nr.span.start, ErrorType::ConstantModification(nr.name.clone()));
        }

        Ok(v.value)
    } else {
        err(nr.span.start, ErrorType::UnknownVariable(nr.name.clone()))
    }
}

unsafe fn assign(ctx: &Context, op: Operator, var: LLVMValueRef, val: LLVMValueRef, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    if op == Operator::Assign {
        return Ok(LLVMBuildStore(ctx.builder, val, var));
    }

    let var_type = LLVMTypeOf(var);
    try!(check_numeric_operands(ctx, op, LLVMGetElementType(var_type), LLVMTypeOf(val), span.start));
    let var_val = LLVMBuildLoad(ctx.builder, var, cstr("loadtmp"));
    let new_val = match op
    {
        Operator::AddAssign => {
            if is_floating_point(ctx.context, var_type) {
                LLVMBuildFAdd(ctx.builder, var_val, val, cstr("op"))
            } else {
                LLVMBuildAdd(ctx.builder, var_val, val, cstr("op"))
            }
        },
        Operator::SubAssign => {
            if is_floating_point(ctx.context, var_type) {
                LLVMBuildFSub(ctx.builder, var_val, val, cstr("op"))
            } else {
                LLVMBuildSub(ctx.builder, var_val, val, cstr("op"))
            }
        },
        Operator::MulAssign => {
            if is_floating_point(ctx.context, var_type) {
                LLVMBuildFMul(ctx.builder, var_val, val, cstr("op"))
            } else {
                LLVMBuildMul(ctx.builder, var_val, val, cstr("op"))
            }
        },
        Operator::DivAssign =>  {
            if is_floating_point(ctx.context, var_type) {
                LLVMBuildFDiv(ctx.builder, var_val, val, cstr("op"))
            } else {
                LLVMBuildUDiv(ctx.builder, var_val, val, cstr("op"))
            }
        },
        _ => {
            return err(span.start, ErrorType::InvalidOperator(format!("'{}' isn't an assigment operator", op)));
        }
    };

    LLVMBuildStore(ctx.builder, new_val, var);
    Ok(new_val) // Return the new value
}

unsafe fn gen_member_var(ctx: &Context, this: LLVMValueRef, st: &StructType, nr: &NameRef, store: bool, private_allowed: bool) -> Result<LLVMValueRef, CompileError>
{
    if let Some((idx, mvar)) = st.get_member(&nr.name) {
        if mvar.constant && store {
            return err(nr.span.start, ErrorType::ConstantModification(nr.name.clone()));
        }

        if !mvar.public && !private_allowed {
            return err(nr.span.start, ErrorType::PrivateMemberAccess(nr.name.clone()));
        }

        let this_ptr = if LLVMTypeOf(this) == LLVMPointerType(st.typ, 0) {
            this
        } else if LLVMTypeOf(this) == LLVMPointerType(LLVMPointerType(st.typ, 0), 0) {
            // Dereference this, to get the actual struct ptr
            LLVMBuildLoad(ctx.builder, this, cstr("this"))
        } else {
            return err(nr.span.start, ErrorType::TypeError(format!("Type mismatch when accessing member variable")));
        };

        let element = LLVMBuildStructGEP(ctx.builder, this_ptr, idx as u32, cstr("elptr"));
        if store {
            Ok(element)
        } else {
            Ok(LLVMBuildLoad(ctx.builder, element, cstr("elload")))
        }
    } else {
        err(nr.span.start, ErrorType::UnknownStructMember(st.name.clone(), nr.name.clone()))
    }
}

unsafe fn gen_nested_member_access_by_name(
    ctx: &mut Context,
    this: LLVMValueRef,
    st: &StructType,
    next: &MemberAccess,
    store: bool,
    private_allowed: bool,
    member_name: &str) -> Result<LLVMValueRef, CompileError>
{
    let (idx, mvar) = try!(st.get_member(member_name).ok_or(CompileError::new(next.span.start, ErrorType::UnknownStructMember(st.name.clone(), member_name.into()))));
    if !mvar.public && !private_allowed {
        return err(next.span.start, ErrorType::PrivateMemberAccess(mvar.name.clone()));
    }

    let next_st = try!(ctx.get_struct_type(&mvar.typ, next.span.start));
    let new_this = LLVMBuildStructGEP(ctx.builder, this, idx as u32, cstr("elptr"));
    match next.member
    {
        Member::Call(ref c) => gen_member_call(ctx, c, &next_st, new_this, false),
        Member::Var(ref nr) => gen_member_var(ctx, new_this, &next_st, nr, store, false),
        Member::Nested(ref next_next) => gen_nested_member_access(ctx, new_this, &next_st, next_next, store, false),
    }
}

unsafe fn gen_nested_member_access(ctx: &mut Context, this: LLVMValueRef, st: &StructType, next: &MemberAccess, store: bool, private_allowed: bool) -> Result<LLVMValueRef, CompileError>
{
    match *next.target.deref()
    {
        Expression::NameRef(ref nr) => {
            gen_nested_member_access_by_name(ctx, this, st, next, store, private_allowed, &nr.name)
        },
        _ => {
            Err(type_error(next.span.start, "Expected name reference".into()))
        },
    }
}

unsafe fn gen_member_access(ctx: &mut Context, a: &MemberAccess, store: bool) -> Result<LLVMValueRef, CompileError>
{
    let st = try!(ctx
        .infer_type(&a.target)
        .and_then(|t| ctx.get_struct_type(&t, a.span.start)));
    let target_ptr = try!(gen_target(ctx, &a.target));

    let private_allowed = if let &Expression::NameRef(ref nr) = a.target.deref() {
        nr.name == "self"
    } else {
        false
    };

    match a.member
    {
        Member::Call(ref c) => gen_member_call(ctx, c, &st, target_ptr, private_allowed),
        Member::Var(ref nr) => gen_member_var(ctx, target_ptr, &st, nr, store, private_allowed),
        Member::Nested(ref next) => gen_nested_member_access(ctx, target_ptr, &st, next, store, private_allowed),
    }
}

unsafe fn gen_index_operation(ctx: &mut Context, iop: &IndexOperation, store: bool) -> Result<LLVMValueRef, CompileError>
{
    let index = try!(gen_expression(ctx, &iop.index_expr));
    if !is_integer(ctx.context, LLVMTypeOf(index)) {
        return Err(type_error(iop.index_expr.span().start, format!("Indexing must be done with an integer expression")));
    }

    let target_type = try!(ctx.infer_type(&iop.target));
    match target_type
    {
        Type::Slice(_) => {
            let slice = try!(gen_target(ctx, &iop.target));
            let slice_data = LLVMBuildStructGEP(ctx.builder, LLVMBuildLoad(ctx.builder, slice, cstr("slice_data_struct")), 1, cstr("slice_data"));
            let mut index_expr = vec![index];
            let target_ptr = LLVMBuildGEP(ctx.builder, LLVMBuildLoad(ctx.builder, slice_data, cstr("slice_data_ptr")), index_expr.as_mut_ptr(), 1, cstr("target_ptr"));
            if store
            {
                Ok(target_ptr)
            }
            else
            {
                Ok(LLVMBuildLoad(ctx.builder, target_ptr, cstr("element")))
            }
        },
        Type::Array(_, _) => {
            let array = try!(gen_target(ctx, &iop.target));
            let mut index_expr = vec![const_int(ctx.context, 0), index];
            let target_ptr = LLVMBuildGEP(ctx.builder, array, index_expr.as_mut_ptr(), 2, cstr("target_ptr"));
            if store
            {
                Ok(target_ptr)
            }
            else
            {
                Ok(LLVMBuildLoad(ctx.builder, target_ptr, cstr("element")))
            }
        },
        _ => Err(type_error(iop.span.start, format!("Indexing not supported on {}", target_type))),
    }
}

unsafe fn gen_target(ctx: &mut Context, target: &Expression) -> Result<LLVMValueRef, CompileError>
{
    match *target
    {
        Expression::NameRef(ref nr) => {
            gen_name_ref(ctx, nr, true)
        },

        Expression::MemberAccess(ref ma) => {
            gen_member_access(ctx, ma, true)
        },

        Expression::IndexOperation(ref iop) => {
            gen_index_operation(ctx, iop, true)
        },
        _ => err(target.span().start, ErrorType::TypeError(format!("Invalid left hand side expression"))),
    }
}

unsafe fn gen_assignment(ctx: &mut Context, a: &Assignment) -> Result<LLVMValueRef, CompileError>
{
    let target_ptr = try!(gen_target(ctx, &a.target));
    let rhs_val = try!(gen_expression(ctx, &a.expression));
    let rhs_type = LLVMTypeOf(rhs_val);
    let target_type = LLVMTypeOf(target_ptr);
    if let Some(cv) = convert(ctx, rhs_val, LLVMGetElementType(target_type)) {
        assign(ctx, a.operator, target_ptr, cv, &a.span)
    } else {
        let msg = format!("Attempting to assign an expression of type '{}' to a variable of type '{}'",
            type_name(rhs_type), type_name(target_type));
        err(a.span.start, ErrorType::TypeError(msg))
    }
}

unsafe fn gen_const_object_construction(ctx: &mut Context, oc: &ObjectConstruction) -> Result<LLVMValueRef, CompileError>
{
    let st: Rc<StructType> = try!(ctx
        .get_complex_type(&oc.object_type.name)
        .ok_or(CompileError::new(oc.span.start, ErrorType::UnknownType(oc.object_type.name.clone()))));

    if oc.args.len() > st.members.len() {
        return err(oc.span.start, ErrorType::ArgumentCountMismatch(
            format!("Too many arguments in construction of an object of type '{}', maximum {} allowed",
                st.name, st.members.len())));
    }

    let mut vals = Vec::new();
    for (idx, m) in st.members.iter().enumerate() {
        let v = if let Some(ref e) = oc.args.get(idx) {
            try!(gen_const_expression(ctx, e))
        } else {
            // Use default initializer
            try!(gen_const_expression(ctx, &m.init))
        };

        if LLVMIsConstant(v) != 0 {
            vals.push(v);
        } else {
            return err(oc.span.start, ErrorType::ExpectedConstExpr(format!("Global structs must be initialized with constant expressions")));
        }
    }

    Ok(LLVMConstStructInContext(ctx.context, vals.as_mut_ptr(), vals.len() as u32, 0))
}

unsafe fn gen_object_construction(ctx: &mut Context, oc: &ObjectConstruction, ptr: LLVMValueRef) -> Result<(), CompileError>
{
    if ctx.in_global_context()
    {
        LLVMSetInitializer(ptr, try!(gen_const_object_construction(ctx, oc)));
    }
    else
    {
        let st: Rc<StructType> = try!(ctx
            .get_complex_type(&oc.object_type.name)
            .ok_or(CompileError::new(oc.span.start, ErrorType::UnknownType(oc.object_type.name.clone()))));

        if oc.args.len() > st.members.len() {
            return err(oc.span.start, ErrorType::ArgumentCountMismatch(
                format!("Too many arguments in construction of an object of type '{}', maximum {} allowed",
                    st.name, st.members.len())));
        }

        for (idx, m) in st.members.iter().enumerate() {

            let element = LLVMBuildStructGEP(ctx.builder, ptr, idx as u32, cstr("elptr"));
            if let Some(ref e) = oc.args.get(idx) {
                try!(gen_expression_store(ctx, e, element))
            } else {
                // Use default initializer
                try!(gen_expression_store(ctx, &m.init, element))
            }
        }
    }

    Ok(())
}

unsafe fn gen_const_array_literal(ctx: &mut Context, a: &ArrayLiteral) -> Result<LLVMValueRef, CompileError>
{
    let element_type = try!(ctx.infer_array_element_type(a));
    let llvm_type = try!(ctx.resolve_type(&element_type).ok_or(
        type_error(a.span.start, format!("Unknown type {}", element_type))
    ));

    let mut vals = Vec::new();
    for element in &a.elements
    {
        let element_val = try!(gen_const_expression(ctx, element));
        if LLVMIsConstant(element_val) != 0 {
            vals.push(element_val);
        } else {
            return err(a.span.start, ErrorType::ExpectedConstExpr(format!("Global arrays must be initialized with constant expressions")));
        }
    }

    Ok(LLVMConstArray(llvm_type, vals.as_mut_ptr(), vals.len() as u32))
}

unsafe fn gen_array_literal_store(ctx: &mut Context, a: &ArrayLiteral, ptr: LLVMValueRef) -> Result<(), CompileError>
{
    if ctx.in_global_context()
    {
        LLVMSetInitializer(ptr, try!(gen_const_array_literal(ctx, a)));
    }
    else
    {
        for (idx, element) in a.elements.iter().enumerate()
        {
            let mut index_expr = vec![const_int(ctx.context, 0), const_int(ctx.context, idx as u64)];
            let elptr = LLVMBuildGEP(ctx.builder, ptr, index_expr.as_mut_ptr(), 2, cstr("elptr"));
            try!(gen_expression_store(ctx, element, elptr));
        }
    }

    Ok(())
}

unsafe fn gen_array_literal(ctx: &mut Context, a: &ArrayLiteral) -> Result<LLVMValueRef, CompileError>
{
    let element_type = try!(ctx.infer_array_element_type(a));
    let llvm_type = try!(ctx.resolve_type(&element_type)
        .ok_or(type_error(a.span.start, format!("Unknown type '{}'", element_type))));
    let var = LLVMBuildAlloca(ctx.builder, LLVMArrayType(llvm_type, a.elements.len() as u32), cstr("local_var"));
    try!(gen_array_literal_store(ctx, a, var));
    Ok(var)
}

unsafe fn gen_const_array_initializer(ctx: &mut Context, a: &ArrayInitializer) -> Result<LLVMValueRef, CompileError>
{
    let element_type = try!(ctx.infer_type(&a.init));
    let llvm_type = try!(ctx.resolve_type(&element_type).ok_or(
        type_error(a.span.start, format!("Unknown type {}", element_type))
    ));

    let element_val = try!(gen_const_expression(ctx, &a.init));
    if LLVMIsConstant(element_val) == 0 {
        return err(a.span.start, ErrorType::ExpectedConstExpr(format!("Global arrays must be initialized with constant expressions")));
    }

    let mut vals = Vec::with_capacity(a.times as usize);
    for _ in 0..a.times {
        vals.push(element_val);
    }

    Ok(LLVMConstArray(llvm_type, vals.as_mut_ptr(), vals.len() as u32))
}

unsafe fn gen_array_initializer_store(ctx: &mut Context, a: &ArrayInitializer, ptr: LLVMValueRef) -> Result<(), CompileError>
{
    if ctx.in_global_context()
    {
        LLVMSetInitializer(ptr, try!(gen_const_array_initializer(ctx, a)));
    }
    else
    {
        for idx in 0..a.times
        {
            let mut index_expr = vec![const_int(ctx.context, 0), const_int(ctx.context, idx as u64)];
            let elptr = LLVMBuildGEP(ctx.builder, ptr, index_expr.as_mut_ptr(), 2, cstr("elptr"));
            try!(gen_expression_store(ctx, &a.init, elptr));
        }
    }

    Ok(())
}

unsafe fn gen_array_initializer(ctx: &mut Context, a: &ArrayInitializer) -> Result<LLVMValueRef, CompileError>
{
    let array_element_type = try!(ctx.infer_type(&a.init));
    let llvm_type = try!(ctx.resolve_type(&array_element_type)
        .ok_or(type_error(a.span.start, format!("Unknown type '{}'", array_element_type))));
    let var = LLVMBuildAlloca(ctx.builder, LLVMArrayType(llvm_type, a.times as u32), cstr("local_var"));
    try!(gen_array_initializer_store(ctx, a, var));
    Ok(var)
}

pub unsafe fn gen_expression(ctx: &mut Context, e: &Expression) -> Result<LLVMValueRef, CompileError>
{
    match *e
    {
        Expression::IntLiteral(ref span, integer) => gen_integer(ctx, integer, span),
        Expression::FloatLiteral(ref span, ref s) => gen_float(ctx, s, span),
        Expression::StringLiteral(ref span, ref s) => gen_string_literal(ctx, s, span),
        Expression::ArrayLiteral(ref a) => gen_array_literal(ctx, a),
        Expression::ArrayInitializer(ref a) => gen_array_initializer(ctx, a),
        Expression::UnaryOp(ref op) => gen_unary(ctx, op),
        Expression::PostFixUnaryOp(ref op) => gen_pf_unary(ctx, op),
        Expression::BinaryOp(ref op) => gen_binary(ctx, op),
        Expression::Enclosed(ref span, ref e) => gen_enclosed(ctx, e, span),
        Expression::Call(ref c) => gen_call(ctx, c),
        Expression::NameRef(ref nr) => gen_name_ref(ctx, nr, false),
        Expression::Assignment(ref a) => gen_assignment(ctx, a),
        Expression::MemberAccess(ref ma) => gen_member_access(ctx, ma, false),
        Expression::IndexOperation(ref iop) => gen_index_operation(ctx, iop, false),
        _ => err(e.span().start, ErrorType::TypeError(format!("Use gen_expression_store (e = {:?})", e))),
    }
}

unsafe fn store(ctx: &mut Context, e: &Expression, ptr: LLVMValueRef) -> Result<(), CompileError>
{
    let v = try!(gen_expression(ctx, e));
    let dst_typ = LLVMGetElementType(LLVMTypeOf(ptr));
    if let Some(cv) = convert(ctx, v, dst_typ)
    {
        if ctx.in_global_context()
        {
            if LLVMIsConstant(cv) != 0 {
                LLVMSetInitializer(ptr, cv);
            } else {
                return err(e.span().start, ErrorType::ExpectedConstExpr(format!("Global variables and constants must be initialized with a constant expression")));
            }
        }
        else
        {
           LLVMBuildStore(ctx.builder, cv, ptr);
        }
        Ok(())
    }
    else
    {
       let msg = format!("Wrong type ({}, expected {})", type_name(LLVMTypeOf(v)), type_name(dst_typ));
       return err(e.span().start, ErrorType::TypeError(msg));
    }
}

pub unsafe fn gen_expression_store(ctx: &mut Context, e: &Expression, ptr: LLVMValueRef) -> Result<(), CompileError>
{
    match *e
    {
        Expression::ObjectConstruction(ref oc) => gen_object_construction(ctx, oc, ptr),
        Expression::ArrayLiteral(ref a) => gen_array_literal_store(ctx, a, ptr),
        Expression::ArrayInitializer(ref a) => gen_array_initializer_store(ctx, a, ptr),
        _ => store(ctx, e, ptr),
    }
}

unsafe fn gen_const_expression(ctx: &mut Context, e: &Expression) -> Result<LLVMValueRef, CompileError>
{
    match *e
    {
        Expression::IntLiteral(ref span, integer) => gen_integer(ctx, integer, span),
        Expression::FloatLiteral(ref span, ref s) => gen_float(ctx, s, span),
        Expression::StringLiteral(_, ref s) => gen_const_string_literal(ctx, s),
        Expression::ArrayLiteral(ref a) => gen_const_array_literal(ctx, a),
        Expression::ObjectConstruction(ref oc) => gen_const_object_construction(ctx, oc),
        _ => {
            let v = try!(gen_expression(ctx, e));
            if LLVMIsConstant(v) == 0 {
                Err(expected_const_expr(e.span().start, format!("Expected a constant expression")))
            } else {
                Ok(v)
            }
        },
    }
}
