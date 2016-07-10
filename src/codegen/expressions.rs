use llvm::*;
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

pub unsafe fn const_int(ctx: LLVMContextRef, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx), v, 0)
}

unsafe fn gen_float(ctx: &mut Context, num: &str, span: &Span) -> Result<LLVMValueRef, CompileError>
{
    match num.parse::<f64>() {
        Ok(f) => Ok(LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f)),
        Err(_) => err(span.start, ErrorType::InvalidFloatingPoint)
    }
}

unsafe fn gen_integer(ctx: &mut Context, i: u64, _span: &Span) -> Result<LLVMValueRef, CompileError>
{
    Ok(const_int(ctx.context, i))
}

unsafe fn gen_string_literal(ctx: &mut Context, s: &str, _span: &Span) -> Result<LLVMValueRef, CompileError>
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as u32), cstr("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);

    // Initialize with string:
    LLVMSetInitializer(glob, LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1));
    Ok(glob)
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
        Operator::Increment | Operator::Decrement => gen_unary(ctx, op),
        _ => err(op.span.start, ErrorType::InvalidUnaryOperator(op.operator)),
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

fn is_same_kind(a: LLVMTypeKind, b: LLVMTypeKind) -> bool
{
    (a as usize) == (b as usize)
}

// Convert a value to a different type, if possible
unsafe fn convert(b: LLVMBuilderRef, from: LLVMValueRef, to: LLVMTypeRef) ->  Option<LLVMValueRef>
{
    let from_type = LLVMTypeOf(from);
    if from_type == to {
        return Some(from); // Same types, so no problem
    }

    let array_to_ptr =
        is_same_kind(LLVMGetTypeKind(from_type), LLVMTypeKind::LLVMPointerTypeKind) &&
        is_same_kind(LLVMGetTypeKind(to), LLVMTypeKind::LLVMPointerTypeKind) &&
        is_same_kind(LLVMGetTypeKind(LLVMGetElementType(from_type)), LLVMTypeKind::LLVMArrayTypeKind) &&
        LLVMGetElementType(LLVMGetElementType(from_type)) == LLVMGetElementType(to);
    if array_to_ptr {
        let cast = LLVMBuildBitCast(b, from, to, cstr("cast"));
        return Some(cast);
    }

    None
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
        let nval = convert(ctx.builder, arg_vals[i], func.args[i]);
        match nval {
            Some(val) => {
                arg_vals[i] = val;
            },
            None => {
                let val_type = LLVMTypeOf(arg_vals[i]);
                let msg = format!("Argument {} of function '{}' has the wrong type\n  Expecting {}, got {}",
                                i, c.name, type_name(func.args[i]), type_name(val_type));
                return err(arg.span().start, ErrorType::TypeError(msg));
            },
        }
    }

    Ok(LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr("")))
}

unsafe fn gen_name_ref(ctx: &mut Context, nr: &NameRef) -> Result<LLVMValueRef, CompileError>
{
    if let Some(ref v) = ctx.get_variable(&nr.name) {
        Ok(LLVMBuildLoad(ctx.builder, v.value, cstr("load")))
    } else {
        err(nr.span.start, ErrorType::UnknownVariable(nr.name.clone()))
    }
}

unsafe fn assign(ctx: &mut Context, op: Operator, var: LLVMValueRef, val: LLVMValueRef, span: &Span) -> Result<LLVMValueRef, CompileError>
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

unsafe fn gen_assignment(ctx: &mut Context, a: &Assignment) -> Result<LLVMValueRef, CompileError>
{
    if !ctx.has_variable(&a.target) {
        return err(a.span.start, ErrorType::UnknownVariable(a.target.clone()));
    }

    let rhs_val = try!(gen_expression(ctx, &a.expression));
    let rhs_type = LLVMTypeOf(rhs_val);
    let (var, constant) = ctx.get_variable(&a.target).map(|v| (v.value, v.constant)).unwrap();
    if constant {
        return err(a.span.start, ErrorType::ConstantModification(a.target.clone()));
    }

    let var_type = LLVMTypeOf(var);
    if let Some(cv) = convert(ctx.builder, rhs_val, LLVMGetElementType(var_type)) {
        assign(ctx, a.operator, var, cv, &a.span)
    } else {
        let msg = format!("Attempting to assign an expression of type '{}' to a variable of type '{}'",
            type_name(rhs_type), type_name(var_type));
        err(a.span.start, ErrorType::TypeError(msg))
    }
}

unsafe fn gen_object_construction(ctx: &mut Context, oc: &ObjectConstruction) -> Result<LLVMValueRef, CompileError>
{
    use std::rc::Rc;
    let st: Rc<StructType> = try!(ctx
        .get_complex_type(&oc.object_type)
        .ok_or(CompileError::new(oc.span.start, ErrorType::UnknownType(oc.object_type.clone()))));

    if oc.args.len() > st.members.len() {
        return err(oc.span.start, ErrorType::ArgumentCountMismatch(
            format!("Too many arguments in construction of an object of type '{}', maximum {} allowed",
                st.name, st.members.len())));
    }

    let ptr = LLVMBuildAlloca(ctx.builder, st.typ, cstr("newvar"));

    for (idx, m) in st.members.iter().enumerate() {
        let element = LLVMBuildStructGEP(ctx.builder, ptr, idx as u32, cstr("elptr"));

        let v = if let Some(ref e) = oc.args.get(idx) {
            try!(gen_expression(ctx, e))
        } else {
            // Use default initializer
            try!(gen_expression(ctx, &m.init))
        };

        if let Some(cv) = convert(ctx.builder, v, m.typ) {
            LLVMBuildStore(ctx.builder, cv, element);
        } else {
            let msg = format!("Expression to initialize member {} of struct {}, has the wrong type ({}, expected {})",
                    idx, st.name, type_name(LLVMTypeOf(v)), type_name(m.typ));
            return err(oc.span.start, ErrorType::TypeError(msg));
        }
    }

    Ok(ptr)
}

#[allow(unused_variables)]
unsafe fn gen_member_access(ctx: &mut Context, a: &MemberAccess) -> Result<LLVMValueRef, CompileError>
{
    return err(a.span.start, ErrorType::UnexpectedEOF);
}

pub unsafe fn gen_expression(ctx: &mut Context, e: &Expression) -> Result<LLVMValueRef, CompileError>
{
    match *e
    {
        Expression::IntLiteral(ref span, integer) => gen_integer(ctx, integer, span),
        Expression::FloatLiteral(ref span, ref s) => gen_float(ctx, s, span),
        Expression::StringLiteral(ref span, ref s) => gen_string_literal(ctx, s, span),
        Expression::UnaryOp(ref op) => gen_unary(ctx, op),
        Expression::PostFixUnaryOp(ref op) => gen_pf_unary(ctx, op),
        Expression::BinaryOp(ref op) => gen_binary(ctx, op),
        Expression::Enclosed(ref span, ref e) => gen_enclosed(ctx, e, span),
        Expression::Call(ref c) => gen_call(ctx, c),
        Expression::NameRef(ref nr) => gen_name_ref(ctx, nr),
        Expression::Assignment(ref a) => gen_assignment(ctx, a),
        Expression::ObjectConstruction(ref oc) => gen_object_construction(ctx, oc),
        Expression::MemberAccess(ref ma) => gen_member_access(ctx, ma),
    }
}
