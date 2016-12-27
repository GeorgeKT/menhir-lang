use std::ptr;
use std::collections::HashMap;
use libc;
use llvm::core::*;
use llvm::prelude::*;
use llvm::*;

use ast::*;
use codegen::*;
use bytecode::*;
use parser::Operator;


pub unsafe fn const_int(ctx: &Context, v: u64) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt64TypeInContext(ctx.context), v, 0)
}

pub unsafe fn const_bool(ctx: &Context, v: bool) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0)
}

unsafe fn const_float(ctx: &Context, num: &str) -> LLVMValueRef
{
    match num.parse::<f64>()
    {
        Ok(f) => LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), f),
        Err(_) => panic!("Internal Compiler Error: {} is not a valid floating point number", num)
    }
}

unsafe fn gen_array_literal(ctx: &Context, elements: &Vec<Var>, array: &Array)
{
    let len = const_int(ctx, elements.len() as u64);
    array.init(ctx, len);
    for (idx, element) in elements.iter().enumerate()
    {
        let element_var = ctx.get_variable(&element.name).expect("Unknown variable").value.load(ctx.builder);
        let index = const_int(ctx, idx as u64);
        let el_ptr = array.get_element(ctx, index);
        el_ptr.store_direct(ctx, element_var);
    }
}

unsafe fn gen_const_string_literal(ctx: &Context, s: &str) -> ValueRef
{
    ValueRef::Const(LLVMConstStringInContext(ctx.context, s.as_ptr() as *const i8, s.len() as u32, 1))
}

unsafe fn gen_string_literal(ctx: &Context, s: &str, array: &Array)
{
    let glob = LLVMAddGlobal(ctx.module, LLVMArrayType(LLVMInt8TypeInContext(ctx.context), s.len() as libc::c_uint),  cstr!("string"));

    LLVMSetLinkage(glob, LLVMLinkage::LLVMInternalLinkage);
    LLVMSetGlobalConstant(glob, 1);
    let string_data = gen_const_string_literal(ctx, s);
    LLVMSetInitializer(glob, string_data.get());
    array.fill_with_string_literal(ctx, glob, s.len())
}

unsafe fn gen_literal(ctx: &mut Context, dst: &Var, lit: &ByteCodeLiteral)
{
    let dst_vr = get_value_ref(ctx, dst);


    match *lit
    {
        ByteCodeLiteral::Int(v) => dst_vr.store_direct(ctx, const_int(ctx, v)),
        ByteCodeLiteral::Float(ref v) => dst_vr.store_direct(ctx, const_float(ctx, v)),
        ByteCodeLiteral::Char(v) => dst_vr.store_direct(ctx, LLVMConstInt(LLVMInt8TypeInContext(ctx.context), v as u64, 0)),
        ByteCodeLiteral::Bool(v) => dst_vr.store_direct(ctx, const_bool(ctx, v)),

        ByteCodeLiteral::String(ref v) => {
            let fill_string_literal = |dst_vr: &ValueRef| {
                if let &ValueRef::Array(ref array) = dst_vr {
                    gen_string_literal(ctx, v, array)
                } else {
                    panic!("Internal Compiler Error: Expecting array ValueRef")
                }
            };

            match dst_vr
            {
                ValueRef::Array(_) => fill_string_literal(&dst_vr),
                ValueRef::HeapPtr(_, _) => fill_string_literal(&dst_vr.deref(ctx)),
                _ => panic!("Internal Compiler Error: Expecting array ValueRef"),
            }
        },

        ByteCodeLiteral::Array(ref vars) => {
            let fill_array_literal = |dst_vr: &ValueRef| {
                if let &ValueRef::Array(ref array) = dst_vr {
                    gen_array_literal(ctx, vars, array)
                } else {
                    panic!("Internal Compiler Error: Expecting array ValueRef")
                }
            };

            match dst_vr
            {
                ValueRef::Array(_) => fill_array_literal(&dst_vr),
                ValueRef::HeapPtr(_, _) => fill_array_literal(&dst_vr.deref(ctx)),
                _ => panic!("Internal Compiler Error: Expecting array ValueRef"),
            }
        },
    }
}

unsafe fn gen_int_bin_op(ctx: &Context, l: LLVMValueRef, r: LLVMValueRef, op: Operator) -> LLVMValueRef
{
    match op
    {
        Operator::Add => LLVMBuildAdd(ctx.builder, l, r, cstr!("add")),
        Operator::Sub => LLVMBuildSub(ctx.builder, l, r, cstr!("sub")),
        Operator::Mul => LLVMBuildMul(ctx.builder, l, r, cstr!("mul")),
        Operator::Div => LLVMBuildUDiv(ctx.builder, l, r, cstr!("div")),
        Operator::Mod => LLVMBuildURem(ctx.builder, l, r, cstr!("mod")),
        Operator::LessThan => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, l, r, cstr!("cmp")),
        Operator::LessThanEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, l, r, cstr!("cmp")),
        Operator::GreaterThan => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, l, r, cstr!("cmp")),
        Operator::GreaterThanEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, l, r, cstr!("cmp")),
        Operator::Equals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, l, r, cstr!("cmp")),
        Operator::NotEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, l, r, cstr!("cmp")),
        _ => panic!("Internal Compiler Error: Operator {} is not supported on integers", op),
    }
}

unsafe fn gen_float_bin_op(ctx: &Context, l: LLVMValueRef, r: LLVMValueRef, op: Operator) -> LLVMValueRef
{
    match op
    {
        Operator::Add => LLVMBuildFAdd(ctx.builder, l, r, cstr!("add")),
        Operator::Sub => LLVMBuildFSub(ctx.builder, l, r, cstr!("sub")),
        Operator::Mul => LLVMBuildFMul(ctx.builder, l, r, cstr!("mul")),
        Operator::Div => LLVMBuildFDiv(ctx.builder, l, r, cstr!("div")),
        Operator::Mod => LLVMBuildFRem(ctx.builder, l, r, cstr!("mod")),
        Operator::LessThan => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLT, l, r, cstr!("cmp")),
        Operator::LessThanEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOLE, l, r, cstr!("cmp")),
        Operator::GreaterThan => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGT, l, r, cstr!("cmp")),
        Operator::GreaterThanEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOGE, l, r, cstr!("cmp")),
        Operator::Equals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealOEQ, l, r, cstr!("cmp")),
        Operator::NotEquals => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealONE, l, r, cstr!("cmp")),
        _ => panic!("Internal Compiler Error: Operator {} is not supported on floats", op),
    }
}

unsafe fn gen_bool_bin_op(ctx: &Context, l: LLVMValueRef, r: LLVMValueRef, op: Operator) -> LLVMValueRef
{
    match op
    {
        Operator::And => LLVMBuildAnd(ctx.builder, l, r, cstr!("and")),
        Operator::Or => LLVMBuildOr(ctx.builder, l, r, cstr!("or")),
        Operator::Equals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, l, r, cstr!("cmp")),
        Operator::NotEquals => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, l, r, cstr!("cmp")),
        _ => panic!("Internal Compiler Error: Operator {} is not supported on bools", op),
    }
}

unsafe fn gen_array_bin_op(ctx: &mut Context, dst: &Var, left: &Var, right: &Var, op: Operator)
{
    match op {
        Operator::Add => {
            let l = ctx.get_variable(&left.name).expect("Unknown variable").value.deref(ctx);
            let r = ctx.get_variable(&right.name).expect("Unknown variable").value.deref(ctx);
            let dst = get_value_ref(ctx, dst);
            match (l, r)
            {
                (ValueRef::Array(ref ar), ValueRef::Array(ref br)) => {
                    let dst_val = ValueRef::Array(Array::concat(ctx, ar, br));
                    dst.store(ctx, &dst_val);
                }
                _ => panic!("Internal Compiler Error: Operator {} expects two arrays", op),
            }
        },
        _ => panic!("Internal Compiler Error: Operator {} is not supported on arrays", op),
    }
}

unsafe fn gen_bin_op(ctx: &mut Context, dst: &Var, left: &Var, right: &Var, op: Operator)
{
    if let Type::Array(_) = left.typ {
        return gen_array_bin_op(ctx, dst, left, right, op);
    }

    let l = ctx.get_variable(&left.name).expect("Unknown variable").value.load(ctx.builder);
    let r = ctx.get_variable(&right.name).expect("Unknown variable").value.load(ctx.builder);
    let dst_vr = get_value_ref(ctx, dst);
    dst_vr.store_direct(ctx, match left.typ
    {
        Type::Int | Type::Char =>  {
            gen_int_bin_op(ctx, l, r, op)
        },
        Type::Float => {
            gen_float_bin_op(ctx, l, r, op)
        },
        Type::Bool => {
            gen_bool_bin_op(ctx, l, r, op)
        },
        Type::Enum(_) => {
            gen_int_bin_op(ctx, l, r, op)
        },
        _ => panic!("Internal Compiler Error: Binary operator {} not support on type {}", op, left.typ),
    });
}

unsafe fn gen_unary_op(ctx: &mut Context, dst: &Var, target: &Var, op: Operator)
{
    let t = ctx.get_variable(&target.name).expect("Unknown variable").value.load(ctx.builder);
    let dst = get_value_ref(ctx, dst);
    match op
    {
        Operator::Sub => {
            dst.store_direct(ctx, LLVMBuildNeg(ctx.builder, t, cstr!("neg")));
        },
        Operator::Not => {
            dst.store_direct(ctx, LLVMBuildNot(ctx.builder, t, cstr!("not")));
        },
        _ => panic!("Internal Compiler Error: Operator {} is not a unary operator", op),
    }
}

unsafe fn gen_call_args(ctx: &Context, args: &Vec<Var>) -> Vec<LLVMValueRef>
{
    let mut arg_vals = Vec::with_capacity(args.len());
    for arg in args.iter()
    {
        let var = ctx.get_variable(&arg.name).expect("Unknown variable");
        arg_vals.push(var.value.load(ctx.builder))
    }
    arg_vals
}

unsafe fn gen_call(ctx: &mut Context, _dst: &Var, name: &str, args: &Vec<Var>)
{
    let func = ctx.get_function(name).expect("Internal Compiler Error: Unknown function");
    let mut arg_vals = gen_call_args(ctx, args);
    let _ret = LLVMBuildCall(ctx.builder, func.function, arg_vals.as_mut_ptr(), arg_vals.len() as libc::c_uint, cstr!("ret"));
    panic!("Fix this !!!!");
    /*
    if dst.typ.allocate_on_heap() {
        ctx.add_variable(&dst.name, ValueRef::new(ret, &dst.typ));
    } else {
        let dst_val = get_value_ref(ctx, dst);
        dst_val.store_direct(ctx, ret);
    }
    */
}

unsafe fn gen_struct_member(ctx: &mut Context, _dst: &Var, obj: &Var, index: usize)
{
    let struct_object = ctx.get_variable(&obj.name).expect("Unknown variable");
    let _member_ptr = struct_object.value.member(ctx, index);
    panic!("Fix this !!!!");
    /*
    if dst.typ.allocate_on_heap() {
        ctx.add_variable(&dst.name, member_ptr);
    } else {
        let dst_var = get_value_ref(ctx, dst);
        dst_var.store(ctx, &member_ptr);
    }
    */
}

unsafe fn gen_array_property(ctx: &mut Context, dst: &Var, array: &Var, property: ArrayProperty)
{
    let array_object = &ctx.get_variable(&array.name).expect("Unknown variable").value;
    let prop_ptr = array_object.array_property(ctx, property);
    ctx.add_variable(&dst.name, prop_ptr);
}

unsafe fn gen_ref(ctx: &mut Context, dst: &Var, var: &Var)
{
    if let Type::Func(_) = dst.typ
    {
        let func = ctx.get_function(&var.name).expect("Unknown function");
        let dst_var = get_value_ref(ctx, dst);
        dst_var.store_direct(ctx, func.function);
        ctx.add_function_alias(&dst.name, func);
    }
    else
    {
        let var_object = ctx.get_variable(&var.name).expect("Unknown variable");
        let dst_var = get_value_ref(ctx, dst);
        dst_var.store(ctx, &var_object.value);
    }
}

unsafe fn gen_array_head(ctx: &mut Context, dst: &Var, array: &Var)
{
    let array_object = ctx.get_variable(&array.name).expect("Unknown variable");

    if let ValueRef::Array(ref a) = array_object.value {
        let dst_var = get_value_ref(ctx, dst);
        dst_var.store(ctx, &a.head(ctx));
    } else {
        panic!("Expecting an array ValueRef");
    }
}

unsafe fn gen_array_tail(ctx: &mut Context, dst: &Var, array: &Var)
{
    let array_object = ctx.get_variable(&array.name).expect("Unknown variable");
    let dst_var = get_value_ref(ctx, dst);
    if let (&ValueRef::Array(ref a), ValueRef::Array(ref da)) = (&array_object.value, dst_var) {
        a.tail(ctx, da);
    } else {
        panic!("Expecting an array ValueRef");
    }
}

fn gen_func_expr(ctx: &mut Context, dst: &Var, name: &str)
{
    let func = ctx.get_function(&name).expect("Internal Compiler Error: Unknown function");
    ctx.add_variable(&dst.name, ValueRef::Const(func.function));
    ctx.add_function_alias(&dst.name, func);
}

unsafe fn gen_sum_type_index(ctx: &mut Context, dst: &Var, obj: &Var)
{
    let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
    let case_type_ptr = obj_var.value.case_type(ctx);
    ctx.add_variable(&dst.name, case_type_ptr);
}

unsafe fn gen_sum_type_struct(ctx: &mut Context, dst: &Var, obj: &Var, index: usize)
{
    let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
    let case_struct = obj_var.value.case_struct(ctx, index);
    ctx.add_variable(&dst.name, case_struct);
}

unsafe fn gen_sum_type_case(ctx: &mut Context, dst: &Var, index: usize)
{
    let dst_vr = get_value_ref(ctx, dst);
    let case_type_ptr = dst_vr.case_type(ctx);
    case_type_ptr.store_direct(ctx, const_int(ctx, index as u64));
}

unsafe fn gen_heap_alloc(ctx: &mut Context, var: &Var, typ: &Type)
{
    let vr = get_value_ref(ctx, var);
    let typ = ctx.resolve_type(typ);
    let ptr = ctx.heap_alloc(typ, &var.name);
    vr.store_direct(ctx, ptr);
}

unsafe fn get_value_ref(ctx: &mut Context, var: &Var) -> ValueRef
{
    if let Some(ref vr) = ctx.get_variable(&var.name) {
        vr.value.clone()
    } else {
        let vr = ValueRef::alloc(ctx, &var.typ);
        ctx.add_variable(&var.name, vr.clone());
        vr
    }
}

unsafe fn gen_expr(ctx: &mut Context, dst: &Var, expr: &ByteCodeExpression)
{
    match *expr
    {
        ByteCodeExpression::Literal(ref l) => gen_literal(ctx, dst, l),
        ByteCodeExpression::UnaryOp(op, ref v) => gen_unary_op(ctx, dst, v, op),
        ByteCodeExpression::BinaryOp(op, ref a, ref b) => gen_bin_op(ctx, dst, a, b, op),
        ByteCodeExpression::Call(ref name, ref args) => gen_call(ctx, dst, name, args),
        ByteCodeExpression::StructMember(ref obj, index) => gen_struct_member(ctx, dst, obj, index),
        ByteCodeExpression::ArrayProperty(ref array, ref property) => gen_array_property(ctx, dst, array, property.clone()),
        ByteCodeExpression::ArrayHead(ref array) => gen_array_head(ctx, dst, array),
        ByteCodeExpression::ArrayTail(ref array) => gen_array_tail(ctx, dst, array),
        ByteCodeExpression::SumTypeIndex(ref obj) => gen_sum_type_index(ctx, dst, obj),
        ByteCodeExpression::SumTypeStruct(ref obj, index) => gen_sum_type_struct(ctx, dst, obj, index),
        ByteCodeExpression::SumTypeCase(index) => gen_sum_type_case(ctx, dst, index),
        ByteCodeExpression::Ref(ref obj) => gen_ref(ctx, dst, obj),
        ByteCodeExpression::Func(ref func) => gen_func_expr(ctx, dst, func),
        ByteCodeExpression::HeapAlloc(ref typ) => gen_heap_alloc(ctx, dst, typ),
    }
}

pub unsafe fn gen_instruction(ctx: &mut Context, instr: &Instruction, blocks: &HashMap<BasicBlockRef, LLVMBasicBlockRef>)
{
    //print!(">> {}", instr);
    match *instr
    {
        Instruction::Set{ref var, ref expr} => {
            gen_expr(ctx, var, expr);
        },

        Instruction::SetStructMember{ref obj, member_index, ref value} => {
            let struct_object = ctx.get_variable(&obj.name).expect("Unknown variable obj");
            let member_ptr = struct_object.value.member(ctx, member_index);
            member_ptr.store(ctx, &ctx.get_variable(&value.name).expect("Unknown variable value").value);
        },

        Instruction::Alloc(ref var) => {
            let typ = match var.typ
            {
                Type::Func(_) => LLVMPointerType(ctx.resolve_type(&var.typ), 0),
                Type::Struct(_) => LLVMPointerType(ctx.resolve_type(&var.typ), 0),
                Type::Sum(_) => LLVMPointerType(ctx.resolve_type(&var.typ), 0),
                Type::Array(_) => LLVMPointerType(ctx.resolve_type(&var.typ), 0),
                _ => ctx.resolve_type(&var.typ),
            };

            let v = ValueRef::HeapPtr(ctx.stack_alloc(typ, &var.name), var.typ.clone());
            ctx.add_variable(&var.name, v);
        },

        Instruction::Return(ref var) => {
            let ret = ctx.get_variable(&var.name).expect("Unknown variable");
            LLVMBuildRet(ctx.builder, ret.value.load(ctx.builder));
        },

        Instruction::ReturnVoid => {
            LLVMBuildRetVoid(ctx.builder);
        },

        Instruction::StartScope => {
            ctx.push_stack(ptr::null_mut());
        },

        Instruction::EndScope => {
            ctx.pop_stack();
        },

        Instruction::Bind{ref name, ref var} => {
            if let Type::Func(_) = var.typ {
                let func = ctx.get_function(&var.name).expect("Unknown function");
                ctx.add_variable(&name, ValueRef::new(func.function, &var.typ));
                ctx.add_function_alias(&name, func);
            } else {
                let var = ctx.get_variable(&var.name).expect("Unknown variable");
                ctx.add_variable(&name, var.value.clone());
            }
        },

        Instruction::Branch(ref bb) => {
            let llvm_bb = blocks.get(bb).expect("Unknown basic block");
            LLVMBuildBr(ctx.builder, *llvm_bb);
        },

        Instruction::BranchIf{ref cond, ref on_true, ref on_false} => {
            let on_true_bb = blocks.get(&on_true).expect("Unknown basic block");
            let on_false_bb = blocks.get(&on_false).expect("Unknown basic block");
            let cond = ctx.get_variable(&cond.name).expect("Unknown variable");
            LLVMBuildCondBr(ctx.builder, cond.value.load(ctx.builder), *on_true_bb, *on_false_bb);
        },

        Instruction::Delete(ref var) => {
            let var = ctx.get_variable(&var.name).expect("Unknown variable");
            LLVMBuildFree(ctx.builder, var.value.load(ctx.builder));
        },
    }
}
