use std::collections::HashMap;
use std::ffi::CString;
use libc::*;
use llvm::*;
use llvm::core::*;
use llvm::prelude::*;
use bytecode::*;
use ast::{Type, Operator, ptr_type};
use super::valueref::ValueRef;
use super::context::Context;

pub unsafe fn const_int(ctx: &Context, v: isize) -> LLVMValueRef
{
    LLVMConstInt(ctx.native_int_type(), v as c_ulonglong, 1)
}

pub unsafe fn const_uint(ctx: &Context, v: usize) -> LLVMValueRef
{
    LLVMConstInt(ctx.native_int_type(), v as c_ulonglong, 0)
}

pub unsafe fn const_bool(ctx: &Context, v: bool) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt1TypeInContext(ctx.context), if v {1} else {0}, 0)
}

unsafe fn const_float(ctx: &Context, v: f64) -> LLVMValueRef
{
    LLVMConstReal(LLVMDoubleTypeInContext(ctx.context), v)
}

unsafe fn const_char(ctx: &Context, c: char) -> LLVMValueRef
{
    LLVMConstInt(LLVMInt32TypeInContext(ctx.context), c as c_ulonglong, 0)
}

unsafe fn get_variable(ctx: &Context, name: &str) -> ValueRef
{
    ctx.get_variable(name).expect("Unknown variable").value.clone()
}

pub unsafe fn get_operand(ctx: &Context, operand: &Operand) -> ValueRef
{
    match *operand
    {
        Operand::Func(ref func) => {
            let fi = ctx.get_function(func).expect("Unknown function");
            ValueRef::new(
                fi.function,
                fi.sig.typ.clone()
            )
        }

        Operand::Var(ref v) => {
            get_variable(ctx, &v.name)
        }

        Operand::AddressOf(ref v) => {
            ctx.get_variable(&v.name)
                .expect("Unknown variable")
                .value.address_of()
        }

        Operand::Int(v) => ValueRef::new(const_int(ctx, v), Type::Int),
        Operand::UInt(v) => ValueRef::new(const_uint(ctx, v), Type::UInt),
        Operand::Float(v) => ValueRef::new(const_float(ctx, v), Type::Float),
        Operand::Char(v) => ValueRef::new(const_char(ctx, v), Type::Char),
        Operand::String(ref _s) => panic!("NYI"),
        Operand::Bool(v) => ValueRef::new(const_bool(ctx, v), Type::Bool),
    }
}

pub unsafe fn copy(ctx: &Context, dst: LLVMValueRef, src: LLVMValueRef, typ: LLVMTypeRef)
{
    let func = ctx.get_function("memcpy").expect("memcpy not found");
    let void_ptr_type = LLVMPointerType(LLVMVoidTypeInContext(ctx.context), 0);
    let mut args = vec![
        LLVMBuildBitCast(ctx.builder, dst, void_ptr_type, cstr!("dst_cast")),
        LLVMBuildBitCast(ctx.builder, src, void_ptr_type, cstr!("src_cast")),
        const_uint(ctx, ctx.target_machine.size_of_type(typ))
    ];
    LLVMBuildCall(ctx.builder, func.function, args.as_mut_ptr(), args.len() as c_uint, cstr!("ac"));
}

unsafe fn get_function_arg(ctx: &Context, operand: &Operand) -> LLVMValueRef
{
    match *operand
    {
        Operand::Var(ref v) => {
            let src = get_variable(ctx, &v.name);
            let inner_type = src.typ.get_pointer_element_type().expect("Expecting pointer type here");
            if inner_type.pass_by_value() {
                src.load(ctx)
            } else {
                let llvm_type = ctx.resolve_type(inner_type);
                let dst = stack_alloc(ctx, llvm_type, "argcopy");
                copy(ctx, dst, src.value, llvm_type);
                dst
            }
        },

        Operand::AddressOf(ref v) => {
            let src = get_variable(ctx, &v.name);
            src.value
        }

        _ => get_operand(ctx, operand).load(ctx)
    }
}

unsafe fn stack_alloc(ctx: &Context, typ: LLVMTypeRef, name: &str) -> LLVMValueRef
{
    let func = ctx.get_current_function();
    let entry_bb = LLVMGetEntryBasicBlock(func);
    let current_bb = LLVMGetInsertBlock(ctx.builder);
    // We allocate in the entry block
    LLVMPositionBuilder(ctx.builder, entry_bb, LLVMGetFirstInstruction(entry_bb));

    let name = CString::new(name).expect("Invalid string");
    let alloc = LLVMBuildAlloca(ctx.builder, typ, name.as_ptr());
    LLVMPositionBuilderAtEnd(ctx.builder, current_bb); // Position the builder where it was before
    alloc
}

unsafe fn gen_unary_op(ctx: &Context, dst: &Var, operator: Operator, src: &Operand)
{
    let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
    let src_value = get_operand(ctx, src).load(ctx);
    let result = match (operator, &dst.typ)
    {
        (Operator::Sub, &Type::Int) |
        (Operator::Sub, &Type::UInt) => LLVMBuildNeg(ctx.builder, src_value, cstr!("neg")),
        (Operator::Sub, &Type::Float) => LLVMBuildFNeg(ctx.builder, src_value, cstr!("neg")),
        (Operator::Not, &Type::Bool) => LLVMBuildNot(ctx.builder, src_value, cstr!("not")),
        _ => panic!("Unsupported unary operator"),
    };

    dst_var.value.store(ctx, &ValueRef::new(result, dst.typ.clone()));
}

unsafe fn gen_binary_op(ctx: &Context, dst: &Var, op: Operator, left: &Operand, right: &Operand)
{
    let left_type = left.get_type();
    let left = get_operand(ctx, left).load(ctx);
    let right = get_operand(ctx, right).load(ctx);

    let value = match (op, left_type)
    {
        (Operator::Add, Type::Int) => LLVMBuildAdd(ctx.builder, left, right, cstr!("bop")),
        (Operator::Add, Type::UInt) => LLVMBuildAdd(ctx.builder, left, right, cstr!("bop")),
        (Operator::Add, Type::Float) => LLVMBuildFAdd(ctx.builder, left, right, cstr!("bop")),

        (Operator::Sub, Type::Int) => LLVMBuildSub(ctx.builder, left, right, cstr!("bop")),
        (Operator::Sub, Type::UInt) => LLVMBuildSub(ctx.builder, left, right, cstr!("bop")),
        (Operator::Sub, Type::Float) => LLVMBuildFSub(ctx.builder, left, right, cstr!("bop")),

        (Operator::Mul, Type::Int) => LLVMBuildMul(ctx.builder, left, right, cstr!("bop")),
        (Operator::Mul, Type::UInt) => LLVMBuildMul(ctx.builder, left, right, cstr!("bop")),
        (Operator::Mul, Type::Float) => LLVMBuildFMul(ctx.builder, left, right, cstr!("bop")),

        (Operator::Div, Type::Int) => LLVMBuildSDiv(ctx.builder, left, right, cstr!("bop")),
        (Operator::Div, Type::UInt) => LLVMBuildUDiv(ctx.builder, left, right, cstr!("bop")),
        (Operator::Div, Type::Float) => LLVMBuildFDiv(ctx.builder, left, right, cstr!("bop")),

        (Operator::Mod, Type::Int) => LLVMBuildSRem(ctx.builder, left, right, cstr!("bop")),
        (Operator::Mod, Type::UInt) => LLVMBuildURem(ctx.builder, left, right, cstr!("bop")),

        (Operator::LessThan, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, left, right, cstr!("bop")),
        (Operator::LessThan, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULT, left, right, cstr!("bop")),
        (Operator::LessThan, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealULT, left, right, cstr!("bop")),
        (Operator::LessThan, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULT, left, right, cstr!("bop")),

        (Operator::GreaterThan, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, left, right, cstr!("bop")),
        (Operator::GreaterThan, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGT, left, right, cstr!("bop")),
        (Operator::GreaterThan, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUGT, left, right, cstr!("bop")),
        (Operator::GreaterThan, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGT, left, right, cstr!("bop")),

        (Operator::LessThanEquals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, left, right, cstr!("bop")),
        (Operator::LessThanEquals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULE, left, right, cstr!("bop")),
        (Operator::LessThanEquals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealULE, left, right, cstr!("bop")),
        (Operator::LessThanEquals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntULE, left, right, cstr!("bop")),

        (Operator::GreaterThanEquals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, left, right, cstr!("bop")),
        (Operator::GreaterThanEquals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGE, left, right, cstr!("bop")),
        (Operator::GreaterThanEquals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUGE, left, right, cstr!("bop")),
        (Operator::GreaterThanEquals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntUGE, left, right, cstr!("bop")),

        (Operator::Equals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::Bool) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),
        (Operator::Equals, Type::Enum(_)) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, left, right, cstr!("bop")),

        (Operator::NotEquals, Type::Int) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::UInt) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::Float) => LLVMBuildFCmp(ctx.builder, LLVMRealPredicate::LLVMRealUNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::Char) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::Bool) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),
        (Operator::NotEquals, Type::Enum(_)) => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, left, right, cstr!("bop")),

        (Operator::And, Type::Bool) => LLVMBuildAnd(ctx.builder, left, right, cstr!("bop")),
        (Operator::Or, Type::Bool) => LLVMBuildOr(ctx.builder, left, right, cstr!("bop")),

        (_, t) => panic!("Operator {} not supported on type {}", op, t),
    };


    let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
    dst_var.value.store(ctx, &ValueRef::new(value, dst.typ.clone()));
}

unsafe fn gen_cast(ctx: &mut Context, dst: &Var, src: &Operand)
{
    let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
    let operand = get_operand(ctx, src);
    let src_type = src.get_type();
    let casted = match (&dst.typ, &src_type)
    {
        (&Type::UInt, &Type::Int) |
        (&Type::Int, &Type::UInt) =>
            LLVMBuildIntCast(ctx.builder, operand.load(ctx), ctx.resolve_type(&dst.typ), cstr!("cast_to_int")),

        (&Type::Int, &Type::Float) =>
            LLVMBuildFPToSI(ctx.builder, operand.load(ctx), ctx.resolve_type(&dst.typ), cstr!("cast_to_int")),

        (&Type::UInt, &Type::Float) =>
            LLVMBuildFPToUI(ctx.builder, operand.load(ctx), ctx.resolve_type(&dst.typ), cstr!("cast_to_int")),

        (&Type::Float, &Type::Int) =>
            LLVMBuildSIToFP(ctx.builder, operand.load(ctx), ctx.resolve_type(&dst.typ), cstr!("cast_to_int")),

        (&Type::Float, &Type::UInt) =>
            LLVMBuildUIToFP(ctx.builder, operand.load(ctx), ctx.resolve_type(&dst.typ), cstr!("cast_to_int")),

        _ => panic!("Cast from type {} to type {} is not allowed", src_type, dst.typ),
    };

    dst_var.value.store(ctx, &ValueRef::new(casted, dst.typ.clone()));
}

pub unsafe fn gen_instruction(ctx: &mut Context, instr: &Instruction, blocks: &HashMap<BasicBlockRef, LLVMBasicBlockRef>)
{
    //print!(">> {}", instr);
    match *instr
    {
        Instruction::Store{ref dst, ref src} => {
            let vr = get_operand(ctx, src);
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            dst_var.value.store(ctx, &vr);
        }

        Instruction::Load{ref dst, ref ptr} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let src_var = ctx.get_variable(&ptr.name).expect("Unknown variable");
            dst_var.value.store(ctx, &src_var.value);
        }

        Instruction::LoadMember{ref dst, ref obj, ref member_index} => { ;
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            let member_ptr = obj_var.value.get_member_ptr(ctx, member_index);
            dst_var.value.store(ctx, &member_ptr);
        }

        Instruction::AddressOfMember{ref dst, ref obj, ref member_index} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            let member_ptr = obj_var.value.get_member_ptr(ctx, member_index);
            dst_var.value.store(ctx, &member_ptr);
        }

        Instruction::StoreMember{ref obj, ref member_index, ref src} => {
            let src_val = get_operand(ctx, src);
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            obj_var.value.store_member(ctx, member_index, &src_val);
        }

        Instruction::AddressOf{ref dst, ref obj} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            dst_var.value.store(ctx, &obj_var.value.address_of());
        }

        Instruction::GetProperty{ref dst, ref obj, ref prop} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            let prop = obj_var.value.get_property(ctx, *prop);
            dst_var.value.store(ctx, &prop);
        }

        Instruction::SetProperty{ref obj, ref prop, ref val} => {
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            obj_var.value.set_property(ctx, *prop, *val);
        }

        Instruction::UnaryOp{ref dst, ref op, ref src} => {
            gen_unary_op(ctx, dst, *op, src);
        }

        Instruction::BinaryOp{ref dst, ref op, ref left, ref right} => {
            gen_binary_op(ctx, dst, *op, left, right);
        }

        Instruction::Call{ref dst, ref func, ref args} => {
            let func = ctx.get_function(func).expect("Unknown function");
            let mut func_args = args.iter()
                .map(|a| get_function_arg(ctx, a))
                .collect::<Vec<_>>();
            if let Some(ref dst) = *dst {
                let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
                let ret = ValueRef::new(
                    LLVMBuildCall(ctx.builder, func.function, func_args.as_mut_ptr(), args.len() as c_uint, cstr!("call")),
                    func.sig.return_type.clone()
                );
                dst_var.value.store(ctx, &ret);
            } else {
                LLVMBuildCall(ctx.builder, func.function, func_args.as_mut_ptr(), args.len() as c_uint, cstr!(""));
            }
        }

        Instruction::Slice{ref dst, ref src, ref start, ref len} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let src_var = ctx.get_variable(&src.name).expect("Unknown variable");
            dst_var.value.create_slice(ctx, &src_var.value, start, len);
        }

        Instruction::IsNil{ref dst, ref obj} => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            let obj_var = ctx.get_variable(&obj.name).expect("Unknown variable");
            dst_var.value.store(ctx, &obj_var.value.is_nil(ctx));
        }

        Instruction::StoreNil(ref dst) => {
            let dst_var = ctx.get_variable(&dst.name).expect("Unknown variable");
            dst_var.value.store_nil(ctx);
        }

        Instruction::Cast{ref dst, ref src} => {
            gen_cast(ctx, dst, src);
        }

        Instruction::GlobalAlloc(ref _var) => {
            panic!("NYI");
        }

        Instruction::StackAlloc(ref var) => {
            let typ = ctx.resolve_type(&var.typ);
            let val = stack_alloc(ctx, typ, &var.name);
            ctx.add_variable(&var.name, ValueRef::new(val, ptr_type(var.typ.clone())));
        }

        Instruction::HeapAlloc(ref var) => {
            let name = CString::new(&var.name[..]).expect("Invalid string");
            let value = LLVMBuildMalloc(ctx.builder, ctx.resolve_type(&var.typ), name.as_ptr());
            ctx.add_variable(&var.name, ValueRef::new(value, ptr_type(var.typ.clone())))
        }

        Instruction::StartScope => {
        }

        Instruction::EndScope => {
        }

        Instruction::Return(ref operand) => {
            LLVMBuildRet(ctx.builder, get_operand(ctx, operand).load(ctx));
        }

        Instruction::ReturnVoid => {
            LLVMBuildRetVoid(ctx.builder);
        }

        Instruction::Branch(ref bbref) => {
            let llvm_bb = blocks.get(bbref).expect("Unknown basic block");
            LLVMBuildBr(ctx.builder, *llvm_bb);
        }

        Instruction::BranchIf{ref cond, ref on_true, ref on_false} => {
            let on_true_bb = blocks.get(on_true).expect("Unknown basic block");
            let on_false_bb = blocks.get(on_false).expect("Unknown basic block");
            LLVMBuildCondBr(ctx.builder, get_operand(ctx, cond).load(ctx), *on_true_bb, *on_false_bb);
        }

        Instruction::Delete(ref var) => {
            LLVMBuildFree(ctx.builder, get_variable(ctx, &var.name).value);
        }

        Instruction::Exit => {
        }
    }
}
