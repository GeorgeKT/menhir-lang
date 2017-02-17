use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;
use libcobra::bytecode::*;
use super::context::Context;


pub fn gen_instruction(ctx: &mut Context, instr: &Instruction, blocks: &HashMap<BasicBlockRef, LLVMBasicBlockRef>)
{
    match *instr
    {
        Instruction::Store{ref dst, ref src} => {
        }

        Instruction::Load{ref dst, ref ptr} => {
        }

        Instruction::LoadMember{ref dst, ref obj, ref member_index} => {
        }

        Instruction::AddressOf{ref dst, ref obj} => {
        }

        Instruction::GetProperty{ref dst, ref obj, ref prop} => {
        }

        Instruction::SetProperty{ref obj, ref prop, ref val} => {
        }

        Instruction::UnaryOp{ref dst, ref op, ref src} => {
        }

        Instruction::BinaryOp{ref dst, ref op, ref left, ref right} => {
        }

        Instruction::Call{ref dst, ref func, ref args} => {
        }

        Instruction::Slice{ref dst, ref src, ref start, ref len} => {
        }

        Instruction::Cast{ref dst, ref src} => {
        }

        Instruction::GlobalAlloc(ref var) => {
        }

        Instruction::StackAlloc(ref var) => {
        }

        Instruction::HeapAlloc(ref var) => {
        }

        Instruction::StartScope => {
        }

        Instruction::EndScope => {
        }

        Instruction::Return(ref operand) => {
        }

        Instruction::ReturnVoid => {
        }

        Instruction::Branch(ref bbref) => {
        }

        Instruction::BranchIf{ref cond, ref on_true, ref on_false} => {
        }

        Instruction::Delete(ref var) => {
        }

        Instruction::Exit => {
        }
    }
}
