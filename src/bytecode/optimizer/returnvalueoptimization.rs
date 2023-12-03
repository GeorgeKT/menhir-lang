use crate::ast::{func_type, ptr_type, Argument, FuncArg, Type};
use crate::bytecode::{
    store_operand_instr, void_call_instr, ByteCodeFunction, ByteCodeModule, Instruction, Operand, Var,
};
use crate::span::Span;
use std::mem;

pub const RVO_PARAM_NAME: &'static str = "$ret_rvo";

fn rvo_needed(func: &ByteCodeFunction) -> bool {
    !func.sig.return_type.pass_by_value() && func.sig.return_type != Type::Void
}

fn rvo_func(func: &mut ByteCodeFunction) {
    let return_type_arg = ptr_type(mem::replace(&mut func.sig.return_type, Type::Void));
    func.sig.args.push(Argument::new(
        RVO_PARAM_NAME,
        return_type_arg.clone(),
        true,
        Span::default(),
    ));

    func.sig.typ = func_type(
        func.sig
            .args
            .iter()
            .map(|a| FuncArg {
                typ: a.typ.clone(),
                mutable: a.mutable,
            })
            .collect(),
        Type::Void,
    );

    func.replace_instruction(|instr: &Instruction| {
        if let Instruction::Return(ref operand) = *instr {
            vec![
                store_operand_instr(&Var::named(RVO_PARAM_NAME, return_type_arg.clone()), operand.clone()),
                Instruction::ReturnVoid,
            ]
        } else {
            Vec::new()
        }
    });
}

fn rvo_replace_calls(bc_func: &mut ByteCodeFunction, rvo_calls: &[String]) {
    bc_func.replace_instruction(|instr: &Instruction| match *instr {
        Instruction::Call {
            ref dst,
            ref func,
            ref args,
        } if rvo_calls.contains(func) => {
            if let Some(ref dst) = *dst {
                let mut new_args = args.clone();
                new_args.push(Operand::AddressOf(dst.clone()));
                vec![void_call_instr(func, new_args)]
            } else {
                Vec::new()
            }
        }

        _ => Vec::new(),
    })
}

pub fn return_value_optimization(module: &mut ByteCodeModule) {
    let mut to_replace = Vec::new();
    for func in module.functions.values_mut() {
        if rvo_needed(func) {
            rvo_func(func);
            to_replace.push(func.sig.name.clone());
        }
    }

    for func in &mut module.imported_functions {
        if rvo_needed(func) {
            rvo_func(func);
            to_replace.push(func.sig.name.clone());
        }
    }

    for func in module.functions.values_mut() {
        rvo_replace_calls(func, &to_replace)
    }
}
