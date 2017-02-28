use std::collections::HashMap;
use ast::Type;
use bytecode::instruction::{Instruction, Operand};
use bytecode::function::{ByteCodeFunction};


struct VarInfo
{
    typ: Type,
    initializer: Operand,
    num_stores: usize,
}

impl VarInfo
{
    fn new(typ: &Type) -> VarInfo
    {
        VarInfo{
            typ: typ.clone(),
            initializer: Operand::Nil,
            num_stores: 0,
        }
    }

    fn is_primitive(&self) -> bool
    {
        match self.typ
        {
            Type::Int | Type::UInt | Type::Float | Type::Bool | Type::String => true,
            _ => false,
        }
    }

    fn can_be_eliminated(&self) -> bool
    {
        self.num_stores == 0 && self.is_primitive()
    }

    fn can_be_replaced_by_initializer(&self) -> bool
    {
        self.num_stores == 1 && self.is_primitive()
    }
}

struct VarInfoMap(HashMap<String, VarInfo>);

impl VarInfoMap
{
    fn remove(&mut self, name: &str)
    {
        self.0.remove(name);
    }

    fn store(&mut self, name: &str, initializer: &Operand)
    {
        self.0.get_mut(name)
            .map(|var_info| {
                var_info.initializer = initializer.clone();
                var_info.num_stores += 1;
            });
    }
}

fn replace_var_by_initializer(instr: &mut Instruction, var: &str, initializer: &Operand)
{
    let do_replace = |op: &mut Operand| {
        let replace = match *op
        {
            Operand::Var(ref mut v) if v.name == var => true,
            _ => false
        };

        if replace {
            *op = initializer.clone();
        }
    };

    match *instr
    {
        Instruction::Store{ref mut src, ..} => do_replace(src),
        Instruction::LoadMember{ref mut member_index, ..} => do_replace(member_index),
        Instruction::UnaryOp{ref mut src, ..} => do_replace(src),
        Instruction::BinaryOp{ref mut left, ref mut right, ..} => {
            do_replace(left);
            do_replace(right);
        },
        Instruction::Call{ref mut args, ..} => {
            for arg in args {
                do_replace(arg)
            }
        },
        Instruction::Slice{ref mut start, ref mut len, ..} => {
            do_replace(start);
            do_replace(len);
        },
        Instruction::Cast{ref mut src, ..} => do_replace(src),
        Instruction::Return(ref mut v) => do_replace(v),
        Instruction::BranchIf{ref mut cond, ..} => do_replace(cond),
        _ => (),
    }
}

fn eliminate_vars_pass(func: &mut ByteCodeFunction) -> usize
{
    let mut vars = VarInfoMap(HashMap::new());

    func.for_each_instruction(|instr: &Instruction| {
        match *instr
        {
            Instruction::StackAlloc(ref var) => {
                vars.0.insert(var.name.clone(), VarInfo::new(&var.typ));
            },

            Instruction::Store{ref dst, ref src} => {
                vars.store(&dst.name, src);
            },

            Instruction::Cast{ref dst, ..} |
            Instruction::Load{ref dst, ..} |
            Instruction::UnaryOp{ref dst, ..} |
            Instruction::BinaryOp{ref dst, ..} |
            Instruction::Slice{ref dst, ..} |
            Instruction::Call{ref dst, ..} => {
                vars.remove(&dst.name);
            },

            Instruction::LoadMember{ref obj, ..} |
            Instruction::AddressOf{ref obj, ..} => {
                vars.remove(&obj.name);
            },

            Instruction::GetProperty{ref obj, ref dst, ..} => {
                vars.remove(&obj.name);
                vars.remove(&dst.name);
            }

            _ => (),
        }

        true
    });

    let mut eliminated = 0;
    for (var, var_info) in &vars.0 {
        if !var_info.can_be_eliminated() && !var_info.can_be_replaced_by_initializer() {
            continue;
        }

        eliminated += 1;

        func.remove_instruction(|instr: &Instruction| {
            match *instr {
                Instruction::StackAlloc(ref v) if v.name == *var => true,
                Instruction::Store{ref dst, ..} if dst.name == *var => true,
                _ => false,
            }
        });

        if var_info.can_be_replaced_by_initializer() {
            func.for_each_instruction_mut(|instr| {
                replace_var_by_initializer(instr, &var, &var_info.initializer);
                true
            });
        }
        break; // Only do one per time
    }
    eliminated
}

pub fn eliminate_vars(func: &mut ByteCodeFunction)
{
    while eliminate_vars_pass(func) > 0 {
    }
}
