mod debugger;
mod value;
mod valueref;
#[cfg(test)]
mod tests;

use std::collections::HashMap;

use ast::{Type, Operator};
use bytecode::*;
use bytecode::ByteCodeFunction;

use self::value::Value;
use self::valueref::ValueRef;
use self::debugger::ByteCodeIndex;

const RETURN_VALUE : &'static str = "@RETURN_VALUE@";

pub type ExecutionResult<T> = Result<T, String>;

#[derive(Debug, Clone)]
struct ReturnAddress
{
    address: ByteCodeIndex,
    result_destination: String,
}

struct StackFrame
{
    vars: HashMap<String, ValueRef>,
    return_address: Option<ReturnAddress>,
}

impl StackFrame
{
    pub fn with_return_address(return_address: ByteCodeIndex, result_destination: String) -> StackFrame
    {
        StackFrame{
            vars: HashMap::new(),
            return_address: Some(ReturnAddress{
                address: return_address,
                result_destination: result_destination,
            })
        }
    }

    pub fn new() -> StackFrame
    {
        StackFrame{
            vars: HashMap::new(),
            return_address: None,
        }
    }

    pub fn has_return_address(&self) -> bool
    {
        self.return_address.is_some()
    }

    pub fn add(&mut self, name: &str, v: Value) -> ExecutionResult<()>
    {
        if self.vars.get(name).is_some() {
            Err(format!("Variable {} already exists", name))
        } else {
            self.vars.insert(name.into(), ValueRef::new(v));
            Ok(())
        }
    }
}

pub struct Interpreter
{
    stack: Vec<StackFrame>,
    globals: StackFrame,
    debug_mode: bool,
}

pub enum StepResult
{
    Continue(ByteCodeIndex),
    Exit(Value),
}

impl Interpreter
{
    pub fn new(debug_mode: bool) -> Interpreter
    {
        Interpreter{
            stack: Vec::new(),
            globals: StackFrame::new(),
            debug_mode: debug_mode,
        }
    }


    fn add_variable(&mut self, name: &str, v: Value) -> ExecutionResult<()>
    {
        let mut sf = self.stack.last_mut().expect("Empty stack");
        sf.add(name, v)
    }

    fn add_global_variable(&mut self, name: &str, v: Value) -> ExecutionResult<()>
    {
        self.globals.add(name, v)
    }

    pub fn get_variable(&self, name: &str) -> ExecutionResult<ValueRef>
    {
        for sf in self.stack.iter().rev() {
            if let Some(v) = sf.vars.get(name) {
                return Ok(v.clone());
            }

            if sf.has_return_address() {
                break;
            }
        }

        self.globals.vars.get(name).cloned().ok_or_else(|| format!("Unknown variable {}", name))
    }

    fn apply_on_variable<Op>(&mut self, name: &str, op: Op) -> ExecutionResult<()>
        where Op: FnOnce(&mut ValueRef) -> ExecutionResult<()>
    {
        for sf in self.stack.iter_mut().rev() {
            if let Some(ref mut v) = sf.vars.get_mut(name) {
                return op(v)
            }

            if sf.has_return_address() {
                break;
            }
        }

        if let Some(ref mut v) = self.globals.vars.get_mut(name) {
            return op(v)
        }

        Err(format!("Unknown variable {}", name))
    }

    fn update_variable(&mut self, name: &str, new_value: Value) -> ExecutionResult<()>
    {
        self.replace_variable(name, ValueRef::new(new_value))
    }

    fn replace_variable(&mut self, name: &str, vr: ValueRef) -> ExecutionResult<()>
    {
        self.apply_on_variable(name, |v: &mut ValueRef| {
            *v = vr;
            Ok(())
        })
    }

    fn get_operand_value(&self, op: &Operand, module: &ByteCodeModule) -> ExecutionResult<Value>
    {
        match *op
        {
            Operand::Int(v) => Ok(Value::Int(v)),
            Operand::UInt(v) => Ok(Value::UInt(v)),
            Operand::Float(v) => Ok(Value::Float(v)),
            Operand::Char(v) => Ok(Value::Char(v as char)),
            Operand::String(ref s) => Ok(Value::String(s.clone())),
            Operand::Bool(v) => Ok(Value::Bool(v)),
            Operand::AddressOf(ref src) => {
                let var = self.get_variable(&src.name)?;
                Ok(Value::Pointer(var))
            }
            Operand::Var(ref src) => Ok(self.get_variable(&src.name)?.clone_value()?),
            Operand::Func(ref func) => Ok(Value::Func(self.get_function(func, module)?.sig.name.clone())),
        }
    }

    fn store(&mut self, name: &str, val: &Operand, module: &ByteCodeModule) -> ExecutionResult<()>
    {
        let val = self.get_operand_value(val, module)?;

        self.apply_on_variable(name, |vr: &mut ValueRef| {
            vr.apply_mut(|v: &mut Value| {
                match *v
                {
                    Value::Optional(ref mut inner) |
                    Value::Pointer(ref mut inner) =>
                        inner.apply_mut(|inner_v: &mut Value| {
                            *inner_v = val;
                            Ok(())
                        })?,
                    _ => *v = val,
                }
                Ok(())
            })
        })
    }

    fn unary_op(&mut self, dst: &str, op: Operator, var: &Operand, module: &ByteCodeModule) -> ExecutionResult<()>
    {
        let val = self.get_operand_value(var, module)?;
        let result =
            match (op, val)
            {
                (Operator::Sub, Value::Int(num)) => Value::Int(-num),
                (Operator::Sub, Value::UInt(num)) => Value::Int(-(num as isize)),
                (Operator::Sub, Value::Float(num)) => Value::Float(-num),
                (Operator::Not, Value::Bool(b)) => Value::Bool(!b),
                _ => return Err(format!("Invalid unary op {}", op)),
            };

        self.update_variable(dst, result)?;
        Ok(())
    }

    #[cfg_attr(feature = "cargo-clippy", allow(float_cmp, match_same_arms))]
    fn binary_op(&mut self, dst: &str, op: Operator, left: &Operand, right: &Operand, module: &ByteCodeModule) -> ExecutionResult<()>
    {
        let left = self.get_operand_value(left, module)?;
        let right = self.get_operand_value(right, module)?;

        let value = match (op, left, right)
        {
            (Operator::Add, Value::Int(l), Value::Int(r)) => Value::Int(l + r),
            (Operator::Add, Value::UInt(l), Value::UInt(r)) => Value::UInt(l + r),
            (Operator::Add, Value::Float(l), Value::Float(r)) => Value::Float(l + r),

            (Operator::Sub, Value::Int(l), Value::Int(r)) => Value::Int(l - r),
            (Operator::Sub, Value::UInt(l), Value::UInt(r)) => Value::UInt(l - r),
            (Operator::Sub, Value::Float(l), Value::Float(r)) => Value::Float(l - r),

            (Operator::Mul, Value::Int(l), Value::Int(r)) => Value::Int(l * r),
            (Operator::Mul, Value::UInt(l), Value::UInt(r)) => Value::UInt(l * r),
            (Operator::Mul, Value::Float(l), Value::Float(r)) => Value::Float(l * r),

            (Operator::Div, Value::Int(l), Value::Int(r)) => Value::Int(l / r),
            (Operator::Div, Value::UInt(l), Value::UInt(r)) => Value::UInt(l / r),
            (Operator::Div, Value::Float(l), Value::Float(r)) => Value::Float(l / r),

            (Operator::Mod, Value::Int(l), Value::Int(r)) => Value::Int(l % r),
            (Operator::Mod, Value::UInt(l), Value::UInt(r)) => Value::UInt(l % r),

            (Operator::LessThan, Value::Int(l), Value::Int(r)) => Value::Bool(l < r),
            (Operator::LessThan, Value::UInt(l), Value::UInt(r)) => Value::Bool(l < r),
            (Operator::LessThan, Value::Float(l), Value::Float(r)) => Value::Bool(l < r),
            (Operator::LessThan, Value::Char(l), Value::Char(r)) => Value::Bool(l < r),

            (Operator::GreaterThan, Value::Int(l), Value::Int(r)) => Value::Bool(l > r),
            (Operator::GreaterThan, Value::UInt(l), Value::UInt(r)) => Value::Bool(l > r),
            (Operator::GreaterThan, Value::Float(l), Value::Float(r)) => Value::Bool(l > r),
            (Operator::GreaterThan, Value::Char(l), Value::Char(r)) => Value::Bool(l > r),

            (Operator::LessThanEquals, Value::Int(l), Value::Int(r)) => Value::Bool(l <= r),
            (Operator::LessThanEquals, Value::UInt(l), Value::UInt(r)) => Value::Bool(l <= r),
            (Operator::LessThanEquals, Value::Float(l), Value::Float(r)) => Value::Bool(l <= r),
            (Operator::LessThanEquals, Value::Char(l), Value::Char(r)) => Value::Bool(l <= r),

            (Operator::GreaterThanEquals, Value::Int(l), Value::Int(r)) => Value::Bool(l >= r),
            (Operator::GreaterThanEquals, Value::UInt(l), Value::UInt(r)) => Value::Bool(l >= r),
            (Operator::GreaterThanEquals, Value::Float(l), Value::Float(r)) => Value::Bool(l >= r),
            (Operator::GreaterThanEquals, Value::Char(l), Value::Char(r)) => Value::Bool(l >= r),

            (Operator::Equals, Value::Int(l), Value::Int(r)) => Value::Bool(l == r),
            (Operator::Equals, Value::UInt(l), Value::UInt(r)) => Value::Bool(l == r),
            (Operator::Equals, Value::Float(l), Value::Float(r)) => Value::Bool(l == r),
            (Operator::Equals, Value::Char(l), Value::Char(r)) => Value::Bool(l == r),
            (Operator::Equals, Value::Bool(l), Value::Bool(r)) => Value::Bool(l == r),
            (Operator::Equals, Value::String(ref l), Value::String(ref r)) => Value::Bool(*l == *r),

            (Operator::NotEquals, Value::Int(l), Value::Int(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::UInt(l), Value::UInt(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::Float(l), Value::Float(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::Char(l), Value::Char(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::Bool(l), Value::Bool(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::String(ref l), Value::String(ref r)) => Value::Bool(*l != *r),

            (Operator::And, Value::Bool(l), Value::Bool(r)) => Value::Bool(l && r),
            (Operator::Or, Value::Bool(l), Value::Bool(r)) => Value::Bool(l || r),


            (_, left, right) => return Err(format!("Operator {} not supported on operands ({}) and ({})", op, left, right)),
        };


        self.update_variable(dst, value)?;
        Ok(())
    }

    fn call(&mut self, dst: Option<&str>, func: &ByteCodeFunction, args: &[Operand], index: &ByteCodeIndex, module: &ByteCodeModule) -> ExecutionResult<StepResult>
    {
        if self.debug_mode {
            println!("{}:", func.sig.name);
        }

        let return_address = index.next();

        let mut arg_values = Vec::new();
        for arg in args {
            let arg_value = self.get_operand_value(arg, module)?;
            arg_values.push(arg_value);
        }

        let dst = dst.unwrap_or("");
        self.stack.push(StackFrame::with_return_address(return_address, dst.into()));
        for (idx, arg_value) in arg_values.into_iter().enumerate() {
            self.add_variable(&func.sig.args[idx].name, arg_value)?;
        }

        Ok(StepResult::Continue(ByteCodeIndex::new(func.sig.name.clone(), 0, 0)))
    }

    fn pop_until_end_of_function(&mut self) -> ExecutionResult<ReturnAddress>
    {
        while let Some(sf) = self.stack.pop() {
            if let Some(return_address) = sf.return_address {
                return Ok(return_address.clone());
            }
        }

        Err("Reached bottom of the stack".into())
    }

    fn ret(&mut self, op: Option<&Operand>, module: &ByteCodeModule) -> ExecutionResult<StepResult>
    {
        let return_address = match op
        {
            Some(operand) => {
                let result = self.get_operand_value(operand, module)?;
                let return_address = self.pop_until_end_of_function()?;
                self.update_variable(&return_address.result_destination, result)?;
                return_address
            }

            None => {
                self.pop_until_end_of_function()?
            }
        };

        Ok(StepResult::Continue(return_address.address))
    }

    fn branch_if(
        &self,
        cond: &Operand,
        on_true: BasicBlockRef,
        on_false: BasicBlockRef,
        index: &ByteCodeIndex,
        module: &ByteCodeModule) -> ExecutionResult<StepResult>
    {
        let val = self.get_operand_value(cond, module)?;
        match val
        {
            Value::Bool(true) => Ok(StepResult::Continue(index.jump(on_true))),
            Value::Bool(false) => Ok(StepResult::Continue(index.jump(on_false))),
            _ => Err(format!("brif operand {} is not a boolean", cond)),
        }
    }

    fn get_member_index(&self, member_index: &Operand) -> ExecutionResult<usize>
    {
        match *member_index
        {
            Operand::Int(index) if index >= 0 => Ok(index as usize),
            Operand::UInt(index) => Ok(index as usize),
            Operand::Var(ref v) =>
                match self.get_variable(&v.name)?.clone_value()?
                {
                    Value::Int(index) if index >= 0 => Ok(index as usize),
                    Value::UInt(index) => Ok(index as usize),
                    _ => Err("load member instruction with non integer or negative index value".into()),
                },
            _ => Err("load member instruction with non integer or negative index value".into()),
        }
    }

    fn load_member(&mut self, dst: &str, obj: &str, member_index: &Operand) -> ExecutionResult<()>
    {
        let obj = self.get_variable(obj)?;
        let index = self.get_member_index(member_index)?;

        let vr = obj.apply(|value: &Value| value.get_member_ptr(index as usize))?;
        self.update_variable(dst, vr.load()?)?;
        Ok(())
    }

    fn store_member(&mut self, obj: &str, member_index: &Operand, src: &Operand, module: &ByteCodeModule) -> ExecutionResult<()>
    {
        let index = self.get_member_index(member_index)?;
        let src_val = self.get_operand_value(src, module)?;
        self.apply_on_variable(obj, |vr: &mut ValueRef|
            vr.apply_mut(|v: &mut Value| {
                v.update_member(index, src_val)
            })
        )
    }

    fn address_of_member(&mut self, dst: &str, obj: &str, member_index: &Operand) -> ExecutionResult<()>
    {
        let obj = self.get_variable(obj)?;
        let index = self.get_member_index(member_index)?;

        let vr = obj.apply(|value: &Value| value.get_member_ptr(index as usize))?;
        self.replace_variable(dst, ValueRef::new(vr))?;
        Ok(())
    }

    fn get_property(&mut self, dst: &str, obj: &str, prop: ByteCodeProperty) -> ExecutionResult<()>
    {
        let obj = self.get_variable(obj)?;
        let val = obj.apply(|vr: &Value| vr.get_property(prop))?;
        self.update_variable(dst, val)?;
        Ok(())
    }

    fn set_property(&mut self, obj: &str, prop: &ByteCodeProperty, val: usize) -> ExecutionResult<()>
    {
        self.apply_on_variable(obj, |vr: &mut ValueRef|
            vr.apply_mut(|v: &mut Value|
                match (prop, v) {
                    (&ByteCodeProperty::SumTypeIndex, &mut Value::Enum(ref mut idx)) |
                    (&ByteCodeProperty::SumTypeIndex, &mut Value::Sum(ref mut idx, _)) => {
                        *idx = val;
                        Ok(())
                    },
                    _ => Err(format!("Setting property {} not support on {}", prop, obj))
                }
            )
        )
    }

    fn get_index(&self, op: &Operand, module: &ByteCodeModule) -> ExecutionResult<usize>
    {
        match self.get_operand_value(op, module)?
        {
            Value::UInt(v) => Ok(v as usize),
            Value::Int(v) if v >= 0 => Ok(v as usize),
            _ => Err(format!("{} is not an integer or a negative integer", op)),
        }
    }

    fn slice(&mut self, dst: &str, array: &str, start: &Operand, len: &Operand, module: &ByteCodeModule) -> ExecutionResult<()>
    {
        let start_value = self.get_index(start, module)?;
        let len_value = self.get_index(len, module)?;
        let slice = self.get_variable(array)?.apply(|vr: &Value| {
            match *vr
            {
                Value::Array(ref arr) => {
                    if start_value + len_value > arr.len() {
                        return Err(format!("Slice index {} out of bounds", start_value + len_value));
                    }

                    let mut slice = Vec::new();
                    for element in &arr[start_value .. (start_value + len_value)] {
                        slice.push(element.to_ptr());
                    }
                    Ok(Value::Slice(slice))
                },

                Value::Slice(ref slice) => {
                    if start_value + len_value > slice.len() {
                        return Err(format!("Slice index {} out of bounds", start_value + len_value));
                    }

                    let subslice = &slice[start_value .. (start_value + len_value)];
                    Ok(Value::Slice(subslice.iter().cloned().collect()))
                },

                _ => {
                    Err(format!("{} is not an array or slice", array))
                },
            }
        })?;

        self.update_variable(dst, slice)?;
        Ok(())
    }

    fn get_function<'a>(&self, func: &str, module: &'a ByteCodeModule) -> ExecutionResult<&'a ByteCodeFunction>
    {
        if module.exit_function.sig.name == func {
            return Ok(&module.exit_function);
        }

        match module.get_function(func) {
            Some(f) => Ok(f),
            None => {
                let v = self.get_variable(func)?;
                v.apply(|val: &Value|
                    if let Value::Func(ref f) = *val {
                        module.get_function(f)
                            .ok_or_else(|| format!("Unknown function {}", f))
                    } else {
                        Err(format!("{} is not callable", func))
                    }
                )
            }
        }
    }

    fn load(&mut self, dst: &str, ptr: &str) -> ExecutionResult<()>
    {
        let ptr = self.get_variable(ptr)?;
        let new_value = ptr.apply(|v: &Value| {
            match *v
            {
                Value::Optional(ref inner) => inner.clone_value(),
                Value::Pointer(ref inner) => inner.clone_value(),
                _ => Err(format!("Load can only be performed on pointers and optionals, not on {}", v)),
            }
        })?;

        self.update_variable(dst, new_value)
    }

    fn cast(&mut self, dst: &Var, src: &Operand, module: &ByteCodeModule) -> ExecutionResult<()>
    {
        let src_val = self.get_operand_value(src, module)?;
        let new_value =
            match (src_val, &dst.typ)
            {
                (Value::Int(s), &Type::UInt) => Ok(Value::UInt(s as usize)),
                (Value::Int(s), &Type::Float) => Ok(Value::Float(s as f64)),
                (Value::UInt(s), &Type::Int) => Ok(Value::Int(s as isize)),
                (Value::UInt(s), &Type::Float) => Ok(Value::Float(s as f64)),
                (Value::Float(s), &Type::Int) => Ok(Value::Int(s as isize)),
                (Value::Float(s), &Type::UInt) => Ok(Value::UInt(s as usize)),
                _ => Err(String::from("Unsupported type cast")),
            }?;
        self.update_variable(&dst.name, new_value)
    }

    fn is_nil(&mut self, dst: &Var, obj: &Var) -> ExecutionResult<()>
    {
        let v = self.get_variable(&obj.name)?.apply(|val: &Value| {
            match *val {
                Value::Optional(ref ov) => ov.apply(|v: &Value| Ok(v.is_nil())),
                Value::Nil => Ok(true),
                _ => Ok(false)
            }
        });
        self.update_variable(&dst.name, Value::Bool(v?))
    }

    fn execute_instruction(&mut self, instr: &Instruction, index: &ByteCodeIndex, module: &ByteCodeModule) -> ExecutionResult<StepResult>
    {
        let next = Ok(StepResult::Continue(index.next()));
        match *instr
        {
            Instruction::Exit => {
                let return_value = self.get_variable(RETURN_VALUE)?.clone_value()?;
                Ok(StepResult::Exit(return_value))
            },

            Instruction::Store{ref dst, ref src} => {
                self.store(&dst.name, src, module)?;
                next
            },

            Instruction::Load{ref dst, ref ptr} => {
                self.load(&dst.name, &ptr.name)?;
                next
            },

            Instruction::LoadMember{ref dst, ref obj, ref member_index} => {
                self.load_member(&dst.name, &obj.name, member_index)?;
                next
            },

            Instruction::StoreMember{ref obj, ref member_index, ref src} => {
                self.store_member(&obj.name, member_index, src, module)?;
                next
            }

            Instruction::AddressOf{ref dst, ref obj} => {
                let ptr = self.get_variable(&obj.name)?.to_ptr();
                self.replace_variable(&dst.name, ptr)?;
                next
            },

            Instruction::AddressOfMember{ref dst, ref obj, ref member_index} => {
                self.address_of_member(&dst.name, &obj.name, member_index)?;
                next
            }

            Instruction::GetProperty{ref dst, ref obj, ref prop} => {
                self.get_property(&dst.name, &obj.name, *prop)?;
                next
            },

            Instruction::SetProperty{ref obj, ref prop, ref val} => {
                self.set_property(&obj.name, prop, *val)?;
                next
            },

            Instruction::UnaryOp{ref dst, op, ref src} => {
                self.unary_op(&dst.name, op, &src, module)?;
                next
            },

            Instruction::BinaryOp{ref dst, op, ref left, ref right} => {
                self.binary_op(&dst.name, op, &left, &right, module)?;
                next
            },

            Instruction::Call{ref dst, ref func, ref args} => {
                let func = self.get_function(func, module)?;
                let args = args.iter().cloned().collect::<Vec<_>>();
                if let Some(ref dst) = *dst {
                    self.call(Some(&dst.name), &func, &args, index, module)
                } else {
                    self.call(None, &func, &args, index, module)
                }
            },

            Instruction::Cast{ref dst, ref src} => {
                self.cast(dst, src, module)?;
                next
            },

            Instruction::IsNil{ref dst, ref obj} => {
                self.is_nil(dst, obj)?;
                next
            },

            Instruction::StoreNil(ref dst) => {
                self.update_variable(&dst.name, Value::Nil)?;
                next
            }

            Instruction::StackAlloc(ref var) => {
                self.add_variable(&var.name, Value::from_type(&var.typ)?)?;
                next
            },

            Instruction::HeapAlloc(ref var) => {
                self.update_variable(&var.name, Value::Pointer(ValueRef::new(Value::Uninitialized)))?;
                next
            },

            Instruction::GlobalAlloc(ref var) => {
                self.add_global_variable(&var.name, Value::from_type(&var.typ)?)?;
                next
            },

            Instruction::Return(ref var) => {
                self.ret(Some(var), module)
            },

            Instruction::ReturnVoid => {
                self.ret(None, module)
            },

            Instruction::Branch(bb) => {
                Ok(StepResult::Continue(index.jump(bb)))
            },

            Instruction::BranchIf{ref cond, on_true, on_false} => {
                self.branch_if(&cond, on_true, on_false, index, module)
            },

            Instruction::Delete(ref var) => {
                self.replace_variable(&var.name, ValueRef::Null)?;
                next
            },

            Instruction::Slice{ref dst, ref src, ref start, ref len} => {
                self.slice(&dst.name, &src.name, &start, &len, module)?;
                next
            },

            Instruction::StartScope => {
                self.stack.push(StackFrame::new());
                next
            },

            Instruction::EndScope => {
                self.stack.pop();
                next
            },
        }
    }


    pub fn run_function(&mut self, function: &str, module: &ByteCodeModule) -> ExecutionResult<Value>
    {
        let mut index = self.start(function, module)?;
        loop {
            if self.debug_mode {
                index.print(module);
            }

            let sr = self.step(&index, module)?;
            index = match sr {
                StepResult::Continue(new_index) => new_index,
                StepResult::Exit(return_value) => {
                    return Ok(return_value)
                }
            }
        }
    }

    pub fn start(&mut self, function: &str, module: &ByteCodeModule) -> ExecutionResult<ByteCodeIndex>
    {
        let func = module.get_function(function).ok_or_else(|| format!("Unknown function {}", function))?;
        let bottom_frame = StackFrame::new();
        self.stack.push(bottom_frame);

        self.add_variable(RETURN_VALUE, Value::Void)?;
        match self.call(Some(RETURN_VALUE), func.clone(), &Vec::new(), &ByteCodeIndex::new(module.exit_function.sig.name.clone(), 0, 0), module)?
        {
            StepResult::Continue(index) => Ok(index),
            StepResult::Exit(_) => Err("Unexpected exit".into())
        }
    }

    pub fn step(&mut self, s: &ByteCodeIndex, module: &ByteCodeModule) -> ExecutionResult<StepResult>
    {
        let func = module.get_function(&s.function)
            .ok_or_else(|| format!("Unknown function {}", s.function))?;
        let bb = func.blocks.get(&s.basic_block)
            .ok_or_else(|| format!("Function does not have basic block {}", s.basic_block))?;

        if let Some(instruction) = bb.instructions.get(s.instruction) {
            self.execute_instruction(instruction, s, module)
        } else {
            Err(format!("Instruction index {} out of bounds", s.instruction))
        }
    }
}

pub fn run_byte_code(module: &ByteCodeModule, function: &str) -> ExecutionResult<Value>
{
    let mut interpreter = Interpreter::new(false);
    interpreter.run_function(function, module)
}

pub use self::debugger::debug_byte_code;
