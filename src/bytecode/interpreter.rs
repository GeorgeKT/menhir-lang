use std::collections::HashMap;
use std::fmt;
use bytecode::*;
use parser::Operator;

const RETURN_VALUE : &'static str = "@RETURN_VALUE@";

#[derive(Debug, Eq, PartialEq)]
pub struct ExecutionError(pub String);

impl fmt::Display for ExecutionError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
struct ReturnAddress
{
    address: ByteCodeIndex,
    result_destination: String,
}

struct StackFrame
{
    vars: HashMap<String, ValueRef>,
    return_address: Option<ReturnAddress>
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
}

pub struct Interpreter
{
    stack: Vec<StackFrame>,
}

pub enum StepResult
{
    Continue(ByteCodeIndex),
    Exit(Value),
}

impl Interpreter
{
    pub fn new() -> Interpreter
    {
        Interpreter{
            stack: Vec::new(),
        }
    }

    fn add_variable(&mut self, name: &str, v: Value) -> Result<(), ExecutionError>
    {
        let mut sf = self.stack.last_mut().expect("Empty stack");
        if sf.vars.get(name).is_some() {
            Err(ExecutionError(format!("Variable {} already exists", name)))
        } else {
            sf.vars.insert(name.into(), ValueRef::new(v));
            Ok(())
        }
    }

    pub fn get_variable(&self, name: &str) -> Result<ValueRef, ExecutionError>
    {
        for sf in self.stack.iter().rev() {
            if let Some(v) = sf.vars.get(name) {
                return Ok(v.clone());
            }
        }

        Err(ExecutionError(format!("Unknown variable {}", name)))
    }

    fn apply_on_variable<Op>(&mut self, name: &str, op: Op) -> Result<(), ExecutionError>
        where Op: FnOnce(&mut ValueRef) -> Result<(), ExecutionError>
    {
        for sf in self.stack.iter_mut().rev() {
            if let Some(ref mut v) = sf.vars.get_mut(name) {
                return op(v)
            }
        }

        Err(ExecutionError(format!("Unknown variable {}", name)))
    }

    fn update_variable(&mut self, name: &str, new_value: Value) -> Result<(), ExecutionError>
    {
        self.replace_variable(name, ValueRef::new(new_value))
    }

    fn replace_variable(&mut self, name: &str, vr: ValueRef) -> Result<(), ExecutionError>
    {
        self.apply_on_variable(name, |v: &mut ValueRef| {
            *v = vr;
            Ok(())
        })
    }

    fn store(&mut self, name: &str, val: Value) -> Result<(), ExecutionError>
    {
        self.apply_on_variable(name, |vr: &mut ValueRef| {
            vr.apply_mut(|v: &mut Value| {
                *v = val;
                Ok(())
            })
        })
    }

    fn unary_op(&mut self, dst: &str, op: Operator, var: &str) -> Result<(), ExecutionError>
    {
        let val = self.get_variable(var)?;
        let result = val.apply(|v: &Value| {
            match (op, v)
            {
                (Operator::Sub, &Value::Int(num)) => Ok(Value::Int(-num)),
                (Operator::Sub, &Value::UInt(num)) => Ok(Value::Int(-(num as i64))),
                (Operator::Sub, &Value::Float(num)) => Ok(Value::Float(-num)),
                (Operator::Not, &Value::Bool(b)) => Ok(Value::Bool(!b)),
                _ => Err(ExecutionError(format!("Invalid unary op"))),
            }
        })?;

        self.update_variable(dst, result)?;
        Ok(())
    }

    fn binary_op(&mut self, dst: &str, op: Operator, left: &str, right: &str) -> Result<(), ExecutionError>
    {
        let left = self.get_variable(left)?.clone_value()?;
        let right = self.get_variable(right)?.clone_value()?;

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

            _ => return Err(ExecutionError(format!("Operator {} not supported on these operands", op))),
        };


        self.update_variable(dst, value)?;
        Ok(())
    }

    fn call(&mut self, dst: &str, name: &str, args: &Vec<String>, index: &ByteCodeIndex, module: &ByteCodeModule) -> Result<StepResult, ExecutionError>
    {
        let func = module.functions.get(name).ok_or(ExecutionError(format!("Unknown function {}", name)))?;
        println!("{}:", func.sig.name);

        let return_address = index.next();
        self.stack.push(StackFrame::with_return_address(return_address, dst.into()));

        for (idx, arg) in args.iter().enumerate() {
            let arg_value = self.get_variable(arg)?.clone_value()?;
            self.add_variable(&func.sig.args[idx].name, arg_value)?;
        }

        Ok(StepResult::Continue(ByteCodeIndex::new(func.clone(), 0, 0)))
    }

    fn pop_until_end_of_function(&mut self) -> Result<ReturnAddress, ExecutionError>
    {
        while let Some(sf) = self.stack.pop() {
            if let Some(return_address) = sf.return_address {
                return Ok(return_address.clone());
            }
        }

        Err(ExecutionError("Reached bottom of the stack".into()))
    }

    fn ret(&mut self, name: Option<&str>) -> Result<StepResult, ExecutionError>
    {
        let return_address = match name
        {
            Some(name) => {
                let result = self.get_variable(name)?.clone();
                let return_address = self.pop_until_end_of_function()?;
                self.replace_variable(&return_address.result_destination, result)?;
                return_address
            }

            None => {
                self.pop_until_end_of_function()?
            }
        };

        Ok(StepResult::Continue(return_address.address))
    }

    fn branch_if(&self, var: &str, on_true: BasicBlockRef, on_false: BasicBlockRef, index: &ByteCodeIndex) -> Result<StepResult, ExecutionError>
    {
        let val = self.get_variable(var)?;
        val.apply(|v: &Value| {
            match v
            {
                &Value::Bool(true) => Ok(StepResult::Continue(index.jump(on_true))),
                &Value::Bool(false) => Ok(StepResult::Continue(index.jump(on_false))),
                _ => Err(ExecutionError(format!("brif operand {} is not a boolean", var))),
            }
        })
    }

    fn load_member(&mut self, dst: &str, obj: &str, member_index: usize) -> Result<(), ExecutionError>
    {
        let obj = self.get_variable(obj)?;
        let vr = obj.apply(|value: &Value| {
            match *value
            {
                Value::Array(ref arr) => {
                    if member_index < arr.len() {
                        Ok(arr[member_index].to_ptr())
                    } else {
                        Err(ExecutionError(format!("Array index out of bounds")))
                    }
                },

                Value::Slice(ref slice) => {
                    if member_index < slice.len() {
                        Ok(slice[member_index].to_ptr())
                    } else {
                        Err(ExecutionError(format!("Slice index out of bounds")))
                    }
                },

                _ => Err(ExecutionError(format!("Load member not supported on {}", value)))
            }
        })?;

        self.replace_variable(dst, vr)?;
        Ok(())
    }

    fn get_property(&mut self, dst: &str, obj: &str, prop: ByteCodeProperty) -> Result<(), ExecutionError>
    {
        let obj = self.get_variable(obj)?;
        let val = obj.apply(|vr: &Value| vr.get_property(prop))?;
        self.update_variable(dst, val)?;
        Ok(())
    }

    fn get_int(&self, name: &str) -> Result<i64, ExecutionError>
    {
        self.get_variable(name)?
            .apply(|vr: &Value| {
                if let &Value::Int(v) = vr {
                    Ok(v)
                } else {
                    Err(ExecutionError(format!("{} is not an integer", name)))
                }
            })
    }

    fn slice(&mut self, dst: &str, array: &str, start: &str, len: &str) -> Result<(), ExecutionError>
    {
        let start_value = self.get_int(start)? as usize;
        let len_value = self.get_int(len)? as usize;
        let slice = self.get_variable(array)?.apply(|vr: &Value| {
            match *vr
            {
                Value::Array(ref arr) => {
                    if start_value + len_value > arr.len() {
                        return Err(ExecutionError(format!("Slice index out of bounds")));
                    }

                    let mut slice = Vec::new();
                    for element in &arr[start_value .. (start_value + len_value)] {
                        slice.push(element.to_ptr());
                    }
                    Ok(Value::Slice(slice))
                },

                Value::Slice(ref slice) => {
                    if start_value + len_value > slice.len() {
                        return Err(ExecutionError(format!("Slice index out of bounds")));
                    }

                    let subslice = &slice[start_value .. (start_value + len_value)];
                    Ok(Value::Slice(subslice.iter().map(|e| e.clone()).collect()))
                },

                _ => {
                    Err(ExecutionError(format!("{} is not an array or slice", array)))
                },
            }
        })?;

        self.update_variable(dst, slice)?;
        Ok(())
    }

    fn execute_instruction(&mut self, instr: &Instruction, index: &ByteCodeIndex, module: &ByteCodeModule) -> Result<StepResult, ExecutionError>
    {
        match *instr
        {
            Instruction::Exit => {
                let return_value = self.get_variable(RETURN_VALUE)?.clone_value()?;
                Ok(StepResult::Exit(return_value))
            },

            Instruction::Store{ref dst, ref src} => {
                let new_value = self.get_variable(&src.name)?.clone_value()?;
                self.store(&dst.name, new_value)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::StoreLit{ref dst, ref lit} => {
                let v = Value::from_literal(lit)?;
                self.store(&dst.name, v)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::StoreFunc{ref dst, ref func} => {
                self.store(&dst.name, Value::Func(func.clone()))?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::LoadMember{ref dst, ref obj, member_index} => {
                self.load_member(&dst.name, &obj.name, member_index)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::GetProperty{ref dst, ref obj, ref prop} => {
                self.get_property(&dst.name, &obj.name, *prop)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::SetProperty{ref dst, ref prop, ref val} => {
                panic!("  setp {} {} {}", dst, prop, val)
            },

            Instruction::UnaryOp{ref dst, op, ref src} => {
                self.unary_op(&dst.name, op, &src.name)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::BinaryOp{ref dst, op, ref left, ref right} => {
                self.binary_op(&dst.name, op, &left.name, &right.name)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::Call{ref dst, ref func, ref args} => {
                self.call(&dst.name, func, &args.iter().map(|a| a.name.clone()).collect(), index, module)
            },

            Instruction::StackAlloc(ref var) => {
                self.add_variable(&var.name, Value::from_type(&var.typ)?)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::HeapAlloc(ref var) => {
                self.update_variable(&var.name, Value::Pointer(ValueRef::new(Value::Uninitialized)))?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::Return(ref var) => {
                self.ret(Some(&var.name))
            },

            Instruction::ReturnVoid => {
                self.ret(None)
            },

            Instruction::Branch(bb) => {
                Ok(StepResult::Continue(index.jump(bb)))
            },

            Instruction::BranchIf{ref cond, on_true, on_false} => {
                self.branch_if(&cond.name, on_true, on_false, index)
            },

            Instruction::Delete(ref var) => {
                self.replace_variable(&var.name, ValueRef::Null)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::Slice{ref dst, ref src, ref start, ref len} => {
                self.slice(&dst.name, &src.name, &start.name, &len.name)?;
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::StartScope => {
                self.stack.push(StackFrame::new());
                Ok(StepResult::Continue(index.next()))
            },

            Instruction::EndScope => {
                self.stack.pop();
                Ok(StepResult::Continue(index.next()))
            },
        }
    }


    pub fn run_function(&mut self, function: &str, args: Vec<Value>, module: &ByteCodeModule) -> Result<Value, ExecutionError>
    {
        let mut index = self.start(function, args, module)?;
        loop {
            print!("{}", index);
            let sr = self.step(&index, module)?;
            index = match sr {
                StepResult::Continue(new_index) => new_index,
                StepResult::Exit(return_value) => {
                    return Ok(return_value)
                }
            }
        }
    }

    pub fn start(&mut self, function: &str, args: Vec<Value>, module: &ByteCodeModule) -> Result<ByteCodeIndex, ExecutionError>
    {
        let bottom_frame = StackFrame::new();
        self.stack.push(bottom_frame);

        let mut call_args = Vec::new();
        for arg in &args {
            let name = format!("arg0");
            self.add_variable(&name, arg.clone())?;
            call_args.push(name);
        }

        self.add_variable(RETURN_VALUE, Value::Void)?;
        match self.call(RETURN_VALUE, function, &call_args, &ByteCodeIndex::new(module.exit_function.clone(), 0, 0), module)?
        {
            StepResult::Continue(index) => Ok(index),
            StepResult::Exit(_) => Err(ExecutionError("Unexpected exit".into()))
        }
    }

    pub fn step(&mut self, s: &ByteCodeIndex, module: &ByteCodeModule) -> Result<StepResult, ExecutionError>
    {
        let bb = s.function.blocks.get(&s.basic_block)
            .ok_or(ExecutionError(format!("Function does not have basic block {}", s.basic_block)))?;

        if let Some(ref instruction) = bb.instructions.get(s.instruction) {
            self.execute_instruction(instruction, s, module)
        } else {
            Err(ExecutionError(format!("Instruction index out of bounds")))
        }
    }
}

pub fn run_byte_code(module: &ByteCodeModule, function: &str) -> Result<Value, ExecutionError>
{
    let mut interpreter = Interpreter::new();
    interpreter.run_function(function, vec![], module)
}
