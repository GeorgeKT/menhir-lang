use std::collections::HashMap;
use std::rc::Rc;
use std::ops::Deref;
use bytecode::*;
use ast::Operator;
use super::function::*;
use super::instruction::*;
use super::value::Value;
use super::valueref::ValueRef;
use super::debugger::ByteCodeIndex;

const RETURN_VALUE : &'static str = "@RETURN_VALUE@";


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

    pub fn has_return_address(&self) -> bool
    {
        self.return_address.is_some()
    }

    pub fn add(&mut self, name: &str, v: Value) -> Result<(), ExecutionError>
    {
        if self.vars.get(name).is_some() {
            Err(ExecutionError(format!("Variable {} already exists", name)))
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
            globals: StackFrame::new(),
        }
    }


    fn add_variable(&mut self, name: &str, v: Value) -> Result<(), ExecutionError>
    {
        let mut sf = self.stack.last_mut().expect("Empty stack");
        sf.add(name, v)
    }

    fn add_global_variable(&mut self, name: &str, v: Value) -> Result<(), ExecutionError>
    {
        self.globals.add(name, v)
    }

    pub fn get_variable(&self, name: &str) -> Result<ValueRef, ExecutionError>
    {
        for sf in self.stack.iter().rev() {
            if let Some(v) = sf.vars.get(name) {
                return Ok(v.clone());
            }

            if sf.has_return_address() {
                break;
            }
        }

        self.globals.vars.get(name).cloned().ok_or_else(|| ExecutionError(format!("Unknown variable {}", name)))
    }

    fn apply_on_variable<Op>(&mut self, name: &str, op: Op) -> Result<(), ExecutionError>
        where Op: FnOnce(&mut ValueRef) -> Result<(), ExecutionError>
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
                match *v
                {
                    Value::Optional(ref mut inner) => *inner = Box::new(val),
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
                _ => Err(ExecutionError(format!("Invalid unary op {}", op))),
            }
        })?;

        self.update_variable(dst, result)?;
        Ok(())
    }

    #[cfg_attr(feature = "cargo-clippy", allow(float_cmp, match_same_arms))]
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
            (Operator::Equals, Value::Optional(ref inner), Value::Nil) => Value::Bool(inner.is_nil()),
            (Operator::Equals, Value::Nil, Value::Nil) => Value::Bool(true),
            (Operator::Equals, _, Value::Nil) |
            (Operator::Equals, Value::Nil, _) => Value::Bool(false),

            (Operator::NotEquals, Value::Int(l), Value::Int(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::UInt(l), Value::UInt(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::Float(l), Value::Float(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::Char(l), Value::Char(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::Bool(l), Value::Bool(r)) => Value::Bool(l != r),
            (Operator::NotEquals, Value::String(ref l), Value::String(ref r)) => Value::Bool(*l != *r),
            (Operator::NotEquals, Value::Optional(ref inner), Value::Nil) => Value::Bool(!inner.is_nil()),
            (Operator::NotEquals, Value::Nil, Value::Nil) => Value::Bool(false),
            (Operator::NotEquals, _, Value::Nil) |
            (Operator::NotEquals, Value::Nil, _) => Value::Bool(true),

            (Operator::And, Value::Bool(l), Value::Bool(r)) => Value::Bool(l && r),
            (Operator::Or, Value::Bool(l), Value::Bool(r)) => Value::Bool(l || r),

            (Operator::Or, Value::Optional(inner), right) => {
                if inner.is_nil() {
                    right
                } else {
                    inner.deref().clone()
                }
            }

            _ => return Err(ExecutionError(format!("Operator {} not supported on these operands", op))),
        };


        self.update_variable(dst, value)?;
        Ok(())
    }

    fn call(&mut self, dst: &str, func: Rc<ByteCodeFunction>, args: &[String], index: &ByteCodeIndex) -> Result<StepResult, ExecutionError>
    {
        println!("{}:", func.sig.name);
        let return_address = index.next();

        let mut arg_values = Vec::new();
        for arg in args {
            let arg_value = self.get_variable(arg)?.clone_value()?;
            arg_values.push(arg_value);
        }

        self.stack.push(StackFrame::with_return_address(return_address, dst.into()));
        for (idx, arg_value) in arg_values.into_iter().enumerate() {
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
            match *v
            {
                Value::Bool(true) => Ok(StepResult::Continue(index.jump(on_true))),
                Value::Bool(false) => Ok(StepResult::Continue(index.jump(on_false))),
                _ => Err(ExecutionError(format!("brif operand {} is not a boolean", var))),
            }
        })
    }

    fn load_member(&mut self, dst: &str, obj: &str, member_index: &Operand) -> Result<(), ExecutionError>
    {
        let obj = self.get_variable(obj)?;
        let index = match *member_index
        {
            Operand::Const(ByteCodeLiteral::Int(index)) => index as usize,
            Operand::Var(ref v) =>
                if let Value::Int(v) = self.get_variable(&v.name)?.clone_value()? {
                    v as usize
                } else {
                    return Err(ExecutionError("load member instruction with non integer index value".into()));
                },
            _ => return Err(ExecutionError("load member instruction with non integer index value".into())),
        };

        let vr = obj.apply(|value: &Value| value.get_member_ptr(index as usize))?;
        self.replace_variable(dst, ValueRef::new(vr))?;
        Ok(())
    }

    fn get_property(&mut self, dst: &str, obj: &str, prop: ByteCodeProperty) -> Result<(), ExecutionError>
    {
        let obj = self.get_variable(obj)?;
        let val = obj.apply(|vr: &Value| vr.get_property(prop))?;
        self.update_variable(dst, val)?;
        Ok(())
    }

    fn set_property(&mut self, obj: &str, prop: &ByteCodeProperty, val: usize) -> Result<(), ExecutionError>
    {
        self.apply_on_variable(obj, |vr: &mut ValueRef|
            vr.apply_mut(|v: &mut Value|
                match (prop, v) {
                    (&ByteCodeProperty::SumTypeIndex, &mut Value::Enum(ref mut idx)) |
                    (&ByteCodeProperty::SumTypeIndex, &mut Value::Sum(ref mut idx, _)) => {
                        *idx = val;
                        Ok(())
                    },
                    _ => Err(ExecutionError(format!("Setting property {} not support on {}", prop, obj)))
                }
            )
        )
    }

    fn get_int(&self, name: &str) -> Result<i64, ExecutionError>
    {
        self.get_variable(name)?
            .apply(|vr: &Value| {
                if let Value::Int(v) = *vr {
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
                        return Err(ExecutionError(format!("Slice index {} out of bounds", start_value + len_value)));
                    }

                    let mut slice = Vec::new();
                    for element in &arr[start_value .. (start_value + len_value)] {
                        slice.push(element.to_ptr());
                    }
                    Ok(Value::Slice(slice))
                },

                Value::Slice(ref slice) => {
                    if start_value + len_value > slice.len() {
                        return Err(ExecutionError(format!("Slice index {} out of bounds", start_value + len_value)));
                    }

                    let subslice = &slice[start_value .. (start_value + len_value)];
                    Ok(Value::Slice(subslice.iter().cloned().collect()))
                },

                _ => {
                    Err(ExecutionError(format!("{} is not an array or slice", array)))
                },
            }
        })?;

        self.update_variable(dst, slice)?;
        Ok(())
    }

    fn get_function(&self, func: &str, module: &ByteCodeModule) -> Result<Rc<ByteCodeFunction>, ExecutionError>
    {
        match module.functions.get(func) {
            Some(f) => Ok(f.clone()),
            None => {
                let v = self.get_variable(func)?;
                v.apply(|val: &Value|
                    if let Value::Func(ref f) = *val {
                        Ok(f.clone())
                    } else {
                        Err(ExecutionError(format!("{} is not callable", func)))
                    }
                )
            }
        }
    }

    fn load(&mut self, dst: &str, ptr: &str) -> Result<(), ExecutionError>
    {
        let ptr = self.get_variable(ptr)?;
        let new_value = ptr.apply(|v: &Value| {
            match *v
            {
                Value::Optional(ref inner) => Ok(inner.deref().clone()),
                Value::Pointer(ref inner) => inner.clone_value(),
                _ => Err(ExecutionError(format!("Load can only be performed on pointers and optionals, not on {}", v))),
            }
        })?;

        self.update_variable(dst, new_value)
    }

    fn execute_instruction(&mut self, instr: &Instruction, index: &ByteCodeIndex, module: &ByteCodeModule) -> Result<StepResult, ExecutionError>
    {
        let next = Ok(StepResult::Continue(index.next()));
        match *instr
        {
            Instruction::Exit => {
                let return_value = self.get_variable(RETURN_VALUE)?.clone_value()?;
                Ok(StepResult::Exit(return_value))
            },

            Instruction::Store{ref dst, ref src} => {
                let new_value = self.get_variable(&src.name)?.clone_value()?;
                self.store(&dst.name, new_value)?;
                next
            },

            Instruction::StoreLit{ref dst, ref lit} => {
                let v = Value::from_literal(lit)?;
                self.store(&dst.name, v)?;
                next
            },

            Instruction::StoreFunc{ref dst, ref func} => {
                let func = self.get_function(func, module)?;
                self.store(&dst.name, Value::Func(func))?;
                next
            },

            Instruction::Load{ref dst, ref ptr} => {
                self.load(&dst.name, &ptr.name)?;
                next
            },

            Instruction::LoadMember{ref dst, ref obj, ref member_index} => {
                self.load_member(&dst.name, &obj.name, &member_index)?;
                next
            },

            Instruction::AddressOf{ref dst, ref obj} => {
                let ptr = self.get_variable(&obj.name)?.to_ptr();
                self.replace_variable(&dst.name, ptr)?;
                next
            },

            Instruction::GetProperty{ref dst, ref obj, ref prop} => {
                self.get_property(&dst.name, &obj.name, *prop)?;
                next
            },

            Instruction::SetProperty{ref obj, ref prop, ref val} => {
                self.set_property(&obj.name, prop, *val)?;
                next
            },

            Instruction::UnaryOp{ref dst, op, ref src} => {
                self.unary_op(&dst.name, op, &src.name)?;
                next
            },

            Instruction::BinaryOp{ref dst, op, ref left, ref right} => {
                self.binary_op(&dst.name, op, &left.name, &right.name)?;
                next
            },

            Instruction::Call{ref dst, ref func, ref args} => {
                let func = self.get_function(func, module)?;
                let args = args.iter().map(|a| a.name.clone()).collect::<Vec<_>>();
                self.call(&dst.name, func, &args, index)
            },

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
                next
            },

            Instruction::Slice{ref dst, ref src, ref start, ref len} => {
                self.slice(&dst.name, &src.name, &start.name, &len.name)?;
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
        let func = module.functions.get(function).ok_or_else(|| ExecutionError(format!("Unknown function {}", function)))?;
        let bottom_frame = StackFrame::new();
        self.stack.push(bottom_frame);

        let mut call_args = Vec::new();
        for arg in &args {
            let name = "arg0".to_string();
            self.add_variable(&name, arg.clone())?;
            call_args.push(name);
        }

        self.add_variable(RETURN_VALUE, Value::Void)?;
        match self.call(RETURN_VALUE, func.clone(), &call_args, &ByteCodeIndex::new(module.exit_function.clone(), 0, 0))?
        {
            StepResult::Continue(index) => Ok(index),
            StepResult::Exit(_) => Err(ExecutionError("Unexpected exit".into()))
        }
    }

    pub fn step(&mut self, s: &ByteCodeIndex, module: &ByteCodeModule) -> Result<StepResult, ExecutionError>
    {
        let bb = s.function.blocks.get(&s.basic_block)
            .ok_or_else(|| ExecutionError(format!("Function does not have basic block {}", s.basic_block)))?;

        if let Some(instruction) = bb.instructions.get(s.instruction) {
            self.execute_instruction(instruction, s, module)
        } else {
            Err(ExecutionError(format!("Instruction index {} out of bounds", s.instruction)))
        }
    }
}

pub fn run_byte_code(module: &ByteCodeModule, function: &str) -> Result<Value, ExecutionError>
{
    let mut interpreter = Interpreter::new();
    interpreter.run_function(function, vec![], module)
}
