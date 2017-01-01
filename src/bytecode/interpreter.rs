use std::fmt;
use std::rc::{Weak, Rc};
use std::cell::RefCell;
use std::collections::HashMap;
use itertools::free::join;
use ast::Type;
use bytecode::*;
use parser::Operator;

#[derive(Debug, Eq, PartialEq)]
pub struct ExecutionError(pub String);

#[derive(Debug, Clone)]
pub enum ValueRef
{
    Owner(Rc<RefCell<Value>>),
    Ptr(Weak<RefCell<Value>>),
    Null,
}
impl ValueRef
{
    fn new(v: Value) -> ValueRef
    {
        ValueRef::Owner(Rc::new(RefCell::new(v)))
    }

    fn clone_value(&self) -> Result<Value, ExecutionError>
    {
        match *self
        {
            ValueRef::Owner(ref v) => Ok(v.borrow().clone()),
            ValueRef::Ptr(ref v) => {
                if let Some(rv) = v.upgrade() {
                    Ok(rv.borrow().clone())
                } else {
                    Err(ExecutionError(format!("Dangling pointer, owner of element pointed to is gone")))
                }
            }
            ValueRef::Null => {
                Err(ExecutionError(format!("Dangling pointer, pointer has been deleted")))
            }
        }
    }

    fn to_ptr(&self) -> ValueRef
    {
        if let ValueRef::Owner(ref v) = *self {
            ValueRef::Ptr(Rc::downgrade(v))
        } else {
            self.clone()
        }
    }

    fn apply<Op, R>(&self, op: Op) -> Result<R, ExecutionError>
        where Op: Fn(&Value) -> Result<R, ExecutionError>, R: Sized
    {
        match *self
        {
            ValueRef::Owner(ref v) => {
                op(&v.borrow())
            },
            ValueRef::Ptr(ref v) => {
                if let Some(rv) = v.upgrade() {
                    op(&rv.borrow())
                } else {
                    Err(ExecutionError(format!("Dangling pointer, owner of element pointed to is gone")))
                }
            }
            ValueRef::Null => {
                Err(ExecutionError(format!("Dangling pointer, pointer has been deleted")))
            }
        }
    }
}

impl fmt::Display for ValueRef
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            ValueRef::Owner(ref v) => {
                let inner = v.borrow();
                write!(f, "{}", *inner)
            },

            ValueRef::Ptr(ref v) => {
                if let Some(rv) = v.upgrade() {
                    let inner = rv.borrow();
                    write!(f, "{}", *inner)
                } else {
                    println!("Danging pointer access");
                    Err(fmt::Error)
                }
            },

            ValueRef::Null => {
                write!(f, "null")
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value
{
    Uninitialized,
    Void,
    Int(i64),
    UInt(u64),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Array(Vec<ValueRef>),
    Slice(Vec<ValueRef>),
    Func(String),
    Struct(Vec<ValueRef>),
    Sum(usize, Box<ValueRef>),
    Enum(usize),
    Pointer(ValueRef),
}

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        match *self
        {
            Value::Uninitialized => write!(f, "uninitialized"),
            Value::Void => write!(f, "void"),
            Value::Int(v) => write!(f, "int {}", v),
            Value::UInt(v) => write!(f, "uint {}", v),
            Value::Float(v) => write!(f, "float {}", v),
            Value::Char(v) => write!(f, "char {}", v),
            Value::Bool(v) => write!(f, "bool {}", v),
            Value::Array(ref v) => write!(f, "[{}]", join(v.iter(), ", ")),
            Value::String(ref s) => write!(f, "\"{}\"", s),
            Value::Slice(ref v) => write!(f, "[{}]", join(v.iter(), ", ")),
            Value::Func(ref v) => write!(f, "func {}", v),
            Value::Struct(ref v) => write!(f, "{{{}}}", join(v.iter(), ", ")),
            Value::Sum(idx, ref v) => write!(f, "sum {} {}", idx, v),
            Value::Enum(idx) => write!(f, "enum {}", idx),
            Value::Pointer(ref v) => write!(f, "pointer {}", v),
        }
    }
}

impl Value
{
    pub fn to_exit_code(&self) -> i32
    {
        match *self
        {
            Value::Int(v) => v as i32,
            Value::Bool(v) => if v == true {0i32} else {1i32},
            _ => 1i32,
        }
    }

    pub fn from_literal(lit: &ByteCodeLiteral) -> Result<Value, ExecutionError>
    {
        match lit
        {
            &ByteCodeLiteral::Int(v) => Ok(Value::Int(v as i64)),
            &ByteCodeLiteral::Float(ref num) => {
                match num.parse::<f64>()
                {
                    Ok(f) => Ok(Value::Float(f)),
                    Err(_) => Err(ExecutionError(format!("{} is not a valid floating point number", num))),
                }
            },
            &ByteCodeLiteral::Char(v) => Ok(Value::Char(v as char)),
            &ByteCodeLiteral::String(ref s) => Ok(Value::String(s.clone())),
            &ByteCodeLiteral::Bool(v) => Ok(Value::Bool(v)),
        }
    }

    pub fn from_type(typ: &Type) -> Result<Value, ExecutionError>
    {
        match *typ
        {
            Type::Unknown | Type::Unresolved(_) | Type::Generic(_) => panic!("Types must be known before the interpreter can run"),
            Type::Void => Ok(Value::Void),
            Type::Int => Ok(Value::Int(0)),
            Type::UInt => Ok(Value::UInt(0)),
            Type::Float => Ok(Value::Float(0.0)),
            Type::Char => Ok(Value::Char('x')),
            Type::Bool => Ok(Value::Bool(false)),
            Type::String => Ok(Value::String(String::default())),
            Type::Array(ref at) => {
                let mut array = Vec::with_capacity(at.len);
                for _ in 0..at.len {
                    let element = Value::from_type(&at.element_type)?;
                    array.push(ValueRef::new(element));
                }
                Ok(Value::Array(array))
            },
            Type::Slice(_) => Ok(Value::Slice(Vec::new())),
            Type::Func(_) => Ok(Value::Func(String::new())),
            Type::Struct(ref st) => {
                let mut members = Vec::new();
                for m in &st.members {
                    let member = Value::from_type(&m.typ)?;
                    members.push(ValueRef::new(member));
                }
                Ok(Value::Struct(members))
            },
            Type::Enum(_) => Ok(Value::Enum(0)),
            Type::Sum(ref st) => {
                let first_case = Value::from_type(&st.cases[0].typ)?;
                Ok(Value::Sum(0, Box::new(ValueRef::new(first_case))))
            },
            Type::Pointer(ref inner) => {
                let inner = Value::from_type(inner)?;
                Ok(Value::Pointer(ValueRef::new(inner)))
            },

        }
    }

    fn get_property(&self, prop: ByteCodeProperty) -> Result<Value, ExecutionError>
    {
        match (self, prop)
        {
            (&Value::Array(ref a), ByteCodeProperty::Len) => Ok(Value::Int(a.len() as i64)),
            (&Value::Slice(ref a), ByteCodeProperty::Len) => Ok(Value::Int(a.len() as i64)),
            (&Value::Sum(idx, _), ByteCodeProperty::SumTypeIndex) => Ok(Value::Int(idx as i64)),
            (&Value::Enum(idx), ByteCodeProperty::SumTypeIndex) => Ok(Value::Int(idx as i64)),
            _  => Err(ExecutionError(format!("Unknown property {}", prop))),
        }
    }
}

struct StackFrame
{
    vars: HashMap<String, ValueRef>,
    result: ValueRef,
}

impl StackFrame
{
    pub fn new() -> StackFrame
    {
        StackFrame{
            vars: HashMap::new(),
            result: ValueRef::new(Value::Void),
        }
    }
}

struct Interpreter<'a>
{
    module: &'a ByteCodeModule,
    stack: Vec<StackFrame>,
}

enum Action
{
    Continue,
    Jump(BasicBlockRef),
    Return,
}

impl<'a> Interpreter<'a>
{
    pub fn new(module: &ByteCodeModule) -> Interpreter
    {
        Interpreter{
            module: module,
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

    fn get_variable(&self, name: &str) -> Result<ValueRef, ExecutionError>
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

    fn unary_op(&mut self, dst: &str, op: Operator, var: &str) -> Result<Action, ExecutionError>
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
        Ok(Action::Continue)
    }

    fn binary_op(&mut self, dst: &str, op: Operator, left: &str, right: &str) -> Result<Action, ExecutionError>
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
        Ok(Action::Continue)
    }

    fn call(&mut self, dst: &str, name: &str, args: &Vec<Var>) -> Result<Action, ExecutionError>
    {
        let mut function_args = Vec::new();
        for arg in args {
            let arg_value = self.get_variable(&arg.name)?.clone_value()?;
            function_args.push(arg_value);
        }

        let result = self.run_function(name, function_args)?;
        self.replace_variable(dst, result)?;
        Ok(Action::Continue)
    }

    fn ret(&mut self, name: &str) -> Result<Action, ExecutionError>
    {
        let val = self.get_variable(name)?.clone();
        let mut sf = self.stack.last_mut().expect("Empty stack");
        sf.result = val;
        Ok(Action::Return)
    }

    fn branch_if(&self, var: &str, on_true: BasicBlockRef, on_false: BasicBlockRef) ->  Result<Action, ExecutionError>
    {
        let val = self.get_variable(var)?;
        val.apply(|v: &Value| {
            match v
            {
                &Value::Bool(true) => Ok(Action::Jump(on_true)),
                &Value::Bool(false) => Ok(Action::Jump(on_false)),
                _ => Err(ExecutionError(format!("brif operand {} is not a boolean", var))),
            }
        })
    }

    fn store(&mut self, dst: &str, src: &str) -> Result<Action, ExecutionError>
    {
        let new_value = self.get_variable(src)?.clone_value()?;
        self.apply_on_variable(dst, |v: &mut ValueRef| {
            match *v
            {
                ValueRef::Owner(ref mut inner) => {
                    let mut inner = inner.borrow_mut();
                    *inner = new_value;
                    Ok(())
                }

                ValueRef::Ptr(ref mut inner) => {
                    if let Some(rv) = inner.upgrade() {
                        let mut inner = rv.borrow_mut();
                        *inner = new_value;
                        Ok(())
                    } else {
                        Err(ExecutionError(format!("Store on dangling pointer")))
                    }
                },

                ValueRef::Null => Err(ExecutionError(format!("Store on deleted pointer"))),
            }
        })?;

        Ok(Action::Continue)
    }

    fn load_member(&mut self, dst: &str, obj: &str, member_index: usize) -> Result<Action, ExecutionError>
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
        Ok(Action::Continue)
    }

    fn get_property(&mut self, dst: &str, obj: &str, prop: ByteCodeProperty) -> Result<Action, ExecutionError>
    {
        let obj = self.get_variable(obj)?;
        let val = obj.apply(|vr: &Value| vr.get_property(prop))?;
        self.update_variable(dst, val)?;
        Ok(Action::Continue)
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

    fn slice(&mut self, dst: &str, array: &str, start: &str, len: &str) -> Result<Action, ExecutionError>
    {
        let start_value = self.get_int(start)? as usize;
        let len_value = self.get_int(len)? as usize;
        println!("> slice {} {} {} {}", dst, array, start_value, len_value);
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
        Ok(Action::Continue)
    }

    fn execute_instruction(&mut self, instr: &Instruction) -> Result<Action, ExecutionError>
    {
        print!("{}", instr);
        match *instr
        {
            Instruction::Store{ref dst, ref src} => {
                self.store(&dst.name, &src.name)
            },

            Instruction::StoreLit{ref dst, ref lit} => {
                let v = Value::from_literal(lit)?;
                self.update_variable(&dst.name, v)?;
                Ok(Action::Continue)
            },

            Instruction::StoreFunc{ref dst, ref func} => {
                self.update_variable(&dst.name, Value::Func(func.clone()))?;
                Ok(Action::Continue)
            },

            Instruction::LoadMember{ref dst, ref obj, member_index} => {
                self.load_member(&dst.name, &obj.name, member_index)
            },

            Instruction::GetProperty{ref dst, ref obj, ref prop} => {
                self.get_property(&dst.name, &obj.name, *prop)
            },

            Instruction::SetProperty{ref dst, ref prop, ref val} => {
                panic!("  setp {} {} {}", dst, prop, val)
            },

            Instruction::UnaryOp{ref dst, op, ref src} => {
                self.unary_op(&dst.name, op, &src.name)
            },

            Instruction::BinaryOp{ref dst, op, ref left, ref right} => {
                self.binary_op(&dst.name, op, &left.name, &right.name)
            },

            Instruction::Call{ref dst, ref func, ref args} => {
                self.call(&dst.name, func, args)
            },

            Instruction::StackAlloc(ref var) => {
                self.add_variable(&var.name, Value::from_type(&var.typ)?)?;
                Ok(Action::Continue)
            },

            Instruction::HeapAlloc(ref var) => {
                self.update_variable(&var.name, Value::Pointer(ValueRef::new(Value::Uninitialized)))?;
                Ok(Action::Continue)
            },

            Instruction::Return(ref var) => {
                self.ret(&var.name)
            },

            Instruction::ReturnVoid => {
                Ok(Action::Return)
            },

            Instruction::Branch(bb) => {
                Ok(Action::Jump(bb))
            },

            Instruction::BranchIf{ref cond, on_true, on_false} => {
                self.branch_if(&cond.name, on_true, on_false)
            },

            Instruction::Delete(ref var) => {
                self.replace_variable(&var.name, ValueRef::Null)?;
                Ok(Action::Continue)
            },

            Instruction::Slice{ref dst, ref src, ref start, ref len} => {
                self.slice(&dst.name, &src.name, &start.name, &len.name)
            },

            Instruction::StartScope => {
                self.stack.push(StackFrame::new());
                Ok(Action::Continue)
            },

            Instruction::EndScope => {
                self.stack.pop();
                Ok(Action::Continue)
            },
        }
    }

    fn run_bb(&mut self, func: &ByteCodeFunction, bb_ref: BasicBlockRef) -> Result<Action, ExecutionError>
    {
        if let Some(ref bb) = func.blocks.get(&bb_ref) {
            for inst in &bb.instructions {
                let action = self.execute_instruction(inst)?;
                if let Action::Continue = action {
                    continue;
                } else {
                    return Ok(action);
                }
            }

            Ok(Action::Continue)
        } else {
            Err(ExecutionError(format!("Cannot execute basic block {} of function {}", bb_ref, func.sig.name)))
        }
    }

    fn run(&mut self, func: &ByteCodeFunction, args: Vec<Value>) -> Result<ValueRef, ExecutionError>
    {
        println!("{}:", func.sig.name);
        self.stack.push(StackFrame::new());
        for (idx, val) in args.into_iter().enumerate() {
            self.add_variable(&func.sig.args[idx].name, val)?;
        }

        let mut bb_ref = 0;
        loop {
            match self.run_bb(func, bb_ref)?
            {
                Action::Continue => {bb_ref += 1;},
                Action::Jump(bb) => {bb_ref = bb},
                Action::Return => break,
            }
        }

        let sf = self.stack.pop().expect("Empty stack");
        println!("end");
        Ok(sf.result.clone())
    }

    pub fn run_function(&mut self, function: &str, args: Vec<Value>) -> Result<ValueRef, ExecutionError>
    {
        match self.module.functions.get(function)
        {
            Some(ref func) => {
                self.run(func, args)
            },

            None => {
                Err(ExecutionError(format!("Unknown function {}", function)))
            }
        }
    }
}

pub fn run_byte_code(module: &ByteCodeModule, function: &str) -> Result<Value, ExecutionError>
{
    let mut interpreter = Interpreter::new(module);
    let vr = interpreter.run_function(function, vec![])?;
    vr.clone_value()
}
