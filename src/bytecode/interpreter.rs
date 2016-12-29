use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use itertools::free::join;
use bytecode::*;
use parser::Operator;

#[derive(Debug)]
pub struct ExecutionError(pub String);

#[derive(Debug, Clone)]
pub struct ValueRef(Rc<RefCell<Value>>);

impl ValueRef
{
    fn new(v: Value) -> ValueRef
    {
        ValueRef(Rc::new(RefCell::new(v)))
    }

    fn clone_value(&self) -> Value
    {
        self.0.borrow().clone()
    }
}

impl fmt::Display for ValueRef
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>
    {
        let v = self.0.borrow();
        write!(f, "{}", *v)
    }
}

#[allow(dead_code)]
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
    Struct(Vec<Value>),
    Sum(usize, Box<Value>),
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
        if sf.vars.get(name).is_none() {
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

    fn update_variable(&mut self, name: &str, new_value: Value) -> Result<(), ExecutionError>
    {
        for sf in self.stack.iter_mut().rev() {
            if let Some(v) = sf.vars.get_mut(name) {
                *v = ValueRef::new(new_value);
                return Ok(())
            }
        }

        Err(ExecutionError(format!("Unknown variable {}", name)))
    }

    fn make_value(&self, lit: &ByteCodeLiteral) -> Result<Value, ExecutionError>
    {
        match lit
        {
            &ByteCodeLiteral::Int(v) => Ok(Value::UInt(v)),
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
            &ByteCodeLiteral::Array(ref members) => {
                let mut vars = Vec::new();
                for var in members {
                    vars.push(self.get_variable(&var.name)?);
                }
                Ok(Value::Array(vars))
            },
        }
    }

    fn unary_op(&self, _dst: &str, _op: Operator, _var: &str) -> Result<Action, ExecutionError>
    {
        panic!("NYI");
    }

    fn binary_op(&self, _dst: &str, _op: Operator, _left: &str, _right: &str) -> Result<Action, ExecutionError>
    {
        panic!("NYI");
    }

    fn call(&self, _dst: &str, _name: &str, _args: &Vec<Var>) -> Result<Action, ExecutionError>
    {
        panic!("NYI");
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
        let inner = val.0.borrow();
        match inner.deref()
        {
            &Value::Bool(true) => Ok(Action::Jump(on_true)),
            &Value::Bool(false) => Ok(Action::Jump(on_false)),
            _ => Err(ExecutionError(format!("brif operand {} is not a boolean", var))),
        }
    }

    fn execute_instruction(&mut self, instr: &Instruction) -> Result<Action, ExecutionError>
    {
        println!("{}", instr);
        match *instr
        {
            Instruction::Load{ref dst, ref src} => {
                panic!("  load {} {}", dst, src)
            },

            Instruction::Store{ref dst, ref src} => {
                panic!(" str {} {}", dst, src)
            },

            Instruction::StoreLit{ref dst, ref lit} => {
                panic!("  strlit {} {}", dst, lit)
            },

            Instruction::StoreFunc{ref dst, ref func} => {
                panic!("  strfunc {} {}", dst, func)
            },

            Instruction::LoadMember{ref dst, ref obj, member_index} => {
                panic!("  ldrm {} {}.{}", dst, obj, member_index)
            },

            Instruction::GetProperty{ref dst, ref obj, ref prop} => {
                panic!("  getp {} {}.{}", dst, obj, prop)
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
                self.add_variable(&var.name, Value::Uninitialized)?;
                Ok(Action::Continue)
            },

            Instruction::HeapAlloc(ref var) => {
                panic!("  halloc {}", var)
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
                panic!("  delete {}", var)
            },

            Instruction::Slice{ref dst, ref src, ref start, ref len} => {
                panic!("  slice {} {} {} {}", dst, src, start, len)
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

    fn run(&mut self, func: &ByteCodeFunction, args: Vec<(String, Value)>) -> Result<ValueRef, ExecutionError>
    {
        println!("{}:", func.sig.name);
        self.stack.push(StackFrame::new());
        for (name, val) in args {
            self.add_variable(&name, val)?;
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
        Ok(sf.result.clone())
    }

    pub fn run_function(&mut self, function: &str, args: Vec<(String, Value)>) -> Result<ValueRef, ExecutionError>
    {
        match self.module.functions.iter().find(|func| func.sig.name == function)
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
    let vr = interpreter.run_function(function, vec![]);
    vr.map(|r| r.clone_value())
}
