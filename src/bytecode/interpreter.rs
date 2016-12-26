use std::collections::HashMap;
use std::fmt;
use itertools::free::join;
use bytecode::*;
use parser::Operator;

#[derive(Debug)]
pub struct ExecutionError(pub String);

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
    Array(Vec<Value>),
    Func(String),
    Struct(Vec<Value>),
    Sum(usize, Box<Value>),
    Enum(usize),
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
            Value::Func(ref v) => write!(f, "func {}", v),
            Value::Struct(ref v) => write!(f, "{{{}}}", join(v.iter(), ", ")),
            Value::Sum(idx, ref v) => write!(f, "sum {} {}", idx, v),
            Value::Enum(idx) => write!(f, "enum {}", idx),
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
    vars: HashMap<String, Value>,
    result: Value,
}

impl StackFrame
{
    pub fn new() -> StackFrame
    {
        StackFrame{
            vars: HashMap::new(),
            result: Value::Void,
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
            sf.vars.insert(name.into(), v);
            Ok(())
        }
    }

    fn get_variable(&self, name: &str) -> Result<&Value, ExecutionError>
    {
        for sf in self.stack.iter().rev() {
            if let Some(ref v) = sf.vars.get(name) {
                return Ok(v);
            }
        }

        Err(ExecutionError(format!("Unknown variable {}", name)))
    }

    fn update_variable(&mut self, name: &str, new_value: Value) -> Result<(), ExecutionError>
    {
        for sf in self.stack.iter_mut().rev() {
            if let Some(v) = sf.vars.get_mut(name) {
                *v = new_value;
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
            &ByteCodeLiteral::String(ref s) => Ok(Value::Array(s.chars().map(|c| Value::Char(c)).collect())),
            &ByteCodeLiteral::Bool(v) => Ok(Value::Bool(v)),
            &ByteCodeLiteral::Array(ref members) => {
                let mut vars = Vec::new();
                for var in members {
                    vars.push(self.get_variable(&var.name)?.clone());
                }
                Ok(Value::Array(vars))
            },
        }
    }

    fn unary_op(&self, op: Operator, var: &str) -> Result<Value, ExecutionError>
    {
        panic!("NYI");
    }

    fn binary_op(&self, op: Operator, left: &str, right: &str) -> Result<Value, ExecutionError>
    {
        panic!("NYI");
    }

    fn call(&self, name: &str, args: &Vec<Var>) -> Result<Value, ExecutionError>
    {
        panic!("NYI");
    }

    fn set(&mut self, name: &str, expr: &ByteCodeExpression) -> Result<(), ExecutionError>
    {
        let value = match *expr
        {
            ByteCodeExpression::Literal(ref l) => self.make_value(l)?,
            ByteCodeExpression::UnaryOp(op, ref v) => self.unary_op(op, &v.name)?,
            ByteCodeExpression::BinaryOp(op, ref a, ref b) => self.binary_op(op, &a.name, &b.name)?,
            ByteCodeExpression::Call(ref name, ref args) => self.call(name, args)?,
            ByteCodeExpression::StructMember(ref obj, index) => panic!("NYI"),
            ByteCodeExpression::SumTypeIndex(ref obj) => panic!("NYI"),
            ByteCodeExpression::SumTypeStruct(ref obj, index) => panic!("NYI"),
            ByteCodeExpression::SumTypeCase(index) => panic!("NYI"),
            ByteCodeExpression::ArrayProperty(ref array, ref property) => panic!("NYI"),
            ByteCodeExpression::ArrayHead(ref array) => panic!("NYI"),
            ByteCodeExpression::ArrayTail(ref array) => panic!("NYI"),
            ByteCodeExpression::Ref(ref obj) => panic!("NYI"),
            ByteCodeExpression::Func(ref func) => panic!("NYI"),
            ByteCodeExpression::HeapAlloc(ref typ) => panic!("NYI"),
        };

        self.update_variable(name, value)
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
        match val
        {
            &Value::Bool(true) => Ok(Action::Jump(on_true)),
            &Value::Bool(false) => Ok(Action::Jump(on_false)),
            _ => Err(ExecutionError(format!("brif operand {} is not a boolean", var))),
        }
    }

    fn execute_instruction(&mut self, current_bb: BasicBlockRef, instr: &Instruction) -> Result<Action, ExecutionError>
    {
        println!("{}", instr);
        match *instr
        {
            Instruction::Alloc(ref var) => {
                self.add_variable(&var.name, Value::Uninitialized)?;
                Ok(Action::Continue)
            },
            Instruction::SetStructMember{ref obj, member_index, ref value} => {
                panic!("NYI");
            },
            Instruction::StartScope => {
                self.stack.push(StackFrame::new());
                Ok(Action::Continue)
            },
            Instruction::EndScope => {
                self.stack.pop();
                Ok(Action::Continue)
            },
            Instruction::Bind{ref name, ref var} => {
                panic!("NYI");
            },
            Instruction::Set{ref var, ref expr} => {
                self.set(&var.name, expr)?;
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
            Instruction::IncRef(ref v) => {
                panic!("NYI");
            },
            Instruction::DecRef(ref v) => {
                panic!("NYI");
            },
        }
    }

    fn run_bb(&mut self, func: &ByteCodeFunction, bb_ref: BasicBlockRef) -> Result<Action, ExecutionError>
    {
        if let Some(ref bb) = func.blocks.get(&bb_ref) {
            for inst in &bb.instructions {
                let action = self.execute_instruction(bb_ref, inst)?;
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

    fn run(&mut self, func: &ByteCodeFunction, args: Vec<(String, Value)>) -> Result<Value, ExecutionError>
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
        Ok(sf.result)
    }

    pub fn run_function(&mut self, function: &str, args: Vec<(String, Value)>) -> Result<Value, ExecutionError>
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
    interpreter.run_function(function, vec![])
}
