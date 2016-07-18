use std::rc::Rc;
use std::collections::HashMap;
use llvm::prelude::*;

use ast::*;


pub struct VariableInstance
{
    pub value: LLVMValueRef,
    pub name: String,
    pub constant: bool,
    pub public: bool,
    pub global: bool,
    pub typ: Type,
}

pub enum PassingMode
{
    Copy,
    Value,
}

pub struct FunctionInstance
{
    pub function: LLVMValueRef,
    pub name: String,
    pub args: Vec<(LLVMTypeRef, PassingMode)>,
    pub return_type: LLVMTypeRef,
    pub sig: FunctionSignature,
    pub public: bool,
    pub external: bool,
}

pub struct StructMemberVar
{
    pub name: String,
    pub typ: Type,
    pub llvm_typ: LLVMTypeRef,
    pub constant: bool,
    pub public: bool,
    pub init: Expression,
}

pub struct StructType
{
    pub name: String,
    pub typ: LLVMTypeRef,
    pub members: Vec<Rc<StructMemberVar>>,
    pub public: bool,
}

impl StructType
{
    pub fn get_member(&self, name: &str) -> Option<(usize, Rc<StructMemberVar>)>
    {
        for (idx, m) in self.members.iter().enumerate() {
            if m.name == name {
                return Some((idx, m.clone()));
            }
        }

        None
    }
}

pub struct SymbolTable
{
    vars: HashMap<String, Rc<VariableInstance>>,
    funcs: HashMap<String, Rc<FunctionInstance>>,
    complex_types: HashMap<String, Rc<StructType>>,
}

impl SymbolTable
{
    pub fn new() -> SymbolTable
    {
        SymbolTable{
            vars: HashMap::new(),
            funcs: HashMap::new(),
            complex_types: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, var: Rc<VariableInstance>)
    {
        let name = var.name.clone();
        self.vars.insert(name, var);
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>>
    {
        self.vars.get(name).map(|v| v.clone())
    }

    pub fn add_function(&mut self, f: Rc<FunctionInstance>)
    {
        let name = f.name.clone();
        self.funcs.insert(name, f);
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>>
    {
        self.funcs.get(name).map(|fi_rc| fi_rc.clone())
    }

    pub fn get_complex_type(&self, name: &str) -> Option<Rc<StructType>>
    {
        self.complex_types.get(name).map(|st| st.clone())
    }

    pub fn add_complex_type(&mut self, st: Rc<StructType>)
    {
        self.complex_types.insert(st.name.clone(), st);
    }
}
