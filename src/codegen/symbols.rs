use std::rc::Rc;
use std::collections::HashMap;
use llvm::prelude::*;

use ast::*;


pub struct VariableInstance
{
    pub value: LLVMValueRef,
    pub name: String,
    pub constant: bool,
    pub typ: Type,
}

pub struct FunctionInstance
{
    pub function: LLVMValueRef,
    pub name: String,
    pub args: Vec<LLVMTypeRef>,
    pub return_type: LLVMTypeRef,
    pub sig: FunctionSignature,
    pub public: bool,
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

    pub fn add_variable(&mut self, name: &str, value: LLVMValueRef, constant: bool, typ: Type)
    {
        self.vars.insert(name.into(), Rc::new(VariableInstance{
            value: value,
            name: name.into(),
            constant: constant,
            typ: typ,
        }));
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>>
    {
        self.vars.get(name).map(|v| v.clone())
    }

    pub fn add_function(&mut self, f: FunctionInstance)
    {
        let name = f.name.clone();
        self.funcs.insert(name, Rc::new(f));
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>>
    {
        self.funcs.get(name).map(|fi_rc| fi_rc.clone())
    }

    pub fn get_complex_type(&self, name: &str) -> Option<Rc<StructType>>
    {
        self.complex_types.get(name).map(|st| st.clone())
    }

    pub fn add_complex_type(&mut self, st: StructType)
    {
        self.complex_types.insert(st.name.clone(), Rc::new(st));
    }
}
