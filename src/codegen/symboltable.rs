use std::rc::Rc;
use std::collections::HashMap;
use llvm::prelude::*;

use ast::{FunctionSignature};
use codegen::ValueRef;

pub struct FunctionInstance
{
    pub function: LLVMValueRef,
    pub name: String,
    pub args: Vec<LLVMTypeRef>,
    pub return_type: LLVMTypeRef,
    pub sig: FunctionSignature,
}

pub struct VariableInstance
{
    pub value: ValueRef,
    pub name: String,
}

pub struct SymbolTable
{
    vars: HashMap<String, Rc<VariableInstance>>,
    funcs: HashMap<String, Rc<FunctionInstance>>,
}

impl SymbolTable
{
    pub fn new() -> SymbolTable
    {
        SymbolTable{
            vars: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, var: Rc<VariableInstance>)
    {
        // We already checked for duplicates during the type checking fase
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
}
