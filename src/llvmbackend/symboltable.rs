use llvm_sys::prelude::*;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Type;
use crate::llvmbackend::valueref::ValueRef;

pub struct FunctionInstance {
    pub function: LLVMValueRef,
    pub name: String,
    pub return_type: Type,
    pub typ: Type,
}

impl FunctionInstance {
    pub fn new(name: &str, func: LLVMValueRef, return_type: Type, typ: Type) -> FunctionInstance {
        FunctionInstance {
            name: name.into(),
            function: func,
            return_type,
            typ,
        }
    }
}

pub struct VariableInstance {
    pub value: ValueRef,
    pub name: String,
}

pub struct SymbolTable {
    vars: HashMap<String, Rc<VariableInstance>>,
    funcs: HashMap<String, Rc<FunctionInstance>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            vars: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, var: Rc<VariableInstance>) {
        // We already checked for duplicates during the type checking fase
        let name = var.name.clone();
        self.vars.insert(name, var);
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>> {
        self.vars.get(name).cloned()
    }

    pub fn add_function(&mut self, f: Rc<FunctionInstance>) {
        let name = f.name.clone();
        self.funcs.insert(name, f);
    }

    /*
    pub fn add_function_alias(&mut self, alias: &str, f: Rc<FunctionInstance>)
    {
        self.funcs.insert(alias.into(), f);
    }
    */

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>> {
        self.funcs.get(name).cloned()
    }
}
