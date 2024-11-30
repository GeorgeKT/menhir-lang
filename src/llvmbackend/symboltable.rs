use llvm_sys::prelude::*;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::FunctionSignature;
use crate::llvmbackend::valueref::ValueRef;

pub struct FunctionInstance {
    pub function: LLVMValueRef,
    pub name: String,
    pub sig: FunctionSignature,
}

impl FunctionInstance {
    pub fn new(func: LLVMValueRef, name: impl Into<String>, sig: FunctionSignature) -> FunctionInstance {
        FunctionInstance {
            function: func,
            sig,
            name: name.into(),
        }
    }
}

pub struct VariableInstance {
    pub value: ValueRef,
    pub name: String,
}

pub type VariableInstancePtr = Rc<VariableInstance>;

pub struct SymbolTable {
    vars: BTreeMap<String, VariableInstancePtr>,
    funcs: BTreeMap<String, Rc<FunctionInstance>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            vars: BTreeMap::new(),
            funcs: BTreeMap::new(),
        }
    }

    pub fn add_variable(&mut self, var: VariableInstancePtr) {
        // We already checked for duplicates during the type checking fase
        let name = var.name.clone();
        self.vars.insert(name, var);
    }

    pub fn get_variable(&self, name: &str) -> Option<VariableInstancePtr> {
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
