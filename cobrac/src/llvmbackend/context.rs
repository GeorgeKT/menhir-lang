use std::ffi::CString;
use std::rc::Rc;
use std::ptr;
use llvm::prelude::*;
use llvm::core::*;
use libcobra::ast::Type;
use super::symboltable::{SymbolTable, FunctionInstance, VariableInstance};
use super::target::TargetMachine;
use super::valueref::ValueRef;



struct StackFrame
{
    pub symbols: SymbolTable,
    pub current_function: LLVMValueRef,
}

impl StackFrame
{
    pub fn new(current_function: LLVMValueRef) -> StackFrame
    {
        StackFrame{
            symbols: SymbolTable::new(),
            current_function: current_function,
        }
    }
}


pub struct Context
{
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    target_machine: TargetMachine,
    name: String,
    stack: Vec<StackFrame>,
}

impl Context
{
    pub fn new(module_name: &str) -> Result<Context, String>
    {
        unsafe {
            let context_name = CString::new(module_name).expect("Invalid module name");
            let context = LLVMContextCreate();
            let target_machine = TargetMachine::new()?;
            Ok(Context {
                context: context,
                module: LLVMModuleCreateWithNameInContext(context_name.as_ptr(), context),
                builder: LLVMCreateBuilderInContext(context),
                target_machine: target_machine,
                name: module_name.into(),
                stack: Vec::new(),
            })
        }
    }


    pub fn add_variable(&mut self, name: &str, vr: ValueRef)
    {
        let var = Rc::new(VariableInstance{
            value: vr,
            name: name.into(),
        });
        self.add_variable_instance(var);
    }

    pub fn add_variable_instance(&mut self, vi: Rc<VariableInstance>)
    {
        self.stack.last_mut().expect("Stack is empty").symbols.add_variable(vi);
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>>
    {
        for sf in self.stack.iter().rev()
        {
            let v = sf.symbols.get_variable(name);
            if v.is_some() {
                return v;
            }
        }

        None
    }

    pub fn add_function(&mut self, f: Rc<FunctionInstance>)
    {
        self.stack.last_mut().expect("Stack is empty").symbols.add_function(f)
    }

    pub fn add_function_alias(&mut self, alias: &str, f: Rc<FunctionInstance>)
    {
        self.stack.last_mut().expect("Stack is empty").symbols.add_function_alias(alias, f)
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>>
    {
        for sf in self.stack.iter().rev()
        {
            let func = sf.symbols.get_function(name);
            if func.is_some() {
                return func;
            }
        }

        None
    }

    pub fn push_stack(&mut self, func: LLVMValueRef)
    {
        self.stack.push(StackFrame::new(func));
    }

    pub fn pop_stack(&mut self)
    {
        self.stack.pop();
    }

    pub fn get_current_function(&self) -> LLVMValueRef
    {
        for sf in self.stack.iter().rev()
        {
            if sf.current_function != ptr::null_mut() {
                return sf.current_function;
            }
        }

        panic!("Internal Compiler Error: No current function on stack, we should have caught this !");
    }

    pub fn resolve_type(&self, typ: &Type) -> LLVMTypeRef
    {
        unsafe{
            use llvmbackend::types::to_llvm_type;
            to_llvm_type(self.context, &self.target_machine, typ)
        }
    }

}


impl Drop for Context
{
    fn drop(&mut self)
    {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            if self.module != ptr::null_mut() {
                LLVMDisposeModule(self.module);
            }
            LLVMContextDispose(self.context);
        }
    }
}
