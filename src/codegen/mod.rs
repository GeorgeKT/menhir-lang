mod statements;
mod expressions;

use std::collections::HashMap;
use std::ffi::{CString};
use std::os::raw::c_char;
use std::marker::PhantomData;

use llvm::prelude::*;
use llvm::core::*;

use ast::*;
use compileerror::*;

use self::statements::*;
pub use self::expressions::gen_expression;

pub fn cstr(s: &str) -> *const c_char
{
    CString::new(s).expect("Valid C string").as_ptr()
}

pub struct VariableInstance
{
    pub value: LLVMValueRef,
    pub constant: bool,
}

pub struct StackFrame
{
    function: LLVMValueRef,
    vars: HashMap<String, VariableInstance>,
}

impl StackFrame
{
    pub unsafe fn return_type(&self) -> LLVMTypeRef
    {
        LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(self.function)))
    }

    pub fn add_variable(&mut self, name: &str, value: LLVMValueRef, constant: bool)
    {
        self.vars.insert(name.into(), VariableInstance{value: value, constant: constant});
    }

    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut VariableInstance>
    {
        self.vars.get_mut(name)
    }

    pub fn get_variable(&self, name: &str) -> Option<&VariableInstance>
    {
        self.vars.get(name)
    }
}

pub struct Context<'a>
{
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub stack: Vec<StackFrame>,
    _pd: PhantomData<&'a i32>,
}

impl<'a> Context<'a>
{
    pub fn new(name: &str) -> Context
    {
        let cname = CString::new(name).expect("Invalid module name");
        unsafe {
            let context = LLVMContextCreate();
            Context{
                context: context,
                module: LLVMModuleCreateWithName(cname.as_ptr()),
                builder: LLVMCreateBuilderInContext(context),
                stack: Vec::new(),
                _pd: PhantomData{},
            }
        }
    }

    pub fn push_stack_frame(&mut self, fun: LLVMValueRef)
    {
        self.stack.push(StackFrame{
            function: fun,
            vars: HashMap::new(),
        });
    }

    pub fn pop_stack_frame(&mut self)
    {
        self.stack.pop();
    }

    pub fn top_stack_frame(&mut self) -> Option<&mut StackFrame>
    {
        self.stack.last_mut()
    }

    pub unsafe fn dump(&self)
    {
        // Dump the module as IR to stdout.
        LLVMDumpModule(self.module);
    }

    pub fn get_variable_mut(&'a mut self, name: &str) -> Option<&'a mut VariableInstance>
    {
        for sf in self.stack.iter_mut().rev() {
            let v = sf.get_variable_mut(name);
            if v.is_some() {
                return v;
            }
        }
        None
    }

    pub fn get_variable(&'a self, name: &str) -> Option<&'a VariableInstance>
    {
        for sf in self.stack.iter().rev() {
            let v = sf.get_variable(name);
            if v.is_some() {
                return v;
            }
        }
        None
    }

    pub fn has_variable(&self, name: &str) -> bool
    {
        self.get_variable(name).is_some()
    }

    pub unsafe fn resolve_type(&self, typ: &Type) -> Option<LLVMTypeRef>
    {
        match *typ
        {
            Type::Primitive(_, ref name) =>
                match &name[..] {
                    "uint8" | "int8" | "char" | "byte" => Some(LLVMInt8TypeInContext(self.context)),
                    "uint16" | "int16" => Some(LLVMInt16TypeInContext(self.context)),
                    "uint32" | "int32" => Some(LLVMInt32TypeInContext(self.context)),
                    "int" | "uint" | "uint64"| "bool" => Some(LLVMInt64TypeInContext(self.context)),
                    "float" => Some(LLVMFloatTypeInContext(self.context)),
                    "double" => Some(LLVMDoubleTypeInContext(self.context)),
                    _ => None,
                },
            _ => None,
        }
    }
}

impl<'a> Drop for Context<'a>
{
    fn drop(&mut self)
    {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}


pub fn codegen(prog: &Program) -> Result<(), CompileError>
{
    unsafe {
        // Set up a context, module and builder in that context.
        let mut ctx = Context::new(&prog.name);
        try!(gen_program(&mut ctx, prog));
        ctx.dump();
        Ok(())
    }
}
