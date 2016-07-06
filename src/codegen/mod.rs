mod statements;
mod expressions;

use std::ffi::{CString};
use std::os::raw::c_char;
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

pub struct Context
{
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
}

impl Context
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
            }
        }
    }


    pub unsafe fn dump(&self)
    {
        // Dump the module as IR to stdout.
        LLVMDumpModule(self.module);
    }
}

impl Drop for Context
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
