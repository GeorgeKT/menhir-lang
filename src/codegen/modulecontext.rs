use llvm::prelude::*;
use llvm::core::*;
use std::ffi::CString;

use compileerror::*;
use codegen::symbols::*;


pub struct ModuleContext
{
    pub module: LLVMModuleRef,
    pub name: String,
    path: String, // e.g. cobra::
    public_symbols: SymbolTable,
}

impl ModuleContext
{
    pub fn new(ctx: LLVMContextRef, name: &str, path: &str) -> ModuleContext
    {
        let cname = CString::new(name).expect("Invalid module name");
        unsafe {
            ModuleContext{
                module: LLVMModuleCreateWithNameInContext(cname.as_ptr(), ctx),
                name: name.into(),
                path: path.into(),
                public_symbols: SymbolTable::new(),
            }
        }
    }

    pub unsafe fn optimize(&self) -> Result<(), CompileError>
    {
        use std::ptr;
        use llvm::transforms::pass_manager_builder::*;

        let pmb = LLVMPassManagerBuilderCreate();
        let pm = LLVMCreateFunctionPassManagerForModule(self.module);
        LLVMInitializeFunctionPassManager(pm);

        LLVMPassManagerBuilderSetOptLevel(pmb, 2);
        LLVMPassManagerBuilderPopulateFunctionPassManager(pmb, pm);

        let mut func = LLVMGetFirstFunction(self.module);
        while func != ptr::null_mut() {
            LLVMRunFunctionPassManager(pm, func);
            func = LLVMGetNextFunction(func);
        }

        LLVMDisposePassManager(pm);
        LLVMPassManagerBuilderDispose(pmb);
        Ok(())
    }

}

impl Drop for ModuleContext
{
    fn drop(&mut self)
    {
        unsafe {LLVMDisposeModule(self.module)}
    }
}
