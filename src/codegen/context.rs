use std::ptr;
use std::rc::Rc;
use std::os::raw::c_char;
use std::ffi::{CStr, CString};
use std::fs::DirBuilder;

use llvm::prelude::*;
use llvm::core::*;
use llvm::target_machine::*;

use ast::{Type};
use codegen::{cstr, cstr_mut};
use compileerror::{Pos, CompileResult, CompileError, ErrorCode, err};
use codegen::symboltable::{VariableInstance, FunctionInstance, SymbolTable};


pub struct Context
{
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    name: String,
    stack: Vec<SymbolTable>,
}

impl Context
{
	pub fn new(module_name: &str) -> Context
	{
		unsafe {
            let cname = CString::new(module_name).expect("Invalid module name");
            let context = LLVMContextCreate();
            Context{
                context: context,
                module: LLVMModuleCreateWithNameInContext(cname.as_ptr(), context),
                builder: LLVMCreateBuilderInContext(context),
                name: module_name.into(),
                stack: vec![SymbolTable::new()],
            }
        }
	}

    pub fn add_variable(&mut self, var: Rc<VariableInstance>)
    {
        self.stack.last_mut().expect("Stack is empty").add_variable(var)
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>>
    {
        for sf in self.stack.iter().rev() 
        {
            let v = sf.get_variable(name);
            if v.is_some() {
                return v;
            }
        }

        None
    }

    pub fn add_function(&mut self, f: Rc<FunctionInstance>)
    {
        self.stack.last_mut().expect("Stack is empty").add_function(f)
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>>
    {
        for sf in self.stack.iter().rev() 
        {
            let func = sf.get_function(name);
            if func.is_some() {
                return func;
            }
        }

        None
    }

    pub fn push_stack(&mut self)
    {
        self.stack.push(SymbolTable::new());
    }

    pub fn pop_stack(&mut self)
    {
        self.stack.pop();
    }


    pub unsafe fn gen_object_file(&self, build_dir: &str) -> CompileResult<String>
    {
        let target_triple = CStr::from_ptr(LLVMGetDefaultTargetTriple());
        let target_triple_str = target_triple.to_str().expect("Invalid target triple");
        println!("Compiling for {}", target_triple_str);

        let mut target: LLVMTargetRef = ptr::null_mut();
        let mut error_message: *mut c_char = ptr::null_mut();
        if LLVMGetTargetFromTriple(target_triple.as_ptr(), &mut target, &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
            let e = format!("Unable to get an LLVM target reference for {}: {}", target_triple_str, msg);
            LLVMDisposeMessage(error_message);
            return err(Pos::zero(), ErrorCode::CodegenError, e);
        }

        let target_machine = LLVMCreateTargetMachine(
            target,
            target_triple.as_ptr(),
            cstr(""),
            cstr(""),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        if target_machine == ptr::null_mut() {
            let e = format!("Unable to get a LLVM target machine for {}", target_triple_str);
            return err(Pos::zero(), ErrorCode::CodegenError, e);
        }

        try!(DirBuilder::new()
            .recursive(true)
            .create(build_dir)
            .map_err(|e| CompileError::new(
                Pos::zero(),
                ErrorCode::CodegenError,
                format!("Unable to create directory for {}: {}", build_dir, e))));


        let obj_file_name = format!("{}/{}.cobra.o", build_dir, self.name);
        println!("  Building {}", obj_file_name);

        let mut error_message: *mut c_char = ptr::null_mut();
        if LLVMTargetMachineEmitToFile(target_machine, self.module, cstr_mut(&obj_file_name), LLVMCodeGenFileType::LLVMObjectFile, &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
            let e = format!("Unable to create object file: {}", msg);
            LLVMDisposeMessage(error_message);
            LLVMDisposeTargetMachine(target_machine);
            return err(Pos::zero(), ErrorCode::CodegenError, e);
        }


        LLVMDisposeTargetMachine(target_machine);
        Ok(obj_file_name)
    }

    pub fn optimize(&self) -> CompileResult<()>
    {
        unsafe{
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
        }
        Ok(())
    }

    pub fn verify(&self) -> CompileResult<()>
    {
        use llvm::analysis::*;
        unsafe {
            let mut error_message: *mut c_char = ptr::null_mut();
            if LLVMVerifyModule(self.module, LLVMVerifierFailureAction::LLVMReturnStatusAction, &mut error_message) != 0 {
                let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
                let e = format!("Module verification error: {}", msg);
                LLVMDisposeMessage(error_message);
                err(Pos::zero(), ErrorCode::CodegenError, e)
            } else {
                Ok(())
            }
        }
    }

    #[cfg(test)]
    pub fn take_module_ref(&mut self) -> LLVMModuleRef
    {
        use std::mem;
        mem::replace(&mut self.module, ptr::null_mut())
    }

    pub unsafe fn resolve_type(&mut self, typ: &Type) -> Option<LLVMTypeRef>
    {
        match *typ
        {
            Type::Void => Some(LLVMVoidTypeInContext(self.context)),
            Type::Int => Some(LLVMInt64TypeInContext(self.context)),
            Type::Bool => Some(LLVMInt1TypeInContext(self.context)),
            Type::Float => Some(LLVMDoubleTypeInContext(self.context)),
            _ => None,
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
