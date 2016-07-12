use std::os::raw::c_char;
use std::ffi::{CStr, CString};
use std::fs::DirBuilder;
use std::ptr;
use std::rc::Rc;

use llvm::prelude::*;
use llvm::core::*;
use llvm::target_machine::*;
use llvm::analysis::*;

use compileerror::*;
use codegen::*;
use codegen::symbols::*;
use codegen::stackframe::*;
use codegen::context::*;
use ast::*;


pub struct ModuleContext
{
    pub module: LLVMModuleRef,
    name: String,
    path: String,
    public_symbols: SymbolTable,
    private_symbols: SymbolTable,
    stack: Vec<StackFrame>,
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
                private_symbols: SymbolTable::new(),
                stack: Vec::new(),
            }
        }
    }

    pub fn in_global_context(&self) -> bool
    {
        self.stack.is_empty()
    }

    pub fn optimize(&self) -> Result<(), CompileError>
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


    pub fn gen_object_file(&self, target_machine: LLVMTargetMachineRef, build_dir: &str) -> Result<String, CompileError>
    {
    	try!(DirBuilder::new()
    		.recursive(true)
    		.create(build_dir)
    		.map_err(|e| CompileError::new(
    			Pos::zero(),
    			ErrorType::CodegenError(
    				format!("Unable to create directory for {}: {}", build_dir, e)))));


    	let obj_file_name = format!("{}/{}.o", build_dir, self.name);
    	println!("  Building {}", self.name);
        unsafe {
            let mut error_message: *mut c_char = ptr::null_mut();
            if LLVMTargetMachineEmitToFile(target_machine, self.module, cstr_mut(&obj_file_name), LLVMCodeGenFileType::LLVMObjectFile, &mut error_message) != 0 {
                let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
                let e = format!("Unable to create object file: {}", msg);
                LLVMDisposeMessage(error_message);
                LLVMDisposeTargetMachine(target_machine);
                return err(Pos::zero(), ErrorType::CodegenError(e));
            }
        }

        Ok(obj_file_name)
    }

    pub fn push_stack_frame(&mut self, fun: LLVMValueRef)
    {
        self.stack.push(StackFrame::new(fun));
    }

    pub fn pop_stack_frame(&mut self)
    {
        self.stack.pop();
    }

    pub fn get_variable(&self, name: &str, private_allowed: bool) -> Option<Rc<VariableInstance>>
    {
        for sf in self.stack.iter().rev() {
            let v = sf.symbols.get_variable(name);
            if v.is_some() {
                return v;
            }
        }

        let v = self.public_symbols.get_variable(name);
        if v.is_some() {
            return v;
        }

        if private_allowed {
            let v = self.private_symbols.get_variable(name);
            if v.is_some() {
                return v;
            }
        }

        None
    }

    pub fn add_variable(&mut self, var: Rc<VariableInstance>)
    {
        if let Some(ref mut sf) = self.stack.last_mut() {
            sf.symbols.add_variable(var);
        } else {
            // It's a global
            if var.public {
                self.public_symbols.add_variable(var);
            } else {
                self.private_symbols.add_variable(var);
            }
        }
    }

    pub fn get_function(&self, name: &str, private_allowed: bool) -> Option<Rc<FunctionInstance>>
    {
        for sf in self.stack.iter().rev() {
            let f = sf.symbols.get_function(name);
            if f.is_some() {
                return f;
            }
        }

        let f = self.public_symbols.get_function(name);
        if f.is_some() {
            return f;
        }

        if private_allowed {
            let f = self.private_symbols.get_function(name);
            if f.is_some() {
                return f;
            }
        }
        None
    }

    pub fn add_function(&mut self, f: Rc<FunctionInstance>)
    {
        if let Some(ref mut sf) = self.stack.last_mut() {
            sf.symbols.add_function(f);
        } else {
            // It's a global
            if f.public {
                self.public_symbols.add_function(f);
            } else {
                self.private_symbols.add_function(f);
            }
        }
    }

    pub fn get_complex_type(&self, name: &str, private_allowed: bool) -> Option<Rc<StructType>>
    {
        for sf in self.stack.iter().rev() {
            let f = sf.symbols.get_complex_type(name);
            if f.is_some() {
                return f;
            }
        }

        let f = self.public_symbols.get_complex_type(name);
        if f.is_some() {
            return f;
        }

        if private_allowed {
            let f = self.private_symbols.get_complex_type(name);
            if f.is_some() {
                return f;
            }
        }

        None
    }

    pub fn add_complex_type(&mut self, st: Rc<StructType>)
    {
        if let Some(ref mut sf) = self.stack.last_mut() {
            sf.symbols.add_complex_type(st);
        } else {
            // It's a global
            if st.public {
                self.public_symbols.add_complex_type(st);
            } else {
                self.private_symbols.add_complex_type(st);
            }
        }
    }

    pub fn get_current_function(&self) -> LLVMValueRef
    {
        self.stack.last().expect("Stack is empty").get_current_function()
    }

    pub fn verify_module(&self) -> Result<(), CompileError>
    {
        unsafe {
            let mut error_message: *mut c_char = ptr::null_mut();
            if LLVMVerifyModule(self.module, LLVMVerifierFailureAction::LLVMReturnStatusAction, &mut error_message) != 0 {
                let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
                let e = format!("Module verification error: {}", msg);
                LLVMDisposeMessage(error_message);
                err(Pos::zero(), ErrorType::CodegenError(e))
            } else {
                Ok(())
            }
        }
    }
}

impl Drop for ModuleContext
{
    fn drop(&mut self)
    {
        unsafe {LLVMDisposeModule(self.module)}
    }
}

pub fn import_module(ctx: &mut Context, m: &ModuleName) -> Result<(), CompileError>
{
    use parser::*;
    use codegen::statements::gen_module;

    let path = m.parts.join("/") + ".cobra";
    let module = try!(parse_file(&path, ParseMode::Module));
    let namespace = m.parts.join("::") + "::";
    let mc = Box::new(ModuleContext::new(ctx.context, m.parts.last().expect("Invalid module name"), &namespace));
    let old_mc = ctx.set_current_module(mc);
    unsafe {
        try!(gen_module(ctx, &module));
        LLVMDumpModule(ctx.get_current_module_ref());
    }

    let mc = ctx.set_current_module(old_mc);
    ctx.add_module(mc);
    Ok(())
}
