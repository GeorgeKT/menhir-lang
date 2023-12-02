use super::symboltable::{FunctionInstance, SymbolTable, VariableInstance};
use super::target::TargetMachine;
use super::valueref::ValueRef;
use super::CodeGenOptions;
use crate::ast::{ptr_type, Type};
use crate::compileerror::{code_gen_error, code_gen_result, CompileResult};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::{c_char, CStr, CString};
use std::fs::DirBuilder;
use std::ptr;
use std::rc::Rc;

struct StackFrame {
    pub symbols: SymbolTable,
    pub current_function: LLVMValueRef,
}

impl StackFrame {
    pub fn new(current_function: LLVMValueRef) -> StackFrame {
        StackFrame {
            symbols: SymbolTable::new(),
            current_function,
        }
    }
}

pub struct Context {
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub target_machine: TargetMachine,
    name: String,
    stack: Vec<StackFrame>,
}

impl Context {
    pub fn new(module_name: &str) -> Result<Context, String> {
        unsafe {
            let context = LLVMContextCreate();
            LLVMContextSetOpaquePointers(context, 0);
            let context_name = CString::new(module_name).expect("Invalid module name");
            let module = LLVMModuleCreateWithNameInContext(context_name.as_ptr(), context);
            let name = module_name.into();
            Ok(Context {
                context,
                module,
                builder: LLVMCreateBuilderInContext(context),
                target_machine: TargetMachine::new().map_err(|e| format!("{e}"))?,
                name,
                stack: vec![StackFrame::new(ptr::null_mut())],
            })
        }
    }

    pub fn set_variable(&mut self, name: &str, vr: ValueRef) -> CompileResult<()> {
        if let Some(vi) = self.get_variable_instance(name) {
            unsafe {
                vi.value.store(self, &vr)?;
            }
            Ok(())
        } else {
            let var = Rc::new(VariableInstance {
                value: vr,
                name: name.into(),
            });
            self.add_variable_instance(var)
        }
    }

    pub fn add_variable_instance(&mut self, vi: Rc<VariableInstance>) -> CompileResult<()> {
        self.stack
            .last_mut()
            .ok_or_else(|| code_gen_error("Stack is empty"))?
            .symbols
            .add_variable(vi);
        Ok(())
    }

    pub fn stack_alloc(&mut self, name: &str, typ: &Type) -> CompileResult<LLVMValueRef> {
        unsafe {
            let mut llvm_type = self.resolve_type(typ)?;
            if let Type::Func(_) = typ {
                llvm_type = LLVMPointerType(llvm_type, 0);
            }
            let func = self.get_current_function()?;
            let entry_bb = LLVMGetEntryBasicBlock(func);
            let current_bb = LLVMGetInsertBlock(self.builder);
            // We allocate in the entry block
            LLVMPositionBuilder(self.builder, entry_bb, LLVMGetFirstInstruction(entry_bb));

            let name = CString::new(name).expect("Invalid string");
            let alloc = LLVMBuildAlloca(self.builder, llvm_type, name.as_ptr());
            LLVMPositionBuilderAtEnd(self.builder, current_bb); // Position the builder where it was before
            Ok(alloc)
        }
    }

    fn get_variable_instance(&self, name: &str) -> Option<Rc<VariableInstance>> {
        for sf in self.stack.iter().rev() {
            let v = sf.symbols.get_variable(name);
            if v.is_some() {
                return v;
            }
        }
        None
    }

    pub fn get_variable(&mut self, name: &str, typ: &Type) -> CompileResult<ValueRef> {
        if let Some(vi) = self.get_variable_instance(name) {
            return Ok(vi.value.clone());
        }

        let val = self.stack_alloc(name, typ)?;
        let ret = ValueRef::new(val, ptr_type(typ.clone()));
        self.set_variable(name, ret.clone())?;
        Ok(ret)
    }

    pub fn add_function(&mut self, f: Rc<FunctionInstance>) -> CompileResult<()> {
        self.stack
            .last_mut()
            .ok_or_else(|| code_gen_error("Stack is empty"))?
            .symbols
            .add_function(f);
        Ok(())
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>> {
        for sf in self.stack.iter().rev() {
            let func = sf.symbols.get_function(name);
            if func.is_some() {
                return func;
            }
        }

        None
    }

    pub fn push_stack(&mut self, func: LLVMValueRef) {
        self.stack.push(StackFrame::new(func));
    }

    pub fn pop_stack(&mut self) {
        self.stack.pop();
    }

    pub fn get_current_function(&self) -> CompileResult<LLVMValueRef> {
        for sf in self.stack.iter().rev() {
            if !sf.current_function.is_null() {
                return Ok(sf.current_function);
            }
        }

        code_gen_result("Internal Compiler Error: No current function on stack")
    }

    pub fn resolve_type(&self, typ: &Type) -> CompileResult<LLVMTypeRef> {
        unsafe {
            use crate::llvmbackend::types::to_llvm_type;
            to_llvm_type(self.context, &self.target_machine, typ)
        }
    }

    pub fn native_uint_type(&self) -> CompileResult<LLVMTypeRef> {
        self.resolve_type(&self.target_machine.target.native_uint_type)
    }

    pub fn dump_module(&self) {
        println!("LLVM IR: {}", self.name);
        // Dump the module as IR to stdout.
        unsafe {
            LLVMDumpModule(self.module);
        }
        println!("----------------------");
    }

    unsafe fn save_ir(&self, opts: &CodeGenOptions) -> Result<(), String> {
        let mut error_message: *mut c_char = ptr::null_mut();
        let ir_file_name =
            CString::new(format!("{}/{}.ir", opts.build_dir.display(), self.name)).expect("Invalid String");
        if LLVMPrintModuleToFile(self.module, ir_file_name.as_ptr(), &mut error_message) != 0 {
            let msg = CStr::from_ptr(error_message)
                .to_str()
                .expect("Invalid C string");
            let e = format!("Unable to create ir file: {}", msg);
            LLVMDisposeMessage(error_message);
            return Err(e);
        }

        Ok(())
    }

    pub unsafe fn gen_object_file(&self, opts: &CodeGenOptions) -> Result<String, String> {
        if opts.optimize {
            self.optimize()?;
        }

        self.save_ir(opts)?;
        if opts.dump_ir {
            self.dump_module();
        }

        DirBuilder::new()
            .recursive(true)
            .create(&opts.build_dir)
            .map_err(|e| format!("Unable to create directory for {}: {}", opts.build_dir.display(), e))?;

        let obj_file_name = format!("{}/{}.mhr.o", opts.build_dir.display(), self.name);
        println!("  Building {}", obj_file_name);
        self.target_machine
            .emit_to_file(self.module, &obj_file_name)?;
        Ok(obj_file_name)
    }

    unsafe fn optimize(&self) -> Result<(), String> {
        use llvm_sys::transforms::pass_manager_builder::*;

        let pass_builder = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderSetOptLevel(pass_builder, 3);
        LLVMPassManagerBuilderSetSizeLevel(pass_builder, 0);

        let function_passes = LLVMCreateFunctionPassManagerForModule(self.module);
        let module_passes = LLVMCreatePassManager();

        LLVMPassManagerBuilderPopulateFunctionPassManager(pass_builder, function_passes);
        LLVMPassManagerBuilderPopulateModulePassManager(pass_builder, module_passes);
        LLVMPassManagerBuilderDispose(pass_builder);

        LLVMInitializeFunctionPassManager(function_passes);

        let mut func = LLVMGetFirstFunction(self.module);
        while !func.is_null() {
            LLVMRunFunctionPassManager(function_passes, func);
            func = LLVMGetNextFunction(func);
        }

        LLVMRunPassManager(module_passes, self.module);
        LLVMDisposePassManager(function_passes);
        LLVMDisposePassManager(module_passes);
        Ok(())
    }

    pub fn verify(&self) -> Result<(), String> {
        use libc::c_char;
        use llvm_sys::analysis::*;
        unsafe {
            let mut error_message: *mut c_char = ptr::null_mut();
            if LLVMVerifyModule(
                self.module,
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                &mut error_message,
            ) != 0
            {
                self.dump_module();

                let msg = CStr::from_ptr(error_message)
                    .to_str()
                    .expect("Invalid C string");
                let e = format!("Module verification error: {}", msg);
                LLVMDisposeMessage(error_message);
                Err(e)
            } else {
                LLVMDisposeMessage(error_message);
                Ok(())
            }
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            if !self.module.is_null() {
                LLVMDisposeModule(self.module);
            }
            LLVMContextDispose(self.context);
        }
    }
}
