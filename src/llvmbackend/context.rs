use super::symboltable::{FunctionInstance, SymbolTable, VariableInstance};
use super::target::TargetMachine;
use super::valueref::ValueRef;
use super::CodeGenOptions;
use crate::ast::{ptr_type, Type};
use llvm::core::*;
use llvm::prelude::*;
use std::ffi::CString;
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
            current_function: current_function,
        }
    }
}

pub struct Context<'a> {
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub target_machine: &'a TargetMachine,
    name: String,
    stack: Vec<StackFrame>,
}

impl<'a> Context<'a> {
    pub fn new(module_name: &str, target_machine: &'a TargetMachine) -> Result<Context<'a>, String> {
        unsafe {
            let context_name = CString::new(module_name).expect("Invalid module name");
            let context = LLVMContextCreate();
            Ok(Context::<'a> {
                context: context,
                module: LLVMModuleCreateWithNameInContext(context_name.as_ptr(), context),
                builder: LLVMCreateBuilderInContext(context),
                target_machine: target_machine,
                name: module_name.into(),
                stack: vec![StackFrame::new(ptr::null_mut())],
            })
        }
    }

    pub fn set_variable(&mut self, name: &str, vr: ValueRef) {
        if let Some(vi) = self.get_variable_instance(name) {
            unsafe {
                vi.value.store(self, &vr);
            }
        } else {
            let var = Rc::new(VariableInstance {
                value: vr,
                name: name.into(),
            });
            self.add_variable_instance(var);
        }
    }

    pub fn add_variable_instance(&mut self, vi: Rc<VariableInstance>) {
        self.stack
            .last_mut()
            .expect("Stack is empty")
            .symbols
            .add_variable(vi);
    }

    pub fn stack_alloc(&mut self, name: &str, typ: &Type) -> LLVMValueRef {
        unsafe {
            let mut llvm_type = self.resolve_type(typ);
            if let Type::Func(_) = typ {
                llvm_type = LLVMPointerType(llvm_type, 0);
            }
            let func = self.get_current_function();
            let entry_bb = LLVMGetEntryBasicBlock(func);
            let current_bb = LLVMGetInsertBlock(self.builder);
            // We allocate in the entry block
            LLVMPositionBuilder(self.builder, entry_bb, LLVMGetFirstInstruction(entry_bb));

            let name = CString::new(name).expect("Invalid string");
            let alloc = LLVMBuildAlloca(self.builder, llvm_type, name.as_ptr());
            LLVMPositionBuilderAtEnd(self.builder, current_bb); // Position the builder where it was before
            alloc
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

    pub fn get_variable(&mut self, name: &str, typ: &Type) -> ValueRef {
        if let Some(vi) = self.get_variable_instance(name) {
            return vi.value.clone();
        }

        let val = self.stack_alloc(name, typ);
        let ret = ValueRef::new(val, ptr_type(typ.clone()));
        self.set_variable(name, ret.clone());
        ret
    }

    pub fn add_function(&mut self, f: Rc<FunctionInstance>) {
        self.stack
            .last_mut()
            .expect("Stack is empty")
            .symbols
            .add_function(f)
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

    pub fn get_current_function(&self) -> LLVMValueRef {
        for sf in self.stack.iter().rev() {
            if !sf.current_function.is_null() {
                return sf.current_function;
            }
        }

        panic!("Internal Compiler Error: No current function on stack");
    }

    pub fn resolve_type(&self, typ: &Type) -> LLVMTypeRef {
        unsafe {
            use crate::llvmbackend::types::to_llvm_type;
            to_llvm_type(self.context, self.target_machine, typ)
        }
    }

    pub fn native_uint_type(&self) -> LLVMTypeRef {
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

    pub unsafe fn gen_object_file(&self, opts: &CodeGenOptions) -> Result<String, String> {
        if opts.optimize {
            self.optimize()?;
        }

        if opts.dump_ir {
            self.dump_module();
        }

        DirBuilder::new()
            .recursive(true)
            .create(&opts.build_dir)
            .map_err(|e| format!("Unable to create directory for {}: {}", opts.build_dir, e))?;

        let obj_file_name = format!("{}/{}.mhr.o", opts.build_dir, self.name);
        println!("  Building {}", obj_file_name);
        self.target_machine
            .emit_to_file(self.module, &obj_file_name)?;
        Ok(obj_file_name)
    }

    unsafe fn optimize(&self) -> Result<(), String> {
        use llvm::transforms::pass_manager_builder::*;

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
        use llvm::analysis::*;
        use std::ffi::CStr;
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

impl<'a> Drop for Context<'a> {
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
