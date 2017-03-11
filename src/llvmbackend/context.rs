use std::ffi::CString;
use std::rc::Rc;
use std::fs::DirBuilder;
use std::ptr;
use llvm::prelude::*;
use llvm::core::*;
use ast::Type;
use super::CodeGenOptions;
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
    pub target_machine: TargetMachine,
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
                stack: vec![StackFrame::new(ptr::null_mut())],
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

    /*
    pub fn add_function_alias(&mut self, alias: &str, f: Rc<FunctionInstance>)
    {
        self.stack.last_mut().expect("Stack is empty").symbols.add_function_alias(alias, f)
    }
    */

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

    pub fn dump_module(&self)
    {
        println!("LLVM IR: {}", self.name);
        // Dump the module as IR to stdout.
        unsafe {
            LLVMDumpModule(self.module);
        }
        println!("----------------------");
    }


    pub unsafe fn gen_object_file(&self, opts: &CodeGenOptions) -> Result<String, String>
    {
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


        let obj_file_name = format!("{}/{}.cobra.o", opts.build_dir, self.name);
        println!("  Building {}", obj_file_name);
        self.target_machine.emit_to_file(self.module, &obj_file_name)?;
        Ok(obj_file_name)
    }

    unsafe fn optimize(&self) -> Result<(), String>
    {
        use llvm::transforms::pass_manager_builder::*;
        use llvm::target::LLVMAddTargetData;

        let pass_builder = LLVMPassManagerBuilderCreate();
        LLVMPassManagerBuilderSetOptLevel(pass_builder, 3);
        LLVMPassManagerBuilderSetSizeLevel(pass_builder, 0);

        let function_passes = LLVMCreateFunctionPassManagerForModule(self.module);
        let module_passes = LLVMCreatePassManager();
        let lto_passes = LLVMCreatePassManager();

        LLVMAddTargetData(self.target_machine.target_data, module_passes);

        LLVMPassManagerBuilderPopulateFunctionPassManager(pass_builder, function_passes);
        LLVMPassManagerBuilderPopulateModulePassManager(pass_builder, module_passes);
        LLVMPassManagerBuilderPopulateLTOPassManager(pass_builder, lto_passes, 1, 1);
        LLVMPassManagerBuilderDispose(pass_builder);

        LLVMInitializeFunctionPassManager(function_passes);

        let mut func = LLVMGetFirstFunction(self.module);
        while func != ptr::null_mut() {
            LLVMRunFunctionPassManager(function_passes, func);
            func = LLVMGetNextFunction(func);
        }

        LLVMRunPassManager(module_passes, self.module);
        LLVMRunPassManager(lto_passes, self.module);
        LLVMDisposePassManager(function_passes);
        LLVMDisposePassManager(module_passes);
        LLVMDisposePassManager(lto_passes);
        Ok(())
    }

    pub fn verify(&self) -> Result<(), String>
    {
        use llvm::analysis::*;
        use libc::c_char;
        use std::ffi::CStr;
        unsafe {
            let mut error_message: *mut c_char = ptr::null_mut();
            if LLVMVerifyModule(self.module, LLVMVerifierFailureAction::LLVMReturnStatusAction, &mut error_message) != 0 {

                self.dump_module();

                let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
                let e = format!("Module verification error: {}", msg);
                LLVMDisposeMessage(error_message);
                Err(e)
            } else {
                Ok(())
            }
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
