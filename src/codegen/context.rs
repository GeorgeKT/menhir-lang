use std::ptr;
use std::rc::Rc;
use std::os::raw::{c_char, c_uint};
use std::ffi::{CStr, CString};
use std::fs::DirBuilder;
use std::collections::HashMap;

use llvm::prelude::*;
use llvm::core::*;
use llvm::target_machine::*;
use llvm::target::*;

use ast::{Type};
use codegen::{cstr, cstr_mut, type_name, ValueRef, CodeGenOptions};
use compileerror::{Pos, CompileResult, CompileError, ErrorCode, err};
use codegen::symboltable::{VariableInstance, FunctionInstance, SymbolTable};
use codegen::slice::{new_slice_type};

pub struct StackFrame
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
    name: String,
    stack: Vec<StackFrame>,
    slice_type_cache: HashMap<String, LLVMTypeRef>,
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
                stack: vec![StackFrame::new(ptr::null_mut())],
                slice_type_cache: HashMap::new(),
            }
        }
	}

    pub fn add_variable(&mut self, name: &str, vr: ValueRef)
    {
        let var = Rc::new(VariableInstance{
            value: vr,
            name: name.into(),
        });
        self.stack.last_mut().expect("Stack is empty").symbols.add_variable(var)
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

        panic!("No current function on stack, we should have caught this !");
    }

    pub unsafe fn alloc(&self, typ: LLVMTypeRef, name: &str) -> LLVMValueRef
    {
        let func = self.get_current_function();
        let entry_bb = LLVMGetEntryBasicBlock(func);
        let current_bb = LLVMGetInsertBlock(self.builder);
        // We allocate in the entry block
        LLVMPositionBuilder(self.builder, entry_bb, LLVMGetFirstInstruction(entry_bb));
        let alloc = LLVMBuildAlloca(self.builder, typ, cstr(name));
        LLVMPositionBuilderAtEnd(self.builder, current_bb); // Position the builder where it was before
        alloc
    }

    pub unsafe fn gen_object_file(&self, opts: &CodeGenOptions) -> CompileResult<String>
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

        if opts.optimize {
            try!(self.optimize(target_machine));
        }

        if opts.dump_ir {
            println!("LLVM IR: {}", self.name);
            // Dump the module as IR to stdout.
            LLVMDumpModule(self.module);
            println!("----------------------");
        }

        try!(DirBuilder::new()
            .recursive(true)
            .create(&opts.build_dir)
            .map_err(|e| CompileError::new(
                Pos::zero(),
                ErrorCode::CodegenError,
                format!("Unable to create directory for {}: {}", opts.build_dir, e))));


        let obj_file_name = format!("{}/{}.cobra.o", opts.build_dir, self.name);
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

    pub fn optimize(&self, target_machine: LLVMTargetMachineRef) -> CompileResult<()>
    {
        unsafe{
            use llvm::transforms::pass_manager_builder::*;

            let pass_builder = LLVMPassManagerBuilderCreate();
            LLVMPassManagerBuilderSetOptLevel(pass_builder, 3);
            LLVMPassManagerBuilderSetSizeLevel(pass_builder, 0);

            let function_passes = LLVMCreateFunctionPassManagerForModule(self.module);
            let module_passes = LLVMCreatePassManager();

            LLVMAddTargetData(LLVMGetTargetMachineData(target_machine), module_passes);

            LLVMPassManagerBuilderPopulateFunctionPassManager(pass_builder, function_passes);
            LLVMPassManagerBuilderPopulateModulePassManager(pass_builder, module_passes);

            LLVMPassManagerBuilderDispose(pass_builder);

            LLVMInitializeFunctionPassManager(function_passes);

            let mut func = LLVMGetFirstFunction(self.module);
            while func != ptr::null_mut() {
                LLVMRunFunctionPassManager(function_passes, func);
                func = LLVMGetNextFunction(func);
            }

            LLVMRunPassManager(module_passes, self.module);
            LLVMDisposePassManager(function_passes);
            LLVMDisposePassManager(module_passes);
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

    pub unsafe fn get_slice_type(&mut self, element_type: LLVMTypeRef) -> LLVMTypeRef
    {
        let name = format!("slice-{}", type_name(element_type));
        if let Some(t) = self.slice_type_cache.get(&name) {
            return *t;
        }

        let slice_type = new_slice_type(self.context, element_type);
        self.slice_type_cache.insert(name, slice_type);
        slice_type
    }

    pub unsafe fn resolve_type(&mut self, typ: &Type) -> LLVMTypeRef
    {
        match *typ
        {
            Type::Void => LLVMVoidTypeInContext(self.context),
            Type::Int => LLVMInt64TypeInContext(self.context),
            Type::Bool => LLVMInt1TypeInContext(self.context),
            Type::Float => LLVMDoubleTypeInContext(self.context),
            Type::Array(ref et, len) => {
                LLVMArrayType(self.resolve_type(et), len as u32)
            },
            Type::Slice(ref et) => {
                let e = self.resolve_type(et);
                self.get_slice_type(e)
            },
            Type::Func(ref args, ref ret) => {
                let mut llvm_arg_types = Vec::with_capacity(args.len());
                for arg in args {
                    llvm_arg_types.push(self.resolve_type(arg));
                }

                LLVMFunctionType(self.resolve_type(ret), llvm_arg_types.as_mut_ptr(), args.len() as c_uint, 0)
            },
            Type::Struct(ref members) => {
                let mut llvm_member_types = Vec::with_capacity(members.len());
                for m in members {
                    llvm_member_types.push(self.resolve_type(&m.typ));
                }
                LLVMStructType(llvm_member_types.as_mut_ptr(), llvm_member_types.len() as c_uint, 0)
            },
            Type::String => panic!("Not yet implemented"),
            Type::Generic(_) => panic!("Internal Compiler Error: All generic types must have been resolved before code generation"),
            Type::Unresolved(_) => panic!("Internal Compiler Error: All types must be resolved before code generation"),
            Type::Unknown => panic!("Internal Compiler Error: all types must be known before code generation"),
        }
    }

    pub fn in_global_context(&self) -> bool
    {
        self.stack.len() == 1
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
