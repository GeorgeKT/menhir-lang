use std::ptr;
use std::rc::Rc;
use std::os::raw::{c_char, c_uint};
use std::ffi::{CStr, CString};
use std::fs::DirBuilder;

use llvm::prelude::*;
use llvm::core::*;
use llvm::target::*;

use ast::{Type, array_type};
use codegen::{ValueRef, CodeGenOptions, TargetMachine, const_int};
use compileerror::{CompileResult, CompileError, ErrorCode, err};
use codegen::symboltable::{VariableInstance, FunctionInstance, SymbolTable};
use span::Span;


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
    target_machine: TargetMachine,
    name: String,
    stack: Vec<StackFrame>,
    builtins: SymbolTable,
}

impl Context
{
	pub fn new(module_name: &str) -> CompileResult<Context>
	{
		unsafe {
            let cname = CString::new(module_name).expect("Invalid module name");
            let context = LLVMContextCreate();
            let target_machine = try!(TargetMachine::new());
            Ok(Context{
                context: context,
                module: LLVMModuleCreateWithNameInContext(cname.as_ptr(), context),
                builder: LLVMCreateBuilderInContext(context),
                target_machine: target_machine,
                name: module_name.into(),
                stack: vec![StackFrame::new(ptr::null_mut())],
                builtins: SymbolTable::new(),
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

    pub fn add_builtin(&mut self, f: Rc<FunctionInstance>)
    {
        self.builtins.add_function(f);
    }

    pub fn get_builtin(&self, name: &str) -> Rc<FunctionInstance>
    {
        self.builtins
            .get_function(name)
            .expect("Internal Compiler Error: Attempting to get unknown builtin function")
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

    pub unsafe fn stack_alloc(&self, typ: LLVMTypeRef, name: &str) -> LLVMValueRef
    {
        let func = self.get_current_function();
        let entry_bb = LLVMGetEntryBasicBlock(func);
        let current_bb = LLVMGetInsertBlock(self.builder);
        // We allocate in the entry block
        LLVMPositionBuilder(self.builder, entry_bb, LLVMGetFirstInstruction(entry_bb));

        let name = CString::new(name).expect("Invalid string");
        let alloc = LLVMBuildAlloca(self.builder, typ, name.as_ptr());
        LLVMPositionBuilderAtEnd(self.builder, current_bb); // Position the builder where it was before
        alloc
    }

    pub unsafe fn heap_alloc(&self, typ: LLVMTypeRef, name: &str) -> LLVMValueRef
    {
        let arc_alloc = self.get_builtin("arc_alloc");
        let size = self.target_machine.size_of_type(typ);
        let mut args = vec![
            const_int(self, size as u64)
        ];
        let name = CString::new(name).expect("Invalid string");
        let void_ptr = LLVMBuildCall(self.builder, arc_alloc.function, args.as_mut_ptr(), 1, name.as_ptr());
        LLVMBuildBitCast(self.builder, void_ptr, LLVMPointerType(typ, 0), cstr!("cast_to_ptr"))
    }

    pub unsafe fn heap_alloc_array(&self, element_type: LLVMTypeRef, len: LLVMValueRef, name: &str) -> LLVMValueRef
    {
        let arc_alloc = self.get_builtin("arc_alloc");
        let size = self.target_machine.size_of_type(element_type);
        let mut args = vec![
            LLVMBuildMul(self.builder, len, const_int(self, size as u64), cstr!("array_len"))
        ];
        let name = CString::new(name).expect("Invalid string");
        let void_ptr = LLVMBuildCall(self.builder, arc_alloc.function, args.as_mut_ptr(), 1, name.as_ptr());
        LLVMBuildBitCast(self.builder, void_ptr, LLVMPointerType(element_type, 0), cstr!("cast_to_ptr"))
    }

    pub unsafe fn gen_object_file(&self, opts: &CodeGenOptions) -> CompileResult<String>
    {
        if opts.optimize {
            try!(self.optimize());
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
                &Span::default(),
                ErrorCode::CodegenError,
                format!("Unable to create directory for {}: {}", opts.build_dir, e))));


        let obj_file_name = format!("{}/{}.cobra.o", opts.build_dir, self.name);
        println!("  Building {}", obj_file_name);
        try!(self.target_machine.emit_to_file(self.module, &obj_file_name));
        Ok(obj_file_name)
    }

    pub unsafe fn optimize(&self) -> CompileResult<()>
    {
        use llvm::transforms::pass_manager_builder::*;

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

    pub fn verify(&self) -> CompileResult<()>
    {
        use llvm::analysis::*;
        unsafe {
            let mut error_message: *mut c_char = ptr::null_mut();
            if LLVMVerifyModule(self.module, LLVMVerifierFailureAction::LLVMReturnStatusAction, &mut error_message) != 0 {
                let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
                let e = format!("Module verification error: {}", msg);
                LLVMDisposeMessage(error_message);
                err(&Span::default(), ErrorCode::CodegenError, e)
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

    pub unsafe fn size_of_type(&self, typ: &Type) -> usize
    {
        let llvm_type = self.resolve_type(typ);
        self.target_machine.size_of_type(llvm_type)
    }

    pub unsafe fn resolve_type(&self, typ: &Type) -> LLVMTypeRef
    {
        match *typ
        {
            Type::Void => LLVMVoidTypeInContext(self.context),
            Type::VoidPtr => LLVMPointerType(LLVMVoidTypeInContext(self.context), 0),
            Type::Int => LLVMInt64TypeInContext(self.context),
            Type::Bool => LLVMInt1TypeInContext(self.context),
            Type::Float => LLVMDoubleTypeInContext(self.context),
            Type::Char => LLVMInt8TypeInContext(self.context),
            Type::EmptyArray => self.resolve_type(&array_type(Type::Int)),
            Type::Array(ref at) => {
                let element_type = self.resolve_type(&at.element_type);
                let mut member_types = vec![
                    LLVMPointerType(element_type, 0),      // Pointer to data
                    LLVMInt64TypeInContext(self.context),  // Length of string
                    LLVMInt64TypeInContext(self.context),  // Offset in data pointer
                    LLVMInt1TypeInContext(self.context),   // Heap allocated flag
                ];
                LLVMStructType(member_types.as_mut_ptr(), member_types.len() as c_uint, 0)
            },
            Type::Func(ref ft) => {
                let mut llvm_arg_types = Vec::with_capacity(ft.args.len());
                for arg in &ft.args {
                    llvm_arg_types.push(self.resolve_type(arg));
                }

                LLVMPointerType(
                    LLVMFunctionType(
                        self.resolve_type(&ft.return_type),
                        llvm_arg_types.as_mut_ptr(),
                        ft.args.len() as c_uint, 0
                    ),
                    0
                )
            },
            Type::Struct(ref st) => {
                let mut llvm_member_types = Vec::with_capacity(st.members.len());
                for m in &st.members {
                    llvm_member_types.push(self.resolve_type(&m.typ));
                }
                LLVMStructType(llvm_member_types.as_mut_ptr(), llvm_member_types.len() as c_uint, 0)
            },
            Type::Sum(ref st) => {
                let mut member_types = vec![LLVMInt64TypeInContext(self.context)]; // first entry is the tag

                // Calculate the biggest type
                let mut largest_type = ptr::null_mut();
                for c in &st.cases {
                    let case_typ = self.resolve_type(&c.typ);
                    if largest_type == ptr::null_mut() || self.target_machine.size_of_type(case_typ) > self.target_machine.size_of_type(largest_type) {
                        largest_type = case_typ;
                    }
                }

                // Use the largest type, we will cast to the other case types
                member_types.push(largest_type);
                LLVMStructType(member_types.as_mut_ptr(), member_types.len() as c_uint, 0)
            },
            Type::Enum(_) => {
                LLVMInt64TypeInContext(self.context)
            },
            Type::Generic(_) => {
                panic!("Internal Compiler Error: All generic types must have been resolved before code generation")
            },
            Type::Unresolved(_) => panic!("Internal Compiler Error: All types must be resolved before code generation"),
            Type::Unknown => panic!("Internal Compiler Error: all types must be known before code generation"),
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
