mod statements;
mod expressions;
mod linker;

use std::collections::HashMap;
use std::ffi::CString;
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

pub fn cstr_mut(s: &str) -> *mut c_char
{
    CString::new(s).expect("Valid C string").into_raw()
}

pub struct VariableInstance
{
    pub value: LLVMValueRef,
    pub constant: bool,
}

pub struct FunctionInstance
{
    pub function: LLVMValueRef,
    pub name: String,
    pub args: Vec<LLVMTypeRef>,
    pub return_type: LLVMTypeRef,
}

pub struct StackFrame
{
    function: LLVMValueRef,
    vars: HashMap<String, VariableInstance>,
    funcs: HashMap<String, FunctionInstance>,
    current_bb: LLVMBasicBlockRef,
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

    pub fn add_function(&mut self, f: FunctionInstance)
    {
        let name = f.name.clone();
        self.funcs.insert(name, f);
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionInstance>
    {
        self.funcs.get(name)
    }

    pub fn set_current_bb(&mut self, bb: LLVMBasicBlockRef)
    {
        self.current_bb = bb;
    }

    pub fn get_current_bb(&self) -> LLVMBasicBlockRef
    {
        self.current_bb
    }
}

pub struct Context<'a>
{
    pub context: LLVMContextRef,
    pub module: LLVMModuleRef,
    pub module_name: String,
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
                module_name: name.into(),
                builder: LLVMCreateBuilderInContext(context),
                stack: Vec::new(),
                _pd: PhantomData{},
            }
        }
    }

    pub fn push_stack_frame(&mut self, fun: LLVMValueRef, bb: LLVMBasicBlockRef)
    {
        self.stack.push(StackFrame{
            function: fun,
            vars: HashMap::new(),
            funcs: HashMap::new(),
            current_bb: bb,
        });
    }

    pub fn pop_stack_frame(&mut self)
    {
        self.stack.pop();
    }

    pub fn top_stack_frame(&mut self) -> &mut StackFrame
    {
        self.stack.last_mut().expect("Empty stack")
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

    pub fn get_function(&'a self, name: &str) -> Option<&'a FunctionInstance>
    {
        for sf in self.stack.iter().rev() {
            let f = sf.get_function(name);
            if f.is_some() {
                return f;
            }
        }
        None
    }

    pub fn has_function(&self, name: &str) -> bool
    {
        self.get_function(name).is_some()
    }

    pub unsafe fn resolve_type(&self, typ: &Type) -> Option<LLVMTypeRef>
    {
        match *typ
        {
            Type::Void => Some(LLVMVoidTypeInContext(self.context)),
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

pub struct CodeGenOptions
{
    pub build_dir: String,
    pub program_name: String,
    pub runtime_library: String,
    pub dump_ir: bool,
}

pub fn codegen(prog: &Program, opts: &CodeGenOptions) -> Result<(), CompileError>
{
    use self::linker::*;

    unsafe {
        llvm_init();
        // Set up a context, module and builder in that context.
        let mut ctx = Context::new(&prog.name);
        try!(gen_program(&mut ctx, prog));
        if opts.dump_ir {
            ctx.dump();
        }
        link(&ctx, &opts.build_dir, &opts.program_name, &opts.runtime_library)
    }
}
