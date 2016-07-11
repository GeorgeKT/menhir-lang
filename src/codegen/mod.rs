mod statements;
mod expressions;
mod linker;

use std::rc::Rc;
use std::collections::HashMap;
use std::ffi::CString;
use std::os::raw::c_char;
use std::marker::PhantomData;

use llvm::prelude::*;
use llvm::core::*;

use ast::*;
use compileerror::*;

pub use self::statements::*;
pub use self::expressions::*;

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
    pub name: String,
    pub constant: bool,
    pub typ: Type,
}

pub struct FunctionInstance
{
    pub function: LLVMValueRef,
    pub name: String,
    pub args: Vec<LLVMTypeRef>,
    pub return_type: LLVMTypeRef,
    pub sig: FunctionSignature,
    pub public: bool,
}

pub struct StructMemberVar
{
    pub name: String,
    pub typ: Type,
    pub llvm_typ: LLVMTypeRef,
    pub constant: bool,
    pub public: bool,
    pub init: Expression,
}

pub struct StructType
{
    pub name: String,
    pub typ: LLVMTypeRef,
    pub members: Vec<Rc<StructMemberVar>>,
}

impl StructType
{
    pub fn get_member(&self, name: &str) -> Option<(usize, Rc<StructMemberVar>)>
    {
        for (idx, m) in self.members.iter().enumerate() {
            if m.name == name {
                return Some((idx, m.clone()));
            }
        }

        None
    }
}

pub struct StackFrame
{
    function: LLVMValueRef,
    vars: HashMap<String, Rc<VariableInstance>>,
    funcs: HashMap<String, FunctionInstance>,
    complex_types: HashMap<String, Rc<StructType>>,
    current_bb: LLVMBasicBlockRef,
}

impl StackFrame
{
    pub unsafe fn return_type(&self) -> LLVMTypeRef
    {
        LLVMGetReturnType(LLVMGetElementType(LLVMTypeOf(self.function)))
    }

    pub fn add_variable(&mut self, name: &str, value: LLVMValueRef, constant: bool, typ: Type)
    {
        self.vars.insert(name.into(), Rc::new(VariableInstance{
            value: value,
            name: name.into(),
            constant: constant,
            typ: typ,
        }));
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>>
    {
        self.vars.get(name).map(|v| v.clone())
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

    pub fn get_current_function(&self) -> LLVMValueRef
    {
        self.function
    }

    pub fn get_complex_type(&self, name: &str) -> Option<Rc<StructType>>
    {
        self.complex_types.get(name).map(|st| st.clone())
    }

    pub fn add_complex_type(&mut self, st: StructType)
    {
        self.complex_types.insert(st.name.clone(), Rc::new(st));
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
                module: LLVMModuleCreateWithNameInContext(cname.as_ptr(), context),
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
            complex_types: HashMap::new(),
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

    pub fn get_variable(&'a self, name: &str) -> Option<Rc<VariableInstance>>
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

    fn get_complex_type(&'a self, name: &str) -> Option<Rc<StructType>>
    {
        for sf in self.stack.iter().rev() {
            let f = sf.get_complex_type(name);
            if f.is_some() {
                return f;
            }
        }
        None
    }

    pub unsafe fn resolve_type(&self, typ: &Type) -> Option<LLVMTypeRef>
    {
        match *typ
        {
            Type::Void => Some(LLVMVoidTypeInContext(self.context)),
            Type::Primitive(ref name) =>
                match &name[..] {
                    "uint8" | "int8" | "char" | "byte" => Some(LLVMInt8TypeInContext(self.context)),
                    "uint16" | "int16" => Some(LLVMInt16TypeInContext(self.context)),
                    "uint32" | "int32" => Some(LLVMInt32TypeInContext(self.context)),
                    "int" | "uint" | "uint64"| "bool" => Some(LLVMInt64TypeInContext(self.context)),
                    "float" => Some(LLVMFloatTypeInContext(self.context)),
                    "double" => Some(LLVMDoubleTypeInContext(self.context)),
                    _ => None,
                },

            Type::Pointer(ref st) => {
                self.resolve_type(&st).map(|t| LLVMPointerType(t, 0))
            },
            Type::Complex(ref name) => {
                self.get_complex_type(&name).map(|ct| ct.typ)
            },
            _ => None,
        }
    }

    fn infer_member_var_type(&self, st: &StructType, nr: &NameRef) -> Result<Type, CompileError>
    {
        if let Some((_, mvar)) = st.get_member(&nr.name) {
            if mvar.typ == Type::Unknown {
                self.infer_type(&mvar.init)
            } else {
                Ok(mvar.typ.clone())
            }
        } else {
            err(nr.span.start, ErrorType::UnknownStructMember(st.name.clone(), nr.name.clone()))
        }
    }

    fn infer_nested_member_type(&self, st: &StructType, next: &MemberAccess) -> Result<Type, CompileError>
    {
        if let Some((_, mvar)) = st.get_member(&next.name) {
            let st = match mvar.typ {
                Type::Complex(ref ctype) => try!(self.get_complex_type(&ctype).ok_or(CompileError::new(next.span.start, ErrorType::TypeError(format!("Unknown type {}", ctype))))),
                _ => return err(next.span.start, ErrorType::TypeError(format!("Member variable {} is not a struct", next.name))),
            };

            match next.member
            {
                Member::Call(ref c) => self.infer_call_type(c),
                Member::Var(ref nr) => self.infer_member_var_type(&st, nr),
                Member::Nested(ref next) => self.infer_nested_member_type(&st, next),
            }
        } else {
            err(next.span.start, ErrorType::UnknownStructMember(st.name.clone(), next.name.clone()))
        }
    }

    fn infer_member_type(&self, a: &MemberAccess) -> Result<Type, CompileError>
    {
        if let Some(ref v) = self.get_variable(&a.name) {
            match v.typ
            {
                Type::Complex(ref ctype) => {
                    let st = try!(self.get_complex_type(&ctype).ok_or(CompileError::new(a.span.start, ErrorType::TypeError(format!("Unknown type {}", ctype)))));
                    match a.member
                    {
                        Member::Call(ref c) => self.infer_call_type(c),
                        Member::Var(ref nr) => self.infer_member_var_type(&st, nr),
                        Member::Nested(ref next) => self.infer_nested_member_type(&st, next),
                    }
                },
                _ => err(a.span.start, ErrorType::TypeError(format!("Variable {} is not a struct", a.name))),
            }
        } else {
            err(a.span.start, ErrorType::UnknownVariable(a.name.clone()))
        }
    }

    fn infer_call_type(&self, c: &Call) -> Result<Type, CompileError>
    {
        if let Some(f) = self.get_function(&c.name) {
            Ok(f.sig.return_type.clone())
        } else {
            err(c.span.start, ErrorType::UnknownFunction(c.name.clone()))
        }
    }

    pub fn infer_type(&self, e: &Expression) -> Result<Type, CompileError>
    {
        match *e
        {
            Expression::IntLiteral(_, _) => Ok(Type::Primitive("int".into())),
            Expression::FloatLiteral(_, _) => Ok(Type::Primitive("double".into())),
            Expression::StringLiteral(_, _) => Ok(Type::Primitive("string".into())),
            Expression::UnaryOp(ref op) => self.infer_type(&op.expression),
            Expression::PostFixUnaryOp(ref op) => self.infer_type(&op.expression),
            Expression::BinaryOp(ref op) => {
                let lt = try!(self.infer_type(&op.left));
                let rt = try!(self.infer_type(&op.right));
                if lt != rt {
                    let msg = format!("Type mismatch in '{}' operation  (left hand side {}, right hand side {})", op.operator, lt, rt);
                    err(op.span.start, ErrorType::TypeError(msg))
                } else {
                    Ok(rt)
                }
            },
            Expression::Enclosed(_, ref e) => self.infer_type(e),
            Expression::Call(ref c) => self.infer_call_type(c),
            Expression::NameRef(ref nr) => {
                if let Some(v) = self.get_variable(&nr.name) {
                    Ok(v.typ.clone())
                } else {
                    err(nr.span.start, ErrorType::UnknownVariable(nr.name.clone()))
                }
            },
            Expression::Assignment(ref a) => self.infer_type(&a.expression),
            Expression::ObjectConstruction(ref oc) => {
                Ok(Type::ptr(Type::Complex(oc.object_type.clone())))
            },
            Expression::MemberAccess(ref ma) => {
                self.infer_member_type(ma)
            },
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
    pub optimize: bool,
}


unsafe fn optimize_module(ctx: &Context) -> Result<(), CompileError>
{
    use std::ptr;
    use llvm::transforms::pass_manager_builder::*;

    let pmb = LLVMPassManagerBuilderCreate();
    let pm = LLVMCreateFunctionPassManagerForModule(ctx.module);
    LLVMInitializeFunctionPassManager(pm);

    LLVMPassManagerBuilderSetOptLevel(pmb, 2);
    LLVMPassManagerBuilderPopulateFunctionPassManager(pmb, pm);

    let mut func = LLVMGetFirstFunction(ctx.module);
    while func != ptr::null_mut() {
        LLVMRunFunctionPassManager(pm, func);
        func = LLVMGetNextFunction(func);
    }

    LLVMDisposePassManager(pm);
    LLVMPassManagerBuilderDispose(pmb);
    Ok(())
}


pub fn codegen(prog: &Program, opts: &CodeGenOptions) -> Result<(), CompileError>
{
    use self::linker::*;

    unsafe {
        llvm_init();
        // Set up a context, module and builder in that context.
        let mut ctx = Context::new(&prog.name);
        try!(gen_program(&mut ctx, prog));

        if opts.optimize {
            try!(optimize_module(&mut ctx));
        }

        if opts.dump_ir {
            println!("LLVM IR:");
            ctx.dump();
            println!("----------------------");
        }

        link(&ctx, &opts.build_dir, &opts.program_name, &opts.runtime_library)
    }
}
