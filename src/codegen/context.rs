use std::rc::Rc;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::ptr;
use std::mem;

use llvm::prelude::*;
use llvm::core::*;
use llvm::target_machine::*;

use ast::*;
use compileerror::*;
use codegen::cstr;
use codegen::modulecontext::*;
use codegen::symbols::*;

pub struct Context
{
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    modules: Vec<Box<ModuleContext>>,
    current_module: Box<ModuleContext>,
}

impl Context
{
    pub fn new(name: &str) -> Context
    {
        unsafe {
            let context = LLVMContextCreate();
            Context{
                context: context,
                modules: Vec::new(),
                current_module: Box::new(ModuleContext::new(context, name, "::")),
                builder: LLVMCreateBuilderInContext(context),
            }
        }
    }

    pub fn get_current_module_ref(&self) -> LLVMModuleRef
    {
        self.current_module.module
    }

    // Set the current module, return the old module
    pub fn set_current_module(&mut self, new_current: Box<ModuleContext>) -> Box<ModuleContext>
    {
        mem::replace(&mut self.current_module, new_current)
    }

    pub fn add_module(&mut self, mc: Box<ModuleContext>)
    {
        self.modules.push(mc);
    }

    pub fn verify_modules(&mut self) -> Result<(), CompileError>
    {
        for md in &self.modules {
            try!(md.verify_module());
        }
        self.current_module.verify_module()
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

    pub fn optimize(&self) -> Result<(), CompileError>
    {
        for md in &self.modules {
            try!(md.optimize());
        }
        self.current_module.optimize()
    }


    pub unsafe fn gen_object_files(&self, build_dir: &str) -> Result<Vec<String>, CompileError>
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
            return err(Pos::zero(), ErrorType::CodegenError(e));
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
            return err(Pos::zero(), ErrorType::CodegenError(e));
        }

        let mut obj_files = Vec::new();

        for md in &self.modules {
            obj_files.push(try!(md.gen_object_file(target_machine, build_dir)));
        }
        obj_files.push(try!(self.current_module.gen_object_file(target_machine, build_dir)));

        LLVMDisposeTargetMachine(target_machine);
        Ok(obj_files)
    }


    pub fn push_stack_frame(&mut self, fun: LLVMValueRef)
    {
        self.current_module.push_stack_frame(fun);
    }

    pub fn pop_stack_frame(&mut self)
    {
        self.current_module.pop_stack_frame();
    }

    pub fn in_global_context(&self) -> bool
    {
        self.current_module.in_global_context()
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>>
    {
        let v = self.current_module.get_variable(name, true);
        if v.is_some() {
            return v;
        }

        for md in &self.modules {
            let v = md.get_variable(name, false);
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

    pub fn add_variable(&mut self, name: &str, value: LLVMValueRef, constant: bool, public: bool, typ: Type)
    {
        let var = Rc::new(VariableInstance{
            value: value,
            name: name.into(),
            constant: constant,
            public: public,
            typ: typ,
        });
        self.current_module.add_variable(var);
    }

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>>
    {
        let func = self.current_module.get_function(name, true);
        if func.is_some() {
            return func;

        }

        for md in &self.modules {
            let func = md.get_function(name, false);
            if func.is_some() {
                return func;
            }
        }
        None
    }

    pub fn has_function(&self, name: &str) -> bool
    {
        self.get_function(name).is_some()
    }

    pub fn add_function(&mut self, fi: FunctionInstance)
    {
        self.current_module.add_function(Rc::new(fi));
    }

    pub fn get_complex_type(&self, name: &str) -> Option<Rc<StructType>>
    {
        let ct = self.current_module.get_complex_type(name, true);
        if ct.is_some() {
            return ct;
        }

        for md in &self.modules {
            let ct = md.get_complex_type(name, false);
            if ct.is_some() {
                return ct;
            }
        }

        None
    }

    pub fn add_complex_type(&mut self, st: StructType)
    {
        self.current_module.add_complex_type(Rc::new(st));
    }

    pub fn get_current_function(&self) -> LLVMValueRef
    {
        self.current_module.get_current_function()
    }
}

impl Drop for Context
{
    fn drop(&mut self)
    {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMContextDispose(self.context);
        }
    }
}
