use std::rc::Rc;
use llvm::prelude::*;
use llvm::core::*;

use ast::*;
use compileerror::*;
use codegen::modulecontext::*;
use codegen::stackframe::*;
use codegen::symbols::*;

pub struct Context
{
    pub context: LLVMContextRef,
    pub builder: LLVMBuilderRef,
    imports: Vec<ModuleContext>,
    module: ModuleContext,
    stack: Vec<StackFrame>,
}

impl Context
{
    pub fn new(name: &str) -> Context
    {
        unsafe {
            let context = LLVMContextCreate();
            Context{
                context: context,
                imports: Vec::new(),
                module: ModuleContext::new(context, name, "::"),
                builder: LLVMCreateBuilderInContext(context),
                stack: Vec::new(),
            }
        }
    }

    pub fn get_module(&self) -> LLVMModuleRef
    {
        self.module.module
    }

    pub fn get_module_name(&self) -> String
    {
        self.module.name.clone()
    }

/*
    pub fn add_module(&mut self, name: &str, path: &str) -> LLVMModuleRef
    {
        let m = ModuleContext::new(self.context, name, path);
        let mref = m.module;
        self.modules.push(m);
        mref
    }
*/
    pub fn push_stack_frame(&mut self, fun: LLVMValueRef, bb: LLVMBasicBlockRef)
    {
        self.stack.push(StackFrame::new(fun, bb));
    }

    pub fn pop_stack_frame(&mut self)
    {
        self.stack.pop();
    }

    pub fn top_stack_frame(&mut self) -> &mut StackFrame
    {
        self.stack.last_mut().expect("Empty stack")
    }

    pub fn get_variable(&self, name: &str) -> Option<Rc<VariableInstance>>
    {
        for sf in self.stack.iter().rev() {
            let v = sf.symbols.get_variable(name);
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

    pub fn get_function(&self, name: &str) -> Option<Rc<FunctionInstance>>
    {
        for sf in self.stack.iter().rev() {
            let f = sf.symbols.get_function(name);
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

    pub fn get_complex_type(&self, name: &str) -> Option<Rc<StructType>>
    {
        for sf in self.stack.iter().rev() {
            let f = sf.symbols.get_complex_type(name);
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

    pub fn optimize(&self) -> Result<(), CompileError>
    {
        unsafe {
            self.module.optimize()
        }
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
