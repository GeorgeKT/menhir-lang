use std::rc::Rc;
use llvm::prelude::*;
use compileerror::*;
use codegen::symbols::*;
use codegen::stackframe::*;
use codegen::context::*;
use ast::*;


pub struct ModuleContext
{
    namespace: String,
    public_symbols: SymbolTable,
    private_symbols: SymbolTable,
    stack: Vec<StackFrame>,
}

impl ModuleContext
{
    pub fn new(namespace: &str) -> ModuleContext
    {
        ModuleContext{
            namespace: namespace.into(),
            public_symbols: SymbolTable::new(),
            private_symbols: SymbolTable::new(),
            stack: Vec::new(),
        }
    }

    pub fn in_global_context(&self) -> bool
    {
        self.stack.is_empty()
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
}

pub fn import_module(ctx: &mut Context, m: &ModuleName) -> Result<(), CompileError>
{
    use parser::*;
    use codegen::statements::gen_module;

    let path = m.parts.join("/") + ".cobra";
    let module = try!(parse_file(&path, ParseMode::Module));
    let namespace = m.parts.join("::") + "::";
    let mc = Box::new(ModuleContext::new(&namespace));
    let old_mc = ctx.set_current_module(mc);
    unsafe {
        try!(gen_module(ctx, &module));
    }

    let mc = ctx.set_current_module(old_mc);
    ctx.add_module(mc);
    Ok(())
}
