mod array;
mod context;
mod expressions;
mod linker;
mod symboltable;
mod structvalue;
mod sumtypevalue;
mod target;
mod valueref;
#[cfg(test)]
mod tests;

use std::os::raw::c_char;
use std::ffi::{CString, CStr};
use std::rc::Rc;

use llvm::prelude::*;
use llvm::core::*;

use ast::*;
use compileerror::{CompileResult};
use codegen::expressions::{gen_function, gen_function_sig};
use span::Span;

pub use codegen::expressions::const_int;
pub use codegen::context::{Context, CodeGenMode};
pub use codegen::linker::link;
pub use codegen::valueref::ValueRef;
pub use codegen::array::Array;
pub use codegen::structvalue::StructValue;
pub use codegen::sumtypevalue::SumTypeValue;
pub use codegen::target::TargetMachine;


pub fn cstr(s: &str) -> *const c_char
{
    CString::new(s).expect("Valid C string").as_ptr()
}

pub fn cstr_mut(s: &str) -> *mut c_char
{
    CString::new(s).expect("Valid C string").into_raw()
}

#[allow(unused)]
pub fn type_name(tr: LLVMTypeRef) -> String
{
    unsafe {
        let n = LLVMPrintTypeToString(tr);
        let name = CStr::from_ptr(n).to_str().expect("Invalid C String").to_owned();
        LLVMDisposeMessage(n);
        name
    }
}

pub fn llvm_init()
{
    unsafe {
        use llvm::initialization::*;
        use llvm::target::*;
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmPrinters();
        LLVM_InitializeAllAsmParsers();

        let pass_registry = LLVMGetGlobalPassRegistry();
        LLVMInitializeCore(pass_registry);
        LLVMInitializeTransformUtils(pass_registry);
        LLVMInitializeScalarOpts(pass_registry);
        LLVMInitializeObjCARCOpts(pass_registry);
        LLVMInitializeVectorization(pass_registry);
        LLVMInitializeInstCombine(pass_registry);
        LLVMInitializeIPO(pass_registry);
        LLVMInitializeInstrumentation(pass_registry);
        LLVMInitializeAnalysis(pass_registry);
        LLVMInitializeIPA(pass_registry);
        LLVMInitializeCodeGen(pass_registry);
        LLVMInitializeTarget(pass_registry);
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

fn add_builtin_functions(ctx: &mut Context)
{
    /*
    As defined in cobra-runtime:
    void* arc_alloc(size_t size);
    void arc_inc_ref(void* ptr);
    void arc_dec_ref(void* ptr);
    */

    let functions = vec![
        sig(
            "arc_alloc",
            Type::VoidPtr,
            vec![
                Argument::new("size".into(), Type::Int, Span::default())
            ],
            Span::default()
        ),
        sig(
            "arc_inc_ref",
            Type::Void,
            vec![
                Argument::new("ptr".into(), Type::VoidPtr, Span::default())
            ],
            Span::default()
        ),
        sig(
            "arc_dec_ref",
            Type::Void,
            vec![
                Argument::new("ptr".into(), Type::VoidPtr, Span::default())
            ],
            Span::default()
        )
    ];

    for func_sig in &functions {
        let instance = unsafe {
            gen_function_sig(ctx, &func_sig)
        };
        ctx.add_builtin(Rc::new(instance));
    }
}

fn gen_module(ctx: &mut Context, module: &Module)
{
    for ref func in module.functions.values() {
        if !func.is_generic() {
            unsafe {
                let fi = Rc::new(gen_function_sig(ctx, &func.sig));
                ctx.add_function(fi);
            }
        }
    }

    for ref func in module.externals.values() {
        unsafe {
            let fi = gen_function_sig(ctx, &func.sig);
            ctx.add_function(Rc::new(fi));
        }
    }

    for ref func in module.functions.values() {
        if !func.is_generic() {
            unsafe{
                gen_function(ctx, &func.sig, &func.expression);
            }
        }
    }
}

pub fn codegen(m: &Module, mode: CodeGenMode) -> CompileResult<Context>
{
    unsafe {
        // Set up a context, module and builder in that context.
        let mut ctx = try!(Context::new(&m.name, mode));
        add_builtin_functions(&mut ctx);
        gen_module(&mut ctx, m);

        match ctx.verify()
        {
            Err(e) => {
                LLVMDumpModule(ctx.module);
                return Err(e);
            }
            _ => (),
        }

        Ok(ctx)
    }
}
