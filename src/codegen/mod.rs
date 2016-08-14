mod context;
mod expressions;
mod linker;
mod slice;
mod symboltable;
mod valueref;
#[cfg(test)]
mod tests;

use std::os::raw::c_char;
use std::ffi::{CString, CStr};

use llvm::prelude::*;
use llvm::core::*;

use ast::Module;
use compileerror::CompileResult;
use codegen::expressions::gen_expression;

pub use codegen::expressions::const_int;
pub use codegen::context::{Context};
pub use codegen::linker::link;
pub use codegen::valueref::ValueRef;


pub fn cstr(s: &str) -> *const c_char
{
    CString::new(s).expect("Valid C string").as_ptr()
}

pub fn cstr_mut(s: &str) -> *mut c_char
{
    CString::new(s).expect("Valid C string").into_raw()
}

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

fn gen_module(ctx: &mut Context, module: &Module) -> CompileResult<()>
{
    for e in &module.expressions {
        try!(gen_expression(ctx, e));
    }

    Ok(())
}

pub fn codegen(m: &Module, opts: &CodeGenOptions) -> CompileResult<Context>
{
    unsafe {
        // Set up a context, module and builder in that context.
        let mut ctx = Context::new(&m.name);
        try!(gen_module(&mut ctx, m));

        match ctx.verify()
        {
            Err(e) => {
                LLVMDumpModule(ctx.module);
                return Err(e);
            }
            _ => (),
        }

        if opts.optimize {
            try!(ctx.optimize());
        }

        if opts.dump_ir {
            println!("LLVM IR: {}", m.name);
            // Dump the module as IR to stdout.
            LLVMDumpModule(ctx.module);
            println!("----------------------");
        }


        Ok(ctx)
    }
}
