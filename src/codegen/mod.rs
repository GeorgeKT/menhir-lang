macro_rules! cstr {
    ($lit:expr) => {
        {
            use std::ffi::CStr;
            use libc::c_char;
            CStr::from_bytes_with_nul_unchecked(concat!($lit, "\0").as_bytes()).as_ptr() as *const c_char
        }
    }
}


mod array;
mod builtin;
mod context;
mod instructions;
mod function;
mod linker;
mod symboltable;
mod structvalue;
mod sumtypevalue;
mod target;
mod valueref;
#[cfg(test)]
mod tests;

use std::ffi::{CStr};
use std::rc::Rc;

use llvm::prelude::*;
use llvm::core::*;

use ast::*;
use llrep::*;
use compileerror::{CompileResult};
use codegen::function::{gen_function, gen_function_sig};
use codegen::builtin::add_builtin_functions;


pub use codegen::instructions::{const_int, const_bool, gen_instruction};
pub use codegen::context::{Context};
pub use codegen::linker::link;
pub use codegen::valueref::ValueRef;
pub use codegen::array::Array;
pub use codegen::structvalue::StructValue;
pub use codegen::symboltable::{SymbolTable, FunctionInstance, VariableInstance};
pub use codegen::sumtypevalue::SumTypeValue;
pub use codegen::target::TargetMachine;


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
    pub dump_ir: bool,
    pub optimize: bool,
}

fn gen_module(ctx: &mut Context, module: &LLModule)
{
    unsafe {
        for func in &module.functions {
            let fi = Rc::new(gen_function_sig(ctx, &func.sig));
            ctx.add_function(fi);
            for lambda in &func.lambdas {
                let li = Rc::new(gen_function_sig(ctx, &lambda.sig));
                ctx.add_function(li);
            }
        }

        for func in module.functions.iter().filter(|f| !f.is_empty()) {
            for lambda in &func.lambdas {
                gen_function(ctx, lambda);
            }
            gen_function(ctx, func);
        }
    }
}

pub fn codegen(m: &LLModule) -> CompileResult<Context>
{
    unsafe {
        // Set up a context, module and builder in that context.
        let mut ctx = try!(Context::new(&m.name));
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
