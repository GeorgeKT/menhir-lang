macro_rules! cstr {
    ($lit:expr) => {
        {
            use std::ffi::CStr;
            use libc::c_char;
            CStr::from_bytes_with_nul_unchecked(concat!($lit, "\0").as_bytes()).as_ptr() as *const c_char
        }
    }
}


mod context;
mod function;
mod instructions;
mod symboltable;
mod target;
mod types;
mod valueref;

use llvm::core::*;

use libcobra::bytecode::{START_CODE_FUNCTION, ByteCodeModule};
use self::function::{gen_function, gen_function_sig};
use self::context::Context;

pub struct CodeGenOptions
{
    pub build_dir: String,
    pub program_name: String,
    pub dump_ir: bool,
    pub optimize: bool,
}

fn llvm_init()
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


pub fn llvm_code_generation(bc_mod: &ByteCodeModule, options: &CodeGenOptions) -> Result<(), String>
{
    llvm_init();

    let mut ctx = Context::new(&bc_mod.name)?;

    unsafe {
        for func in bc_mod.functions.values() {
            if func.sig.name != START_CODE_FUNCTION {
                gen_function_sig(&mut ctx, &func.sig);
            }
        }

        for func in bc_mod.functions.values() {
            if func.sig.name != START_CODE_FUNCTION {
                gen_function(&mut ctx, func);
            }
        }

        ctx.verify()?;
        let _object_file = ctx.gen_object_file(options)?;
    }

    Ok(())
}
