//use llvm::prelude::*;
use llvm::core::*;
use ast::Module;
use compileerror::CompileResult;

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

pub struct CodeGenOptions
{
    pub build_dir: String,
    pub program_name: String,
    pub runtime_library: String,
    pub dump_ir: bool,
    pub optimize: bool,
}

pub fn codegen(_m: &Module, _opts: &CodeGenOptions) -> CompileResult<()>
{
    llvm_init();
    /*
    unsafe {
        // Set up a context, module and builder in that context.
        let mut ctx = Context::new(&m.name);
        try!(gen_module(&mut ctx, m));

        match ctx.verify()
        {
            Err(e) => {
                LLVMDumpModule(ctx.get_module_ref());
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
            LLVMDumpModule(ctx.get_module_ref());
            println!("----------------------");
        }


        Ok(ctx)
    }
    */
    Ok(())
}
