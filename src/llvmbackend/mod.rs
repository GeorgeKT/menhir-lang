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

/*
Disabled for now, the ORC jit stuff is crashing
#[cfg(test)]
mod tests;
#[cfg(test)]
mod jit;
*/

use std::process::{Output, Command};
use llvm::core::*;

use bytecode::{ByteCodeModule};
pub use self::target::TargetMachine;
use self::valueref::ValueRef;
use self::function::{gen_function, gen_function_sig, add_libc_functions};
use self::context::Context;

#[derive(Copy, Clone, Debug)]
pub enum OutputType
{
    Binary,
    StaticLib,
    SharedLib,
}

pub struct CodeGenOptions
{
    pub build_dir: String,
    pub output_file_name: String,
    pub output_type: OutputType,
    pub dump_ir: bool,
    pub optimize: bool,
}


pub fn llvm_init() -> Result<TargetMachine, String>
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
        TargetMachine::new()
    }
}

pub fn llvm_code_generation<'a>(bc_mod: &ByteCodeModule, target_machine: &'a TargetMachine) -> Result<Context<'a>, String>
{
    let mut ctx = Context::new(&bc_mod.name, target_machine)?;

    unsafe {
        add_libc_functions(&mut ctx);

        for (glob_name, glob_val) in &bc_mod.globals {
            let v = ValueRef::from_const(&ctx, glob_val);
            ctx.add_variable(glob_name, v);
        }

        for func in bc_mod.functions.values() {
            if func.sig.name == bc_mod.main_function_name() {
                gen_function_sig(&mut ctx, &func.sig, Some("main"));
            } else {
                gen_function_sig(&mut ctx, &func.sig, None);
            }
        }

        for func in bc_mod.functions.values() {
            gen_function(&mut ctx, func);
        }

        ctx.verify()?;
    }

    Ok(ctx)
}


pub fn link(ctx: &Context, opts: &CodeGenOptions) -> Result<(), String>
{
    let obj_file = unsafe{
        ctx.gen_object_file(opts)?
    };

    let output_file_path = format!("{}/{}", opts.build_dir, opts.output_file_name);

    let mut cmd = match opts.output_type {
        OutputType::Binary => {
            let mut cmd = Command::new("gcc");
            cmd.arg("-o").arg(&output_file_path).arg(obj_file);
            cmd
        },

        OutputType::StaticLib => {
            let mut cmd = Command::new("ar");
            cmd.arg("rcs").arg(&output_file_path).arg(obj_file);
            cmd
        }

        OutputType::SharedLib => {
            let mut cmd = Command::new("gcc");
            cmd.arg("-shared").arg("-o").arg(&output_file_path).arg(obj_file);
            cmd
        }
    };

    println!("  Linking {}", output_file_path);
    let output: Output = cmd
        .output()
        .map_err(|e| format!("Unable to spawn the linker: {}", e))?;


    if !output.status.success() {
        let out = String::from_utf8(output.stderr).expect("Invalid stdout from ld");
        let msg = format!("Linking {} failed:\n{}", output_file_path, out);
        return Err(msg);
    }

    Ok(())
}
