macro_rules! cstr {
    ($lit:expr) => {{
        use libc::c_char;
        use std::ffi::CStr;
        CStr::from_bytes_with_nul_unchecked(concat!($lit, "\0").as_bytes()).as_ptr() as *const c_char
    }};
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

use itertools::Itertools;
use llvm_sys::core::*;
use llvm_sys::LLVMLinkage;
use std::ffi::CString;
use std::fmt;
use std::path::PathBuf;
use std::process::{Command, Output};

pub use self::context::Context;
use self::function::{gen_function, gen_function_sig};
pub use self::target::TargetMachine;
use self::valueref::ValueRef;
use crate::ast::ptr_type;
use crate::build::PackageDescription;
use crate::bytecode::{ByteCodeModule, Constant};
use crate::compileerror::{code_gen_error, CompileResult};
use serde_derive::{Deserialize, Serialize};

#[derive(Copy, Clone, Debug, Serialize, Deserialize, Default, PartialEq, Eq)]
pub enum OutputType {
    #[serde(rename = "binary")]
    #[default]
    Binary,
    #[serde(rename = "staticlib")]
    StaticLib,
    #[serde(rename = "sharedlib")]
    SharedLib,
}

impl fmt::Display for OutputType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OutputType::Binary => write!(f, "binary"),
            OutputType::SharedLib => write!(f, "sharedlib"),
            OutputType::StaticLib => write!(f, "staticlib"),
        }
    }
}

pub struct CodeGenOptions {
    pub build_dir: PathBuf,
    pub output_file_name: String,
    pub output_type: OutputType,
    pub dump_ir: bool,
    pub optimize: bool,
}

pub fn llvm_init() -> CompileResult<()> {
    unsafe {
        use llvm_sys::initialization::*;
        use llvm_sys::target::*;
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
        Ok(())
    }
}

pub fn llvm_shutdown() {
    unsafe {
        LLVMShutdown();
    }
}

unsafe fn gen_global(ctx: &mut Context, glob_name: &str, glob_value: &Constant) -> CompileResult<()> {
    let v = ValueRef::const_global(ctx, glob_value)?;
    let name = CString::new(glob_name.as_bytes()).map_err(|_| code_gen_error("Invalid string"))?;
    let glob = LLVMAddGlobal(ctx.module, ctx.resolve_type(&v.typ)?, name.as_ptr());
    LLVMSetLinkage(glob, LLVMLinkage::LLVMExternalLinkage);
    LLVMSetInitializer(glob, v.value);
    ctx.set_variable(glob_name, ValueRef::new(glob, ptr_type(v.typ)))
}

unsafe fn add_metadata(ctx: &mut Context, module_name: &str, desc: &PackageDescription) -> CompileResult<()> {
    let metadata = format!(
        "name:{};version:{};author:{};email:{};license:{}",
        desc.name, desc.version, desc.author, desc.email, desc.license
    );

    gen_global(
        ctx,
        &format!("__MENHIR_{}_BUILD_INFO__", module_name),
        &Constant::String(metadata),
    )?;
    Ok(())
}

pub fn llvm_code_generation<'a>(
    bc_mod: &ByteCodeModule,
    ctx: &mut Context,
    desc: &PackageDescription,
) -> CompileResult<()> {
    unsafe {
        for func in &bc_mod.imported_functions {
            gen_function_sig(ctx, &func.sig, None)?;
        }

        for (glob_name, glob_val) in &bc_mod.globals {
            gen_global(ctx, glob_name, glob_val)?;
        }

        add_metadata(ctx, &bc_mod.name, desc)?;

        for (_, func) in bc_mod.functions.iter().sorted_by_key(|x| x.0) {
            if func.sig.name == bc_mod.main_function_name() {
                gen_function_sig(ctx, &func.sig, Some("main"))?;
            } else {
                gen_function_sig(ctx, &func.sig, None)?;
            }
        }

        for (_, func) in bc_mod.functions.iter().sorted_by_key(|x| x.0) {
            if !func.external {
                gen_function(ctx, func)?;
            }
        }

        ctx.verify()?;
    }

    Ok(())
}

#[derive(Default)]
pub struct LinkerFlags {
    pub linker_paths: Vec<PathBuf>,
    pub linker_shared_libs: Vec<String>,
    pub linker_static_libs: Vec<PathBuf>,
}

impl LinkerFlags {
    pub fn add_flags(&self, cmd: &mut Command) {
        for path in &self.linker_paths {
            cmd.arg("-L").arg(path);
        }

        for lib in &self.linker_static_libs {
            cmd.arg(lib);
        }

        for lib in &self.linker_shared_libs {
            cmd.arg("-l").arg(lib);
        }
    }
}

pub fn link(ctx: &Context, opts: &CodeGenOptions, linker_flags: &LinkerFlags) -> Result<(), String> {
    let obj_file = unsafe { ctx.gen_object_file(opts)? };

    let output_file_path = opts.build_dir.join(format!("{}", opts.output_file_name));

    let mut cmd = match opts.output_type {
        OutputType::Binary => {
            let mut cmd = Command::new("gcc");
            cmd.arg("-o").arg(&output_file_path).arg(obj_file);
            linker_flags.add_flags(&mut cmd);
            cmd
        }

        OutputType::StaticLib => {
            let mut cmd = Command::new("ar");
            cmd.arg("rcs").arg(&output_file_path).arg(obj_file);
            cmd
        }

        OutputType::SharedLib => {
            let mut cmd = Command::new("gcc");
            cmd.arg("-shared")
                .arg("-o")
                .arg(&output_file_path)
                .arg(obj_file);
            linker_flags.add_flags(&mut cmd);
            cmd
        }
    };

    println!("  Linking {}", output_file_path.display());
    let output: Output = cmd
        .output()
        .map_err(|e| format!("Unable to spawn the linker: {}", e))?;

    if !output.status.success() {
        let out = String::from_utf8(output.stderr).expect("Invalid stdout from ld");
        let msg = format!("Linking {} failed:\n{}", output_file_path.display(), out);
        return Err(msg);
    }

    Ok(())
}
