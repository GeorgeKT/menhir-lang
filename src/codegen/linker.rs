use std::ptr;
use std::fs::DirBuilder;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::process::{Output, Command};

use llvm::core::*;
use llvm::target::*;
use llvm::target_machine::*;


use codegen::*;
use codegen::context::*;
use compileerror::*;

pub unsafe fn llvm_init()
{
	use llvm::initialization::*;

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

unsafe fn gen_obj_files(ctx: &Context, build_dir: &str) -> Result<Vec<String>, CompileError>
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

	try!(DirBuilder::new()
		.recursive(true)
		.create(build_dir)
		.map_err(|e| CompileError::new(
			Pos::zero(),
			ErrorType::CodegenError(
				format!("Unable to create directory for {}: {}", build_dir, e)))));


    let mod_name = ctx.get_module_name();
	let obj_file_name = format!("{}/{}.o", build_dir, mod_name);
	println!("  Building {}", mod_name);
    if LLVMTargetMachineEmitToFile(target_machine, ctx.get_module(), cstr_mut(&obj_file_name), LLVMCodeGenFileType::LLVMObjectFile, &mut error_message) != 0 {
        let msg = CStr::from_ptr(error_message).to_str().expect("Invalid C string");
        let e = format!("Unable to create object file: {}", msg);
        LLVMDisposeMessage(error_message);
        LLVMDisposeTargetMachine(target_machine);
        return err(Pos::zero(), ErrorType::CodegenError(e));
    }
    obj_files.push(obj_file_name);

    LLVMDisposeTargetMachine(target_machine);
    Ok(obj_files)
}

pub fn link(ctx: &Context, opts: &CodeGenOptions) -> Result<(), CompileError>
{
	let obj_files = unsafe{try!(gen_obj_files(ctx, &opts.build_dir))};
	let program_path = format!("{}/{}", opts.build_dir, opts.program_name);

	let mut cmd = Command::new("ld");
	cmd.arg("--gc-sections");
	cmd.arg("-o").arg(&program_path);

	for obj_file in &obj_files {
		cmd.arg(obj_file);
	}

	println!("  Linking {}", program_path);
	cmd.arg(&opts.runtime_library);
	let output: Output = try!(cmd
		.output()
		.map_err(|e| CompileError::new(
			Pos::zero(),
			ErrorType::CodegenError(
				format!("Unable to spawn the linker: {}", e)))));


	if !output.status.success() {
		let out = String::from_utf8(output.stderr).expect("Invalid stdout from ld");
		let msg = format!("Linking {} failed:\n{}", program_path, out);
		return err(Pos::zero(), ErrorType::CodegenError(msg));
	}

	Ok(())
}
