use std::process::{Output, Command};

use codegen::*;
use codegen::context::*;
use compileerror::*;


pub fn link(ctx: &Context, opts: &CodeGenOptions) -> Result<(), CompileError>
{
	let obj_files = unsafe{
        try!(ctx.gen_object_files(&opts.build_dir))
    };

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
