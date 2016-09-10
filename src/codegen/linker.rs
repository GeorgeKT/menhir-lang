use std::process::{Output, Command};

use codegen::{CodeGenOptions};
use codegen::context::{Context};
use compileerror::{CompileError, CompileResult, Pos, ErrorCode, err};


pub fn link(ctx: &Context, opts: &CodeGenOptions) -> CompileResult<()>
{
    let obj_file = unsafe{
        try!(ctx.gen_object_file(&opts))
    };

    let program_path = format!("{}/{}", opts.build_dir, opts.program_name);

    let mut cmd = Command::new("gcc");
    cmd.arg("-o").arg(&program_path).arg(obj_file).arg(&opts.runtime_library);

    println!("  Linking {}", program_path);
    let output: Output = try!(cmd
        .output()
        .map_err(|e| CompileError::new(
            Pos::zero(),
            ErrorCode::CodegenError,
            format!("Unable to spawn the linker: {}", e))));


    if !output.status.success() {
        let out = String::from_utf8(output.stderr).expect("Invalid stdout from ld");
        let msg = format!("Linking {} failed:\n{}", program_path, out);
        return err(Pos::zero(), ErrorCode::CodegenError, msg);
    }

    Ok(())
}
