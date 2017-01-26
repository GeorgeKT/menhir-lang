use std::process::{Output, Command};

use codegen::{CodeGenOptions};
use codegen::context::{Context};
use compileerror::{CompileError, CompileResult, ErrorCode, err};
use span::Span;


pub fn link(ctx: &Context, opts: &CodeGenOptions) -> CompileResult<()>
{
    let obj_file = unsafe{
        ctx.gen_object_file(&opts)?
    };

    let program_path = format!("{}/{}", opts.build_dir, opts.program_name);

    let mut cmd = Command::new("gcc");
    cmd.arg("-o").arg(&program_path).arg(obj_file).arg("-lcobraruntime");

    println!("  Linking {}", program_path);
    let output: Output = cmd
        .output()
        .map_err(|e| CompileError::new(
            &Span::default(),
            ErrorCode::CodegenError,
            format!("Unable to spawn the linker: {}", e)))?;


    if !output.status.success() {
        let out = String::from_utf8(output.stderr).expect("Invalid stdout from ld");
        let msg = format!("Linking {} failed:\n{}", program_path, out);
        return err(&Span::default(), msg);
    }

    Ok(())
}
