mod statements;
mod expressions;
mod linker;
mod stackframe;
mod symbols;
mod modulecontext;
mod context;

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use llvm::prelude::*;
use llvm::core::*;

use ast::*;
use compileerror::*;
use codegen::context::*;
use codegen::statements::*;
pub use codegen::linker::*;

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

pub struct CodeGenOptions
{
    pub build_dir: String,
    pub program_name: String,
    pub runtime_library: String,
    pub dump_ir: bool,
    pub optimize: bool,
}


pub fn codegen(m: &Module, opts: &CodeGenOptions) -> Result<Context, CompileError>
{
    use self::linker::*;
    unsafe {
        llvm_init();
        // Set up a context, module and builder in that context.
        let mut ctx = Context::new(&m.name);
        try!(gen_program(&mut ctx, m));

        if opts.optimize {
            try!(ctx.optimize());
        }

        if opts.dump_ir {
            println!("LLVM IR: {}", m.name);
            // Dump the module as IR to stdout.
            LLVMDumpModule(ctx.get_module());
            println!("----------------------");
        }

        Ok(ctx)
    }
}
