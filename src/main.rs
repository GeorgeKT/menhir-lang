extern crate llvm_sys as llvm;
extern crate libc;
extern crate docopt;
extern crate rustc_serialize;

mod ast;
mod codegen;
mod compileerror;
mod parser;


use std::ffi::OsStr;
use std::fs;
use std::path::Path;

use docopt::Docopt;

use ast::*;
use codegen::*;
use parser::parse_program;
use compileerror::CompileError;

static USAGE: &'static str =  "
Usage: cobra [options] <input-file>
       cobra --help

options:
  --help                                       Show this message.
  -d, --debug                                  Debug mode.
  -o <output-file>, --output=<output-file>     Name of binary to create (by default input-file without the extensions)
";
  

#[derive(RustcDecodable, Debug)]
struct Args
{
    flag_debug: Option<bool>,
    arg_input_file: Option<String>,
    flag_output: Option<String>,
}

fn parse_file(file_path: &str) -> Result<Program, CompileError>
{
    let mut file = try!(fs::File::open(file_path));
    let path = Path::new(file_path);
    let filename: &OsStr = path.file_name().expect("Invalid filename");
    parse_program(&mut file, filename.to_str().expect("Invalid UTF8 filename"))
}

fn find_runtime_library() -> Option<String> 
{
    let paths = [
        "/usr/lib/libcobraruntime.a",
        "/usr/local/lib/libcobraruntime.a",
        "../librt/libcobraruntime.a",
        "./librt/libcobraruntime.a",
        "./libcobraruntime.a",
    ];

    for p in &paths {
        if Path::new(*p).exists() {
            return Some((*p).into());
        }
    }

    None
}

fn default_output_file(input_file: &str) -> String
{
    Path::new(&input_file)
        .file_stem()
        .expect("Invalid input file")
        .to_str()
        .expect("Invalid input file")
        .into()
}

fn main()
{
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let input_file = args.arg_input_file.expect("Missing input file argument");
    let output_file = args.flag_output.unwrap_or(default_output_file(&input_file));
    match parse_file(&input_file)
    {
        Err(e) => println!("Error: {}", e),
        Ok(p) => {
            let opts = CodeGenOptions{
                dump_ir: args.flag_debug.unwrap_or(false),
                build_dir: "cobra-build".into(),
                program_name: output_file,
                runtime_library: find_runtime_library().expect("Unable to find the cobra runtime library"),
            };

            if let Err(e) = codegen(&p, &opts) {
                println!("Error: {}", e);
            }
        },
    }
}
