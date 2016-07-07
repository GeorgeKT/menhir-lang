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
use std::path;

use docopt::Docopt;

use ast::*;
use codegen::*;
use parser::parse_program;
use compileerror::CompileError;

static USAGE: &'static str =  "
Usage: cobra [options] <input-file>
       cobra --help

Options:
  --help                                       Show this message.
  -d, --debug                                  Debug mode
";

#[derive(RustcDecodable, Debug)]
struct Args
{
    flag_debug: Option<bool>,
    arg_input_file: Option<String>,
}

fn parse_file(file_path: &str) -> Result<Program, CompileError>
{
    let mut file = try!(fs::File::open(file_path));
    let path = path::Path::new(file_path);
    let filename: &OsStr = path.file_name().expect("Invalid filename");
    parse_program(&mut file, filename.to_str().expect("Invalid UTF8 filename"))
}

fn main()
{
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let filename = args.arg_input_file.expect("Missing input file argument");
    let opts = CodeGenOptions{
        dump_ir: args.flag_debug.unwrap_or(false),
    };

    match parse_file(&filename)
    {
        Err(e) => println!("Error: {}", e),
        Ok(p) => {
            //p.print(0);
            if let Err(e) = codegen(&p, &opts) {
                println!("Error: {}", e);
            } else {
                println!("Success !");
            }
        },
    }
}
