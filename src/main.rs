extern crate llvm_sys as llvm;
extern crate libc;

mod ast;
mod codegen;
mod compileerror;
mod lexer;
mod parser;
mod tokens;
mod tokenqueue;

use std::env;
use std::ffi::OsStr;
use std::fs;
use std::path;
use std::process;

use ast::*;
use codegen::codegen;
use parser::parse_program;
use compileerror::CompileError;

fn usage()
{
    println!("Usage: cobra <input-file>");
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
    let mut args = env::args();
    if args.len() != 2
    {
        usage();
        process::exit(1);
    }

    let filename = args.nth(1).expect("Missing file argument");

    match parse_file(&filename)
    {
        Err(e) => println!("Error: {}", e),
        Ok(p) => {
            //p.print(0);
            if let Err(e) = codegen(&p) {
                println!("Error: {}", e);
            }
        },
    }
}
