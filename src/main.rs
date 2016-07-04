mod ast;
mod compileerror;
mod lexer;
mod parser;
mod tokens;
mod tokenqueue;

use std::env;
use std::fs;
use std::process;

use ast::{TreePrinter, Program};
use parser::parse_program;
use compileerror::CompileError;

fn usage()
{
    println!("Usage: cobra <input-file>");
}

fn parse_file(filename: &str) -> Result<Program, CompileError>
{
    let mut file = try!(fs::File::open(filename));
    parse_program(&mut file)
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
        Err(e) => println!("Error: {:?}", e),
        Ok(p) => {
            p.print(0);
        },
    }
}
