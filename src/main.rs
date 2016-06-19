//mod ast;
mod compileerror;
mod lexer;

use std::env;
use std::fs;
use std::process;
use lexer::Lexer;

use compileerror::CompileError;

fn usage()
{
    println!("Usage: cobra <input-file>");
}

fn parse_file(filename: &str) -> Result<(), CompileError>
{
    let mut file = try!(fs::File::open(filename));
    let mut l = Lexer::new();
    try!(l.read(&mut file));
    l.dump();
    Ok(())
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
        Ok(_) => {},
    }
}
