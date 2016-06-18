mod ast;
mod compileerror;
mod lexer;

use std::env;
use std::fs;
use std::process;
use lexer::lex;

fn usage()
{
    println!("Usage: cobra <input-file>");
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
    match fs::File::open(&filename)
        .map(|mut file| lex(&mut file).map(|prog| prog.dump()))
    {
        Err(e) => println!("Error: {}", e),
        Ok(_) => println!("OK"),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
