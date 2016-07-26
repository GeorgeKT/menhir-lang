extern crate docopt;
extern crate rustc_serialize;
extern crate itertools;

#[allow(dead_code)]
mod compileerror;
mod parser;
mod ast;

use docopt::Docopt;
use ast::TreePrinter;
use parser::parse_file;

static USAGE: &'static str =  "
Usage: cobra [options] <input-file>
       cobra --help

options:
  --help                                       Show this message.
  -d, --debug                                  Debug mode.
  -O, --optimize                               Optimize the code.
  -o <output-file>, --output=<output-file>     Name of binary to create (by default input-file without the extensions)
";


#[derive(RustcDecodable, Debug)]
struct Args
{
    flag_debug: Option<bool>,
    flag_optimize: Option<bool>,
    arg_input_file: Option<String>,
    flag_output: Option<String>,
}


fn main()
{
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let input_file = args.arg_input_file.expect("Missing input file argument");
    //let output_file = args.flag_output.unwrap_or(default_output_file(&input_file));
    match parse_file(&input_file)
    {
        Ok(expressions) => {
            for e in expressions.iter()
            {
                e.print(0);
            }
        }
        Err(e) => println!("Error: {}", e),
    }
}
