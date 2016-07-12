extern crate llvm_sys as llvm;
extern crate libc;
extern crate docopt;
extern crate rustc_serialize;

mod ast;
mod codegen;
mod compileerror;
mod parser;

use std::path::Path;
use docopt::Docopt;
use codegen::*;
use parser::*;


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


    let debug_compiler = args.flag_debug.unwrap_or(false);
    let opts = CodeGenOptions{
        dump_ir: debug_compiler,
        build_dir: "build".into(),
        program_name: output_file,
        runtime_library: find_runtime_library().expect("Unable to find the cobra runtime library"),
        optimize: args.flag_optimize.unwrap_or(false),
    };

    let r = parse_file(&input_file, ParseMode::Block)
        .and_then(|p| codegen(&p, &opts))
        .and_then(|ctx| link(&ctx, &opts));

    if let Err(e) = r
    {
        println!("Error: {}", e);
    }
}
