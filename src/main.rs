extern crate llvm_sys as llvm;
extern crate libffi;
extern crate libc;
extern crate docopt;
extern crate rustc_serialize;
extern crate itertools;
extern crate uuid;

mod ast;
#[macro_use]
mod codegen;
mod compileerror;
mod parser;
mod passes;
mod span;


use std::path::{Path, PathBuf};
use codegen::{CodeGenOptions, codegen, link, llvm_init};
use docopt::Docopt;
use parser::{ParserOptions, parse_file};
use passes::{type_check_module};


static USAGE: &'static str =  "
Usage: cobra [options] <input-file>
       cobra --help

options:
  --help                                       Show this message.
  -d, --debug                                  Debug mode.
  -O, --optimize                               Optimize the code.
  -I <imports>, --imports=<imports>            Directory to look for imports, use a comma separated list for more then one.
  -o <output-file>, --output=<output-file>     Name of binary to create (by default input-file without the extensions)
";


#[derive(RustcDecodable, Debug)]
struct Args
{
    flag_debug: Option<bool>,
    flag_optimize: Option<bool>,
    arg_input_file: Option<String>,
    flag_output: Option<String>,
    flag_imports: Option<String>,
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


    let parser_options = ParserOptions{
        import_dirs: args.flag_imports
            .map(|dirs| dirs.split(',').map(|p| PathBuf::from(p)).collect())
            .unwrap_or(Vec::new()),
    };

    let opts = CodeGenOptions{
        dump_ir: debug_compiler,
        build_dir: "build".into(),
        program_name: output_file,
        optimize: args.flag_optimize.unwrap_or(false),
    };

    match parse_file(&parser_options, &input_file).and_then(|mut module| {
        /*
        use ast::TreePrinter;
        module.print(0);
        */
        try!(type_check_module(&mut module));
        llvm_init();
        let mut ctx = try!(codegen(&module));
        link(&mut ctx, &opts)
    })
    {
        Ok(_) => {},
        Err(e) => e.print(),
    }
}
