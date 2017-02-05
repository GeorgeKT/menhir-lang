//extern crate llvm_sys as llvm;
//extern crate libffi;
//extern crate libc;
extern crate docopt;
extern crate rustc_serialize;
extern crate itertools;
extern crate uuid;
extern crate shrust;

extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_cbor;

mod ast;
#[macro_use]
//mod codegen;
mod compileerror;
mod bytecode;
mod parser;
mod typechecker;
#[cfg(test)] mod testcode;
mod span;

use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::exit;
//use codegen::{CodeGenOptions, codegen, link, llvm_init};
use docopt::Docopt;
use parser::{ParserOptions, parse_file};
use typechecker::{type_check_module};
use bytecode::{compile_to_byte_code, run_byte_code, debug_byte_code, optimize_module, ByteCodeModule, OptimizationLevel};
use compileerror::{CompileResult};


static USAGE: &'static str =  "
Usage: cobra [options] <input-file>
       cobra --help

options:
  --help                                       Show this message.
  -D <dump>, --dump=<dump>                     Dump internal compiler state for debug purposes. Argument can be all, ast, bytecode or ir.
                                               A comma separated list of these values is also supported.
  -O, --optimize                               Optimize the code.
  -I <imports>, --imports=<imports>            Directory to look for imports, use a comma separated list for more then one.
  -o <output-file>, --output=<output-file>     Name of binary to create (by default input-file without the extensions).
  -t <binary-type>, --type=<binary-type>       Binary type, allowed values: bytecode or elf
  -i --interpret                               Execute the code in the interpreter.
  -d --debug                                   Run the interpreter in debug mode.
";


#[derive(RustcDecodable, Debug)]
struct Args
{
    flag_dump: Option<String>,
    flag_optimize: Option<bool>,
    arg_input_file: Option<String>,
    flag_output: Option<String>,
    flag_type: Option<String>,
    flag_imports: Option<String>,
    flag_interpret: Option<bool>,
    flag_debug: Option<bool>,
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

fn dump_byte_code(bc_mod: &ByteCodeModule, dump_flags: &str)
{
    if dump_flags.contains("bytecode") || dump_flags.contains("all") {
        println!("bytecode:");
        println!("------\n");
        println!("{}", bc_mod);
        println!("------\n");
    }
}

fn parse(parser_options: &ParserOptions, input_file: &str, dump_flags: &str, optimize: bool) -> CompileResult<ByteCodeModule>
{
    let mut module = parse_file(&parser_options, input_file)?;
    type_check_module(&mut module)?;

    if dump_flags.contains("ast") || dump_flags.contains("all") {
        println!("AST:");
        println!("------\n");
        use ast::TreePrinter;
        module.print(0);
        println!("------\n");
    }

    let mut bc_mod = compile_to_byte_code(&module);
    if optimize {
        optimize_module(&mut bc_mod, OptimizationLevel::Normal);
    } else {
        optimize_module(&mut bc_mod, OptimizationLevel::Minimal);
    }

    dump_byte_code(&bc_mod, &dump_flags);
    Ok(bc_mod)

}

fn run() -> CompileResult<i32>
{
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let input_file = args.arg_input_file.expect("Missing input file argument");
    let run_interpreter = args.flag_interpret.unwrap_or(false);
    let run_debugger = args.flag_debug.unwrap_or(false);
    let optimize = args.flag_optimize.unwrap_or(false);
    let output_file = args.flag_output.unwrap_or_else(|| default_output_file(&input_file));
    let binary_type = args.flag_type.unwrap_or("bytecode".into());
    let dump_flags = args.flag_dump.unwrap_or_default();


    let parser_options = ParserOptions{
        import_dirs: args.flag_imports
            .map(|dirs| dirs.split(',').map(PathBuf::from).collect())
            .unwrap_or_else(Vec::new),
    };


    /*
        let opts = CodeGenOptions{
            dump_ir: dump_flags.contains("ir") || dump_flags.contains("all"),
            build_dir: "build".into(),
            program_name: output_file,
            optimize: args.flag_optimize.unwrap_or(false),
        };
    */


    let bc_mod = parse(&parser_options, &input_file, &dump_flags, optimize)?;

    if !run_debugger && !run_interpreter {

        match &binary_type[..]
        {
            "bytecode" => {
                use serde_cbor::ser;
                println!("Generating bytecode binary {}.cbor", output_file);
                let mut file = File::create(&format!("{}.cbor", output_file))?;
                match ser::to_writer_sd(&mut file, &bc_mod)
                {
                    Ok(()) => Ok(0),
                    Err(msg) => {
                        println!("Failed to save {}: {}", output_file, msg);
                        Ok(-1)
                    }
                }
            },

            "elf" => {
                /*
                llvm_init();
                let mut ctx = codegen(&bc_mod)?;
                link(&mut ctx, &opts)
                */
                panic!("NYI");
            }

            _ => {
                println!("Unknown binary type {}", binary_type);
                Ok(1)
            }
        }

    } else {
        use bytecode::START_CODE_FUNCTION;
        let ret = if run_debugger {
            debug_byte_code(&bc_mod, START_CODE_FUNCTION)
        } else {
            run_byte_code(&bc_mod, START_CODE_FUNCTION)
        };

        match ret
        {
            Ok(ret) => {
                Ok(ret.to_exit_code())
            },
            Err(e) => {
                println!("Failed to execute program: {}", e.0);
                Ok(-1)
            }
        }
    }
}

fn main()
{
    match run()
    {
        Ok(ret) => exit(ret),
        Err(e) => {
            e.print();
            exit(-1);
        },
    }
}
