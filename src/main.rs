extern crate llvm_sys as llvm;
extern crate libffi;
extern crate libc;
extern crate docopt;
extern crate rustc_serialize;
extern crate itertools;
extern crate uuid;
extern crate shrust;

mod ast;
#[macro_use]
//mod codegen;
mod compileerror;
mod bytecode;
mod parser;
mod typechecker;
mod span;


use std::path::{Path, PathBuf};
use std::process::exit;
//use codegen::{CodeGenOptions, codegen, link, llvm_init};
use docopt::Docopt;
use parser::{ParserOptions, parse_file};
use typechecker::{type_check_module};
use bytecode::{compile_to_byte_code, run_byte_code, debug_byte_code};


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

fn main()
{
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let input_file = args.arg_input_file.expect("Missing input file argument");
    let run_interpreter = args.flag_interpret.unwrap_or(false);
    let run_debugger = args.flag_debug.unwrap_or(false);
    let _output_file = args.flag_output.unwrap_or(default_output_file(&input_file));
    let dump_flags = args.flag_dump.unwrap_or_default();


    let parser_options = ParserOptions{
        import_dirs: args.flag_imports
            .map(|dirs| dirs.split(',').map(|p| PathBuf::from(p)).collect())
            .unwrap_or(Vec::new()),
    };

/*
    let opts = CodeGenOptions{
        dump_ir: dump_flags.contains("ir") || dump_flags.contains("all"),
        build_dir: "build".into(),
        program_name: output_file,
        optimize: args.flag_optimize.unwrap_or(false),
    };
*/
    let ret = parse_file(&parser_options, &input_file).and_then(|mut module| {

        type_check_module(&mut module)?;

        if dump_flags.contains("ast") || dump_flags.contains("all") {
            println!("AST:");
            println!("------\n");
            use ast::TreePrinter;
            module.print(0);
            println!("------\n");
        }


        let bc_mod = compile_to_byte_code(&module);

        if dump_flags.contains("bytecode") || dump_flags.contains("all") {
            println!("bytecode:");
            println!("------\n");
            println!("{}", bc_mod);
            println!("------\n");
        }

        if !run_debugger && !run_interpreter {
            /*
            llvm_init();
            let mut ctx = codegen(&bc_mod)?;
            link(&mut ctx, &opts)
            */
            panic!("NYI");
        } else {
            let ret = if run_debugger {
                debug_byte_code(&bc_mod, "main")
            } else {
                run_byte_code(&bc_mod, "main")
            };
            match ret
            {
                Ok(ret) => {
                    exit(ret.to_exit_code())
                },
                Err(e) => {
                    println!("Failed to execute program: {}", e.0);
                    exit(-1);
                }
            }
        }

        Ok(())
    });

    match ret
    {
        Ok(_) => {},
        Err(e) => {
            e.print();
            exit(-1);
        },
    }
}
