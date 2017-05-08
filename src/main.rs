extern crate llvm_sys as llvm;
extern crate libc;
extern crate shrust;
extern crate itertools;
#[macro_use]
extern crate clap;
extern crate uuid;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_cbor;


mod ast;
mod compileerror;
mod bytecode;
mod parser;
mod typechecker;
mod span;
mod llvmbackend;
mod interpreter;
mod target;

use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::exit;
use clap::ArgMatches;
use parser::{ParserOptions, parse_file};
use typechecker::{type_check_module};
use bytecode::{compile_to_byte_code, optimize_module, ByteCodeModule, OptimizationLevel};
use compileerror::{CompileResult, CompileError};
use llvmbackend::{CodeGenOptions, llvm_code_generation, llvm_init, link};
use interpreter::{run_byte_code, debug_byte_code};
use target::{register_target};


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

    let mut bc_mod = compile_to_byte_code(&module)?;
    if optimize {
        optimize_module(&mut bc_mod, OptimizationLevel::Normal);
    } else {
        optimize_module(&mut bc_mod, OptimizationLevel::Minimal);
    }

    dump_byte_code(&bc_mod, &dump_flags);
    Ok(bc_mod)
}




fn build_command(matches: &ArgMatches, dump_flags: &str) -> CompileResult<i32>
{
    let input_file = matches.value_of("INPUT_FILE").expect("No input file given");
    let optimize = matches.is_present("OPTIMIZE");
    let output_file = matches.value_of("OUTPUT_FILE").map(|v| v.to_string()).unwrap_or_else(|| default_output_file(&input_file));
    let binary_type = matches.value_of("BINARY_TYPE").unwrap_or("exe");

    let target_machine = llvm_init()?;
    register_target(&target_machine);

    let parser_options = ParserOptions{
        import_dirs: matches.value_of("IMPORTS")
            .map(|dirs| dirs.split(',').map(PathBuf::from).collect())
            .unwrap_or_else(Vec::new),
    };

    let bc_mod = parse(&parser_options, &input_file, &dump_flags, optimize)?;
    match &binary_type[..]
    {
        "bytecode" => {
            println!("Generating bytecode binary {}.byte", output_file);
            let mut file = File::create(&format!("{}.byte", output_file))?;
            bc_mod.save(&mut file)
                .map_err(|msg| CompileError::Other(format!("Failed to save {}: {}", output_file, msg)))?;
            Ok(0)
        },

        "exe" => {
            let opts = CodeGenOptions{
                dump_ir: dump_flags.contains("ir") || dump_flags.contains("all"),
                build_dir: "build".into(),
                program_name: output_file.into(),
                optimize: optimize,
            };

            let ctx = llvm_code_generation(&bc_mod, &target_machine).map_err(|msg| CompileError::Other(msg))?;
            link(&ctx, &opts)?;
            Ok(0)
        }

        _ => {
            Err(CompileError::Other(format!("Unknown binary type {}", binary_type)))
        }
    }
}


fn run_command(matches: &ArgMatches, dump_flags: &str) -> CompileResult<i32>
{
    let target_machine = llvm_init()?;
    register_target(&target_machine);

    let input_file = matches.value_of("INPUT_FILE").expect("Missing input file argument");
    let run_debugger = matches.is_present("DEBUG");
    let optimize = matches.is_present("OPTIMIZE");

    let bc_mod = if input_file.ends_with(".cobra") {
        let parser_options = ParserOptions{
            import_dirs: matches.value_of("IMPORTS")
                .map(|dirs| dirs.split(',').map(PathBuf::from).collect())
                .unwrap_or_else(Vec::new),
        };

        parse(&parser_options, &input_file, &dump_flags, optimize)?
    } else {
        let mut file = File::open(&input_file)
            .map_err(|err| CompileError::IO(format!("Cannot open {}: {}", input_file, err)))?;

        ByteCodeModule::load(&mut file)?
    };

    let ret = if run_debugger {
        debug_byte_code(&bc_mod)?
    } else {
        run_byte_code(&bc_mod)?
    };

    Ok(ret.to_exit_code())
}

fn run() -> CompileResult<i32>
{
    let app = clap_app!(cobrac =>
        (version: "0.1")
        (author: "Joris Guisson <joris.guisson@gmail.com>")
        (about: "Nomad language compiler")
        (@arg DUMP: -d --dump +takes_value "Dump internal compiler state for debug purposes. Argument can be all, ast, bytecode or ir. A comma separated list of these values is also supported.")
        (@subcommand build =>
            (about: "Build a nomad file")
            (version: "0.1")
            (@arg INPUT_FILE: +required "File to build")
            (@arg BINARY_TYPE: -t --type +takes_value "Binary type, allowed values: bytecode or exe")
            (@arg OUTPUT_FILE: -o --output +takes_value "Name of binary to create (by default input file without the extensions)")
            (@arg OPTIMIZE: -O --optimize "Optimize the code")
            (@arg IMPORTS: -I --imports +takes_value "Directory to look for imports, use a comma separated list for more then one.")
        )
        (@subcommand run =>
            (about: "Run a nomad file in interpreted mode")
            (version: "0.1")
            (@arg INPUT_FILE: +required "File to run, both source file and bytecode files are allowed")
            (@arg OPTIMIZE: -O --optimize "Optimize the code")
            (@arg IMPORTS: -I --imports +takes_value "Directory to look for imports, use a comma separated list for more then one.")
            (@arg DEBUG: -d --debug "Debug the code")
        )
    );

    let matches = app.get_matches();
    let dump_flags = matches.value_of("DUMP").unwrap_or("");

    if let Some(build_matches) = matches.subcommand_matches("build") {
        build_command(&build_matches, &dump_flags)
    } else if let Some(run_matches) = matches.subcommand_matches("run") {
        run_command(&run_matches, &dump_flags)
    } else {
        println!("{}", matches.usage());
        Ok(1)
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
