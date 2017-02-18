extern crate llvm_sys as llvm;
extern crate libffi;
extern crate libc;
extern crate libcobra;
#[macro_use]
extern crate clap;

mod llvmbackend;

use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::exit;
use libcobra::parser::{ParserOptions, parse_file};
use libcobra::typechecker::{type_check_module};
use libcobra::bytecode::{compile_to_byte_code, optimize_module, ByteCodeModule, OptimizationLevel};
use libcobra::compileerror::{CompileResult, CompileError};
use llvmbackend::{CodeGenOptions, llvm_code_generation};


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
        use libcobra::ast::TreePrinter;
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
    let matches = clap_app!(cobrac =>
        (version: "1.0")
        (author: "Joris Guisson <joris.guisson@gmail.com>")
        (about: "cobra language compiler")
        (@arg DUMP: -d --dump +takes_value "Dump internal compiler state for debug purposes. Argument can be all, ast, bytecode or ir. A comma separated list of these values is also supported.")
        (@arg INPUT_FILE: +required "File to compile")
        (@arg OPTIMIZE: -O --optimize "Optimize the code")
        (@arg BINARY_TYPE: -t --type +takes_value "Binary type, allowed values: bytecode or exe")
        (@arg OUTPUT_FILE: -o --output +takes_value "Name of binary to create (by default input file without the extensions)")
        (@arg IMPORTS: -I --imports +takes_value "Directory to look for imports, use a comma separated list for more then one.")
    ).get_matches();

    let input_file = matches.value_of("INPUT_FILE").expect("No input file given");
    let optimize = matches.is_present("OPTIMIZE");
    let output_file = matches.value_of("OUTPUT_FILE").map(|v| v.to_string()).unwrap_or_else(|| default_output_file(&input_file));
    let binary_type = matches.value_of("BINARY_TYPE").unwrap_or("exe");
    let dump_flags = matches.value_of("DUMP").unwrap_or("");

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

            llvm_code_generation(&bc_mod, &opts)
                .map_err(|msg| CompileError::Other(msg))?;
            Ok(0)
        }

        _ => {
            Err(CompileError::Other(format!("Unknown binary type {}", binary_type)))
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
