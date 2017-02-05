extern crate shrust;
extern crate docopt;
extern crate rustc_serialize;
extern crate itertools;
extern crate libcobra;

mod interpreter;
mod debugger;
mod value;
mod valueref;

#[cfg(test)]
mod tests;

use std::fs::File;
use std::process::exit;
use docopt::Docopt;
use libcobra::bytecode::{ByteCodeModule, START_CODE_FUNCTION};
use interpreter::{ExecutionError, run_byte_code};
use debugger::debug_byte_code;

static USAGE: &'static str =  "
Usage: cobrai [options] <bytecode-file>
       cobrai --help

options:
  --help                                       Show this message.
  -d --debug                                   Run the interpreter in debug mode.
";


#[derive(RustcDecodable, Debug)]
struct Args
{
    arg_bytecode_file: Option<String>,
    flag_debug: Option<bool>,
}

fn run() -> Result<i32, ExecutionError>
{
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    let bytecode_file = args.arg_bytecode_file.expect("Missing input file argument");
    let run_debugger = args.flag_debug.unwrap_or(false);

    let mut file = File::open(&bytecode_file)
        .map_err(|err| ExecutionError(format!("Cannot open {}: {}", bytecode_file, err)))?;

    let bc_mod = ByteCodeModule::load(&mut file)
        .map_err(|err| ExecutionError(err))?;

    let ret = if run_debugger {
        debug_byte_code(&bc_mod, START_CODE_FUNCTION)?
    } else {
        run_byte_code(&bc_mod, START_CODE_FUNCTION)?
    };

    Ok(ret.to_exit_code())
}

fn main()
{
    match run()
    {
        Ok(ret) => {
            exit(ret)
        },
        Err(e) => {
            println!("Failed to execute program: {}", e.0);
            exit(-1)
        }
    }
}