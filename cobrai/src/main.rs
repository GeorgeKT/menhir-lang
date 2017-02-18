extern crate shrust;
#[macro_use]
extern crate clap;
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
use libcobra::bytecode::{ByteCodeModule, START_CODE_FUNCTION};
use interpreter::{ExecutionError, run_byte_code};
use debugger::debug_byte_code;


fn run() -> Result<i32, ExecutionError>
{
    let matches = clap_app!(cobrai =>
            (version: "0.1")
            (author: "Joris Guisson <joris.guisson@gmail.com>")
            (about: "cobra language bytecode interpreter")
            (@arg DEBUG: -d --debug "Run the interpreter in debug mode")
            (@arg BYTECODE_FILE: +required "Byte code file to run/debug")
        ).get_matches();

    let bytecode_file = matches.value_of("BYTECODE_FILE").expect("Missing input file argument");
    let run_debugger = matches.is_present("DEBUG");

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