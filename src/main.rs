extern crate llvm_sys as llvm;
extern crate libc;
extern crate itertools;
#[macro_use]
extern crate clap;
extern crate uuid;
extern crate toml;
#[macro_use]
extern crate serde_derive;
extern crate serde;

mod ast;
mod compileerror;
mod bytecode;
mod parser;
mod typechecker;
mod span;
mod llvmbackend;
mod target;
mod package;

use std::path::{PathBuf};
use std::process::exit;
use clap::ArgMatches;

use parser::{ParserOptions};
use compileerror::{CompileResult};
use llvmbackend::{OutputType, llvm_init};
use package::{PackageData, BuildOptions};


fn build_command(matches: &ArgMatches, dump_flags: &str) -> CompileResult<i32>
{
    let input_file = matches.value_of("INPUT_FILE").expect("No input file given");
    let build_options = BuildOptions{
        parser_options: ParserOptions{
            import_dirs: matches.value_of("IMPORTS")
                .map(|dirs| dirs.split(',').map(PathBuf::from).collect())
                .unwrap_or_else(Vec::new),
        },
        optimize: matches.is_present("OPTIMIZE"),
        dump_flags: dump_flags.into(),
        target_machine: llvm_init()?
    };

    let output_type = match matches.value_of("LIB") {
        Some("static") => OutputType::StaticLib,
        Some("shared") => OutputType::SharedLib,
        _ => OutputType::Binary,
    };

    let pkg = PackageData::single_file(&input_file, output_type)?;
    pkg.build(&build_options)?;
    Ok(0)
}


fn build_package_command(matches: &ArgMatches, dump_flags: &str) -> CompileResult<i32>
{
    let package_toml = if let Some(toml) = matches.value_of("PACKAGE_TOML") {
        toml
    } else {
        "./package.toml"
    };

    let pkg = PackageData::load(package_toml)?;
    let build_options = BuildOptions{
        parser_options: ParserOptions{
            import_dirs: matches.value_of("IMPORTS")
                .map(|dirs| dirs.split(',').map(PathBuf::from).collect())
                .unwrap_or_else(Vec::new),
        },
        optimize: matches.is_present("OPTIMIZE"),
        dump_flags: dump_flags.into(),
        target_machine: llvm_init()?
    };
    pkg.build(&build_options)?;
    Ok(0)
}

fn run() -> CompileResult<i32>
{
    let app = clap_app!(cobrac =>
        (version: "0.1")
        (author: "Joris Guisson <joris.guisson@gmail.com>")
        (about: "Nomad language compiler")
        (@arg DUMP: -d --dump +takes_value "Dump internal compiler state for debug purposes. Argument can be all, ast, bytecode or ir. A comma separated list of these values is also supported.")
        (@arg TARGET_TRIPLET: -t --triplet "Print the default target triplet of the current system, and exit")
        (@subcommand build =>
            (about: "Build a menhir file")
            (@arg INPUT_FILE: +required "File to build")
            (@arg OUTPUT_FILE: -o --output +takes_value "Name of binary to create (by default input file without the extensions)")
            (@arg OPTIMIZE: -O --optimize "Optimize the code")
            (@arg IMPORTS: -I --imports +takes_value "Directory to look for imports, use a comma separated list for more then one.")
            (@arg LIB: -l --lib +takes_value possible_value[static shared] "Create a library, type of library must be pass")
        )
        (@subcommand buildpkg =>
            (about: "Build a menhir package.")
            (@arg PACKAGE_TOML: -p --package +takes_value "Specify the package.toml file. If not specified, menhir will look in the current directory for one.")
            (@arg OPTIMIZE: -O --optimize "Optimize the code")
            (@arg IMPORTS: -I --imports +takes_value "Directory to look for imports, use a comma separated list for more then one.")
        )
    );

    let matches = app.get_matches();
    let dump_flags = matches.value_of("DUMP").unwrap_or("");

    if matches.is_present("TARGET_TRIPLET") {
        let target_machine = llvm_init()?;
        print!("{}", target_machine.target.triplet);
        Ok(0)
    } else if let Some(build_matches) = matches.subcommand_matches("build") {
        build_command(build_matches, dump_flags)
    } else if let Some(build_matches) = matches.subcommand_matches("buildpkg") {
        build_package_command(build_matches, dump_flags)
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
