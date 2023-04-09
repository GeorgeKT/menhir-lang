extern crate itertools;
extern crate libc;
extern crate llvm_sys as llvm;
#[macro_use]
extern crate clap;
extern crate toml;
extern crate uuid;
#[macro_use]
extern crate serde_derive;
extern crate bincode;
extern crate either;
extern crate serde;

mod ast;
mod bytecode;
mod compileerror;
mod exportlibrary;
mod llvmbackend;
mod package;
mod packagebuild;
mod parser;
mod span;
mod target;
mod timer;
mod typechecker;

use clap::ArgMatches;
use std::fs::File;
use std::path::PathBuf;
use std::process::exit;

use compileerror::CompileResult;
use exportlibrary::ExportLibrary;
use llvmbackend::{llvm_init, llvm_shutdown, OutputType};
use packagebuild::{BuildOptions, PackageData};

fn build_command(matches: &ArgMatches, dump_flags: &str) -> CompileResult<i32> {
    let input_file = matches.value_of("INPUT_FILE").expect("No input file given");
    let build_options = BuildOptions {
        optimize: matches.is_present("OPTIMIZE"),
        dump_flags: dump_flags.into(),
        target_machine: llvm_init()?,
        sources_directory: String::new(),
        import_directories: matches
            .value_of("IMPORTS")
            .map(|dirs| dirs.split(',').map(PathBuf::from).collect())
            .unwrap_or_else(Vec::new),
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

fn build_package_command(matches: &ArgMatches, dump_flags: &str) -> CompileResult<i32> {
    let package_toml = if let Some(toml) = matches.value_of("PACKAGE_TOML") {
        toml
    } else {
        "./package.toml"
    };

    let pkg = PackageData::load(package_toml)?;
    let build_options = BuildOptions {
        optimize: matches.is_present("OPTIMIZE"),
        dump_flags: dump_flags.into(),
        target_machine: llvm_init()?,
        sources_directory: "src".into(),
        import_directories: matches
            .value_of("IMPORTS")
            .map(|dirs| dirs.split(',').map(PathBuf::from).collect())
            .unwrap_or_else(Vec::new),
    };
    pkg.build(&build_options)?;
    Ok(0)
}

fn exports_command(matches: &ArgMatches) -> CompileResult<i32> {
    let exports_file_path = matches
        .value_of("EXPORTS_FILE")
        .ok_or_else(|| "No exports file given".to_owned())?;
    let mut exports_file = File::open(&exports_file_path)?;
    let lib = ExportLibrary::load(&mut exports_file)?;
    println!("{}", lib);
    Ok(0)
}

fn run() -> CompileResult<i32> {
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
        (@subcommand exports =>
            (about: "List the exported symbols in an exports file")
            (@arg EXPORTS_FILE: +required "Exports file")
        )
    );

    let matches = app.get_matches();
    let dump_flags = matches.value_of("DUMP").unwrap_or("");

    if matches.is_present("TARGET_TRIPLET") {
        let target_machine = llvm_init()?;
        print!("{}", target_machine.target.triplet);
        Ok(0)
    } else if let Some(matches) = matches.subcommand_matches("build") {
        build_command(matches, dump_flags)
    } else if let Some(matches) = matches.subcommand_matches("buildpkg") {
        build_package_command(matches, dump_flags)
    } else if let Some(matches) = matches.subcommand_matches("exports") {
        exports_command(matches)
    } else {
        println!("{}", matches.usage());
        Ok(1)
    }
}

fn main() {
    match run() {
        Ok(ret) => {
            llvm_shutdown();
            exit(ret)
        }
        Err(e) => {
            e.print();
            llvm_shutdown();
            exit(-1);
        }
    }
}
