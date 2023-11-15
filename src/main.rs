mod ast;
mod bytecode;
mod cli;
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

use std::fs::File;
use std::path::PathBuf;
use std::process::exit;

use clap::Parser;

use crate::cli::{BuildCommand, BuildPkgCommand, CompilerCommand, ExportsCommand, LibraryType, CLI};
use crate::compileerror::CompileResult;
use crate::exportlibrary::ExportLibrary;
use crate::llvmbackend::{llvm_init, llvm_shutdown, OutputType, TargetMachine};
use crate::packagebuild::{BuildOptions, PackageData};

fn build_command(bc: BuildCommand) -> CompileResult<i32> {
    let build_options = BuildOptions {
        optimize: bc.optimize,
        dump_flags: bc.dump,
        target_machine: llvm_init()?,
        sources_directory: PathBuf::from("."),
        build_directory: PathBuf::from("build"),
        import_directories: bc.imports,
    };

    let output_type = match bc.lib {
        Some(LibraryType::Static) => OutputType::StaticLib,
        Some(LibraryType::Shared) => OutputType::SharedLib,
        _ => OutputType::Binary,
    };

    let pkg = PackageData::single_file(&bc.input_file, output_type)?;
    pkg.build(&build_options)?;
    Ok(0)
}

fn build_package_command(b: BuildPkgCommand) -> CompileResult<i32> {
    let package_toml = if let Some(toml) = &b.input_file {
        toml.clone()
    } else {
        PathBuf::from("./package.toml")
    };

    let root_dir = if let Some(root_dir) = package_toml.parent() {
        root_dir.to_owned()
    } else {
        PathBuf::from(".")
    };

    let pkg = PackageData::load(package_toml)?;
    let build_options = BuildOptions {
        optimize: b.optimize,
        dump_flags: b.dump,
        target_machine: llvm_init()?,
        sources_directory: root_dir.join("src"),
        build_directory: root_dir.join("build"),
        import_directories: b.imports,
    };
    pkg.build(&build_options)?;
    Ok(0)
}

fn exports_command(e: ExportsCommand) -> CompileResult<i32> {
    let mut exports_file = File::open(e.exports_file)?;
    let lib = ExportLibrary::load(&mut exports_file)?;
    println!("{}", lib);
    Ok(0)
}

fn run() -> CompileResult<i32> {
    let cli = CLI::parse();
    match cli.command {
        CompilerCommand::Build(b) => build_command(b),
        CompilerCommand::BuildPkg(b) => build_package_command(b),
        CompilerCommand::Exports(e) => exports_command(e),
        CompilerCommand::Info { triplet } => {
            if triplet {
                llvm_init()?;
                print!("{}", TargetMachine::new()?.target.triplet);
            }

            Ok(0)
        }
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
