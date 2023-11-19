mod ast;
mod buildinputs;
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
use cli::{CleanCommand, RunCommand};
use packagebuild::CleanOptions;

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

fn load_package_data(input_file: &Option<PathBuf>) -> CompileResult<(PackageData, PathBuf)> {
    let package_toml = if let Some(toml) = input_file {
        toml.clone()
    } else {
        PathBuf::from("./package.toml")
    };

    let root_dir = if let Some(root_dir) = package_toml.parent() {
        root_dir.to_owned()
    } else {
        PathBuf::from(".")
    };

    Ok((PackageData::load(package_toml)?, root_dir))
}

fn build_package_command(b: BuildPkgCommand) -> CompileResult<i32> {
    let (pkg, root_dir) = load_package_data(&b.input_file)?;
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

fn clean_command(c: CleanCommand) -> CompileResult<i32> {
    let (pkg, root_dir) = load_package_data(&c.input_file)?;
    let clean_options = CleanOptions {
        target_machine: llvm_init()?,
        build_directory: root_dir.join("build"),
    };
    pkg.clean(&clean_options)?;
    Ok(0)
}

fn run_command(r: RunCommand) -> CompileResult<i32> {
    let (pkg, root_dir) = load_package_data(&r.input_file)?;
    let build_options = BuildOptions {
        optimize: r.optimize,
        dump_flags: r.dump.clone(),
        target_machine: llvm_init()?,
        sources_directory: root_dir.join("src"),
        build_directory: root_dir.join("build"),
        import_directories: r.imports.clone(),
    };
    pkg.build(&build_options)?;
    let ec = pkg.run(&r.target, &build_options, &r)?;
    Ok(ec.code().unwrap_or(255))
}

fn run() -> CompileResult<i32> {
    let cli = CLI::parse();
    match cli.command {
        CompilerCommand::Build(b) => build_command(b),
        CompilerCommand::BuildPkg(b) => build_package_command(b),
        CompilerCommand::Exports(e) => exports_command(e),
        CompilerCommand::Info(i) => {
            if i.triplet {
                llvm_init()?;
                print!("{}", TargetMachine::new()?.target.triplet);
            }

            Ok(0)
        }
        CompilerCommand::Clean(c) => clean_command(c),
        CompilerCommand::Run(r) => run_command(r),
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
