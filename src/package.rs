use std::fs::{File};
use std::io::{Read};
use std::path::{Path, PathBuf};
use toml;

use ast::TreePrinter;
use parser::{ParserOptions, parse_files};
use llvmbackend::TargetMachine;
use bytecode::{compile_to_byte_code, optimize_module, OptimizationLevel};
use llvmbackend::{CodeGenOptions, OutputType, llvm_code_generation, link};
use compileerror::{CompileResult, CompileError};
use typechecker::type_check_package;


pub struct BuildOptions
{
    pub parser_options: ParserOptions,
    pub optimize: bool,
    pub dump_flags: String,
    pub target_machine: TargetMachine,
    pub sources_directory: String,
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageTarget
{
    name: String,
    #[serde(rename = "type")]
    output_type: OutputType,
    path: Option<PathBuf>,
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageDescription
{
    name: String,
    author: String,
    email: String,
    license: String,
    version: String,
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageData
{
    package: PackageDescription,
    target: Vec<PackageTarget>,
}

impl PackageData
{
    pub fn single_file<P: AsRef<Path>>(path: P, output_type: OutputType) -> CompileResult<PackageData>
    {
        let p = path.as_ref();
        let name = if let Some(stem) = p.file_stem() {
            stem.to_string_lossy().into()
        } else {
            return Err(CompileError::Other(format!("Cannot determine file stem of {}", path.as_ref().to_string_lossy())))
        };

        Ok(PackageData{
            target: vec![
                PackageTarget{
                    name,
                    output_type,
                    path: Some(p.to_owned()),
                }
            ],
            ..Default::default()
        })
    }

    pub fn load<P: AsRef<Path>>(path: P) -> CompileResult<PackageData>
    {
        let mut file = File::open(path.as_ref())?;
        let mut package_data = String::with_capacity(file.metadata()?.len() as usize);
        file.read_to_string(&mut package_data)?;

        let package: PackageData = toml::from_str(&package_data)
            .map_err(|e| CompileError::Other(format!("Failed to decode {}: {}", path.as_ref().to_string_lossy(), e)))?;
        Ok(package)
    }

    pub fn build(&self, build_options: &BuildOptions) -> CompileResult<()>
    {
        println!("Compiling for {}", build_options.target_machine.target.triplet);
        for t in &self.target {
            build_target(t, build_options)?;
        }

        Ok(())
    }
}

fn output_file_name(name: &str, output_type: OutputType) -> String
{
    match output_type {
        OutputType::Binary => name.into(),
        OutputType::StaticLib => format!("lib{}.a", name),
        OutputType::SharedLib => format!("lib{}.so", name),
    }
}

fn build_target(target: &PackageTarget, build_options: &BuildOptions) -> CompileResult<()>
{
    let single_file = format!("{}/{}.mhr", build_options.sources_directory, target.name);
    let dir_name = format!("{}/{}", build_options.sources_directory, target.name);

    let path = if let Some(ref path) = target.path {
        path.as_path()
    } else {
        let path = Path::new(&single_file);
        if path.exists() {
            path
        } else {
            Path::new(&dir_name)
        }
    };

    let mut pkg = if path.exists() && path.is_file() {
        parse_files(&path, &target.name, &build_options.target_machine.target)?
    } else {
        parse_files(&path, &target.name, &build_options.target_machine.target)?
    };

    if build_options.dump_flags.contains("ast") || build_options.dump_flags.contains("all") {
        println!("AST: {}", pkg.name);
        pkg.print(0);
    }

    type_check_package(&mut pkg, &build_options.target_machine.target)?;
    let mut bc_mod = compile_to_byte_code(&pkg, &build_options.target_machine.target)?;

    if build_options.dump_flags.contains("bytecode") || build_options.dump_flags.contains("all") {
        println!("bytecode:");
        println!("------\n");
        println!("{}", bc_mod);
        println!("------\n");
    }

    if build_options.optimize {
        optimize_module(&mut bc_mod, OptimizationLevel::Normal);
    } else {
        optimize_module(&mut bc_mod, OptimizationLevel::Minimal);
    }

    let opts = CodeGenOptions{
        dump_ir: build_options.dump_flags.contains("ir") ||  build_options.dump_flags.contains("all"),
        build_dir: format!("build/{}/{}", build_options.target_machine.target.triplet, target.name),
        output_file_name: output_file_name(&target.name, target.output_type),
        output_type: target.output_type,
        optimize: build_options.optimize,
    };

    let ctx = llvm_code_generation(&bc_mod, &build_options.target_machine).map_err(CompileError::Other)?;
    link(&ctx, &opts)?;


    match opts.output_type
    {
        OutputType::SharedLib | OutputType::StaticLib => {
            let path = format!("{}/{}.mhr.exports", opts.build_dir, target.name);
            let file = File::create(&path)?;
            println!("  Generating {}", path);
            pkg.create_export_library(file)?;
        }

        _ => (),
    }
    Ok(())
}