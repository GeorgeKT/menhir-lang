use std::fs::{File};
use std::io::{Read};
use std::rc::Rc;
use std::path::{Path, PathBuf};
use toml;

use ast::{Import, TreePrinter};
use parser::{parse_files};
use llvmbackend::TargetMachine;
use bytecode::{compile_to_byte_code, optimize_module, OptimizationLevel};
use llvmbackend::{CodeGenOptions, OutputType, llvm_code_generation, link, LinkerFlags};
use compileerror::{CompileResult, CompileError};
use typechecker::type_check_package;
use exportlibrary::ExportLibrary;


pub struct BuildOptions
{
    pub optimize: bool,
    pub dump_flags: String,
    pub target_machine: TargetMachine,
    pub sources_directory: String,
    pub import_directories: Vec<PathBuf>,
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageTarget
{
    name: String,
    #[serde(rename = "type")]
    output_type: OutputType,
    path: Option<PathBuf>,
    depends: Option<Vec<String>>,
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
                    depends: None,
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
            t.build(build_options)?;
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

#[derive(Default)]
struct PackageTargetDeps
{
    linker_flags: LinkerFlags,
    imports: Vec<Rc<Import>>,
}


impl PackageTarget
{
    fn find_dependency(&self, dep: &str, build_options: &BuildOptions, deps: &mut PackageTargetDeps) -> CompileResult<()>
    {
        let path = format!("build/{}/{}/{}.mhr.exports", build_options.target_machine.target.triplet, dep, dep);
        if let Ok(file) = File::open(&path) {
            let export_library = ExportLibrary::load(file)?;
            deps.imports.extend(export_library.imports.iter().cloned());

            match export_library.output_type {
                OutputType::StaticLib => {
                    let lib_path = format!("build/{}/{}/lib{}.a",  build_options.target_machine.target.triplet, dep, dep);
                    deps.linker_flags.linker_static_libs.push(lib_path);
                    Ok(())
                }

                OutputType::SharedLib => {
                    let lib_path = format!("build/{}/{}/",  build_options.target_machine.target.triplet, dep);
                    deps.linker_flags.linker_paths.push(lib_path);
                    deps.linker_flags.linker_shared_libs.push(dep.into());
                    Ok(())
                }

                OutputType::Binary => {
                    Err(CompileError::Other(format!("Depedency {} is a binary, it must be a static or shared library", dep)))
                }
            }


        } else {
            Err(CompileError::Other(format!("Unable to find dependency {}", dep)))
        }
    }

    fn find_dependencies(&self, build_options: &BuildOptions) -> CompileResult<PackageTargetDeps>
    {
        let mut pkg_deps = PackageTargetDeps::default();
        if let Some(ref deps) = self.depends {
            for dep in deps {
                self.find_dependency(dep, build_options, &mut pkg_deps)?;
            }
        }

        Ok(pkg_deps)
    }


    fn build(&self, build_options: &BuildOptions) -> CompileResult<()>
    {
        println!("Building target {}", self.name);
        let single_file = format!("{}/{}.mhr", build_options.sources_directory, self.name);
        let dir_name = format!("{}/{}", build_options.sources_directory, self.name);

        let path = if let Some(ref path) = self.path {
            path.as_path()
        } else {
            let path = Path::new(&single_file);
            if path.exists() {
                path
            } else {
                Path::new(&dir_name)
            }
        };

        let pkg_deps = self.find_dependencies(build_options)?;
        let mut pkg = if path.exists() && path.is_file() {
            parse_files(&path, &self.name, &build_options.target_machine.target, &pkg_deps.imports)?
        } else {
            parse_files(&path, &self.name, &build_options.target_machine.target, &pkg_deps.imports)?
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
            build_dir: format!("build/{}/{}", build_options.target_machine.target.triplet, self.name),
            output_file_name: output_file_name(&self.name, self.output_type),
            output_type: self.output_type,
            optimize: build_options.optimize,
        };



        let ctx = llvm_code_generation(&bc_mod, &build_options.target_machine).map_err(CompileError::Other)?;
        link(&ctx, &opts, &pkg_deps.linker_flags)?;

        match opts.output_type
        {
            OutputType::SharedLib | OutputType::StaticLib => {
                let path = format!("{}/{}.mhr.exports", opts.build_dir, self.name);
                let file = File::create(&path)?;
                println!("  Generating {}", path);
                let export_lib = ExportLibrary::new(&pkg, opts.output_type);
                export_lib.save(file)?;
            }

            _ => (),
        }
        Ok(())
    }
}


