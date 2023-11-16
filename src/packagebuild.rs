use serde_derive::Deserialize;
use std::env;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::ast::TreePrinter;
use crate::bytecode::{compile_to_byte_code, optimize_module, OptimizationLevel};
use crate::cli::Dump;
use crate::compileerror::{CompileError, CompileResult};
use crate::exportlibrary::ExportLibrary;
use crate::llvmbackend::TargetMachine;
use crate::llvmbackend::{link, llvm_code_generation, CodeGenOptions, Context, OutputType};
use crate::package::Package;
use crate::timer::{time_operation, time_operation_mut};

pub struct BuildOptions {
    pub optimize: bool,
    pub dump_flags: Option<Dump>,
    pub target_machine: TargetMachine,
    pub sources_directory: PathBuf,
    pub build_directory: PathBuf,
    pub import_directories: Vec<PathBuf>,
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageTarget {
    name: String,
    #[serde(rename = "type")]
    output_type: OutputType,
    path: Option<PathBuf>,
    depends: Option<Vec<String>>,
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageDescription {
    name: String,
    author: String,
    email: String,
    license: String,
    version: String,
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageData {
    package: PackageDescription,
    target: Vec<PackageTarget>,
}

impl PackageData {
    pub fn single_file<P: AsRef<Path>>(path: P, output_type: OutputType) -> CompileResult<PackageData> {
        let p = path.as_ref();
        let name = if let Some(stem) = p.file_stem() {
            stem.to_string_lossy().into()
        } else {
            return Err(CompileError::Other(format!(
                "Cannot determine file stem of {}",
                path.as_ref().to_string_lossy()
            )));
        };

        Ok(PackageData {
            target: vec![PackageTarget {
                name,
                output_type,
                path: Some(p.to_owned()),
                depends: None,
            }],
            package: PackageDescription::default(),
        })
    }

    pub fn load<P: AsRef<Path>>(path: P) -> CompileResult<PackageData> {
        let mut file = File::open(path.as_ref())?;
        let mut package_data = String::with_capacity(file.metadata()?.len() as usize);
        file.read_to_string(&mut package_data)?;

        let package: PackageData = toml::from_str(&package_data)
            .map_err(|e| CompileError::Other(format!("Failed to decode {}: {}", path.as_ref().to_string_lossy(), e)))?;
        Ok(package)
    }

    pub fn build(&self, build_options: &BuildOptions) -> CompileResult<()> {
        let mut ctx = Context::new(&build_options.target_machine)?;
        println!("Compiling for {}", build_options.target_machine.target.triplet);
        for t in &self.target {
            time_operation_mut(2, "Total build time", || t.build(build_options, &mut ctx))?;
        }

        Ok(())
    }
}

fn output_file_name(name: &str, output_type: OutputType) -> String {
    match output_type {
        OutputType::Binary => name.into(),
        OutputType::StaticLib => format!("lib{}.a", name),
        OutputType::SharedLib => format!("lib{}.so", name),
    }
}

impl PackageTarget {
    fn find_dependency_in_path(
        &self,
        dep: &str,
        deps_dir: &Path,
        pkg: &mut Package,
        build_options: &BuildOptions,
    ) -> CompileResult<bool> {
        let target_triplet = &build_options.target_machine.target.triplet;
        let path = deps_dir.join(format!("{}/{}/{}.mhr.exports", target_triplet, dep, dep));
        if let Ok(mut file) = File::open(&path) {
            let target_triplet = &build_options.target_machine.target.triplet;
            if pkg
                .add_library(&mut file, &path, dep, build_options)
                .is_ok()
            {
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    fn find_dependency(&self, dep: &str, build_options: &BuildOptions, pkg: &mut Package) -> CompileResult<()> {
        // Always try the build directory first
        if self.find_dependency_in_path(dep, &build_options.build_directory, pkg, build_options)? {
            return Ok(());
        }

        for import_dir in &build_options.import_directories {
            if self.find_dependency_in_path(dep, &import_dir, pkg, build_options)? {
                return Ok(());
            }
        }

        if let Ok(import_paths) = env::var("MENHIR_IMPORT_DIRS") {
            for path in import_paths.split(':') {
                if self.find_dependency_in_path(dep, &Path::new(path), pkg, build_options)? {
                    return Ok(());
                }
            }
        }

        Err(CompileError::Other(format!("Unable to find dependency {}", dep)))
    }

    fn find_dependencies(&self, build_options: &BuildOptions, pkg: &mut Package) -> CompileResult<()> {
        if let Some(ref deps) = self.depends {
            for dep in deps {
                self.find_dependency(dep, build_options, pkg)?;
            }
        }

        Ok(())
    }

    fn build(&self, build_options: &BuildOptions, ctx: &mut Context) -> CompileResult<()> {
        let single_file = build_options
            .sources_directory
            .join(format!("{}.mhr", self.name));
        let dir_name = build_options.sources_directory.join(&self.name);

        let path = if let Some(path) = &self.path {
            path.to_owned()
        } else {
            if single_file.exists() {
                single_file
            } else {
                dir_name
            }
        };

        let mut pkg = Package::new(&self.name, self.output_type);
        self.find_dependencies(build_options, &mut pkg)?;
        pkg.find_input_files(&path, build_options)?;
        if !pkg.rebuild_needed(build_options) {
            println!("No build needed for {}", self.name);
            return Ok(());
        } else {
            println!("Building target {}", self.name);
        }

        pkg.save_inputs(build_options)?;
        pkg.parse_files(build_options)?;

        time_operation_mut(2, "Type checking", || {
            pkg.type_check(&build_options.target_machine.target)
        })?;

        match build_options.dump_flags {
            Some(Dump::AST) | Some(Dump::All) => {
                println!("AST: {}", pkg.name);
                pkg.print(0);
            }
            _ => (),
        }

        let mut bc_mod = time_operation(2, "Compile to bytecode", || {
            compile_to_byte_code(&pkg, &build_options.target_machine.target)
        })?;

        let build_dir = build_options
            .build_directory
            .join(format!("{}/{}", build_options.target_machine.target.triplet, self.name));
        std::fs::create_dir_all(&build_dir)?;

        let mut bc_dump = File::create(build_dir.join(format!("{}.bc", self.name)))?;
        writeln!(&mut bc_dump, "{bc_mod}")?;
        drop(bc_dump);

        match &build_options.dump_flags {
            Some(Dump::All) | Some(Dump::ByteCode) => {
                println!("bytecode:");
                println!("------\n");
                println!("{}", bc_mod);
                println!("------\n");
            }
            _ => (),
        }

        time_operation_mut(2, "Optimization", || {
            if build_options.optimize {
                optimize_module(&mut bc_mod, OptimizationLevel::Normal);
            } else {
                optimize_module(&mut bc_mod, OptimizationLevel::Minimal);
            }
        });

        let opts = CodeGenOptions {
            dump_ir: matches!(build_options.dump_flags, Some(Dump::IR) | Some(Dump::All)),
            build_dir,
            output_file_name: output_file_name(&self.name, self.output_type),
            output_type: self.output_type,
            optimize: build_options.optimize,
        };

        time_operation_mut(2, "Code generation", || {
            llvm_code_generation(&bc_mod, ctx, &build_options.target_machine)
        })?;

        time_operation(2, "Linking", || link(&ctx, &opts, &pkg.linker_flags))?;

        match opts.output_type {
            OutputType::SharedLib | OutputType::StaticLib => {
                let path = opts.build_dir.join(format!("{}.mhr.exports", self.name));
                let mut file = File::create(&path)?;
                println!("  Generating {}", path.display());
                let export_lib = ExportLibrary::new(&pkg, opts.output_type);
                export_lib.save(&mut file)?;
            }

            _ => (),
        }
        Ok(())
    }
}
