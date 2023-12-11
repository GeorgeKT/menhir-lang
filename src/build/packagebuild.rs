use console::style;
use serde_derive::{Deserialize, Serialize};
use std::env;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::ExitStatus;

use super::{ExportLibrary, Package};
use crate::ast::TreePrinter;
use crate::cli::Dump;
use crate::cli::RunCommand;
use crate::compileerror::{CompileError, CompileResult};
use crate::lazycode::{compile_to_byte_code, optimize_module, OptimizationLevel};
use crate::llvmbackend::TargetMachine;
use crate::llvmbackend::{link, llvm_code_generation, CodeGenOptions, Context, OutputType};
use crate::timer::{time_operation, time_operation_mut};

pub struct CleanOptions {
    pub target_machine: TargetMachine,
    pub build_directory: PathBuf,
}

pub struct BuildOptions {
    pub optimize: bool,
    pub dump_flags: Vec<Dump>,
    pub target_machine: TargetMachine,
    pub sources_directory: PathBuf,
    pub build_directory: PathBuf,
    pub import_directories: Vec<PathBuf>,
    pub force_rebuild: bool,
    pub show_timing: bool,
}

impl BuildOptions {
    pub fn target_build_dir(&self, target: &str) -> PathBuf {
        self.build_directory
            .join(format!("{}/{}", self.target_machine.target.triplet, target))
    }
}

#[derive(Debug, Deserialize, Default)]
pub struct PackageTarget {
    name: String,
    #[serde(rename = "type")]
    output_type: OutputType,
    path: Option<PathBuf>,
    depends: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, Eq, PartialEq)]
pub struct PackageDescription {
    pub name: String,
    pub author: String,
    pub email: String,
    pub license: String,
    pub version: String,
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
            .map_err(|e| format!("Failed to decode {}: {}", path.as_ref().to_string_lossy(), e))?;
        Ok(package)
    }

    fn get_target(&self, target: &str) -> Option<&PackageTarget> {
        for t in &self.target {
            if t.name == target {
                return Some(t);
            }
        }
        None
    }

    fn build_target_and_dependencies(&self, build_options: &BuildOptions, target: &str) -> CompileResult<()> {
        let Some(t) = self.get_target(target) else {
            return Err(CompileError::Other(format!("Unknown target {target}")));
        };

        if let Some(deps) = &t.depends {
            for dep in deps {
                if self.get_target(dep).is_some() {
                    self.build_target_and_dependencies(build_options, dep)?;
                }
            }
        }

        time_operation_mut(build_options.show_timing, 2, "Total build time", || {
            t.build(build_options, &self.package)
        })?;
        Ok(())
    }

    pub fn build(&self, build_options: &BuildOptions, target_name: &Option<String>) -> CompileResult<()> {
        println!("Compiling for {}", build_options.target_machine.target.triplet);
        if let Some(tgt) = target_name {
            return self.build_target_and_dependencies(build_options, tgt.as_str());
        }

        for t in &self.target {
            time_operation_mut(build_options.show_timing, 2, "Total build time", || {
                t.build(build_options, &self.package)
            })?;
        }
        Ok(())
    }

    pub fn clean(&self, clean_options: &CleanOptions) -> CompileResult<()> {
        let triplet = &clean_options.target_machine.target.triplet;
        for target in &self.target {
            let path = clean_options
                .build_directory
                .join(format!("{}/{}", triplet, target.name));
            if path.exists() {
                std::fs::remove_dir_all(&path).map_err(|e| format!("Failed to remove {}: {}", path.display(), e))?;
            }
        }
        let triplet_dir = clean_options.build_directory.join(triplet);
        if triplet_dir.exists() && std::fs::read_dir(&triplet_dir)?.count() == 0 {
            std::fs::remove_dir_all(&triplet_dir)
                .map_err(|e| format!("Failed to remove {}: {}", triplet_dir.display(), e))?;
        }

        if clean_options.build_directory.exists() && std::fs::read_dir(&clean_options.build_directory)?.count() == 0 {
            std::fs::remove_dir_all(&clean_options.build_directory)
                .map_err(|e| format!("Failed to remove {}: {}", clean_options.build_directory.display(), e))?;
        }
        Ok(())
    }

    fn get_run_target(&self, target: &Option<String>) -> CompileResult<&PackageTarget> {
        if let Some(target) = target {
            for t in &self.target {
                if &t.name != target {
                    continue;
                }
                if t.output_type != OutputType::Binary {
                    return Err(CompileError::Other(format!("Target {} is not a binary", t.name)));
                }

                return Ok(t);
            }

            Err(CompileError::Other(format!("There is no target named {}", target)))
        } else {
            let mut candidate = None;
            for t in &self.target {
                if t.output_type == OutputType::Binary {
                    if candidate.is_some() {
                        return Err(CompileError::Other(format!("Multiple binaries are available, please choose the binary to run using the -t or --target option.")));
                    }
                    candidate = Some(t);
                }
            }

            return candidate.ok_or_else(|| CompileError::Other(format!("This package has no binary to run")));
        }
    }

    pub fn run(
        &self,
        target: &Option<String>,
        build_options: &BuildOptions,
        r: &RunCommand,
    ) -> CompileResult<ExitStatus> {
        let t = self.get_run_target(target)?;
        let binary = build_options.target_build_dir(&t.name).join(&t.name);
        let mut cmd = Command::new(binary);
        cmd.args(r.command_args.iter());
        println!("{} {}", style("Running").green(), t.name);
        let mut p = cmd
            .spawn()
            .map_err(|e| format!("Failed to spawn {}: {e}", t.name))?;
        let ec = p
            .wait()
            .map_err(|e| format!("Failed to wait on {}: {e}", t.name))?;
        Ok(ec)
    }
}

pub fn output_file_name(name: &str, output_type: OutputType) -> String {
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

    fn build(&self, build_options: &BuildOptions, desc: &PackageDescription) -> CompileResult<()> {
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

        let mut pkg = Package::new(&self.name, self.output_type, desc);
        self.find_dependencies(build_options, &mut pkg)?;
        pkg.find_input_files(&path, build_options)?;
        if !pkg.rebuild_needed(build_options) {
            return Ok(());
        } else {
            println!("{} {}", style("Building").green(), self.name);
        }

        pkg.save_inputs(build_options)?;
        pkg.parse_files(build_options)?;

        time_operation_mut(build_options.show_timing, 2, "Type checking", || {
            pkg.type_check(&build_options.target_machine.target)
        })?;

        if build_options.dump_flags.contains(&Dump::AST) || build_options.dump_flags.contains(&Dump::All) {
            println!("AST: {}", pkg.name);
            pkg.print(0);
        }

        let mut bc_mod = time_operation(build_options.show_timing, 2, "Compile to bytecode", || {
            compile_to_byte_code(&pkg, &build_options.target_machine.target)
        })?;

        let build_dir = build_options
            .build_directory
            .join(format!("{}/{}", build_options.target_machine.target.triplet, self.name));
        std::fs::create_dir_all(&build_dir)?;

        let mut bc_dump = File::create(build_dir.join(format!("{}.bc", self.name)))?;
        writeln!(&mut bc_dump, "{bc_mod}")?;
        drop(bc_dump);

        if build_options.dump_flags.contains(&Dump::ByteCode) || build_options.dump_flags.contains(&Dump::All) {
            println!("bytecode:");
            println!("------\n");
            println!("{}", bc_mod);
            println!("------\n");
        }

        time_operation_mut(build_options.show_timing, 2, "Optimization", || {
            if build_options.optimize {
                optimize_module(&mut bc_mod, OptimizationLevel::Normal);
            } else {
                optimize_module(&mut bc_mod, OptimizationLevel::Minimal);
            }
        });

        let opts = CodeGenOptions {
            dump_ir: build_options.dump_flags.contains(&Dump::IR) || build_options.dump_flags.contains(&Dump::All),
            build_dir,
            output_file_name: output_file_name(&self.name, self.output_type),
            output_type: self.output_type,
            optimize: build_options.optimize,
        };

        let mut ctx = Context::new(&self.name)?;
        time_operation_mut(build_options.show_timing, 2, "Code generation", || {
            llvm_code_generation(&bc_mod, &mut ctx, desc)
        })?;

        time_operation(build_options.show_timing, 2, "Linking", || {
            link(&ctx, &opts, &pkg.linker_flags)
        })?;

        match opts.output_type {
            OutputType::SharedLib | OutputType::StaticLib => {
                let path = opts.build_dir.join(format!("{}.mhr.exports", self.name));
                let mut file = File::create(&path)?;
                let export_lib = ExportLibrary::new(&pkg, opts.output_type);
                export_lib.save(&mut file)?;
            }

            _ => (),
        }
        Ok(())
    }
}
