use either::Either;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

use crate::ast::{prefix, Import, ImportMap, Module, TreePrinter};
use crate::buildinputs::{BuildInput, BuildInputs};
use crate::compileerror::{type_error, CompileError, CompileResult};
use crate::exportlibrary::ExportLibrary;
use crate::llvmbackend::{LinkerFlags, OutputType};
use crate::packagebuild::BuildOptions;
use crate::parser::parse_file;
use crate::span::Span;
use crate::target::Target;
use crate::typechecker::type_check_module;

type MissingImportsMap = HashMap<String, Span>;

pub struct ImportData {
    pub imports: HashMap<String, Rc<Import>>,
    pub libraries: Vec<ExportLibrary>,
}

impl ImportData {
    fn find_import(&self, import_name: &str) -> Option<Rc<Import>> {
        if let Some(i) = self.imports.get(import_name) {
            return Some(i.clone());
        }

        for lib in &self.libraries {
            if let Some(i) = lib.find_import(import_name) {
                return Some(i);
            }
        }

        None
    }

    fn resolve_module_imports(&self, module: &Module) -> Either<ImportMap, MissingImportsMap> {
        let mut missing = MissingImportsMap::new();
        let mut imports = ImportMap::new();
        // Try to resolve all the imports
        for import_name in &module.import_names {
            let import = import_name.to_namespace_string();
            if imports.contains_key(&import) {
                continue;
            }

            if let Some(i) = self.find_import(&import) {
                imports.insert(import, i.clone());
            } else {
                missing.insert(import, import_name.span.clone());
            }
        }

        if missing.is_empty() {
            Either::Left(imports)
        } else {
            Either::Right(missing)
        }
    }
}

pub struct Package {
    pub name: String,
    pub modules: HashMap<String, Module>,
    pub inputs: BuildInputs,
    pub import_data: ImportData,
    pub linker_flags: LinkerFlags,
    pub output_type: OutputType,
}

impl Package {
    pub fn new(name: &str, output_type: OutputType) -> Package {
        Package {
            name: name.into(),
            modules: HashMap::new(),
            inputs: BuildInputs::default(),
            import_data: ImportData {
                imports: ImportMap::new(),
                libraries: Vec::new(),
            },
            linker_flags: LinkerFlags::default(),
            output_type,
        }
    }

    pub fn add_library<R: Read>(
        &mut self,
        input: &mut R,
        path: &Path,
        dep: &str,
        build_options: &BuildOptions,
    ) -> Result<(), String> {
        let export_library = ExportLibrary::load(input)?;
        let target_triplet = &build_options.target_machine.target.triplet;
        match export_library.output_type {
            OutputType::StaticLib => {
                let lib_path = build_options
                    .build_directory
                    .join(format!("{}/{}/lib{}.a", target_triplet, dep, dep));
                self.linker_flags
                    .linker_static_libs
                    .push(lib_path.to_owned());
            }

            OutputType::SharedLib => {
                let lib_path = build_options
                    .build_directory
                    .join(format!("{}/{}/", target_triplet, dep));
                self.linker_flags.linker_paths.push(lib_path.to_owned());
                self.linker_flags.linker_shared_libs.push(dep.into());
            }

            OutputType::Binary => {
                return Err("Cannot add a binary as a library".to_string());
            }
        }

        self.import_data.libraries.push(export_library);
        let root = build_options
            .build_directory
            .parent()
            .ok_or_else(|| format!("Build directory has no parent directory"))?;
        self.inputs.add(BuildInput::ExportsFile {
            digest: sha256::try_digest(path)
                .map_err(|e| format!("Failed to calculate digest of {}: {e}", path.display()))?,
            path: path
                .strip_prefix(root)
                .map_err(|e| format!("Failed to strip prefix of export library path"))?
                .to_owned(),
        });

        Ok(())
    }

    fn rec_find_input_files(&mut self, dir: &Path, namespace: &str, build_options: &BuildOptions) -> CompileResult<()> {
        for entry in dir.read_dir()?.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let sub_ns = format!(
                    "{}::{}",
                    namespace,
                    path.file_stem()
                        .and_then(|s| s.to_str())
                        .expect("Path must have a stem")
                );
                self.rec_find_input_files(&path, &sub_ns, build_options)?;
            } else if path.extension() == Some(OsStr::new("mhr")) {
                let sub_ns = format!(
                    "{}::{}",
                    namespace,
                    path.file_stem()
                        .and_then(|s| s.to_str())
                        .expect("Path must have a stem")
                );
                self.inputs.add(BuildInput::SourceFile {
                    digest: sha256::try_digest(&path)
                        .map_err(|e| format!("Failed to calculate digest of {}: {e}", path.display()))?,
                    path: path
                        .strip_prefix(&build_options.sources_directory)
                        .map_err(|e| format!("Failed to strip prefix of source path: {e}"))?
                        .to_owned(),
                    namespace: sub_ns,
                });
            }
        }

        Ok(())
    }

    pub fn rebuild_needed(&self, build_options: &BuildOptions) -> bool {
        let output_file = match self.output_type {
            OutputType::Binary => build_options.build_directory.join(format!(
                "{}/{}/{}",
                build_options.target_machine.target.triplet, self.name, self.name
            )),
            OutputType::StaticLib => build_options.build_directory.join(format!(
                "{}/{}/lib{}.a",
                build_options.target_machine.target.triplet, self.name, self.name
            )),
            OutputType::SharedLib => build_options.build_directory.join(format!(
                "{}/{}/lib{}.so",
                build_options.target_machine.target.triplet, self.name, self.name
            )),
        };

        if !output_file.exists() {
            return true;
        }

        let inputs_file_path = build_options.build_directory.join(format!(
            "{}/{}/inputs",
            build_options.target_machine.target.triplet, self.name
        ));
        let Ok(mut inputs_file) = File::open(inputs_file_path) else {
            return true;
        };

        let Ok(inputs) = BuildInputs::load(&mut inputs_file) else {
            return true;
        };

        if inputs != self.inputs {
            return true;
        }

        false
    }

    pub fn save_inputs(&self, build_options: &BuildOptions) -> CompileResult<()> {
        let inputs_file_path = build_options.build_directory.join(format!(
            "{}/{}/inputs",
            build_options.target_machine.target.triplet, self.name
        ));
        std::fs::create_dir_all(inputs_file_path.parent().expect("Parent path must exist"))?;

        let mut file = File::create(&inputs_file_path)
            .map_err(|e| CompileError::Other(format!("Cannot open {}: {e}", inputs_file_path.display())))?;
        self.inputs
            .save(&mut file)
            .map_err(|e| CompileError::Other(format!("Cannot save {}: {e}", inputs_file_path.display())))?;
        Ok(())
    }

    pub fn parse_files(&mut self, build_options: &BuildOptions) -> CompileResult<()> {
        for file in &self.inputs.inputs {
            match file {
                BuildInput::SourceFile { path, namespace, .. } => {
                    let target = &build_options.target_machine.target;
                    let module = parse_file(&build_options.sources_directory.join(path), namespace, target)?;
                    self.modules.insert(namespace.clone(), module);
                }
                BuildInput::ExportsFile { .. } => {}
            }
        }
        Ok(())
    }

    pub fn find_input_files(&mut self, path: &Path, build_options: &BuildOptions) -> CompileResult<()> {
        if path.exists() && path.is_file() {
            self.inputs.add(BuildInput::SourceFile {
                digest: sha256::try_digest(path)
                    .map_err(|e| format!("Failed to calculate digest of {}: {e}", path.display()))?,
                path: path
                    .strip_prefix(&build_options.sources_directory)
                    .map_err(|e| format!("Failed to strip prefix of source path: {e}"))?
                    .to_owned(),
                namespace: self.name.clone(),
            })
        } else {
            if !path.exists() || !path.is_dir() {
                return Err(CompileError::Other(format!(
                    "Cannot find {}.mhr or the directory {}",
                    self.name, self.name
                )));
            }
            let namespace = self.name.clone();
            self.rec_find_input_files(path, &namespace, build_options)?;
        }

        Ok(())
    }

    pub fn type_check(&mut self, target: &Target) -> CompileResult<()> {
        let mut count = 0;
        while count < self.modules.len() {
            let count_at_start = count;
            let mut all_missing_imports = MissingImportsMap::new();

            for module in self.modules.values_mut() {
                if module.type_checked {
                    continue;
                }

                match self.import_data.resolve_module_imports(module) {
                    Either::Left(imports) => {
                        type_check_module(module, target, &imports)?;
                        self.import_data
                            .imports
                            .insert(module.name.clone(), Rc::new(module.get_exported_symbols(target)));
                        count += 1;
                    }

                    Either::Right(mut missing) => {
                        all_missing_imports.extend(missing.drain());
                    }
                }
            }

            if count_at_start == count {
                let errors = all_missing_imports
                    .iter()
                    .map(|(name, span)| type_error(span, format!("Unknown import {}", name)))
                    .collect();
                return Err(CompileError::Many(errors));
            }
        }

        Ok(())
    }
}

impl TreePrinter for Package {
    fn print(&self, level: usize) {
        let p = prefix(level);
        for module in self.modules.values() {
            println!("{}module: {}", p, module.name);
            module.print(level + 1);
            println!("{}--------------------\n", p);
        }
    }
}
