use std::rc::Rc;
use std::io::Read;
use std::collections::HashMap;
use std::path::Path;
use std::ffi::OsStr;
use either::Either;

use ast::{Module, Import, ImportMap, TreePrinter, prefix};
use llvmbackend::{LinkerFlags, OutputType};
use compileerror::{CompileResult, CompileError, type_error};
use exportlibrary::ExportLibrary;
use parser::parse_file;
use target::Target;
use typechecker::type_check_module;
use span::Span;

type MissingImportsMap = HashMap<String, Span>;

pub struct ImportData
{
    pub imports: HashMap<String, Rc<Import>>,
    pub libraries: Vec<ExportLibrary>,
}

impl ImportData
{
    fn find_import(&self, import_name: &str) -> Option<Rc<Import>>
    {
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

    fn resolve_module_imports(&self, module: &Module) -> Either<ImportMap, MissingImportsMap>
    {
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

pub struct Package
{
    pub name: String,
    pub modules: HashMap<String, Module>,
    pub import_data: ImportData,
    pub linker_flags: LinkerFlags,
}

impl Package
{
    pub fn new(name: &str) -> Package
    {
        Package{
            name: name.into(),
            modules: HashMap::new(),
            import_data: ImportData{
                imports: ImportMap::new(),
                libraries: Vec::new(),
            },
            linker_flags: LinkerFlags::default(),
        }
    }

    pub fn add_library<R: Read>(&mut self, input: &mut R, dep: &str, deps_dir: &str, target_triplet: &str) -> Result<(), String>
    {
        let export_library = ExportLibrary::load(input)?;
        match export_library.output_type {
            OutputType::StaticLib => {
                let lib_path = format!("{}/{}/{}/lib{}.a", deps_dir, target_triplet, dep, dep);
                self.linker_flags.linker_static_libs.push(lib_path);
            }

            OutputType::SharedLib => {
                let lib_path = format!("{}/{}/{}/", deps_dir, target_triplet, dep);
                self.linker_flags.linker_paths.push(lib_path);
                self.linker_flags.linker_shared_libs.push(dep.into());
            }

            OutputType::Binary => {
                return Err(format!("Cannot add a binary as a library"));
            }
        }

        self.import_data.libraries.push(export_library);
        Ok(())
    }

    fn parse_file_tree(&mut self, dir: &Path, namespace: &str, target: &Target) -> CompileResult<()>
    {
        for entry in dir.read_dir()? {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.is_dir() {
                    let sub_ns = format!("{}::{}", namespace, path.file_stem().expect("Path must have a stem").to_string_lossy());
                    self.parse_file_tree(&path, &sub_ns, target)?;
                } else if path.extension() == Some(OsStr::new("mhr")) {
                    let sub_ns = format!("{}::{}", namespace, path.file_stem().expect("Path must have a stem").to_string_lossy());
                    let module = parse_file(&path, &sub_ns, target)?;
                    self.modules.insert(sub_ns, module);
                }
            }
        }

        Ok(())
    }

    pub fn parse_files(&mut self, path: &Path, target: &Target) -> CompileResult<()>
    {
        if path.exists() && path.is_file() {
            self.modules.insert(self.name.clone(), parse_file(path, &self.name, target)?);
        } else {
            if !path.exists() || !path.is_dir() {
                return Err(CompileError::Other(format!("Cannot find {}.mhr or the directory {}", self.name, self.name)))
            }
            let namespace = self.name.clone();
            self.parse_file_tree(path, &namespace, target)?;
        }

        Ok(())
    }

    pub fn type_check(&mut self, target: &Target) -> CompileResult<()>
    {
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
                        self.import_data.imports.insert(module.name.clone(), Rc::new(module.get_exported_symbols(target)));
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
                return Err(CompileError::Many(errors))
            }
        }

        Ok(())
    }
}

impl TreePrinter for Package
{
    fn print(&self, level: usize)
    {
        let p = prefix(level);
        for module in self.modules.values() {
            println!("{}module: {}", p, module.name);
            module.print(level + 1);
            println!("{}--------------------\n", p);
        }
    }
}




