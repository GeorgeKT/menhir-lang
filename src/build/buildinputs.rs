use std::io::{Read, Write};
use std::{collections::HashSet, path::PathBuf};

use serde_derive::{Deserialize, Serialize};

use super::{BuildOptions, PackageDescription};

#[derive(Serialize, Deserialize, Hash, PartialEq, Eq, Default)]
pub struct BuildFlags {
    pub optimize: bool,
    pub import_directories: Vec<PathBuf>,
    pub target: String,
}

#[derive(Debug, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum BuildInput {
    SourceFile {
        path: PathBuf,
        namespace: String,
        digest: String,
    },
    ExportsFile {
        path: PathBuf,
        digest: String,
    },
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct BuildInputs {
    pub inputs: HashSet<BuildInput>,
    pub flags: BuildFlags,
    pub description: PackageDescription,
}

impl BuildInputs {
    pub fn add(&mut self, b: BuildInput) {
        self.inputs.insert(b);
    }

    pub fn set_flags(&mut self, build_options: &BuildOptions) {
        self.flags.optimize = build_options.optimize;
        self.flags.import_directories = build_options.import_directories.clone();
        self.flags.target = build_options.target_machine.target.triplet.clone();
    }

    pub fn load<R: Read>(reader: &mut R) -> Result<BuildInputs, String> {
        let mut data = String::new();
        reader
            .read_to_string(&mut data)
            .map_err(|e| format!("Failed to read inputs file: {e}"))?;
        ron::from_str(&data).map_err(|e| format!("Deserialization error: {}", e))
    }

    pub fn save<W: Write>(&self, writer: &mut W) -> Result<(), String> {
        let data = ron::ser::to_string_pretty(self, ron::ser::PrettyConfig::default())
            .map_err(|e| format!("Failed to serialize inputs file: {e}"))?;
        writer
            .write(data.as_bytes())
            .map_err(|e| format!("Failed to write inputs file: {}", e))?;
        Ok(())
    }
}
