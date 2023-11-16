use std::io::{Read, Write};
use std::{collections::HashSet, path::PathBuf};

use serde_derive::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Hash, PartialEq, Eq)]
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
}

impl BuildInputs {
    pub fn add(&mut self, b: BuildInput) {
        self.inputs.insert(b);
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
