use crate::ast::Import;
use crate::llvmbackend::OutputType;
use crate::package::Package;

use serde_derive::{Deserialize, Serialize};
use std::fmt;
use std::io;
use std::rc::Rc;

#[derive(Serialize, Deserialize)]
pub struct ExportLibrary {
    pub name: String,
    pub imports: Vec<Rc<Import>>,
    pub output_type: OutputType,
}

impl ExportLibrary {
    pub fn new(pkg: &Package, output_type: OutputType) -> ExportLibrary {
        ExportLibrary {
            name: pkg.name.clone(),
            output_type,
            imports: pkg.import_data.imports.values().cloned().collect(),
        }
    }

    pub fn load<R: io::Read>(reader: &mut R) -> Result<ExportLibrary, String> {
        let mut data = String::new();
        reader
            .read_to_string(&mut data)
            .map_err(|e| format!("Failed to read exports library: {e}"))?;
        ron::from_str(&data).map_err(|e| format!("Failed to deserialize export library: {}", e))
    }

    pub fn save<W: io::Write>(&self, writer: &mut W) -> Result<(), String> {
        let data = ron::ser::to_string_pretty(self, ron::ser::PrettyConfig::default())
            .map_err(|e| format!("Failed to serialize exports library: {e}"))?;
        writer
            .write(data.as_bytes())
            .map_err(|e| format!("Failed to write exports library: {}", e))?;
        Ok(())
    }

    pub fn find_import(&self, import_name: &str) -> Option<Rc<Import>> {
        for import in &self.imports {
            if import.namespace == import_name {
                return Some(import.clone());
            }
        }

        None
    }
}

impl fmt::Display for ExportLibrary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Name: {}", self.name)?;
        writeln!(f, "Type: {}", self.output_type)?;
        for import in &self.imports {
            write!(f, "{}", import)?;
        }

        Ok(())
    }
}
