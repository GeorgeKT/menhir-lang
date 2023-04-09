use crate::ast::Import;
use bincode;
use crate::llvmbackend::OutputType;
use crate::package::Package;
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
        bincode::deserialize_from(reader).map_err(|e| format!("Deserialization error: {}", e))
    }

    pub fn save<W: io::Write>(&self, writer: &mut W) -> Result<(), String> {
        bincode::serialize_into(writer, self).map_err(|e| format!("Serialization error: {}", e))
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
