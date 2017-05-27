use std::rc::Rc;
use std::io;
use std::fmt;
use ast::{Package, Import};
use llvmbackend::OutputType;

use serde::Serialize;
use rmp_serde;

#[derive(Serialize, Deserialize)]
pub struct ExportLibrary
{
    pub name: String,
    pub imports: Vec<Rc<Import>>,
    pub output_type: OutputType,
}

impl ExportLibrary
{
    pub fn new(pkg: &Package, output_type: OutputType) -> ExportLibrary
    {
        ExportLibrary{
            name: pkg.name.clone(),
            output_type,
            imports: pkg.imports.values().cloned().collect(),
        }
    }

    pub fn load<R: io::Read>(reader: R) -> Result<ExportLibrary, String>
    {
        let result = rmp_serde::decode::from_read(reader)
            .map_err(|e| format!("Deserialization error: {}", e))?;
        Ok(result)
    }

    pub fn save<W: io::Write>(&self, writer: W) -> Result<(), String>
    {
        let mut s = rmp_serde::Serializer::new(writer);
        self.serialize(&mut s)
            .map_err(|e| format!("Serialization error: {}", e))
    }
}

impl fmt::Display for ExportLibrary
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        writeln!(f, "Name: {}", self.name)?;
        writeln!(f, "Type: {}", self.output_type)?;
        for import in &self.imports {
            write!(f, "{}", import)?;
        }

        Ok(())
    }
}
