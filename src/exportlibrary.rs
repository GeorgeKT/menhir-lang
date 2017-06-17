use std::rc::Rc;
use std::io;
use std::fmt;
use ast::{Package, Import};
use llvmbackend::OutputType;
use bincode;

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

    pub fn load<R: io::Read>(reader: &mut R) -> Result<ExportLibrary, String>
    {
        bincode::deserialize_from(reader, bincode::Infinite)
            .map_err(|e| format!("Deserialization error: {}", e))
    }

    pub fn save<W: io::Write>(&self, writer: &mut W) -> Result<(), String>
    {
        bincode::serialize_into(writer, self, bincode::Infinite)
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
