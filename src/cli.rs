use std::path::PathBuf;
use clap::{Subcommand, Parser, ValueEnum};

#[derive(ValueEnum, Debug, Clone, Copy)]
#[clap(rename_all="lower")]
pub enum Dump {
    All,
    AST,
    ByteCode,
    IR,
}

#[derive(ValueEnum, Debug, Clone, Copy)]
#[clap(rename_all="lower")]
pub enum LibraryType {
    Static,
    Shared,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct CLI {
    #[command(subcommand)]
    pub command: CompilerCommand,
}

#[derive(Subcommand, Debug)]
pub enum CompilerCommand {
    /// Build a menhir file
    Build(BuildCommand),
    /// Build a menhir package
    BuildPkg(BuildPkgCommand),
    /// List the exported symbols in a exports file
    Exports(ExportsCommand),
    Info{
        /// Print the default target triplet of the current system, and exit
        #[arg(short='t', long="triplet", default_value_t=false)]
        triplet: bool,
    }
}


#[derive(Args, Debug)]
pub struct BuildCommand {
    /// File to build
    #[arg(required=true, value_name="MENHIR_FILE")]
    pub input_file: PathBuf,
    /// Name of binary to create (by default input file without the extensions)
    #[arg(short='o', long="output", value_name="EXECUTABLE")]
    pub output_file: Option<PathBuf>,
    /// Optimize the code
    #[arg(short='O', long="optimize", default_value_t=false)]
    pub optimize: bool,
    /// Directory to look for imports, use a comma separated list for more then one.
    #[arg(short='I', long="imports", use_value_delimiter=true, value_delimiter=',')]
    pub imports: Vec<PathBuf>,
    #[arg(short='l', long="lib")]
    pub lib: Option<LibraryType>,
    /// Dump internal compiler state for debug purposes.
    #[arg(short='d', long="dump")]
    pub dump: Option<Dump>,
}

#[derive(Args,Debug)]
pub struct BuildPkgCommand {
    /// Specify the package.toml file. If not specified, menhir will look in the current directory for one.
    #[arg(short='i', long="input", value_name="PACKAGE_TOML")]
    pub input_file: Option<PathBuf>,
    /// Optimize the code
    #[arg(short='O', long="optimize", default_value_t=false)]
    pub optimize: bool,
    /// Directory to look for imports, use a comma separated list for more then one.
    #[arg(short='I', long="imports", use_value_delimiter=true, value_delimiter=',')]
    pub imports: Vec<PathBuf>,
    /// Dump internal compiler state for debug purposes.
    #[arg(short='d', long="dump")]
    pub dump: Option<Dump>,
}

#[derive(Args, Debug)]
pub struct ExportsCommand {
    /// List the exported symbols in an exports file"
    #[arg(required=true, value_name="EXPORTS")]
    pub exports_file: PathBuf,
}
