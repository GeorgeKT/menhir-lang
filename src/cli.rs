use clap::{Args, Parser, Subcommand, ValueEnum};
use std::path::PathBuf;

#[derive(ValueEnum, Debug, Clone, Copy)]
#[clap(rename_all = "lower")]
#[allow(clippy::upper_case_acronyms)]
pub enum Dump {
    All,
    AST,
    ByteCode,
    IR,
}

#[derive(ValueEnum, Debug, Clone, Copy)]
#[clap(rename_all = "lower")]
pub enum LibraryType {
    Static,
    Shared,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[allow(clippy::upper_case_acronyms)]
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
    Info(InfoCommand),
    Clean(CleanCommand),
    Run(RunCommand),
}

#[derive(Args, Debug)]
pub struct BuildCommand {
    /// File to build
    #[arg(required = true, value_name = "MENHIR_FILE")]
    pub input_file: PathBuf,
    /// Name of binary to create (by default input file without the extensions)
    #[arg(short = 'o', long = "output", value_name = "EXECUTABLE")]
    pub output_file: Option<PathBuf>,
    /// Optimize the code
    #[arg(short = 'O', long = "optimize", default_value_t = false)]
    pub optimize: bool,
    /// Directory to look for imports, use a comma separated list for more then one.
    #[arg(short = 'I', long = "imports", use_value_delimiter = true, value_delimiter = ',')]
    pub imports: Vec<PathBuf>,
    #[arg(short = 'l', long = "lib")]
    pub lib: Option<LibraryType>,
    /// Dump internal compiler state for debug purposes.
    #[arg(short = 'd', long = "dump")]
    pub dump: Option<Dump>,
}

#[derive(Args, Debug)]
pub struct BuildPkgCommand {
    /// Specify the package.toml file. If not specified, menhir will look in the current directory for one.
    #[arg(short = 'i', long = "input", value_name = "PACKAGE_TOML")]
    pub input_file: Option<PathBuf>,
    /// Optimize the code
    #[arg(short = 'O', long = "optimize", default_value_t = false)]
    pub optimize: bool,
    /// Directory to look for imports, use a comma separated list for more then one.
    #[arg(short = 'I', long = "imports", use_value_delimiter = true, value_delimiter = ',')]
    pub imports: Vec<PathBuf>,
    /// Dump internal compiler state for debug purposes.
    #[arg(short = 'd', long = "dump")]
    pub dump: Option<Dump>,
}

#[derive(Args, Debug)]
pub struct ExportsCommand {
    /// List the exported symbols in an exports file"
    #[arg(required = true, value_name = "EXPORTS")]
    pub exports_file: PathBuf,
}

#[derive(Args, Debug)]
pub struct CleanCommand {
    /// Specify the package.toml file. If not specified, menhir will look in the current directory for one.
    #[arg(short = 'i', long = "input", value_name = "PACKAGE_TOML")]
    pub input_file: Option<PathBuf>,
}

#[derive(Args, Debug)]
pub struct InfoCommand {
    /// Print the default target triplet of the current system, and exit
    #[arg(short = 't', long = "triplet", default_value_t = false)]
    pub triplet: bool,
}

#[derive(Args, Debug)]
#[clap(trailing_var_arg = true)]
pub struct RunCommand {
    /// Specify the package.toml file. If not specified, menhir will look in the current directory for one.
    #[arg(short = 'i', long = "input", value_name = "PACKAGE_TOML")]
    pub input_file: Option<PathBuf>,
    #[arg(short = 't', long = "target", value_name = "TARGET")]
    pub target: Option<String>,
    /// Optimize the code
    #[arg(short = 'O', long = "optimize", default_value_t = false)]
    pub optimize: bool,
    /// Directory to look for imports, use a comma separated list for more then one.
    #[arg(short = 'I', long = "imports", use_value_delimiter = true, value_delimiter = ',')]
    pub imports: Vec<PathBuf>,
    /// Dump internal compiler state for debug purposes.
    #[arg(short = 'd', long = "dump")]
    pub dump: Option<Dump>,
    #[clap(trailing_var_arg = true, allow_hyphen_values = true)]
    pub command_args: Vec<String>,
}
