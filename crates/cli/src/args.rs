use clap::{ArgAction, Parser, Subcommand};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(name = "jslt", version, about = "JSLT command-line interface")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Command>,

    // Legacy flags (for backwards compatibility when no subcommand is used)
    /// Path to a JSLT program file. Use '-' to read the program from stdin.
    #[arg(short = 'p', long = "program", value_name = "FILE", global = true)]
    pub program: Option<String>,

    /// Inline JSLT expression to evaluate instead of a file.
    #[arg(short = 'e', long = "eval", value_name = "EXPR", conflicts_with = "program", global = true)]
    pub eval: Option<String>,

    /// JSON input file. Use '-' to read the input from stdin.
    #[arg(short = 'i', long = "input", value_name = "FILE", global = true)]
    pub input: Option<String>,

    /// Pretty-print output JSON.
    #[arg(long = "pretty", action = ArgAction::SetTrue, global = true)]
    pub pretty: bool,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Execute a JSLT program (default)
    Run(RunArgs),

    /// Format JSLT source code
    Format(FormatArgs),
}

#[derive(Debug, Parser)]
pub struct RunArgs {
    /// Path to a JSLT program file. Use '-' to read the program from stdin.
    #[arg(short = 'p', long = "program", value_name = "FILE")]
    pub program: Option<String>,

    /// Inline JSLT expression to evaluate instead of a file.
    #[arg(short = 'e', long = "eval", value_name = "EXPR", conflicts_with = "program")]
    pub eval: Option<String>,

    /// JSON input file. Use '-' to read the input from stdin.
    #[arg(short = 'i', long = "input", value_name = "FILE")]
    pub input: Option<String>,

    /// Pretty-print output JSON.
    #[arg(long = "pretty", action = ArgAction::SetTrue)]
    pub pretty: bool,
}

#[derive(Debug, Parser)]
pub struct FormatArgs {
    /// Files to format (or '-' for stdin)
    pub files: Vec<String>,

    /// Check if files are formatted (exit with error if not)
    #[arg(long)]
    pub check: bool,

    /// Format files in-place
    #[arg(long)]
    pub write: bool,

    /// Config file path
    #[arg(long, value_name = "FILE")]
    pub config: Option<PathBuf>,
}
