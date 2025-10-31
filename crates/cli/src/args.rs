use clap::{ArgAction, Parser};

#[derive(Debug, Parser)]
#[command(name = "jslt", version, about = "JSLT command-line interface")]
pub struct Cli {
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
