use crate::args::Cli;
use clap::Parser;
use engine::compile_with_import_path;
use serde_json::Value;
use std::fs;
use std::io::Read;

#[derive(Debug, thiserror::Error)]
pub enum CliError {
    #[error("{0}")]
    Usage(String),

    #[error("{0}")]
    Io(#[from] std::io::Error),

    #[error("{0}")]
    Serde(#[from] serde_json::Error),

    #[error("{0}")]
    Engine(#[from] engine::EngineError),
}

pub fn real_main() -> Result<(), CliError> {
    let cli = Cli::parse();

    let (program_src, program_path_ctx) =
        read_program_source(cli.program.as_deref(), cli.eval.as_deref())?;
    let input = read_input_json(cli.input.as_deref())?;

    let compiled = compile_with_import_path(&program_src, &program_path_ctx)?;
    let out = compiled.apply(&input, None)?;

    if cli.pretty {
        println!("{}", serde_json::to_string_pretty(&out)?);
    } else {
        println!("{}", serde_json::to_string(&out)?);
    }
    Ok(())
}

/// Reads the program source or an inline expression for evaluation.
///
/// This function handles two scenarios:
/// 1. If an inline expression for evaluation (`eval`) is provided, it wraps that
///    inline source and uses the current working directory (`"."`) as the base path
///    to resolve relative imports.
/// 2. If no inline expression is provided, this function would typically handle
///    reading from a program file if implemented fully.
///
/// # Arguments
///
/// * `program` - An optional reference to a string representing the path to the program file.
/// * `eval` - An optional reference to a string representing an inline expression for evaluation.
///
/// # Returns
///
/// * `Ok((String, String))` - A tuple containing the inline source (or file content in a complete
///   implementation) and the base path used for relative imports resolution.
/// * `Err(CliError)` - If there's an error in reading the program source or processing the input.
///
/// # Errors
///
/// Returns a `CliError` in case of invalid input or other failure scenarios during the execution.
///
/// # Example
///
/// ```
/// let inline_expression = Some("print('Hello, world!')");
/// let result = read_program_source(None, inline_expression);
///
/// assert!(result.is_ok());
/// let (source, base_path) = result.unwrap();
/// assert_eq!(source, "print('Hello, world!')");
/// assert_eq!(base_path, ".");
/// ```
fn read_program_source(
    program: Option<&str>,
    eval: Option<&str>,
) -> Result<(String, String), CliError> {
    if let Some(expr) = eval {
        // Inline source; base path "." so relative imports resolve from CWD.
        return Ok((expr.to_string(), ".".to_string()));
    }
    if let Some(p) = program {
        if p == "-" {
            let src = read_stdin_to_string()?;
            return Ok((src, ".".to_string()));
        } else {
            let src = fs::read_to_string(p)?;
            return Ok((src, p.to_string()));
        }
    }
    Err(CliError::Usage("either --program <file|-> or --eval <expr> must be provided".into()))
}

/// Reads a JSON input either from a file or standard input and parses it into a `serde_json::Value`.
///
/// # Arguments
///
/// * `path` - An `Option<&str>` specifying the source of the JSON input:
///   - `Some("-")`: Reads JSON from the standard input.
///   - `Some(p)`: Reads JSON from a file specified by the path `p`.
///   - `None`: Reads JSON from the standard input.
///
/// # Returns
///
/// A `Result<Value, CliError>` where:
/// * `Value` is the successfully parsed JSON as a `serde_json::Value`.
/// * `CliError` indicates an error occurred during reading or parsing the input.
///
/// # Errors
///
/// This function can return an error in the following cases:
/// * If the file specified by `path` cannot be found, accessed, or read.
/// * If reading from the standard input fails.
/// * If the content read is not a valid JSON string.
///
/// # Examples
///
/// ```rust
/// # use serde_json::Value;
/// # use your_crate::errors::CliError;
/// # fn example() -> Result<(), CliError> {
/// let json_data = read_input_json(Some("example.json"))?;
/// println!("Parsed JSON: {}", json_data);
///
/// let json_data_from_stdin = read_input_json(Some("-"))?;
/// println!("Parsed JSON from stdin: {}", json_data_from_stdin);
/// # Ok(())
/// # }
/// ```
fn read_input_json(path: Option<&str>) -> Result<Value, CliError> {
    let s = match path {
        Some("-") => read_stdin_to_string()?,
        Some(p) => fs::read_to_string(p)?,
        None => read_stdin_to_string()?,
    };
    serde_json::from_str::<Value>(&s).map_err(CliError::from)
}

/// Reads the entire standard input into a `String`.
///
/// This function attempts to read data from the standard input (stdin) and
/// store it into a `String`. It returns the data as a `Result<String, CliError>`.
///
/// # Returns
/// - `Ok(String)` containing the full content of stdin if the operation succeeds.
/// - `Err(CliError)` if reading from stdin fails or an error occurs.
///
/// # Errors
/// This function propagates I/O errors encountered when reading from stdin.
/// The specific error type returned is `CliError`.
///
/// # Examples
/// ```no_run
/// use your_crate::read_stdin_to_string; // Replace with the actual path
///
/// fn main() -> Result<(), CliError> {
///     let input = read_stdin_to_string()?;
///     println!("Input: {}", input);
///     Ok(())
/// }
/// ```
fn read_stdin_to_string() -> Result<String, CliError> {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s)?;
    Ok(s)
}
