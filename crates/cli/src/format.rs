//! Format command implementation

use crate::args::FormatArgs;
use crate::run::CliError;
use formatter::{format_source_with_config, FormatConfig};
use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;

pub fn format_command(args: FormatArgs) -> Result<(), CliError> {
    // Load configuration
    let config = if let Some(config_path) = &args.config {
        FormatConfig::from_toml_file(config_path).map_err(CliError::Usage)?
    } else {
        FormatConfig::load_or_default(Path::new("."))
    };

    // If no files specified, read from stdin
    if args.files.is_empty() {
        return format_stdin(&config, args.check);
    }

    let mut had_errors = false;

    for file in &args.files {
        if file == "-" {
            // Read from stdin
            if let Err(e) = format_stdin(&config, args.check) {
                eprintln!("Error formatting stdin: {}", e);
                had_errors = true;
            }
        } else {
            // Read from file
            if let Err(e) = format_file(file, &config, args.check, args.write) {
                eprintln!("Error formatting {}: {}", file, e);
                had_errors = true;
            }
        }
    }

    if had_errors {
        std::process::exit(1);
    }

    Ok(())
}

fn format_stdin(config: &FormatConfig, check: bool) -> Result<(), CliError> {
    let mut source = String::new();
    io::stdin().read_to_string(&mut source)?;

    let formatted = format_source_with_config(&source, config.clone()).map_err(CliError::Usage)?;

    if check {
        if source != formatted {
            eprintln!("stdin is not formatted");
            std::process::exit(1);
        }
    } else {
        print!("{}", formatted);
        io::stdout().flush()?;
    }

    Ok(())
}

fn format_file(
    path: &str,
    config: &FormatConfig,
    check: bool,
    write: bool,
) -> Result<(), CliError> {
    let source = fs::read_to_string(path)?;

    let formatted = format_source_with_config(&source, config.clone())
        .map_err(|e| CliError::Usage(format!("Failed to format {}: {}", path, e)))?;

    if check {
        if source != formatted {
            eprintln!("File {} is not formatted", path);
            std::process::exit(1);
        }
    } else if write {
        fs::write(path, formatted)?;
        eprintln!("Formatted {}", path);
    } else {
        print!("{}", formatted);
        io::stdout().flush()?;
    }

    Ok(())
}
