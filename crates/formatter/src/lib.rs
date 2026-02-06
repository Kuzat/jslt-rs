//! JSLT source code formatter
//!
//! This crate provides formatting functionality for JSLT programs,
//! with configurable indentation and layout options.

pub mod config;
pub mod format_expr;
pub mod format_program;
pub mod format_stmt;
pub mod format_trivia;
pub mod writer;

use ast::Program;
pub use config::FormatConfig;
use writer::Writer;

/// Main formatter struct
pub struct Formatter {
    config: FormatConfig,
}

impl Formatter {
    /// Create a new formatter with the given configuration
    pub fn new(config: FormatConfig) -> Self {
        Self { config }
    }

    /// Format a JSLT program to a string
    pub fn format(&self, program: &Program) -> String {
        let mut writer = Writer::new(self.config.clone());
        format_program::format_program(&mut writer, program);
        writer.into_string()
    }
}

/// Format a program with default configuration
pub fn format(program: &Program) -> String {
    let formatter = Formatter::new(FormatConfig::default());
    formatter.format(program)
}

/// Format a program with custom configuration
pub fn format_with_config(program: &Program, config: FormatConfig) -> String {
    let formatter = Formatter::new(config);
    formatter.format(program)
}

/// Parse and format JSLT source code
pub fn format_source(source: &str) -> Result<String, String> {
    let mut parser = parser::Parser::new(source).map_err(|e| e.to_string())?;
    let program = parser.parse_program().map_err(|e| e.to_string())?;
    Ok(format(&program))
}

/// Parse and format JSLT source code with custom configuration
pub fn format_source_with_config(source: &str, config: FormatConfig) -> Result<String, String> {
    let mut parser = parser::Parser::new(source).map_err(|e| e.to_string())?;
    let program = parser.parse_program().map_err(|e| e.to_string())?;
    Ok(format_with_config(&program, config))
}
