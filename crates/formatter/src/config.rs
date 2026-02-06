//! Formatter configuration

use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

/// Indentation style
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum IndentStyle {
    /// Use spaces for indentation
    Spaces,
    /// Use tabs for indentation
    Tabs,
}

/// Trailing comma configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TrailingComma {
    /// Never add trailing commas
    Never,
    /// Always add trailing commas for multi-line constructs
    Always,
}

/// Formatter configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatConfig {
    /// Number of spaces/tabs per indentation level
    #[serde(default = "default_indent_width")]
    pub indent_width: usize,

    /// Maximum line width before breaking
    #[serde(default = "default_max_width")]
    pub max_width: usize,

    /// Indentation style (spaces or tabs)
    #[serde(default = "default_indent_style")]
    pub indent_style: IndentStyle,

    /// Trailing comma configuration
    #[serde(default = "default_trailing_comma")]
    pub trailing_comma: TrailingComma,
}

fn default_indent_width() -> usize {
    2
}

fn default_max_width() -> usize {
    100
}

fn default_indent_style() -> IndentStyle {
    IndentStyle::Spaces
}

fn default_trailing_comma() -> TrailingComma {
    TrailingComma::Never
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_width: default_indent_width(),
            max_width: default_max_width(),
            indent_style: default_indent_style(),
            trailing_comma: default_trailing_comma(),
        }
    }
}

impl FormatConfig {
    /// Load configuration from a TOML file
    pub fn from_toml_file(path: &Path) -> Result<Self, String> {
        let content =
            fs::read_to_string(path).map_err(|e| format!("Failed to read config file: {}", e))?;
        toml::from_str(&content).map_err(|e| format!("Failed to parse config file: {}", e))
    }

    /// Search for a .jsltfmt config file starting from the given directory
    /// and walking up to parent directories
    pub fn search_config_file(start_dir: &Path) -> Option<PathBuf> {
        let mut current = start_dir.to_path_buf();
        loop {
            let config_path = current.join(".jsltfmt");
            if config_path.exists() && config_path.is_file() {
                return Some(config_path);
            }

            // Move to parent directory
            if !current.pop() {
                break;
            }
        }
        None
    }

    /// Load configuration from a .jsltfmt file in the given directory or its parents,
    /// or return default configuration if no file is found
    pub fn load_or_default(start_dir: &Path) -> Self {
        if let Some(config_path) = Self::search_config_file(start_dir) {
            Self::from_toml_file(&config_path).unwrap_or_default()
        } else {
            Self::default()
        }
    }

    /// Get the indentation string for one level
    pub fn indent_string(&self) -> String {
        match self.indent_style {
            IndentStyle::Spaces => " ".repeat(self.indent_width),
            IndentStyle::Tabs => "\t".repeat(self.indent_width),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = FormatConfig::default();
        assert_eq!(config.indent_width, 2);
        assert_eq!(config.max_width, 100);
        assert_eq!(config.indent_style, IndentStyle::Spaces);
        assert_eq!(config.trailing_comma, TrailingComma::Never);
    }

    #[test]
    fn test_indent_string() {
        let config = FormatConfig {
            indent_width: 4,
            indent_style: IndentStyle::Spaces,
            ..Default::default()
        };
        assert_eq!(config.indent_string(), "    ");

        let config =
            FormatConfig { indent_width: 1, indent_style: IndentStyle::Tabs, ..Default::default() };
        assert_eq!(config.indent_string(), "\t");
    }
}
