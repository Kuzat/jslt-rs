//! Output writer with indentation tracking

use crate::config::FormatConfig;

/// Writer for formatted output
pub struct Writer {
    buffer: String,
    indent_level: usize,
    indent_str: String,
    current_line_length: usize,
    config: FormatConfig,
    at_line_start: bool,
}

impl Writer {
    /// Create a new writer with the given configuration
    pub fn new(config: FormatConfig) -> Self {
        let indent_str = config.indent_string();
        Self {
            buffer: String::new(),
            indent_level: 0,
            indent_str,
            current_line_length: 0,
            config,
            at_line_start: true,
        }
    }

    /// Write a string to the buffer
    pub fn write(&mut self, s: &str) {
        if self.at_line_start && !s.is_empty() {
            self.write_indent();
            self.at_line_start = false;
        }
        self.buffer.push_str(s);
        self.current_line_length += s.len();
    }

    /// Write indentation at the current level
    pub fn write_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.buffer.push_str(&self.indent_str);
            self.current_line_length += self.indent_str.len();
        }
    }

    /// Write a newline
    pub fn newline(&mut self) {
        self.buffer.push('\n');
        self.current_line_length = 0;
        self.at_line_start = true;
    }

    /// Increase indentation level
    pub fn increase_indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decrease indentation level
    pub fn decrease_indent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    /// Check if adding text would fit on the current line
    pub fn fits_on_line(&self, text: &str) -> bool {
        let indent_width =
            if self.at_line_start { self.indent_level * self.indent_str.len() } else { 0 };
        self.current_line_length + indent_width + text.len() <= self.config.max_width
    }

    /// Get the current line length
    pub fn current_line_length(&self) -> usize {
        self.current_line_length
    }

    /// Check if we're at the start of a line
    pub fn at_line_start(&self) -> bool {
        self.at_line_start
    }

    /// Convert the writer into a string
    pub fn into_string(mut self) -> String {
        // Ensure exactly one trailing newline
        while self.buffer.ends_with("\n\n") {
            self.buffer.pop();
        }
        if !self.buffer.ends_with('\n') && !self.buffer.is_empty() {
            self.buffer.push('\n');
        }
        self.buffer
    }

    /// Get a reference to the configuration
    pub fn config(&self) -> &FormatConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_write() {
        let mut w = Writer::new(FormatConfig::default());
        w.write("hello");
        w.write(" ");
        w.write("world");
        assert_eq!(w.into_string(), "hello world\n");
    }

    #[test]
    fn test_newline_and_indent() {
        let mut w = Writer::new(FormatConfig::default());
        w.write("line1");
        w.newline();
        w.increase_indent();
        w.write("line2");
        assert_eq!(w.into_string(), "line1\n  line2\n");
    }

    #[test]
    fn test_fits_on_line() {
        let config = FormatConfig { max_width: 20, ..Default::default() };
        let mut w = Writer::new(config);
        w.write("hello");
        assert!(w.fits_on_line(" world"));
        assert!(!w.fits_on_line(" this is a very long string"));
    }

    #[test]
    fn test_multiple_indent_levels() {
        let mut w = Writer::new(FormatConfig::default());
        w.write("level0");
        w.newline();
        w.increase_indent();
        w.write("level1");
        w.newline();
        w.increase_indent();
        w.write("level2");
        w.newline();
        w.decrease_indent();
        w.write("back to level1");
        assert_eq!(w.into_string(), "level0\n  level1\n    level2\n  back to level1\n");
    }
}
