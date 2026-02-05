//! Trivia (comments) formatting
//!
//! This module handles formatting of comments and whitespace.
//! Currently a stub - will be implemented when parser collects comments.

use crate::writer::Writer;
use ast::TriviaCollection;

/// Format leading trivia (comments before a node)
pub fn format_leading_trivia(_writer: &mut Writer, _trivia: &TriviaCollection) {
    // TODO: Implement when parser collects comments
}

/// Format trailing trivia (comments after a node on the same line)
pub fn format_trailing_trivia(_writer: &mut Writer, _trivia: &TriviaCollection) {
    // TODO: Implement when parser collects comments
}
