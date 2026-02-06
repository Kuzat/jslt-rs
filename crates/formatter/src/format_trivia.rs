//! Trivia (comments) formatting
//!
//! This module handles formatting of comments and whitespace.

use crate::writer::Writer;
use ast::{Trivia, TriviaCollection};

/// Format leading trivia (comments before a node)
pub fn format_leading_trivia(writer: &mut Writer, trivia: &TriviaCollection) {
    for t in &trivia.leading {
        match t {
            Trivia::LineComment(comment) => {
                writer.write("//");
                writer.write(comment);
                writer.newline();
            }
            Trivia::Whitespace(_) => {
                // Whitespace is handled by the writer's newline/indent logic
            }
        }
    }
}

/// Format trailing trivia (comments after a node on the same line)
pub fn format_trailing_trivia(writer: &mut Writer, trivia: &TriviaCollection) {
    for t in &trivia.trailing {
        match t {
            Trivia::LineComment(comment) => {
                writer.write("  //");
                writer.write(comment);
            }
            Trivia::Whitespace(_) => {
                // Whitespace is handled by the writer's newline/indent logic
            }
        }
    }
}
