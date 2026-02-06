//! Trivia (comments and whitespace) tracking for formatting
//!
//! This module provides types for attaching comments to AST nodes,
//! allowing formatters to preserve comments during code transformation.

/// A piece of trivia (comment or whitespace)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Trivia {
    /// Line comment: // ...
    LineComment(String),
    /// Whitespace (newlines, spaces)
    Whitespace(String),
}

/// Collection of trivia attached to an AST node
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TriviaCollection {
    /// Comments/whitespace before the node
    pub leading: Vec<Trivia>,
    /// Comments after the node on the same line
    pub trailing: Vec<Trivia>,
}

impl TriviaCollection {
    /// Create an empty trivia collection
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a collection with only leading trivia
    pub fn with_leading(leading: Vec<Trivia>) -> Self {
        Self { leading, trailing: Vec::new() }
    }

    /// Create a collection with only trailing trivia
    pub fn with_trailing(trailing: Vec<Trivia>) -> Self {
        Self { leading: Vec::new(), trailing }
    }

    /// Check if the collection is empty
    pub fn is_empty(&self) -> bool {
        self.leading.is_empty() && self.trailing.is_empty()
    }

    /// Add leading trivia
    pub fn add_leading(&mut self, trivia: Trivia) {
        self.leading.push(trivia);
    }

    /// Add trailing trivia
    pub fn add_trailing(&mut self, trivia: Trivia) {
        self.trailing.push(trivia);
    }
}
