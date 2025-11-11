//! JSLT Language Server Protocol implementation
//!
//! This provides IDE features like diagnostics, completions, and hover information
//! for the JSLT files

use std::collections::HashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// The backend state for our language server.
///
/// This holds the client connection and tracks all open documents
pub struct JsltLanguageServer {
    /// Client handle for sending notifications/requests back to the editor
    client: Client,

    /// Cache of document contents by URI
    /// We store this so we can re-parse on demand without asking the client
    document_map: tokio::sync::RwLock<HashMap<String, String>>,
}

impl JsltLanguageServer {
    /// Create a new language server instance
    pub fn new(client: Client) -> Self {
        Self { client, document_map: tokio::sync::RwLock::new(HashMap::new()) }
    }

    /// Parse a JSLT document and return diagnostics
    ///
    /// This is where we integrate with out parser to detect syntax errors
    async fn parse_and_diagnose(&self, _uri: &Url, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // try parse the document
        match parser::Parser::new(text) {
            Err(err) => {
                // Lexer failed to parse the document, return the error as a diagnostic
                diagnostics.push(Self::error_to_diagnostic(err, text));
            }
            Ok(mut parser) => match parser.parse_program() {
                Err(err) => {
                    diagnostics.push(Self::error_to_diagnostic(err, text));
                }
                Ok(_program) => {
                    // Parse succeeded, no diagnostics
                }
            },
        };

        diagnostics
    }

    /// Convert a parser error into an LSP Diagnostic
    ///
    /// LSP diagnostics have:
    /// - severity (error, warning, info, hint)
    /// - range (start/end position)
    /// - message (description)
    /// - source (who generated it)
    fn error_to_diagnostic(err: parser::ParseError, text: &str) -> Diagnostic {
        // get the error span if available
        let start = Self::byte_offset_to_position(text, err.span.start);
        let end = Self::byte_offset_to_position(text, err.span.end);

        let range = Range { start, end };

        Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("JSLT Language Server".to_string()),
            message: format!("{}", err),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    /// Convert byte offset to LSP Position (0-based line and column)
    fn byte_offset_to_position(text: &str, offset: usize) -> Position {
        let mut line = 0;
        let mut column = 0;

        for (i, ch) in text.char_indices() {
            if i == offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                column = 0;
            } else {
                column += 1;
            }
        }

        Position { line, character: column }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for JsltLanguageServer {
    /// Called when the editor first connects to initialize the server
    ///
    /// We tell the editor what features we support
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // We support full text document synchronization
                // This means we'll receive the full document content on every change
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),

                // Add more capabilites here as we implement features:
                // - completion_provider: for autocomplete
                // - hover_provider: for documentation on hover
                // - formatting_provider: for code formatting
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "JSLT Language Server".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    /// Called when initialization is complete
    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "JSLT Language Server Initialized").await
    }

    /// Called when the server is shutting down
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    /// Called when a document is opened in the editor
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let text = params.text_document.text;

        // Store the document contents in our cache
        self.document_map.write().await.insert(uri.clone(), text.clone());

        // Parse and send diagnostic
        let diagnostic = self.parse_and_diagnose(&params.text_document.uri, &text).await;

        self.client
            .publish_diagnostics(
                params.text_document.uri,
                diagnostic,
                Some(params.text_document.version),
            )
            .await;
    }

    /// Called when a document changes (user types)
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();

        // Get the new content
        if let Some(change) = params.content_changes.first() {
            let text = change.text.clone();

            // Update our cache
            self.document_map.write().await.insert(uri.clone(), text.clone());

            // Re-parse and send updated diagnostics
            let diagnostic = self.parse_and_diagnose(&params.text_document.uri, &text).await;

            self.client
                .publish_diagnostics(
                    params.text_document.uri,
                    diagnostic,
                    Some(params.text_document.version),
                )
                .await;
        }
    }

    /// Called when a document is closed
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();

        // Remove from cache
        self.document_map.write().await.remove(&uri);

        // Clear diagnostics
        self.client.publish_diagnostics(params.text_document.uri, Vec::new(), None).await;
    }
}

/// Start the language server
///
/// This sets up the LSP service and runs it, communicating via stdin/stdout
pub async fn run_server() {
    // Set up logging
    tracing_subscriber::fmt::init();

    // Create the LSP service
    let (service, socket) = LspService::new(JsltLanguageServer::new);

    // Run the server
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket).serve(service).await;
}
