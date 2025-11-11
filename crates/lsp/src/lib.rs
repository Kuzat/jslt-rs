//! JSLT Language Server Protocol implementation
//!
//! This provides IDE features like diagnostics, completions, and hover information
//! for the JSLT files

use engine::EngineError;
use interp::binder::BindError;
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
    async fn parse_and_diagnose(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Extract the file path for module resolution
        let file_path = uri
            .to_file_path()
            .ok()
            .and_then(|p| p.to_str().map(|s| s.to_string()))
            .unwrap_or_else(|| ".".to_string());

        // Try to compile the document
        match engine::compile_with_import_path(text, &file_path) {
            Ok(_) => {
                // Compilation succeeded, no diagnostics
            }
            Err(err) => {
                // Convert error to diagnostic
                if let Some(diagnostic) = Self::error_to_diagnostic(err, text) {
                    diagnostics.push(diagnostic);
                }
            }
        }

        diagnostics
    }

    /// Convert a parser error into an LSP Diagnostic
    ///
    /// LSP diagnostics have:
    /// - severity (error, warning, info, hint)
    /// - range (start/end position)
    /// - message (description)
    /// - source (who generated it)
    fn error_to_diagnostic(err: EngineError, text: &str) -> Option<Diagnostic> {
        match err {
            EngineError::Parse(parser_err) => {
                // get the error span if available
                let start = Self::byte_offset_to_position(text, parser_err.span.start);
                let end = Self::byte_offset_to_position(text, parser_err.span.end);

                Some(Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("jslt-parser".to_string()),
                    message: format!("{}", parser_err),
                    related_information: None,
                    tags: None,
                    data: None,
                })
            }
            EngineError::Bind(bind_err) => Some(Self::bind_error_to_diagnostic(bind_err, text)),
            EngineError::Runtime(_) => {
                // Runtime errors are not reported as diagnostics since they
                // are not detected at compile time
                None
            }
            EngineError::ModuleError(_) => {
                // Module errors do not contain a span for now so just show the start of the file
                let start = Self::byte_offset_to_position(text, 0);
                let end =
                    Self::byte_offset_to_position(text, text.split_once('\n').unwrap().0.len());

                Some(Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("jslt-parser".to_string()),
                    message: format!("{}", err),
                    related_information: None,
                    tags: None,
                    data: None,
                })
            }
        }
    }

    fn bind_error_to_diagnostic(err: BindError, text: &str) -> Diagnostic {
        match err {
            BindError::UnknownFunction { name, span, suggestions } => {
                let start = Self::byte_offset_to_position(text, span.start);
                let end = Self::byte_offset_to_position(text, span.end);

                Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("jslt-binder".to_string()),
                    message: format!(
                        "unknown function: `{}`, did you mean: {:?}",
                        name, suggestions
                    ),
                    related_information: None,
                    tags: None,
                    data: None,
                }
            }
            BindError::UnknownVariable { name, span, suggestions } => {
                let start = Self::byte_offset_to_position(text, span.start);
                let end = Self::byte_offset_to_position(text, span.end);

                Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("jslt-binder".to_string()),
                    message: format!(
                        "unknown variable: `{}`, did you mean: {:?}",
                        name, suggestions
                    ),
                    related_information: None,
                    tags: None,
                    data: None,
                }
            }
            BindError::NonFunctionCallee { span } => {
                let start = Self::byte_offset_to_position(text, span.start);
                let end = Self::byte_offset_to_position(text, span.end);

                Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("jslt-binder".to_string()),
                    message: "attempted to call a non-function expression".to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                }
            }
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
