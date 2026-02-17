//! JSLT Language Server Protocol implementation
//!
//! This provides IDE features like diagnostics, completions, and hover information
//! for the JSLT files

use engine::EngineError;
use formatter::format_source;
use interp::binder::BindError;
use parser::{parse_import_header, Parser};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
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
    const CODE_PARSE: &'static str = "JSLT_PARSE";
    const CODE_IMPORT_NOT_FOUND: &'static str = "JSLT_IMPORT_NOT_FOUND";
    const CODE_BIND_UNKNOWN_FUNCTION: &'static str = "JSLT_BIND_UNKNOWN_FUNCTION";
    const CODE_BIND_UNKNOWN_VARIABLE: &'static str = "JSLT_BIND_UNKNOWN_VARIABLE";
    const CODE_BIND_NON_FUNCTION_CALLEE: &'static str = "JSLT_BIND_NON_FUNCTION_CALLEE";
    const CODE_MODULE: &'static str = "JSLT_MODULE";
    const DIAGNOSTIC_DOCS_BASE: &'static str =
        "https://github.com/Kuzat/jslt-rs/blob/main/docs/diagnostics.md";
    const CONFIG_FILE_NAMES: [&'static str; 2] = ["jslt-lsp.toml", ".jslt-lsp.toml"];

    /// Create a new language server instance
    pub fn new(client: Client) -> Self {
        Self { client, document_map: tokio::sync::RwLock::new(HashMap::new()) }
    }

    /// Parse a JSLT document and return diagnostics
    ///
    /// This is where we integrate with out parser to detect syntax errors
    async fn parse_and_diagnose(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        let file_path = uri.to_file_path().unwrap_or_else(|_| PathBuf::from("."));
        let roots = Self::module_resolution_roots_for_file(&file_path);

        // Parse first to report as many in-file syntax/lexer errors as possible.
        if let Ok(mut parser) = Parser::new(text) {
            if let Err(parse_errors) = parser.parse_program() {
                let mut diagnostics = Self::parse_errors_to_diagnostics(parse_errors, text);
                diagnostics.extend(Self::module_diagnostics_to_lsp(
                    Self::collect_import_diagnostics_for_roots(text, &roots),
                    text,
                ));
                return diagnostics;
            }
        }

        let module_diags = Self::collect_import_diagnostics_for_roots(text, &roots);
        if !module_diags.is_empty() {
            return Self::module_diagnostics_to_lsp(module_diags, text);
        }

        let mut last_module_error: Option<EngineError> = None;
        for root in roots {
            let virtual_main = root.join("__jslt_lsp_virtual__.jslt");
            match engine::compile_with_import_path(text, &virtual_main.to_string_lossy()) {
                Ok(_) => return Vec::new(),
                Err(err @ EngineError::ModuleErrors(_)) | Err(err @ EngineError::ModuleError(_)) => {
                    last_module_error = Some(err);
                }
                Err(err) => return Self::error_to_diagnostic(err, text),
            }
        }

        if let Some(err) = last_module_error {
            Self::error_to_diagnostic(err, text)
        } else {
            Vec::new()
        }
    }

    fn module_resolution_roots_for_file(file_path: &Path) -> Vec<PathBuf> {
        let default_root = file_path.parent().unwrap_or(Path::new(".")).to_path_buf();
        let mut roots = Vec::new();
        if let Some(config_path) = Self::find_config_path_for_file(file_path) {
            roots.extend(Self::parse_module_roots_from_config(&config_path));
        }
        roots.push(default_root);

        let mut deduped = Vec::new();
        for root in roots {
            if !deduped.iter().any(|existing: &PathBuf| existing == &root) {
                deduped.push(root);
            }
        }
        deduped
    }

    fn find_config_path_for_file(file_path: &Path) -> Option<PathBuf> {
        let mut current = file_path.parent();
        while let Some(dir) = current {
            for name in Self::CONFIG_FILE_NAMES {
                let candidate = dir.join(name);
                if candidate.exists() {
                    return Some(candidate);
                }
            }
            current = dir.parent();
        }
        None
    }

    fn parse_module_roots_from_config(config_path: &Path) -> Vec<PathBuf> {
        let Ok(raw) = fs::read_to_string(config_path) else {
            return Vec::new();
        };
        let Ok(value) = raw.parse::<toml::Value>() else {
            return Vec::new();
        };
        let mut roots = Vec::new();
        let base = config_path.parent().unwrap_or(Path::new("."));

        let mut push_root = |s: &str| {
            if s.is_empty() {
                return;
            }
            let p = Path::new(s);
            let resolved = if p.is_absolute() { p.to_path_buf() } else { base.join(p) };
            roots.push(resolved);
        };

        if let Some(root) = value.get("module_root").and_then(|v| v.as_str()) {
            push_root(root);
        }
        if let Some(root) =
            value.get("imports").and_then(|v| v.get("root")).and_then(|v| v.as_str())
        {
            push_root(root);
        }
        if let Some(arr) = value.get("module_roots").and_then(|v| v.as_array()) {
            for entry in arr {
                if let Some(s) = entry.as_str() {
                    push_root(s);
                }
            }
        }
        if let Some(arr) =
            value.get("imports").and_then(|v| v.get("roots")).and_then(|v| v.as_array())
        {
            for entry in arr {
                if let Some(s) = entry.as_str() {
                    push_root(s);
                }
            }
        }

        roots
    }

    fn collect_import_diagnostics_for_roots(
        text: &str,
        roots: &[PathBuf],
    ) -> Vec<engine::ModuleDiagnostic> {
        let parsed = parse_import_header(text);
        parsed
            .imports
            .into_iter()
            .filter_map(|imp| {
                let exists_anywhere = roots.iter().any(|root| root.join(&imp.path).exists());
                if exists_anywhere {
                    None
                } else {
                    Some(engine::ModuleDiagnostic {
                        message: format!("imported module not found: {}", imp.path),
                        span: Some(imp.span),
                    })
                }
            })
            .collect()
    }

    fn module_diagnostics_to_lsp(
        diagnostics: Vec<engine::ModuleDiagnostic>,
        text: &str,
    ) -> Vec<Diagnostic> {
        let diagnostics: Vec<Diagnostic> = diagnostics
            .into_iter()
            .map(|diag| {
                let (start, end) = match diag.span {
                    Some(span) => (
                        Self::byte_offset_to_position(text, span.start),
                        Self::byte_offset_to_position(text, span.end),
                    ),
                    None => {
                        let start = Self::byte_offset_to_position(text, 0);
                        let end =
                            Self::byte_offset_to_position(text, text.find('\n').unwrap_or(text.len()));
                        (start, end)
                    }
                };
                Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(Self::CODE_IMPORT_NOT_FOUND.to_string())),
                    code_description: Some(CodeDescription {
                        href: Self::diagnostic_href(Self::CODE_IMPORT_NOT_FOUND),
                    }),
                    source: Some("jslt-import".to_string()),
                    message: diag.message,
                    related_information: None,
                    tags: None,
                    data: None,
                }
            })
            .collect();
        Self::normalize_diagnostics(diagnostics)
    }

    fn parse_errors_to_diagnostics(parse_errors: parser::ParseErrors, text: &str) -> Vec<Diagnostic> {
        let diagnostics: Vec<Diagnostic> = parse_errors
            .errors
            .into_iter()
            .map(|err| {
                let start = Self::byte_offset_to_position(text, err.span.start);
                let end = Self::byte_offset_to_position(text, err.span.end);
                let raw = format!("{}", err);
                let message = if raw.contains("expected") && raw.contains("after") {
                    format!("syntax error: {}", raw)
                } else {
                    raw
                };
                Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(Self::CODE_PARSE.to_string())),
                    code_description: Some(CodeDescription {
                        href: Self::diagnostic_href(Self::CODE_PARSE),
                    }),
                    source: Some("jslt-parser".to_string()),
                    message,
                    related_information: None,
                    tags: None,
                    data: None,
                }
            })
            .collect();
        Self::normalize_diagnostics(diagnostics)
    }

    fn normalize_diagnostics(diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
        let mut normalized = diagnostics;
        normalized.sort_by(|a, b| {
            (
                a.range.start.line,
                a.range.start.character,
                a.range.end.line,
                a.range.end.character,
                a.message.as_str(),
                a.source.as_deref().unwrap_or(""),
            )
                .cmp(&(
                    b.range.start.line,
                    b.range.start.character,
                    b.range.end.line,
                    b.range.end.character,
                    b.message.as_str(),
                    b.source.as_deref().unwrap_or(""),
                ))
        });
        normalized.dedup_by(|a, b| {
            a.range == b.range && a.message == b.message && a.source == b.source
        });
        normalized
    }

    /// Convert a parser error into an LSP Diagnostic
    ///
    /// LSP diagnostics have:
    /// - severity (error, warning, info, hint)
    /// - range (start/end position)
    /// - message (description)
    /// - source (who generated it)
    fn error_to_diagnostic(err: EngineError, text: &str) -> Vec<Diagnostic> {
        let mut diagnostic = Vec::new();

        match err {
            EngineError::ParseErrors(parse_errors) => {
                diagnostic.extend(Self::parse_errors_to_diagnostics(parse_errors, text));
            }
            EngineError::Parse(parser_err) => {
                // get the error span if available
                let start = Self::byte_offset_to_position(text, parser_err.span.start);
                let end = Self::byte_offset_to_position(text, parser_err.span.end);

                diagnostic.push(Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(Self::CODE_PARSE.to_string())),
                    code_description: Some(CodeDescription {
                        href: Self::diagnostic_href(Self::CODE_PARSE),
                    }),
                    source: Some("jslt-parser".to_string()),
                    message: format!("{}", parser_err),
                    related_information: None,
                    tags: None,
                    data: None,
                })
            }
            EngineError::Bind(bind_errors) => {
                for bind_err in bind_errors.errors {
                    diagnostic.push(Self::bind_error_to_diagnostic(bind_err, text))
                }
            }
            EngineError::Runtime(_) => {
                // Runtime errors are not reported as diagnostics since they
                // are not detected at compile time
            }
            EngineError::ModuleError(_) => {
                // Module errors do not contain a span for now so just show the start of the file
                let start = Self::byte_offset_to_position(text, 0);
                let end =
                    Self::byte_offset_to_position(text, text.find('\n').unwrap_or(text.len()));

                diagnostic.push(Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(Self::CODE_MODULE.to_string())),
                    code_description: Some(CodeDescription {
                        href: Self::diagnostic_href(Self::CODE_MODULE),
                    }),
                    source: Some("jslt-parser".to_string()),
                    message: format!("{}", err),
                    related_information: None,
                    tags: None,
                    data: None,
                })
            }
            EngineError::ModuleErrors(module_errors) => {
                diagnostic.extend(Self::module_diagnostics_to_lsp(module_errors.diagnostics, text));
            }
        }

        Self::normalize_diagnostics(diagnostic)
    }

    fn bind_error_to_diagnostic(err: BindError, text: &str) -> Diagnostic {
        match err {
            BindError::UnknownFunction { name, span, suggestions } => {
                let start = Self::byte_offset_to_position(text, span.start);
                let end = Self::byte_offset_to_position(text, span.end);

                Diagnostic {
                    range: Range { start, end },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: Some(NumberOrString::String(
                        Self::CODE_BIND_UNKNOWN_FUNCTION.to_string(),
                    )),
                    code_description: Some(CodeDescription {
                        href: Self::diagnostic_href(Self::CODE_BIND_UNKNOWN_FUNCTION),
                    }),
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
                    code: Some(NumberOrString::String(
                        Self::CODE_BIND_UNKNOWN_VARIABLE.to_string(),
                    )),
                    code_description: Some(CodeDescription {
                        href: Self::diagnostic_href(Self::CODE_BIND_UNKNOWN_VARIABLE),
                    }),
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
                    code: Some(NumberOrString::String(
                        Self::CODE_BIND_NON_FUNCTION_CALLEE.to_string(),
                    )),
                    code_description: Some(CodeDescription {
                        href: Self::diagnostic_href(Self::CODE_BIND_NON_FUNCTION_CALLEE),
                    }),
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
        let offset = offset.min(text.len());

        for (i, ch) in text.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                column = 0;
            } else {
                // LSP uses UTF-16 code units, not Unicde chars
                column += ch.len_utf16() as u32;
            }
        }

        Position { line, character: column }
    }

    fn diagnostic_href(code: &str) -> Url {
        Url::parse(&format!(
            "{}#{}",
            Self::DIAGNOSTIC_DOCS_BASE,
            code.to_ascii_lowercase()
        ))
        .expect("valid diagnostic docs URL")
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

                // Code formatting support
                document_formatting_provider: Some(OneOf::Left(true)),

                // Add more capabilites here as we implement features:
                // - completion_provider: for autocomplete
                // - hover_provider: for documentation on hover
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

    /// Called when the editor requests document formatting
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();
        let doc = self.document_map.read().await;

        if let Some(text) = doc.get(&uri) {
            // Parse and format the document
            match format_source(text) {
                Ok(formatted) => {
                    // Only return edits if the content actually changed
                    if &formatted == text {
                        return Ok(None);
                    }

                    // Create TextEdit replacing entire document
                    let start = Position::new(0, 0);
                    // Use a very large line number to cover the whole document
                    let end = Position::new(u32::MAX, u32::MAX);
                    let range = Range::new(start, end);

                    Ok(Some(vec![TextEdit { range, new_text: formatted }]))
                }
                Err(e) => {
                    // Don't format if there are parse errors
                    self.client
                        .log_message(
                            MessageType::WARNING,
                            format!("Cannot format document with parse errors: {}", e),
                        )
                        .await;
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }
}

/// Start the language server
///
/// This sets up the LSP service and runs it, communicating via stdin/stdout
pub async fn run_server() {
    // Set up logging
    tracing_subscriber::fmt().with_writer(std::io::stderr).with_ansi(false).init();

    // Create the LSP service
    let (service, socket) = LspService::new(JsltLanguageServer::new);

    // Run the server
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn parse_errors_are_reported_as_multiple_diagnostics() {
        let text = "! @ def foo( x\nlet a 1\n$";
        let mut parser = Parser::new(text).expect("parser init should recover");
        let parse_errors = parser.parse_program().expect_err("expected parse errors");

        let diags = JsltLanguageServer::parse_errors_to_diagnostics(parse_errors, text);
        assert!(
            diags.len() >= 2,
            "expected multiple diagnostics, got {}",
            diags.len()
        );
        assert!(diags.iter().all(|d| d.source.as_deref() == Some("jslt-parser")));
        assert!(diags.iter().all(|d| {
            matches!(
                d.code.as_ref(),
                Some(NumberOrString::String(code)) if code == JsltLanguageServer::CODE_PARSE
            )
        }));
        assert!(diags.iter().all(|d| d.code_description.is_some()));
    }

    #[test]
    fn engine_parse_errors_expand_to_multiple_diagnostics() {
        let text = "! @ def foo( x\nlet a 1\n$";
        let mut parser = Parser::new(text).expect("parser init should recover");
        let parse_errors = parser.parse_program().expect_err("expected parse errors");

        let diags = JsltLanguageServer::error_to_diagnostic(
            EngineError::ParseErrors(parse_errors),
            text,
        );
        assert!(
            diags.len() >= 2,
            "expected multiple diagnostics, got {}",
            diags.len()
        );
    }

    #[test]
    fn missing_imports_are_reported_from_top_import_block() {
        let test_dir = std::env::temp_dir().join(format!(
            "jslt-lsp-test-{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("clock")
                .as_nanos()
        ));
        fs::create_dir_all(&test_dir).expect("create temp dir");
        let uri = Url::from_file_path(test_dir.join("main.jslt")).expect("file uri");

        let text = r#"
import "does-not-exist" as package

{
  "hello": "world"
  "something": package:
}
"#;
        let file_path = uri.to_file_path().expect("file path");
        let roots = JsltLanguageServer::module_resolution_roots_for_file(&file_path);
        let diags = JsltLanguageServer::module_diagnostics_to_lsp(
            JsltLanguageServer::collect_import_diagnostics_for_roots(text, &roots),
            text,
        );
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].source.as_deref(), Some("jslt-import"));
        assert!(diags[0].message.contains("does-not-exist"));
        assert!(matches!(
            diags[0].code.as_ref(),
            Some(NumberOrString::String(code))
                if code == JsltLanguageServer::CODE_IMPORT_NOT_FOUND
        ));
        assert!(diags[0]
            .code_description
            .as_ref()
            .is_some_and(|desc| desc.href.as_str().contains("#jslt_import_not_found")));
    }

    #[test]
    fn multiple_missing_imports_are_all_reported() {
        let test_dir = std::env::temp_dir().join(format!(
            "jslt-lsp-test-{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("clock")
                .as_nanos()
        ));
        fs::create_dir_all(&test_dir).expect("create temp dir");
        let uri = Url::from_file_path(test_dir.join("main.jslt")).expect("file uri");

        let text = r#"
import "missing-one.jslt" as one
import "missing-two.jslt" as two
.
"#;
        let file_path = uri.to_file_path().expect("file path");
        let roots = JsltLanguageServer::module_resolution_roots_for_file(&file_path);
        let diags = JsltLanguageServer::module_diagnostics_to_lsp(
            JsltLanguageServer::collect_import_diagnostics_for_roots(text, &roots),
            text,
        );
        assert_eq!(diags.len(), 2);
        assert!(diags.iter().all(|d| d.source.as_deref() == Some("jslt-import")));
        assert!(diags.iter().any(|d| d.message.contains("missing-one.jslt")));
        assert!(diags.iter().any(|d| d.message.contains("missing-two.jslt")));
    }

    #[test]
    fn boundary_recovery_produces_multiple_parse_diagnostics() {
        let text = r#"
{
  "a": if ($x "bad") (1 + 2,
  "b": $arr[ ] + foo(1 2),
  "c": .broken.
}
"#;
        let mut parser = Parser::new(text).expect("parser init should recover");
        let parse_errors = parser.parse_program().expect_err("expected parse errors");
        let diags = JsltLanguageServer::parse_errors_to_diagnostics(parse_errors, text);
        assert!(diags.len() >= 4, "expected many diagnostics, got {}", diags.len());
    }

    #[test]
    fn expression_panic_recovery_produces_multiple_parse_diagnostics() {
        let text = r#"
{
  "a": 1 + ,
  "b": 2 * ),
  "c": foo(1, , 2),
  "d": [1, , 2]
}
"#;
        let mut parser = Parser::new(text).expect("parser init should recover");
        let parse_errors = parser.parse_program().expect_err("expected parse errors");
        let diags = JsltLanguageServer::parse_errors_to_diagnostics(parse_errors, text);
        assert!(diags.len() >= 4, "expected many diagnostics, got {}", diags.len());
    }

    #[test]
    fn diagnostics_are_sorted_and_deduplicated() {
        let later = Diagnostic {
            range: Range {
                start: Position { line: 2, character: 5 },
                end: Position { line: 2, character: 9 },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("jslt-parser".to_string()),
            message: "later".to_string(),
            related_information: None,
            tags: None,
            data: None,
        };
        let earlier = Diagnostic {
            range: Range {
                start: Position { line: 1, character: 1 },
                end: Position { line: 1, character: 3 },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("jslt-parser".to_string()),
            message: "earlier".to_string(),
            related_information: None,
            tags: None,
            data: None,
        };

        let out =
            JsltLanguageServer::normalize_diagnostics(vec![later.clone(), earlier.clone(), later]);
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].message, "earlier");
        assert_eq!(out[1].message, "later");
    }

    #[test]
    fn parser_recovery_messages_have_syntax_prefix() {
        let text = r#"
{
  "x": foo(1 2)
}
"#;
        let mut parser = Parser::new(text).expect("parser init should recover");
        let parse_errors = parser.parse_program().expect_err("expected parse errors");
        let diags = JsltLanguageServer::parse_errors_to_diagnostics(parse_errors, text);
        assert!(diags.iter().any(|d| d.message.starts_with("syntax error:")));
    }

    #[test]
    fn binder_diagnostics_have_specific_codes() {
        let text = "$x";
        let unknown_fn = JsltLanguageServer::bind_error_to_diagnostic(
            BindError::UnknownFunction {
                name: "foo".to_string(),
                span: ast::Span { start: 0, end: 3, line: 1, column: 1 },
                suggestions: vec![],
            },
            text,
        );
        assert!(matches!(
            unknown_fn.code.as_ref(),
            Some(NumberOrString::String(code))
                if code == JsltLanguageServer::CODE_BIND_UNKNOWN_FUNCTION
        ));
        assert!(unknown_fn.code_description.is_some());

        let unknown_var = JsltLanguageServer::bind_error_to_diagnostic(
            BindError::UnknownVariable {
                name: "x".to_string(),
                span: ast::Span { start: 0, end: 2, line: 1, column: 1 },
                suggestions: vec![],
            },
            text,
        );
        assert!(matches!(
            unknown_var.code.as_ref(),
            Some(NumberOrString::String(code))
                if code == JsltLanguageServer::CODE_BIND_UNKNOWN_VARIABLE
        ));
        assert!(unknown_var.code_description.is_some());

        let non_fun = JsltLanguageServer::bind_error_to_diagnostic(
            BindError::NonFunctionCallee {
                span: ast::Span { start: 0, end: 1, line: 1, column: 1 },
            },
            text,
        );
        assert!(matches!(
            non_fun.code.as_ref(),
            Some(NumberOrString::String(code))
                if code == JsltLanguageServer::CODE_BIND_NON_FUNCTION_CALLEE
        ));
        assert!(non_fun.code_description.is_some());
    }

    #[test]
    fn module_roots_can_be_configured_via_jslt_lsp_toml() {
        let test_dir = std::env::temp_dir().join(format!(
            "jslt-lsp-config-{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("clock")
                .as_nanos()
        ));
        let main_dir = test_dir.join("queries");
        let resources_dir = test_dir.join("src").join("main").join("resources");
        fs::create_dir_all(&main_dir).expect("create main dir");
        fs::create_dir_all(&resources_dir).expect("create resources dir");
        fs::write(test_dir.join("jslt-lsp.toml"), "module_roots = [\"src/main/resources\"]\n")
            .expect("write config");
        fs::write(resources_dir.join("something.jslt"), ".").expect("write module");
        let main_file = main_dir.join("main.jslt");
        fs::write(&main_file, "import \"something.jslt\" as s\n.").expect("write main");

        let roots = JsltLanguageServer::module_resolution_roots_for_file(&main_file);
        let text = fs::read_to_string(&main_file).expect("read main");
        let diags = JsltLanguageServer::collect_import_diagnostics_for_roots(&text, &roots);
        assert!(diags.is_empty(), "expected no missing import diagnostics");
    }
}
