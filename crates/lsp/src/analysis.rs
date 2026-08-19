use crate::context::{
    AnalysisSnapshot, BinderDiagnostic, CallSite, ImportHeaderEntry, Scope, ScopeId, Symbol,
    SymbolId, SymbolKind,
};
use crate::state::JsltLanguageServer;
use ast::{Expr, Program, Span};
use engine::EngineError;
use interp::binder::{BindError, Binder};
use parser::{Parser, parse_import_header};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tower_lsp::lsp_types::*;

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

    /// Build or retrieve a cached analysis snapshot for a document version.
    pub(crate) async fn analyze_document(
        &self,
        uri: &Url,
        text: &str,
        version: Option<i32>,
    ) -> AnalysisSnapshot {
        if let Some(snapshot) = self.cached_snapshot(uri, version).await {
            return snapshot;
        }

        let snapshot = Self::build_analysis_snapshot(uri, text, version);
        self.cache_snapshot(uri, version, snapshot.clone()).await;
        // Keep the workspace index in sync with the editor buffer so that
        // cross-file lookups see unsaved changes.
        self.index_snapshot(uri, snapshot.clone()).await;
        snapshot
    }

    pub(crate) fn build_analysis_snapshot(
        uri: &Url,
        text: &str,
        version: Option<i32>,
    ) -> AnalysisSnapshot {
        let diagnostics = Self::compute_diagnostics(uri, text);
        let mut snapshot = AnalysisSnapshot::new(version, text.to_string(), diagnostics);

        let import_header = parse_import_header(text);
        snapshot.imports = import_header
            .imports
            .into_iter()
            .map(|imp| ImportHeaderEntry { path: imp.path, alias: imp.alias, span: imp.span })
            .collect();

        let parsed_program =
            if let Ok(mut parser) = Parser::new(text) { parser.parse_program().ok() } else { None };

        if let Some(program) = parsed_program {
            snapshot.binder_diagnostics = Self::collect_binder_diagnostics(&program);
            let mut indexer = SnapshotIndexer::new(Self::full_text_span(text));
            indexer.seed_imports(&snapshot.imports);
            indexer.visit_program(&program);
            let (symbols, scopes, call_sites) = indexer.finish();
            snapshot.symbols = symbols;
            snapshot.scopes = scopes;
            snapshot.call_sites = call_sites;
            snapshot.program = Some(program);
        } else {
            let mut indexer = SnapshotIndexer::new(Self::full_text_span(text));
            indexer.seed_imports(&snapshot.imports);
            let (symbols, scopes, call_sites) = indexer.finish();
            snapshot.symbols = symbols;
            snapshot.scopes = scopes;
            snapshot.call_sites = call_sites;
        }

        snapshot
    }

    fn collect_binder_diagnostics(program: &Program) -> Vec<BinderDiagnostic> {
        let mut binder = Binder::new();
        match binder.bind_program(program) {
            Ok(_) => Vec::new(),
            Err(bind_errors) => bind_errors
                .errors
                .into_iter()
                .map(|err| match err {
                    BindError::UnknownFunction { name, span, suggestions } => BinderDiagnostic {
                        code: Self::CODE_BIND_UNKNOWN_FUNCTION.to_string(),
                        message: format!(
                            "unknown function: `{}`, did you mean: {:?}",
                            name, suggestions
                        ),
                        span,
                    },
                    BindError::UnknownVariable { name, span, suggestions } => BinderDiagnostic {
                        code: Self::CODE_BIND_UNKNOWN_VARIABLE.to_string(),
                        message: format!(
                            "unknown variable: `{}`, did you mean: {:?}",
                            name, suggestions
                        ),
                        span,
                    },
                    BindError::NonFunctionCallee { span } => BinderDiagnostic {
                        code: Self::CODE_BIND_NON_FUNCTION_CALLEE.to_string(),
                        message: "attempted to call a non-function expression".to_string(),
                        span,
                    },
                })
                .collect(),
        }
    }

    fn full_text_span(text: &str) -> Span {
        Span { start: 0, end: text.len(), line: 1, column: 1 }
    }

    pub(crate) fn resolve_import_uri(document_uri: &Url, import_path: &str) -> Option<Url> {
        let file_path = document_uri.to_file_path().ok()?;
        let roots = Self::module_resolution_roots_for_file(&file_path);
        for root in roots {
            let candidate = root.join(import_path);
            if candidate.exists() {
                return Url::from_file_path(candidate).ok();
            }
        }
        None
    }

    fn compute_diagnostics(uri: &Url, text: &str) -> Vec<Diagnostic> {
        let file_path = uri.to_file_path().unwrap_or_else(|_| PathBuf::from("."));
        let roots = Self::module_resolution_roots_for_file(&file_path);

        // Parse first to report as many in-file syntax/lexer errors as possible.
        if let Ok(mut parser) = Parser::new(text)
            && let Err(parse_errors) = parser.parse_program()
        {
            let mut diagnostics = Self::parse_errors_to_diagnostics(parse_errors, text);
            diagnostics.extend(Self::module_diagnostics_to_lsp(
                Self::collect_import_diagnostics_for_roots(text, &roots),
                text,
            ));
            return diagnostics;
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
                Err(err @ EngineError::ModuleErrors(_))
                | Err(err @ EngineError::ModuleError(_)) => {
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
                        let end = Self::byte_offset_to_position(
                            text,
                            text.find('\n').unwrap_or(text.len()),
                        );
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

    fn parse_errors_to_diagnostics(
        parse_errors: parser::ParseErrors,
        text: &str,
    ) -> Vec<Diagnostic> {
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
        normalized
            .dedup_by(|a, b| a.range == b.range && a.message == b.message && a.source == b.source);
        normalized
    }

    /// Convert a parser/compiler error into one or more LSP diagnostics.
    fn error_to_diagnostic(err: EngineError, text: &str) -> Vec<Diagnostic> {
        let mut diagnostic = Vec::new();

        match err {
            EngineError::ParseErrors(parse_errors) => {
                diagnostic.extend(Self::parse_errors_to_diagnostics(parse_errors, text));
            }
            EngineError::Parse(parser_err) => {
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
                // are not detected at compile time.
            }
            EngineError::ModuleError(_) => {
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

    /// Convert byte offset to LSP position (UTF-16 code units).
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
                column += ch.len_utf16() as u32;
            }
        }

        Position { line, character: column }
    }

    fn diagnostic_href(code: &str) -> Url {
        Url::parse(&format!("{}#{}", Self::DIAGNOSTIC_DOCS_BASE, code.to_ascii_lowercase()))
            .expect("valid diagnostic docs URL")
    }
}

#[derive(Debug, Clone, Copy)]
enum Namespace {
    Variable,
    Function,
    ImportAlias,
}

#[derive(Debug, Clone)]
struct ScopeFrame {
    id: ScopeId,
    vars: HashMap<String, SymbolId>,
    funcs: HashMap<String, SymbolId>,
    imports: HashMap<String, SymbolId>,
}

struct SnapshotIndexer {
    symbols: Vec<Symbol>,
    scopes: Vec<Scope>,
    call_sites: Vec<CallSite>,
    scope_stack: Vec<ScopeFrame>,
    next_symbol_id: u32,
    next_scope_id: u32,
}

impl SnapshotIndexer {
    fn new(root_span: Span) -> Self {
        let root_scope =
            Scope { id: ScopeId(0), parent: None, span: root_span, symbols: Vec::new() };
        let root_frame = ScopeFrame {
            id: ScopeId(0),
            vars: HashMap::new(),
            funcs: HashMap::new(),
            imports: HashMap::new(),
        };
        Self {
            symbols: Vec::new(),
            scopes: vec![root_scope],
            call_sites: Vec::new(),
            scope_stack: vec![root_frame],
            next_symbol_id: 0,
            next_scope_id: 1,
        }
    }

    fn finish(self) -> (Vec<Symbol>, Vec<Scope>, Vec<CallSite>) {
        (self.symbols, self.scopes, self.call_sites)
    }

    fn seed_imports(&mut self, imports: &[ImportHeaderEntry]) {
        for imp in imports {
            self.define_symbol(
                SymbolKind::ImportAlias,
                imp.alias.clone(),
                imp.span,
                Some(Namespace::ImportAlias),
            );
            self.define_symbol(SymbolKind::ModulePath, imp.path.clone(), imp.span, None);
        }
    }

    fn visit_program(&mut self, program: &Program) {
        for def in &program.defs {
            self.define_symbol(
                SymbolKind::Function,
                def.name.name.clone(),
                def.name.span,
                Some(Namespace::Function),
            );
        }

        self.define_let_bindings(&program.lets);

        for let_stmt in &program.lets {
            self.visit_let_values(let_stmt);
        }

        for def in &program.defs {
            self.visit_def(def);
        }

        if let Some(body) = &program.body {
            self.visit_expr(body);
        }
    }

    fn visit_def(&mut self, def: &ast::Def) {
        self.enter_scope(def.span);

        for param in &def.params {
            self.define_symbol(
                SymbolKind::Parameter,
                param.name.clone(),
                param.span,
                Some(Namespace::Variable),
            );
        }

        self.define_let_bindings(&def.lets);

        for let_stmt in &def.lets {
            self.visit_let_values(let_stmt);
        }

        self.visit_expr(&def.body);
        self.exit_scope();
    }

    fn define_let_bindings(&mut self, lets: &[ast::Let]) {
        for let_stmt in lets {
            for binding in &let_stmt.bindings {
                self.define_symbol(
                    SymbolKind::LetVariable,
                    binding.name.name.clone(),
                    binding.name.span,
                    Some(Namespace::Variable),
                );
            }
        }
    }

    fn visit_let_values(&mut self, let_stmt: &ast::Let) {
        for binding in &let_stmt.bindings {
            self.visit_expr(&binding.value);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Null(_) | Expr::Bool { .. } | Expr::Number { .. } | Expr::String { .. } => {}
            Expr::This(_) => {}
            Expr::Variable { name } => {
                if let Some(symbol_id) = self.resolve_variable(&name.name) {
                    self.add_reference(symbol_id, name.span);
                }
            }
            Expr::If { cond, then_br, else_br, .. } => {
                self.visit_expr(cond);
                self.visit_expr(then_br);
                if let Some(else_br) = else_br.as_ref() {
                    self.visit_expr(else_br);
                }
            }
            Expr::Unary { expr, .. } => self.visit_expr(expr),
            Expr::Binary { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::Member { target, .. } => self.visit_expr(target),
            Expr::Index { target, index, .. } => {
                self.visit_expr(target);
                self.visit_expr(index);
            }
            Expr::Slice { target, start, end, .. } => {
                self.visit_expr(target);
                if let Some(start) = start {
                    self.visit_expr(start);
                }
                if let Some(end) = end {
                    self.visit_expr(end);
                }
            }
            Expr::Call { callee, args, span } => {
                self.call_sites.push(CallSite {
                    span: *span,
                    callee_span: callee.span(),
                    arg_spans: args.iter().map(Expr::span).collect(),
                    scope: self.current_scope_id(),
                });
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::ArrayLiteral { elements, .. } => {
                for element in elements {
                    self.visit_expr(element);
                }
            }
            Expr::ArrayFor { seq, body, filter, .. } => {
                self.visit_expr(seq);
                self.visit_expr(body);
                if let Some(filter) = filter {
                    self.visit_expr(filter);
                }
            }
            Expr::ObjectLiteral { entries, .. } => {
                for entry in entries {
                    match entry {
                        ast::ObjectEntry::Pair { value, .. } => self.visit_expr(value),
                        ast::ObjectEntry::Spread { value, .. } => self.visit_expr(value),
                    }
                }
            }
            Expr::ObjectFor { seq, key, value, filter, .. } => {
                self.visit_expr(seq);
                self.visit_expr(key);
                self.visit_expr(value);
                if let Some(filter) = filter {
                    self.visit_expr(filter);
                }
            }
            Expr::LetBlock { lets, body, span } => {
                self.enter_scope(*span);
                self.define_let_bindings(lets);
                for let_stmt in lets {
                    self.visit_let_values(let_stmt);
                }
                self.visit_expr(body);
                self.exit_scope();
            }
            Expr::Group { expr, .. } => self.visit_expr(expr),
            Expr::FunctionRef { name, span } => {
                if let Some(symbol_id) = self.resolve_function(name) {
                    self.add_reference(symbol_id, *span);
                    return;
                }

                if let Some((alias, _)) = name.split_once(':')
                    && let Some(alias_symbol) = self.resolve_import_alias(alias)
                {
                    self.add_reference(alias_symbol, *span);
                    return;
                }

                if let Some(alias_symbol) = self.resolve_import_alias(name) {
                    self.add_reference(alias_symbol, *span);
                }
            }
        }
    }

    fn define_symbol(
        &mut self,
        kind: SymbolKind,
        name: String,
        declaration: Span,
        namespace: Option<Namespace>,
    ) -> SymbolId {
        let id = SymbolId(self.next_symbol_id);
        self.next_symbol_id += 1;

        let scope_id = self.current_scope_id();
        self.scopes[scope_id.0 as usize].symbols.push(id);
        self.symbols.push(Symbol {
            id,
            kind,
            name: name.clone(),
            declaration,
            references: Vec::new(),
            scope: scope_id,
        });

        if let Some(ns) = namespace {
            let frame = self.current_scope_frame_mut();
            match ns {
                Namespace::Variable => {
                    frame.vars.insert(name, id);
                }
                Namespace::Function => {
                    frame.funcs.insert(name, id);
                }
                Namespace::ImportAlias => {
                    frame.imports.insert(name, id);
                }
            }
        }

        id
    }

    fn add_reference(&mut self, symbol_id: SymbolId, span: Span) {
        if let Some(symbol) = self.symbols.get_mut(symbol_id.0 as usize) {
            symbol.references.push(span);
        }
    }

    fn enter_scope(&mut self, span: Span) {
        let parent = self.current_scope_id();
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        self.scopes.push(Scope { id, parent: Some(parent), span, symbols: Vec::new() });
        self.scope_stack.push(ScopeFrame {
            id,
            vars: HashMap::new(),
            funcs: HashMap::new(),
            imports: HashMap::new(),
        });
    }

    fn exit_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    fn resolve_variable(&self, name: &str) -> Option<SymbolId> {
        self.scope_stack.iter().rev().find_map(|scope| scope.vars.get(name).copied())
    }

    fn resolve_function(&self, name: &str) -> Option<SymbolId> {
        self.scope_stack.iter().rev().find_map(|scope| scope.funcs.get(name).copied())
    }

    fn resolve_import_alias(&self, name: &str) -> Option<SymbolId> {
        self.scope_stack.iter().rev().find_map(|scope| scope.imports.get(name).copied())
    }

    fn current_scope_id(&self) -> ScopeId {
        self.scope_stack.last().expect("at least root scope").id
    }

    fn current_scope_frame_mut(&mut self) -> &mut ScopeFrame {
        self.scope_stack.last_mut().expect("at least root scope")
    }
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
        assert!(diags.len() >= 2, "expected multiple diagnostics, got {}", diags.len());
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

        let diags =
            JsltLanguageServer::error_to_diagnostic(EngineError::ParseErrors(parse_errors), text);
        assert!(diags.len() >= 2, "expected multiple diagnostics, got {}", diags.len());
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
        assert!(
            diags[0]
                .code_description
                .as_ref()
                .is_some_and(|desc| desc.href.as_str().contains("#jslt_import_not_found"))
        );
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

    #[test]
    fn analysis_snapshot_indexes_symbols_scopes_and_calls() {
        let text = r#"
import "lib.jslt" as lib
def add(x, y) $x + $y
let value = add(1, 2)
add($value, 3)
"#;
        let uri = Url::parse("file:///tmp/snapshot.jslt").expect("uri");
        let snapshot = JsltLanguageServer::build_analysis_snapshot(&uri, text, Some(7));

        assert!(snapshot.program.is_some());
        assert!(snapshot.symbols.iter().any(|s| s.kind == SymbolKind::Function && s.name == "add"));
        assert!(
            snapshot.symbols.iter().any(|s| s.kind == SymbolKind::LetVariable && s.name == "value")
        );
        assert!(snapshot.call_sites.len() >= 2);

        let value_ref_offset = text.find("$value").expect("value reference");
        let value_ref_position =
            JsltLanguageServer::byte_offset_to_position(text, value_ref_offset + 1);
        let symbol = snapshot.symbol_at(value_ref_position).expect("symbol at value ref");
        assert_eq!(symbol.name, "value");

        let call_position = value_ref_position;
        let call = snapshot.call_at(call_position).expect("call at add");
        assert_eq!(call.arg_spans.len(), 2);

        let param_offset = text.find("$x +").expect("param ref");
        let param_position = JsltLanguageServer::byte_offset_to_position(text, param_offset + 1);
        let scope = snapshot.scope_at(param_position).expect("scope at param");
        assert!(scope.parent.is_some(), "parameter should resolve inside function scope");
    }

    #[test]
    fn analysis_snapshot_retains_import_index_on_parse_failure() {
        let text = r#"
import "something.jslt" as pkg
{
  "broken": if (
}
"#;
        let uri = Url::parse("file:///tmp/broken.jslt").expect("uri");
        let snapshot = JsltLanguageServer::build_analysis_snapshot(&uri, text, Some(3));

        assert!(snapshot.program.is_none());
        assert_eq!(snapshot.imports.len(), 1);
        assert!(
            snapshot.symbols.iter().any(|s| s.kind == SymbolKind::ImportAlias && s.name == "pkg")
        );
    }

    #[test]
    fn analysis_snapshot_collects_binder_diagnostics() {
        let text = r#"
def normalize(x) $y
normalize(1)
"#;
        let uri = Url::parse("file:///tmp/binder.jslt").expect("uri");
        let snapshot = JsltLanguageServer::build_analysis_snapshot(&uri, text, Some(1));

        assert!(
            snapshot
                .binder_diagnostics
                .iter()
                .any(|diag| diag.code == JsltLanguageServer::CODE_BIND_UNKNOWN_VARIABLE)
        );
    }

    #[test]
    fn dashed_function_reference_span_is_single_token() {
        let text = r#"
def clean-keys(x) $x
clean-keys(1)
"#;
        let uri = Url::parse("file:///tmp/dashed.jslt").expect("uri");
        let snapshot = JsltLanguageServer::build_analysis_snapshot(&uri, text, Some(1));

        let func = snapshot
            .symbols
            .iter()
            .find(|s| s.kind == SymbolKind::Function && s.name == "clean-keys")
            .expect("function symbol");

        let call_start = text.rfind("clean-keys(1)").expect("call start");
        let expected = (call_start, call_start + "clean-keys".len());
        let refs: Vec<(usize, usize)> = func.references.iter().map(|s| (s.start, s.end)).collect();
        assert!(
            refs.contains(&expected),
            "expected dashed reference span {:?}, got {:?}",
            expected,
            refs
        );
    }
}
