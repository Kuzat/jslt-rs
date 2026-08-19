use ast::{Program, Span};
use tower_lsp::lsp_types::{Diagnostic, Position, Url};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SymbolId(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ScopeId(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SymbolKind {
    Function,
    Parameter,
    LetVariable,
    ImportAlias,
    ModulePath,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct Symbol {
    pub(crate) id: SymbolId,
    pub(crate) kind: SymbolKind,
    pub(crate) name: String,
    pub(crate) declaration: Span,
    pub(crate) references: Vec<Span>,
    pub(crate) scope: ScopeId,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct Scope {
    pub(crate) id: ScopeId,
    pub(crate) parent: Option<ScopeId>,
    pub(crate) span: Span,
    pub(crate) symbols: Vec<SymbolId>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct CallSite {
    pub(crate) span: Span,
    pub(crate) callee_span: Span,
    pub(crate) arg_spans: Vec<Span>,
    pub(crate) scope: ScopeId,
}

#[derive(Debug, Clone)]
pub(crate) struct ImportHeaderEntry {
    pub(crate) path: String,
    pub(crate) alias: String,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct BinderDiagnostic {
    pub(crate) code: String,
    pub(crate) message: String,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct AnalysisSnapshot {
    pub(crate) version: Option<i32>,
    pub(crate) diagnostics: Vec<Diagnostic>,
    pub(crate) program: Option<Program>,
    pub(crate) binder_diagnostics: Vec<BinderDiagnostic>,
    pub(crate) symbols: Vec<Symbol>,
    pub(crate) scopes: Vec<Scope>,
    pub(crate) call_sites: Vec<CallSite>,
    pub(crate) imports: Vec<ImportHeaderEntry>,
    pub(crate) text: String,
}

impl AnalysisSnapshot {
    pub(crate) fn new(version: Option<i32>, text: String, diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            version,
            diagnostics,
            program: None,
            binder_diagnostics: Vec::new(),
            symbols: Vec::new(),
            scopes: Vec::new(),
            call_sites: Vec::new(),
            imports: Vec::new(),
            text,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn symbol_at(&self, position: Position) -> Option<&Symbol> {
        let offset = position_to_byte_offset(&self.text, position)?;
        self.symbols
            .iter()
            .filter_map(|symbol| {
                if span_contains(symbol.declaration, offset) {
                    return Some((span_width(symbol.declaration), symbol));
                }

                symbol
                    .references
                    .iter()
                    .find(|span| span_contains(**span, offset))
                    .map(|span| (span_width(*span), symbol))
            })
            .min_by_key(|(width, symbol)| (*width, symbol.id.0))
            .map(|(_, symbol)| symbol)
    }

    #[allow(dead_code)]
    pub(crate) fn scope_at(&self, position: Position) -> Option<&Scope> {
        let offset = position_to_byte_offset(&self.text, position)?;
        self.scopes
            .iter()
            .filter(|scope| span_contains(scope.span, offset))
            .min_by_key(|scope| (span_width(scope.span), scope.id.0))
    }

    #[allow(dead_code)]
    pub(crate) fn call_at(&self, position: Position) -> Option<&CallSite> {
        let offset = position_to_byte_offset(&self.text, position)?;
        self.call_sites
            .iter()
            .filter(|call| span_contains(call.span, offset))
            .min_by_key(|call| (span_width(call.span), call.span.start))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DocumentContext {
    pub(crate) uri: Url,
    pub(crate) version: Option<i32>,
    pub(crate) text: String,
}

impl DocumentContext {
    pub(crate) fn new(uri: Url, version: Option<i32>, text: String) -> Self {
        Self { uri, version, text }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct WorkspaceView {
    pub(crate) open_document_count: usize,
    pub(crate) indexed_file_count: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct RequestContext {
    pub(crate) document: Option<DocumentContext>,
    pub(crate) snapshot: Option<AnalysisSnapshot>,
    pub(crate) workspace: WorkspaceView,
}

pub(crate) fn position_to_byte_offset(text: &str, position: Position) -> Option<usize> {
    let mut line: u32 = 0;
    let mut offset: usize = 0;

    for segment in text.split_inclusive('\n') {
        if line == position.line {
            let mut utf16_count: u32 = 0;
            for (idx, ch) in segment.char_indices() {
                if utf16_count >= position.character {
                    return Some(offset + idx);
                }
                utf16_count += ch.len_utf16() as u32;
            }
            return Some(offset + segment.len());
        }
        offset += segment.len();
        line += 1;
    }

    if position.line == line && position.character == 0 {
        return Some(offset);
    }

    None
}

pub(crate) fn span_contains(span: Span, offset: usize) -> bool {
    if span.start == span.end {
        offset == span.start
    } else {
        offset >= span.start && offset < span.end
    }
}

pub(crate) fn span_width(span: Span) -> usize {
    span.end.saturating_sub(span.start)
}

pub(crate) fn byte_offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut column = 0u32;
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
