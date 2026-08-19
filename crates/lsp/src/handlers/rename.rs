use std::collections::HashMap;

use ast::Span;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    PrepareRenameResponse, Range, RenameParams, TextDocumentPositionParams, TextEdit, Url,
    WorkspaceEdit,
};

use crate::JsltLanguageServer;
use crate::context::{
    AnalysisSnapshot, Symbol, SymbolKind, position_to_byte_offset, span_contains,
};
use crate::handlers::references::{is_exported_function, qualified_usages, span_to_range};
use crate::naming::{alias_declaration_span, identifier_span, split_qualified};

/// What a rename request at a given position actually renames.
#[derive(Debug, Clone)]
enum RenameTarget {
    /// A def, let or parameter declared in the current document.
    Local(Symbol),
    /// An import alias, which only ever affects the importing document.
    Alias(Symbol),
    /// A `def` in an imported module, reached through `alias:function`.
    Imported { uri: Url, symbol: Symbol },
}

/// A resolved rename target plus the range the editor should highlight.
struct PreparedRename {
    target: RenameTarget,
    range: Range,
}

pub(crate) async fn prepare_rename(
    server: &JsltLanguageServer,
    params: TextDocumentPositionParams,
) -> Result<Option<PrepareRenameResponse>> {
    let uri = params.text_document.uri;
    let position = params.position;

    let Some(snapshot) = snapshot_for(server, &uri).await else {
        return Ok(None);
    };
    let Some(prepared) = resolve_target(server, &uri, &snapshot, position).await else {
        return Ok(None);
    };

    Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
        range: prepared.range,
        placeholder: prepared.target.name().to_string(),
    }))
}

pub(crate) async fn rename(
    server: &JsltLanguageServer,
    params: RenameParams,
) -> Result<Option<WorkspaceEdit>> {
    // The rename box shows `value` rather than `$value`, so a user typing the
    // sigil back in means the identifier, not a name starting with `$`.
    let new_name = params.new_name.strip_prefix('$').unwrap_or(&params.new_name).to_string();
    if !valid_identifier(&new_name) {
        return Ok(None);
    }

    let uri = params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let Some(snapshot) = snapshot_for(server, &uri).await else {
        return Ok(None);
    };
    let Some(prepared) = resolve_target(server, &uri, &snapshot, position).await else {
        return Ok(None);
    };

    let changes = rename_edits(server, &uri, &snapshot, &prepared.target, &new_name).await;
    if changes.is_empty() {
        return Ok(None);
    }

    Ok(Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    }))
}

async fn snapshot_for(server: &JsltLanguageServer, uri: &Url) -> Option<AnalysisSnapshot> {
    let context = server.request_context(uri, None).await;
    let document = context.document?;
    match context.snapshot {
        Some(snapshot) => Some(snapshot),
        None => Some(server.analyze_document(uri, &document.text, document.version).await),
    }
}

/// Work out what the cursor is pointing at and which range covers it.
async fn resolve_target(
    server: &JsltLanguageServer,
    uri: &Url,
    snapshot: &AnalysisSnapshot,
    position: tower_lsp::lsp_types::Position,
) -> Option<PreparedRename> {
    let symbol = snapshot.symbol_at(position)?;
    let offset = position_to_byte_offset(&snapshot.text, position)?;

    if symbol.kind == SymbolKind::ImportAlias {
        return resolve_alias_target(server, uri, snapshot, symbol, position, offset).await;
    }

    if !renamable(symbol) {
        return None;
    }

    // Rename can be triggered from any occurrence, so highlight the one under
    // the cursor rather than always the declaration.
    let span = occurrence_at(symbol, offset).unwrap_or(symbol.declaration);
    Some(PreparedRename {
        target: RenameTarget::Local(symbol.clone()),
        range: span_to_range(&snapshot.text, identifier_span(&snapshot.text, span)),
    })
}

/// Aliases carry coarse spans: the declaration covers the whole `import`
/// statement and references cover the whole `alias:function` token.
async fn resolve_alias_target(
    server: &JsltLanguageServer,
    uri: &Url,
    snapshot: &AnalysisSnapshot,
    symbol: &Symbol,
    position: tower_lsp::lsp_types::Position,
    offset: usize,
) -> Option<PreparedRename> {
    if let Some(reference) = snapshot.reference_at(symbol, position) {
        let qualified = split_qualified(&snapshot.text, reference, &snapshot.import_aliases());

        let Some(qualified) = qualified else {
            return Some(PreparedRename {
                target: RenameTarget::Alias(symbol.clone()),
                range: span_to_range(&snapshot.text, reference),
            });
        };

        // Cursor on the function half renames the imported `def` itself.
        if span_contains(qualified.function_span, offset) {
            let import = snapshot.imports.iter().find(|imp| imp.alias == qualified.alias)?;
            let target_uri = JsltLanguageServer::resolve_import_uri(uri, &import.path)?;
            let file = server.indexed_file(&target_uri).await?;
            let def = file.exported_function(&qualified.function)?;

            return Some(PreparedRename {
                target: RenameTarget::Imported { uri: file.uri.clone(), symbol: def.clone() },
                range: span_to_range(&snapshot.text, qualified.function_span),
            });
        }

        return Some(PreparedRename {
            target: RenameTarget::Alias(symbol.clone()),
            range: span_to_range(&snapshot.text, qualified.alias_span),
        });
    }

    let declaration = alias_declaration_span(&snapshot.text, symbol.declaration, &symbol.name)?;
    Some(PreparedRename {
        target: RenameTarget::Alias(symbol.clone()),
        range: span_to_range(&snapshot.text, declaration),
    })
}

async fn rename_edits(
    server: &JsltLanguageServer,
    uri: &Url,
    snapshot: &AnalysisSnapshot,
    target: &RenameTarget,
    new_name: &str,
) -> HashMap<Url, Vec<TextEdit>> {
    let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

    match target {
        RenameTarget::Alias(symbol) => {
            changes.insert(uri.clone(), alias_edits(snapshot, symbol, new_name));
        }
        RenameTarget::Local(symbol) => {
            let spans = occurrence_spans(&snapshot.text, symbol);
            changes.insert(uri.clone(), edits_for_spans(&snapshot.text, spans, new_name));

            if is_exported_function(symbol) {
                extend_with_importers(server, uri, &symbol.name, new_name, &mut changes).await;
            }
        }
        RenameTarget::Imported { uri: def_uri, symbol } => {
            let Some(file) = server.indexed_file(def_uri).await else {
                return changes;
            };

            let spans = occurrence_spans(&file.snapshot.text, symbol);
            changes.insert(file.uri.clone(), edits_for_spans(&file.snapshot.text, spans, new_name));

            extend_with_importers(server, def_uri, &symbol.name, new_name, &mut changes).await;
        }
    }

    changes.retain(|_, edits| !edits.is_empty());
    changes
}

/// Rewrite only the alias identifier, never the module path or the function
/// half of an `alias:function` reference.
fn alias_edits(snapshot: &AnalysisSnapshot, symbol: &Symbol, new_name: &str) -> Vec<TextEdit> {
    let aliases = snapshot.import_aliases();
    let mut spans = Vec::new();

    if let Some(declaration) =
        alias_declaration_span(&snapshot.text, symbol.declaration, &symbol.name)
    {
        spans.push(declaration);
    }

    for reference in &symbol.references {
        match split_qualified(&snapshot.text, *reference, &aliases) {
            Some(qualified) => spans.push(qualified.alias_span),
            None => spans.push(*reference),
        }
    }

    edits_for_spans(&snapshot.text, spans, new_name)
}

/// Rewrite the function half of `alias:function` in every importing file.
async fn extend_with_importers(
    server: &JsltLanguageServer,
    def_uri: &Url,
    function: &str,
    new_name: &str,
    changes: &mut HashMap<Url, Vec<TextEdit>>,
) {
    for (dependent_uri, alias) in server.dependents_of(def_uri).await {
        let Some(file) = server.indexed_file(&dependent_uri).await else {
            continue;
        };

        let spans: Vec<Span> = qualified_usages(&file, &alias, function)
            .into_iter()
            .map(|usage| usage.function_span)
            .collect();
        if spans.is_empty() {
            continue;
        }

        let edits = edits_for_spans(&file.snapshot.text, spans, new_name);
        changes.entry(file.uri.clone()).or_default().extend(edits);
    }

    for edits in changes.values_mut() {
        sort_and_dedup(edits);
    }
}

/// Every place `symbol` is written, narrowed to the identifier itself.
fn occurrence_spans(text: &str, symbol: &Symbol) -> Vec<Span> {
    std::iter::once(symbol.declaration)
        .chain(symbol.references.iter().copied())
        .map(|span| identifier_span(text, span))
        .collect()
}

fn edits_for_spans(text: &str, spans: Vec<Span>, new_name: &str) -> Vec<TextEdit> {
    let mut edits: Vec<TextEdit> = spans
        .into_iter()
        .map(|span| TextEdit { range: span_to_range(text, span), new_text: new_name.to_string() })
        .collect();
    sort_and_dedup(&mut edits);
    edits
}

fn sort_and_dedup(edits: &mut Vec<TextEdit>) {
    edits.sort_by_key(|edit| {
        (
            edit.range.start.line,
            edit.range.start.character,
            edit.range.end.line,
            edit.range.end.character,
        )
    });
    edits.dedup_by(|a, b| a.range == b.range);
}

fn occurrence_at(symbol: &Symbol, offset: usize) -> Option<Span> {
    if span_contains(symbol.declaration, offset) {
        return Some(symbol.declaration);
    }
    symbol.references.iter().find(|span| span_contains(**span, offset)).copied()
}

impl RenameTarget {
    fn name(&self) -> &str {
        match self {
            Self::Local(symbol) | Self::Alias(symbol) => &symbol.name,
            Self::Imported { symbol, .. } => &symbol.name,
        }
    }
}

fn renamable(symbol: &Symbol) -> bool {
    matches!(
        symbol.kind,
        SymbolKind::Function
            | SymbolKind::Parameter
            | SymbolKind::LetVariable
            | SymbolKind::ImportAlias
    )
}

fn valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == ':' || ch == '-')
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ImportHeaderEntry, ScopeId, SymbolId};

    fn span(start: usize, end: usize) -> Span {
        Span { start, end, line: 1, column: 1 }
    }

    fn alias_snapshot() -> (AnalysisSnapshot, Symbol) {
        let text = "import \"utils.jslt\" as utils\n\nutils:double(.n)\n".to_string();
        let import_span = span(0, 28);
        let reference = span(30, 42);

        let symbol = Symbol {
            id: SymbolId(0),
            kind: SymbolKind::ImportAlias,
            name: "utils".to_string(),
            declaration: import_span,
            references: vec![reference],
            scope: ScopeId(0),
        };

        let snapshot = AnalysisSnapshot {
            version: Some(1),
            diagnostics: Vec::new(),
            program: None,
            binder_diagnostics: Vec::new(),
            symbols: vec![symbol.clone()],
            scopes: Vec::new(),
            call_sites: Vec::new(),
            imports: vec![ImportHeaderEntry {
                path: "utils.jslt".to_string(),
                alias: "utils".to_string(),
                span: import_span,
            }],
            text,
        };

        (snapshot, symbol)
    }

    #[test]
    fn valid_identifier_rejects_empty_or_invalid_start() {
        assert!(!valid_identifier(""));
        assert!(!valid_identifier("1abc"));
        assert!(valid_identifier("abc_1"));
        assert!(valid_identifier("foo:bar"));
    }

    #[test]
    fn renaming_an_alias_only_rewrites_the_alias_identifier() {
        let (snapshot, symbol) = alias_snapshot();

        let edits = alias_edits(&snapshot, &symbol, "helpers");

        assert_eq!(edits.len(), 2);
        // `import "utils.jslt" as <utils>` - the path must be left alone.
        assert_eq!(edits[0].range.start.line, 0);
        assert_eq!(edits[0].range.start.character, 23);
        assert_eq!(edits[0].range.end.character, 28);
        // `<utils>:double(.n)` - the function half must be left alone.
        assert_eq!(edits[1].range.start.line, 2);
        assert_eq!(edits[1].range.start.character, 0);
        assert_eq!(edits[1].range.end.character, 5);
    }

    #[test]
    fn local_edits_cover_declaration_and_every_reference() {
        // References span the whole `$x` token; declarations cover only `x`.
        let text = "let x = 1\n$x + $x\n";
        let symbol = Symbol {
            id: SymbolId(0),
            kind: SymbolKind::LetVariable,
            name: "x".to_string(),
            declaration: span(4, 5),
            references: vec![span(10, 12), span(15, 17)],
            scope: ScopeId(0),
        };

        let edits = edits_for_spans(text, occurrence_spans(text, &symbol), "y");

        assert_eq!(edits.len(), 3);
        assert!(edits.iter().all(|edit| edit.new_text == "y"));
        // Every edit rewrites the identifier only, leaving the `$` in place.
        assert_eq!(edits[1].range.start.character, 1);
        assert_eq!(edits[1].range.end.character, 2);
        assert_eq!(edits[2].range.start.character, 6);
        assert_eq!(edits[2].range.end.character, 7);
    }

    #[test]
    fn occurrence_at_prefers_the_span_under_the_cursor() {
        let symbol = Symbol {
            id: SymbolId(0),
            kind: SymbolKind::LetVariable,
            name: "x".to_string(),
            declaration: span(4, 5),
            references: vec![span(11, 12)],
            scope: ScopeId(0),
        };

        assert_eq!(occurrence_at(&symbol, 4), Some(span(4, 5)));
        assert_eq!(occurrence_at(&symbol, 11), Some(span(11, 12)));
        assert_eq!(occurrence_at(&symbol, 40), None);
    }
}
