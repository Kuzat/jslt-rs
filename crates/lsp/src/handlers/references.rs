use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{Location, Range, ReferenceParams};

use crate::JsltLanguageServer;
use crate::context::{AnalysisSnapshot, Symbol, byte_offset_to_position};

pub(crate) async fn references(
    server: &JsltLanguageServer,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let uri = params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;

    let context = server.request_context(&uri, None).await;
    let Some(document) = context.document else {
        return Ok(None);
    };

    let snapshot = match context.snapshot {
        Some(snapshot) => snapshot,
        None => server.analyze_document(&uri, &document.text, document.version).await,
    };

    let Some(symbol) = snapshot.symbol_at(position) else {
        return Ok(None);
    };

    let mut locations =
        symbol_locations(&snapshot, &uri, symbol, params.context.include_declaration);

    locations.sort_by_key(|loc| {
        (
            loc.range.start.line,
            loc.range.start.character,
            loc.range.end.line,
            loc.range.end.character,
        )
    });
    locations.dedup_by(|a, b| a.range == b.range && a.uri == b.uri);

    if locations.is_empty() {
        return Ok(None);
    }

    Ok(Some(locations))
}

pub(crate) fn symbol_locations(
    snapshot: &AnalysisSnapshot,
    uri: &tower_lsp::lsp_types::Url,
    symbol: &Symbol,
    include_declaration: bool,
) -> Vec<Location> {
    let mut out = Vec::new();
    if include_declaration {
        out.push(Location {
            uri: uri.clone(),
            range: span_to_range(&snapshot.text, symbol.declaration),
        });
    }

    for span in &symbol.references {
        out.push(Location { uri: uri.clone(), range: span_to_range(&snapshot.text, *span) });
    }

    out
}

pub(crate) fn span_to_range(text: &str, span: ast::Span) -> Range {
    Range {
        start: byte_offset_to_position(text, span.start),
        end: byte_offset_to_position(text, span.end),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ScopeId, SymbolId, SymbolKind};

    #[test]
    fn symbol_locations_include_declaration_when_requested() {
        let snapshot = AnalysisSnapshot {
            version: Some(1),
            diagnostics: Vec::new(),
            program: None,
            binder_diagnostics: Vec::new(),
            symbols: Vec::new(),
            scopes: Vec::new(),
            call_sites: Vec::new(),
            imports: Vec::new(),
            text: "let x = $x\n".to_string(),
        };
        let symbol = Symbol {
            id: SymbolId(0),
            kind: SymbolKind::LetVariable,
            name: "x".to_string(),
            declaration: ast::Span { start: 4, end: 5, line: 1, column: 1 },
            references: vec![ast::Span { start: 9, end: 10, line: 1, column: 1 }],
            scope: ScopeId(0),
        };
        let uri = tower_lsp::lsp_types::Url::parse("file:///tmp/references.jslt").expect("uri");

        let with_decl = symbol_locations(&snapshot, &uri, &symbol, true);
        let without_decl = symbol_locations(&snapshot, &uri, &symbol, false);

        assert_eq!(with_decl.len(), 2);
        assert_eq!(without_decl.len(), 1);
    }
}
