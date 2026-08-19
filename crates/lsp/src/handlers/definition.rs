use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Position, Range,
};

use crate::JsltLanguageServer;
use crate::context::{AnalysisSnapshot, Symbol, SymbolKind, byte_offset_to_position};

pub(crate) async fn definition(
    server: &JsltLanguageServer,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let uri = params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

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

    if let Some(location) = definition_location(&snapshot, &uri, symbol) {
        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
    }

    Ok(None)
}

fn definition_location(
    snapshot: &AnalysisSnapshot,
    uri: &tower_lsp::lsp_types::Url,
    symbol: &Symbol,
) -> Option<Location> {
    if symbol.kind == SymbolKind::ImportAlias {
        let import = snapshot.imports.iter().find(|imp| imp.alias == symbol.name)?;
        if let Some(target_uri) = JsltLanguageServer::resolve_import_uri(uri, &import.path) {
            return Some(Location {
                uri: target_uri,
                range: Range::new(Position::new(0, 0), Position::new(0, 0)),
            });
        }
    }

    Some(Location { uri: uri.clone(), range: span_to_range(&snapshot.text, symbol.declaration) })
}

fn span_to_range(text: &str, span: ast::Span) -> Range {
    Range {
        start: byte_offset_to_position(text, span.start),
        end: byte_offset_to_position(text, span.end),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ScopeId, SymbolId};

    #[test]
    fn local_symbol_definition_points_to_its_declaration() {
        let snapshot = AnalysisSnapshot {
            version: Some(1),
            diagnostics: Vec::new(),
            program: None,
            binder_diagnostics: Vec::new(),
            symbols: vec![Symbol {
                id: SymbolId(0),
                kind: SymbolKind::LetVariable,
                name: "x".to_string(),
                declaration: ast::Span { start: 4, end: 5, line: 1, column: 1 },
                references: Vec::new(),
                scope: ScopeId(0),
            }],
            scopes: Vec::new(),
            call_sites: Vec::new(),
            imports: Vec::new(),
            text: "let x = 1\n".to_string(),
        };
        let uri = tower_lsp::lsp_types::Url::parse("file:///tmp/test.jslt").expect("uri");

        let loc = definition_location(&snapshot, &uri, &snapshot.symbols[0]).expect("location");
        assert_eq!(loc.uri, uri);
        assert_eq!(loc.range.start.character, 4);
        assert_eq!(loc.range.end.character, 5);
    }
}
