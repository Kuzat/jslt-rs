use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Position, Range, Url,
};

use crate::JsltLanguageServer;
use crate::context::{
    AnalysisSnapshot, Symbol, SymbolKind, byte_offset_to_position, position_to_byte_offset,
    span_contains,
};
use crate::naming::split_qualified;

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

    // An `alias:function` reference should jump to the `def` in the imported
    // module rather than to the local import statement.
    if symbol.kind == SymbolKind::ImportAlias
        && let Some(location) = imported_definition(server, &snapshot, &uri, symbol, position).await
    {
        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
    }

    if let Some(location) = definition_location(&snapshot, &uri, symbol) {
        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
    }

    Ok(None)
}

/// Resolve a qualified reference under the cursor to its definition in the
/// imported module, falling back to the top of the module file.
async fn imported_definition(
    server: &JsltLanguageServer,
    snapshot: &AnalysisSnapshot,
    uri: &Url,
    symbol: &Symbol,
    position: Position,
) -> Option<Location> {
    let span = snapshot.reference_at(symbol, position)?;
    let qualified = split_qualified(&snapshot.text, span, &snapshot.import_aliases())?;
    let import = snapshot.imports.iter().find(|imp| imp.alias == qualified.alias)?;
    let target = JsltLanguageServer::resolve_import_uri(uri, &import.path)?;

    // The alias half of `alias:function` names the module, not the function.
    let offset = position_to_byte_offset(&snapshot.text, position)?;
    if span_contains(qualified.alias_span, offset) {
        return Some(module_head(target));
    }

    let Some(file) = server.indexed_file(&target).await else {
        return Some(module_head(target));
    };

    match file.exported_function(&qualified.function) {
        Some(def) => Some(Location {
            uri: file.uri.clone(),
            range: span_to_range(&file.snapshot.text, def.declaration),
        }),
        None => Some(module_head(file.uri.clone())),
    }
}

fn definition_location(
    snapshot: &AnalysisSnapshot,
    uri: &Url,
    symbol: &Symbol,
) -> Option<Location> {
    if symbol.kind == SymbolKind::ImportAlias {
        let import = snapshot.imports.iter().find(|imp| imp.alias == symbol.name)?;
        if let Some(target_uri) = JsltLanguageServer::resolve_import_uri(uri, &import.path) {
            return Some(module_head(target_uri));
        }
    }

    Some(Location { uri: uri.clone(), range: span_to_range(&snapshot.text, symbol.declaration) })
}

fn module_head(uri: Url) -> Location {
    Location { uri, range: Range::new(Position::new(0, 0), Position::new(0, 0)) }
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
        let uri = Url::parse("file:///tmp/test.jslt").expect("uri");

        let loc = definition_location(&snapshot, &uri, &snapshot.symbols[0]).expect("location");
        assert_eq!(loc.uri, uri);
        assert_eq!(loc.range.start.character, 4);
        assert_eq!(loc.range.end.character, 5);
    }
}
