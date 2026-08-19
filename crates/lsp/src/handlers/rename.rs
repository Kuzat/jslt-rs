use std::collections::HashMap;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    PrepareRenameResponse, RenameParams, TextDocumentPositionParams, TextEdit, WorkspaceEdit,
};

use crate::JsltLanguageServer;
use crate::context::{Symbol, SymbolKind};
use crate::handlers::references::{span_to_range, symbol_locations};

pub(crate) async fn prepare_rename(
    server: &JsltLanguageServer,
    params: TextDocumentPositionParams,
) -> Result<Option<PrepareRenameResponse>> {
    let uri = params.text_document.uri;
    let position = params.position;

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
    if !renamable(symbol) {
        return Ok(None);
    }

    Ok(Some(PrepareRenameResponse::RangeWithPlaceholder {
        range: span_to_range(&snapshot.text, symbol.declaration),
        placeholder: symbol.name.clone(),
    }))
}

pub(crate) async fn rename(
    server: &JsltLanguageServer,
    params: RenameParams,
) -> Result<Option<WorkspaceEdit>> {
    if !valid_identifier(&params.new_name) {
        return Ok(None);
    }

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
    if !renamable(symbol) {
        return Ok(None);
    }

    let mut edits: Vec<TextEdit> = symbol_locations(&snapshot, &uri, symbol, true)
        .into_iter()
        .map(|loc| TextEdit { range: loc.range, new_text: params.new_name.clone() })
        .collect();
    edits.sort_by_key(|edit| {
        (
            edit.range.start.line,
            edit.range.start.character,
            edit.range.end.line,
            edit.range.end.character,
        )
    });
    edits.dedup_by(|a, b| a.range == b.range);

    let mut changes = HashMap::new();
    changes.insert(uri, edits);

    Ok(Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    }))
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

    #[test]
    fn valid_identifier_rejects_empty_or_invalid_start() {
        assert!(!valid_identifier(""));
        assert!(!valid_identifier("1abc"));
        assert!(valid_identifier("abc_1"));
        assert!(valid_identifier("foo:bar"));
    }
}
