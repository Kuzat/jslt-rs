use std::collections::HashSet;

use serde_json::json;
use stdlib::{Arity, Registry};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, Documentation,
    InsertTextFormat, MarkupContent, MarkupKind, Position,
};

use crate::JsltLanguageServer;
use crate::context::{AnalysisSnapshot, ScopeId, SymbolKind};

pub(crate) async fn completion(
    server: &JsltLanguageServer,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
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

    let mut items = collect_symbol_completions(&snapshot, position);
    items.extend(collect_builtin_completions());
    items.extend(keyword_completions());

    Ok(Some(CompletionResponse::Array(items)))
}

pub(crate) async fn completion_resolve(
    _server: &JsltLanguageServer,
    mut item: CompletionItem,
) -> Result<CompletionItem> {
    let Some(data) = item.data.as_ref() else {
        return Ok(item);
    };

    let kind = data.get("kind").and_then(|v| v.as_str());
    if kind == Some("builtin") {
        let name = data.get("name").and_then(|v| v.as_str()).unwrap_or_default();
        let arity = data.get("arity").and_then(|v| v.as_str()).unwrap_or("unknown arity");
        item.detail = Some(format!("builtin function ({})", arity));
        item.documentation = Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("`{}`\\n\\nJSLT stdlib function with {}.", name, arity),
        }));
    }

    Ok(item)
}

fn collect_symbol_completions(
    snapshot: &AnalysisSnapshot,
    position: Position,
) -> Vec<CompletionItem> {
    let scope_chain = visible_scope_chain(snapshot, position);
    let mut items = Vec::new();
    let mut seen = HashSet::new();

    for (depth, scope_id) in scope_chain.iter().enumerate() {
        let scope = &snapshot.scopes[scope_id.0 as usize];
        for symbol_id in &scope.symbols {
            let symbol = &snapshot.symbols[symbol_id.0 as usize];
            if symbol.kind == SymbolKind::ModulePath {
                continue;
            }
            if !seen.insert(symbol.name.clone()) {
                continue;
            }

            let (kind, detail) = match symbol.kind {
                SymbolKind::Function => {
                    (Some(CompletionItemKind::FUNCTION), Some("function".to_string()))
                }
                SymbolKind::Parameter => {
                    (Some(CompletionItemKind::VARIABLE), Some("parameter".to_string()))
                }
                SymbolKind::LetVariable => {
                    (Some(CompletionItemKind::VARIABLE), Some("variable".to_string()))
                }
                SymbolKind::ImportAlias => {
                    (Some(CompletionItemKind::MODULE), Some("import alias".to_string()))
                }
                SymbolKind::ModulePath => (None, None),
            };

            if kind.is_none() {
                continue;
            }

            items.push(CompletionItem {
                label: symbol.name.clone(),
                kind,
                detail,
                sort_text: Some(format!("{:02}-{}", depth, symbol.name)),
                ..Default::default()
            });
        }
    }

    items
}

fn collect_builtin_completions() -> Vec<CompletionItem> {
    let registry = Registry::with_default();
    let mut names: Vec<&str> = registry.names().collect();
    names.sort_unstable();

    let mut out = Vec::with_capacity(names.len());
    for name in names {
        let arity = registry
            .get_id(name)
            .and_then(|id| registry.get_by_id(id))
            .map(|f| format_arity(f.arity()))
            .unwrap_or_else(|| "unknown arity".to_string());

        out.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(format!("builtin function ({})", arity)),
            sort_text: Some(format!("50-{}", name)),
            data: Some(json!({"kind": "builtin", "name": name, "arity": arity})),
            ..Default::default()
        });
    }
    out
}

fn keyword_completions() -> Vec<CompletionItem> {
    vec![
        keyword_item("def", "def ${1:name}(${2:arg})\\n  ${3:.}"),
        keyword_item("let", "let ${1:name} = ${2:.}"),
        keyword_item("if", "if (${1:cond})\\n  ${2:.}\\nelse\\n  ${3:.}"),
        keyword_item("for", "for (${1:seq}) ${2:.}"),
        plain_keyword("null"),
        plain_keyword("true"),
        plain_keyword("false"),
        plain_keyword("and"),
        plain_keyword("or"),
        plain_keyword("not"),
    ]
}

fn keyword_item(label: &str, snippet: &str) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(CompletionItemKind::KEYWORD),
        sort_text: Some(format!("90-{}", label)),
        insert_text: Some(snippet.to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    }
}

fn plain_keyword(label: &str) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(CompletionItemKind::KEYWORD),
        sort_text: Some(format!("90-{}", label)),
        ..Default::default()
    }
}

fn visible_scope_chain(snapshot: &AnalysisSnapshot, position: Position) -> Vec<ScopeId> {
    let Some(scope) = snapshot.scope_at(position) else {
        return vec![ScopeId(0)];
    };

    let mut chain = Vec::new();
    let mut current = Some(scope.id);
    while let Some(scope_id) = current {
        chain.push(scope_id);
        current = snapshot.scopes[scope_id.0 as usize].parent;
    }
    chain
}

fn format_arity(arity: Arity) -> String {
    match arity {
        Arity::Exact(n) => format!("exactly {}", n),
        Arity::Range { min, max } => match max {
            Some(max) if min == max => format!("exactly {}", min),
            Some(max) => format!("{}..{} args", min, max),
            None => format!("at least {}", min),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{AnalysisSnapshot, Scope, Symbol, SymbolId};
    use ast::Span;
    use tower_lsp::lsp_types::{Diagnostic, Position};

    fn span(start: usize, end: usize) -> Span {
        Span { start, end, line: 1, column: 1 }
    }

    #[test]
    fn visible_scope_symbols_rank_before_outer_scope() {
        let mut snapshot = AnalysisSnapshot::new(
            None,
            "abcdefghijklmnopqrstuvw".to_string(),
            Vec::<Diagnostic>::new(),
        );
        snapshot.scopes = vec![
            Scope { id: ScopeId(0), parent: None, span: span(0, 23), symbols: vec![SymbolId(0)] },
            Scope {
                id: ScopeId(1),
                parent: Some(ScopeId(0)),
                span: span(10, 23),
                symbols: vec![SymbolId(1)],
            },
        ];
        snapshot.symbols = vec![
            Symbol {
                id: SymbolId(0),
                kind: SymbolKind::LetVariable,
                name: "a".to_string(),
                declaration: span(4, 5),
                references: vec![],
                scope: ScopeId(0),
            },
            Symbol {
                id: SymbolId(1),
                kind: SymbolKind::Parameter,
                name: "x".to_string(),
                declaration: span(16, 17),
                references: vec![],
                scope: ScopeId(1),
            },
        ];

        let items = collect_symbol_completions(&snapshot, Position::new(0, 12));
        assert!(items.iter().any(|i| i.label == "x"));
        assert!(items.iter().any(|i| i.label == "a"));
    }

    #[test]
    fn format_arity_renders_useful_labels() {
        assert_eq!(format_arity(Arity::Exact(2)), "exactly 2");
        assert_eq!(format_arity(Arity::Range { min: 1, max: Some(3) }), "1..3 args");
        assert_eq!(format_arity(Arity::Range { min: 2, max: None }), "at least 2");
    }
}
