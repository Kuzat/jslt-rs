use ast::Span;
use stdlib::{Arity, Registry};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    Documentation, Hover, HoverContents, HoverParams, MarkupContent, MarkupKind,
    ParameterInformation, ParameterLabel, Position, Range, SignatureHelp, SignatureHelpParams,
    SignatureInformation,
};

use crate::JsltLanguageServer;
use crate::context::{
    AnalysisSnapshot, CallSite, Symbol, SymbolKind, byte_offset_to_position,
    position_to_byte_offset, span_contains,
};

pub(crate) async fn hover(
    server: &JsltLanguageServer,
    params: HoverParams,
) -> Result<Option<Hover>> {
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

    if let Some(symbol) = snapshot.symbol_at(position) {
        let hover_span =
            best_symbol_hover_span(symbol, position, &snapshot.text).unwrap_or(symbol.declaration);
        let (title, body) = match symbol.kind {
            SymbolKind::Function => {
                let signature = user_defined_signature(&snapshot, &symbol.name)
                    .map(|(label, _)| label)
                    .unwrap_or_else(|| format!("def {}(...)", symbol.name));
                (format!("`{}`", signature), "User-defined function.".to_string())
            }
            SymbolKind::Parameter => (
                format!("`$ {}`", symbol.name).replace("$ ", "$"),
                "Function parameter.".to_string(),
            ),
            SymbolKind::LetVariable => (
                format!("`$ {}`", symbol.name).replace("$ ", "$"),
                "Local variable bound via `let`.".to_string(),
            ),
            SymbolKind::ImportAlias => {
                let path = snapshot
                    .imports
                    .iter()
                    .find(|imp| imp.alias == symbol.name)
                    .map(|imp| imp.path.as_str())
                    .unwrap_or("<unknown>");
                (format!("`import \"{}\" as {}`", path, symbol.name), "Import alias.".to_string())
            }
            SymbolKind::ModulePath => {
                (format!("`{}`", symbol.name), "Imported module path.".to_string())
            }
        };

        return Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("{}\n\n{}", title, body),
            }),
            range: Some(span_to_range(&snapshot.text, hover_span)),
        }));
    }

    if let Some((name, ident_span)) = identifier_and_span_at_position(&snapshot.text, position)
        && let Some((label, arity_text)) = builtin_signature_label(&name)
    {
        return Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("`{}`\n\nJSLT stdlib function with {}.", label, arity_text),
            }),
            range: Some(span_to_range(&snapshot.text, ident_span)),
        }));
    }

    Ok(None)
}

pub(crate) async fn signature_help(
    server: &JsltLanguageServer,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>> {
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

    let Some(call) = snapshot.call_at(position) else {
        return Ok(None);
    };

    let active_parameter = active_parameter_index(call, position, &snapshot.text);
    let Some(callee_name) = callee_name(&snapshot, call) else {
        return Ok(None);
    };

    if let Some((label, params)) = user_defined_signature(&snapshot, &callee_name) {
        let parameters: Vec<ParameterInformation> = params
            .iter()
            .map(|param| ParameterInformation {
                label: ParameterLabel::Simple(param.clone()),
                documentation: None,
            })
            .collect();

        return Ok(Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label,
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: "User-defined function.".to_string(),
                })),
                parameters: Some(parameters.clone()),
                active_parameter: Some(
                    active_parameter.min(parameters.len().saturating_sub(1) as u32),
                ),
            }],
            active_signature: Some(0),
            active_parameter: Some(active_parameter),
        }));
    }

    if let Some((label, arity_text, params)) = builtin_signature(&callee_name) {
        let parameters: Vec<ParameterInformation> = params
            .into_iter()
            .map(|param| ParameterInformation {
                label: ParameterLabel::Simple(param),
                documentation: None,
            })
            .collect();

        return Ok(Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label,
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("JSLT stdlib function with {}.", arity_text),
                })),
                parameters: Some(parameters.clone()),
                active_parameter: Some(
                    active_parameter.min(parameters.len().saturating_sub(1) as u32),
                ),
            }],
            active_signature: Some(0),
            active_parameter: Some(active_parameter),
        }));
    }

    Ok(None)
}

fn callee_name(snapshot: &AnalysisSnapshot, call: &CallSite) -> Option<String> {
    let start = call.callee_span.start.min(snapshot.text.len());
    let end = call.callee_span.end.min(snapshot.text.len());
    if start >= end {
        return None;
    }

    let raw = snapshot.text.get(start..end)?.trim();
    if raw.is_empty() {
        return None;
    }
    if raw.chars().all(is_identifier_char) { Some(raw.to_string()) } else { None }
}

fn active_parameter_index(call: &CallSite, position: Position, text: &str) -> u32 {
    let Some(offset) = position_to_byte_offset(text, position) else {
        return 0;
    };

    if call.arg_spans.is_empty() {
        return 0;
    }

    for (i, arg) in call.arg_spans.iter().enumerate() {
        if offset < arg.start {
            return i as u32;
        }
        if span_contains(*arg, offset) {
            return i as u32;
        }
    }

    (call.arg_spans.len().saturating_sub(1)) as u32
}

fn user_defined_signature(
    snapshot: &AnalysisSnapshot,
    name: &str,
) -> Option<(String, Vec<String>)> {
    let program = snapshot.program.as_ref()?;
    let def = program.defs.iter().find(|d| d.name.name == name)?;
    let params: Vec<String> = def.params.iter().map(|p| p.name.clone()).collect();
    Some((format!("def {}({})", def.name.name, params.join(", ")), params))
}

fn builtin_signature_label(name: &str) -> Option<(String, String)> {
    let registry = Registry::with_default();
    let id = registry.get_id(name)?;
    let func = registry.get_by_id(id)?;
    let arity_text = format_arity(func.arity());
    Some((format!("{}({})", name, arity_text), arity_text))
}

fn builtin_signature(name: &str) -> Option<(String, String, Vec<String>)> {
    let registry = Registry::with_default();
    let id = registry.get_id(name)?;
    let func = registry.get_by_id(id)?;
    let arity = func.arity();
    let arity_text = format_arity(arity);
    let params = arity_parameter_names(arity);
    Some((format!("{}({})", name, arity_text), arity_text, params))
}

fn arity_parameter_names(arity: Arity) -> Vec<String> {
    match arity {
        Arity::Exact(n) => (1..=n).map(|i| format!("arg{}", i)).collect(),
        Arity::Range { min, max } => {
            let upper = max.unwrap_or(min.saturating_add(2)).min(min.saturating_add(4));
            let mut out: Vec<String> = (1..=upper).map(|i| format!("arg{}", i)).collect();
            if max.is_none_or(|m| m > upper) {
                out.push("...".to_string());
            }
            out
        }
    }
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

fn identifier_and_span_at_position(text: &str, position: Position) -> Option<(String, Span)> {
    let offset = position_to_byte_offset(text, position)?;
    let mut start = offset.min(text.len());
    while start > 0 {
        let ch = text[..start].chars().next_back()?;
        if is_identifier_char(ch) {
            start -= ch.len_utf8();
        } else {
            break;
        }
    }

    let mut end = offset.min(text.len());
    while end < text.len() {
        let ch = text[end..].chars().next()?;
        if is_identifier_char(ch) {
            end += ch.len_utf8();
        } else {
            break;
        }
    }

    if start >= end {
        return None;
    }
    Some((text[start..end].to_string(), Span { start, end, line: 1, column: 1 }))
}

fn is_identifier_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_' || ch == ':' || ch == '-'
}

fn symbol_hover_span(symbol: &Symbol, position: Position, text: &str) -> Option<Span> {
    let offset = position_to_byte_offset(text, position)?;

    if span_contains(symbol.declaration, offset) {
        return Some(symbol.declaration);
    }
    symbol.references.iter().find(|span| span_contains(**span, offset)).copied()
}

fn best_symbol_hover_span(symbol: &Symbol, position: Position, text: &str) -> Option<Span> {
    if let Some((ident, ident_span)) = identifier_and_span_at_position(text, position)
        && ident == symbol.name
    {
        return Some(ident_span);
    }
    symbol_hover_span(symbol, position, text)
}

fn span_to_range(text: &str, span: Span) -> Range {
    Range {
        start: byte_offset_to_position(text, span.start),
        end: byte_offset_to_position(text, span.end),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identifier_extraction_supports_dash_names() {
        let text = "format-time(1)";
        let ident = identifier_and_span_at_position(text, Position::new(0, 4));
        assert_eq!(ident.as_ref().map(|(s, _)| s.as_str()), Some("format-time"));
        assert_eq!(ident.as_ref().map(|(_, span)| (span.start, span.end)), Some((0, 11)));
    }

    #[test]
    fn active_parameter_index_tracks_argument_spans() {
        let text = "foo(1, 2, 3)";
        let call = CallSite {
            span: Span { start: 0, end: text.len(), line: 1, column: 1 },
            callee_span: Span { start: 0, end: 3, line: 1, column: 1 },
            arg_spans: vec![
                Span { start: 4, end: 5, line: 1, column: 1 },
                Span { start: 7, end: 8, line: 1, column: 1 },
                Span { start: 10, end: 11, line: 1, column: 1 },
            ],
            scope: crate::context::ScopeId(0),
        };

        assert_eq!(active_parameter_index(&call, Position::new(0, 4), text), 0);
        assert_eq!(active_parameter_index(&call, Position::new(0, 7), text), 1);
        assert_eq!(active_parameter_index(&call, Position::new(0, 10), text), 2);
    }

    #[test]
    fn arity_parameter_names_are_bounded_for_large_ranges() {
        let params = arity_parameter_names(Arity::Range { min: 1, max: Some(1024) });
        assert!(params.len() <= 6);
    }

    #[test]
    fn symbol_hover_span_prefers_reference_span_under_cursor() {
        let symbol = Symbol {
            id: crate::context::SymbolId(1),
            kind: SymbolKind::Function,
            name: "clean-keys".to_string(),
            declaration: Span { start: 100, end: 110, line: 1, column: 1 },
            references: vec![Span { start: 0, end: 10, line: 1, column: 1 }],
            scope: crate::context::ScopeId(0),
        };
        let text = "clean-keys(1)";
        let span = symbol_hover_span(&symbol, Position::new(0, 6), text);
        assert_eq!(span.map(|s| (s.start, s.end)), Some((0, 10)));
    }

    #[test]
    fn best_symbol_hover_span_prefers_identifier_span_match() {
        let symbol = Symbol {
            id: crate::context::SymbolId(1),
            kind: SymbolKind::Function,
            name: "clean-keys".to_string(),
            declaration: Span { start: 100, end: 110, line: 1, column: 1 },
            references: vec![Span { start: 0, end: 5, line: 1, column: 1 }],
            scope: crate::context::ScopeId(0),
        };
        let text = "clean-keys(1)";
        let span = best_symbol_hover_span(&symbol, Position::new(0, 7), text);
        assert_eq!(span.map(|s| (s.start, s.end)), Some((0, 10)));
    }
}
