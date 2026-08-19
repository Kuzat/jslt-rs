//! Helpers for picking apart the source text behind a symbol span.
//!
//! Import aliases need finer-grained ranges than the symbol table stores: a
//! reference to an imported function is indexed as a single `alias:function`
//! span, and an alias declaration is indexed as the whole `import` statement.
//! Navigation and rename need the individual identifier ranges.

use ast::Span;

/// A reference of the form `alias:function`, split into its two identifiers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct QualifiedRef {
    pub(crate) alias: String,
    pub(crate) function: String,
    pub(crate) alias_span: Span,
    pub(crate) function_span: Span,
}

/// Split a reference span into alias and function parts.
///
/// Aliases may themselves contain `:` (`import "x.jslt" as a:b`), so the
/// longest matching known alias wins rather than the first `:`.
pub(crate) fn split_qualified(text: &str, span: Span, aliases: &[String]) -> Option<QualifiedRef> {
    let slice = text.get(span.start..span.end)?;

    let alias = aliases
        .iter()
        .filter(|alias| {
            !alias.is_empty()
                && slice.len() > alias.len()
                && slice.starts_with(alias.as_str())
                && slice.as_bytes()[alias.len()] == b':'
        })
        .max_by_key(|alias| alias.len())?;

    let function = &slice[alias.len() + 1..];
    if function.is_empty() {
        return None;
    }

    Some(QualifiedRef {
        alias: alias.clone(),
        function: function.to_string(),
        alias_span: sub_span(span, 0, alias.len()),
        function_span: sub_span(span, alias.len() + 1, slice.len()),
    })
}

/// The range of the alias identifier inside an `import ... as alias` statement.
///
/// The symbol table records the whole statement as the declaration, which is
/// too coarse to rename or highlight.
pub(crate) fn alias_declaration_span(text: &str, import_span: Span, alias: &str) -> Option<Span> {
    let slice = text.get(import_span.start..import_span.end)?;
    // The alias is the last thing in the statement, so search from the end to
    // avoid matching an identical substring inside the module path.
    let offset = slice.rfind(alias)?;
    Some(sub_span(import_span, offset, offset + alias.len()))
}

/// A sub-range of `span`, offset by byte counts relative to its start.
fn sub_span(span: Span, start: usize, end: usize) -> Span {
    Span { start: span.start + start, end: span.start + end, line: span.line, column: span.column }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span(start: usize, end: usize) -> Span {
        Span { start, end, line: 1, column: 1 }
    }

    #[test]
    fn splits_a_qualified_reference() {
        let text = "utils:double(.n)";
        let aliases = vec!["utils".to_string()];

        let qualified = split_qualified(text, span(0, 12), &aliases).expect("qualified");

        assert_eq!(qualified.alias, "utils");
        assert_eq!(qualified.function, "double");
        assert_eq!(qualified.alias_span, span(0, 5));
        assert_eq!(qualified.function_span, span(6, 12));
    }

    #[test]
    fn prefers_the_longest_matching_alias() {
        let text = "a:b:fn(.x)";
        let aliases = vec!["a".to_string(), "a:b".to_string()];

        let qualified = split_qualified(text, span(0, 6), &aliases).expect("qualified");

        assert_eq!(qualified.alias, "a:b");
        assert_eq!(qualified.function, "fn");
    }

    #[test]
    fn bare_alias_reference_is_not_qualified() {
        let aliases = vec!["utils".to_string()];

        assert!(split_qualified("utils", span(0, 5), &aliases).is_none());
        assert!(split_qualified("utils:", span(0, 6), &aliases).is_none());
    }

    #[test]
    fn finds_the_alias_identifier_in_an_import_statement() {
        let text = "import \"utils.jslt\" as utils\n";

        let alias_span = alias_declaration_span(text, span(0, 28), "utils").expect("alias span");

        assert_eq!(&text[alias_span.start..alias_span.end], "utils");
        assert_eq!(alias_span.start, 23);
    }
}
