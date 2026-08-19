use tower_lsp::lsp_types::{
    CompletionOptions, HoverProviderCapability, OneOf, ServerCapabilities, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind,
};

pub(crate) fn build_server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        // We support full text document synchronization.
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),

        // Code formatting support.
        document_formatting_provider: Some(OneOf::Left(true)),

        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(vec![
                ".".to_string(),
                "$".to_string(),
                "\"".to_string(),
                ":".to_string(),
            ]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: Some(vec![",".to_string()]),
            ..Default::default()
        }),

        // Add more capabilities here as we implement features.
        ..Default::default()
    }
}
