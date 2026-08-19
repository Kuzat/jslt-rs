use tower_lsp::lsp_types::{
    OneOf, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};

pub(crate) fn build_server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        // We support full text document synchronization.
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),

        // Code formatting support.
        document_formatting_provider: Some(OneOf::Left(true)),

        // Add more capabilities here as we implement features.
        ..Default::default()
    }
}
