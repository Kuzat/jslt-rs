use tower_lsp::lsp_types::*;

use crate::JsltLanguageServer;

pub(crate) async fn did_open(server: &JsltLanguageServer, params: DidOpenTextDocumentParams) {
    let uri = params.text_document.uri;
    let version = Some(params.text_document.version);
    let text = params.text_document.text;

    server.upsert_document(&uri, text.clone()).await;
    let snapshot = server.analyze_document(&uri, &text, version).await;

    let context = server.request_context(&uri, version).await;
    if let (Some(document), Some(snapshot)) = (context.document.as_ref(), context.snapshot.as_ref())
    {
        tracing::trace!(
            uri = %document.uri,
            version = ?document.version,
            diagnostics = snapshot.diagnostics.len(),
            symbols = snapshot.symbols.len(),
            scopes = snapshot.scopes.len(),
            calls = snapshot.call_sites.len(),
            open_documents = context.workspace.open_document_count,
            "processed did_open",
        );
    }

    server
        .client
        .publish_diagnostics(
            uri.clone(),
            snapshot.diagnostics.clone(),
            context.document.as_ref().and_then(|d| d.version),
        )
        .await;
}

pub(crate) async fn did_change(server: &JsltLanguageServer, params: DidChangeTextDocumentParams) {
    let uri = params.text_document.uri;
    let version = Some(params.text_document.version);

    if let Some(change) = params.content_changes.first() {
        let text = change.text.clone();
        server.upsert_document(&uri, text.clone()).await;
        let snapshot = server.analyze_document(&uri, &text, version).await;

        let context = server.request_context(&uri, version).await;
        if let (Some(document), Some(snapshot)) =
            (context.document.as_ref(), context.snapshot.as_ref())
        {
            tracing::trace!(
                uri = %document.uri,
                version = ?document.version,
                diagnostics = snapshot.diagnostics.len(),
                symbols = snapshot.symbols.len(),
                scopes = snapshot.scopes.len(),
                calls = snapshot.call_sites.len(),
                open_documents = context.workspace.open_document_count,
                "processed did_change",
            );
        }

        server
            .client
            .publish_diagnostics(
                uri,
                snapshot.diagnostics.clone(),
                context.document.as_ref().and_then(|d| d.version),
            )
            .await;
    }
}

pub(crate) async fn did_close(server: &JsltLanguageServer, params: DidCloseTextDocumentParams) {
    let uri = params.text_document.uri;
    server.remove_document(&uri).await;
    server.client.publish_diagnostics(uri, Vec::new(), None).await;
}
