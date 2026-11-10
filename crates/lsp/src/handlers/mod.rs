mod completion;
mod definition;
mod documents;
mod formatting;
mod hover;
mod lifecycle;
mod references;
mod rename;

use std::future::Future;
use std::time::Instant;

use tower_lsp::LanguageServer;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use crate::JsltLanguageServer;

/// Run a request handler, recording how long it took.
///
/// A sluggish editor is the usual symptom of a slow handler, and the server has
/// no other way to surface where the time went. Enable with `RUST_LOG=debug`.
async fn timed<T>(method: &'static str, work: impl Future<Output = T>) -> T {
    let started = Instant::now();
    let result = work.await;
    tracing::debug!(method, elapsed_us = started.elapsed().as_micros() as u64, "request handled");
    result
}

#[tower_lsp::async_trait]
impl LanguageServer for JsltLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        lifecycle::initialize(self, params).await
    }

    async fn initialized(&self, params: InitializedParams) {
        lifecycle::initialized(self, params).await;
    }

    async fn shutdown(&self) -> Result<()> {
        lifecycle::shutdown()
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        documents::did_open(self, params).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        documents::did_change(self, params).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        documents::did_close(self, params).await;
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        timed("textDocument/formatting", formatting::formatting(self, params)).await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        timed("textDocument/completion", completion::completion(self, params)).await
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        timed("completionItem/resolve", completion::completion_resolve(self, params)).await
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        timed("textDocument/hover", hover::hover(self, params)).await
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        timed("textDocument/signatureHelp", hover::signature_help(self, params)).await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        timed("textDocument/definition", definition::definition(self, params)).await
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        timed("textDocument/references", references::references(self, params)).await
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        timed("textDocument/prepareRename", rename::prepare_rename(self, params)).await
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        timed("textDocument/rename", rename::rename(self, params)).await
    }
}
