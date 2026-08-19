mod completion;
mod documents;
mod formatting;
mod lifecycle;

use tower_lsp::LanguageServer;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use crate::JsltLanguageServer;

#[tower_lsp::async_trait]
impl LanguageServer for JsltLanguageServer {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        lifecycle::initialize()
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
        formatting::formatting(self, params).await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        completion::completion(self, params).await
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        completion::completion_resolve(self, params).await
    }
}
