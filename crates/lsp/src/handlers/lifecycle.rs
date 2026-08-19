use crate::capabilities::build_server_capabilities;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{InitializeResult, InitializedParams, MessageType, ServerInfo};

use crate::JsltLanguageServer;

pub(crate) fn initialize() -> Result<InitializeResult> {
    Ok(InitializeResult {
        capabilities: build_server_capabilities(),
        server_info: Some(ServerInfo {
            name: "JSLT Language Server".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    })
}

pub(crate) async fn initialized(server: &JsltLanguageServer, _: InitializedParams) {
    server.client.log_message(MessageType::INFO, "JSLT Language Server Initialized").await;
}

pub(crate) fn shutdown() -> Result<()> {
    Ok(())
}
