use crate::capabilities::build_server_capabilities;
use std::path::PathBuf;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    InitializeParams, InitializeResult, InitializedParams, MessageType, ServerInfo,
};

use crate::JsltLanguageServer;

pub(crate) async fn initialize(
    server: &JsltLanguageServer,
    params: InitializeParams,
) -> Result<InitializeResult> {
    server.set_workspace_roots(workspace_roots(&params)).await;

    Ok(InitializeResult {
        capabilities: build_server_capabilities(),
        server_info: Some(ServerInfo {
            name: "JSLT Language Server".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    })
}

pub(crate) async fn initialized(server: &JsltLanguageServer, _: InitializedParams) {
    let indexed = server.discover_workspace().await;
    tracing::info!(indexed, "workspace index built");

    server
        .client
        .log_message(
            MessageType::INFO,
            format!("JSLT Language Server initialized ({} files indexed)", indexed),
        )
        .await;
}

pub(crate) fn shutdown() -> Result<()> {
    Ok(())
}

/// Workspace roots from `workspaceFolders`, falling back to the deprecated
/// `rootUri` for older clients.
fn workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots: Vec<PathBuf> = params
        .workspace_folders
        .iter()
        .flatten()
        .filter_map(|folder| folder.uri.to_file_path().ok())
        .collect();

    if roots.is_empty()
        && let Some(root) = params.root_uri.as_ref().and_then(|uri| uri.to_file_path().ok())
    {
        roots.push(root);
    }

    roots.sort();
    roots.dedup();
    roots
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::{Url, WorkspaceFolder};

    #[test]
    fn workspace_folders_take_precedence_over_root_uri() {
        let params = InitializeParams {
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::parse("file:///projects/a").expect("uri"),
                name: "a".to_string(),
            }]),
            root_uri: Some(Url::parse("file:///projects/b").expect("uri")),
            ..Default::default()
        };

        assert_eq!(workspace_roots(&params), vec![PathBuf::from("/projects/a")]);
    }

    #[test]
    fn root_uri_is_used_when_no_folders_are_reported() {
        let params = InitializeParams {
            root_uri: Some(Url::parse("file:///projects/b").expect("uri")),
            ..Default::default()
        };

        assert_eq!(workspace_roots(&params), vec![PathBuf::from("/projects/b")]);
    }
}
