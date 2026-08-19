use crate::context::{AnalysisSnapshot, DocumentContext, RequestContext, WorkspaceView};
use crate::errors::HandlerError;
use crate::workspace::{IndexedFile, WorkspaceIndex};
use std::collections::HashMap;
use std::path::PathBuf;
use tower_lsp::Client;
use tower_lsp::lsp_types::{MessageType, Url};

#[derive(Debug, Clone)]
struct CachedSnapshot {
    version: Option<i32>,
    snapshot: AnalysisSnapshot,
}

/// The backend state for our language server.
///
/// This holds the client connection and tracks all open documents.
pub struct JsltLanguageServer {
    /// Client handle for sending notifications/requests back to the editor.
    pub(crate) client: Client,

    /// Cache of document contents by URI.
    pub(crate) document_map: tokio::sync::RwLock<HashMap<String, String>>,

    /// Cache of analysis snapshots by URI.
    snapshot_map: tokio::sync::RwLock<HashMap<String, CachedSnapshot>>,

    /// Workspace-wide file index and module graph.
    workspace_index: tokio::sync::RwLock<WorkspaceIndex>,
}

impl JsltLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            document_map: tokio::sync::RwLock::new(HashMap::new()),
            snapshot_map: tokio::sync::RwLock::new(HashMap::new()),
            workspace_index: tokio::sync::RwLock::new(WorkspaceIndex::default()),
        }
    }

    pub(crate) async fn upsert_document(&self, uri: &Url, text: String) {
        self.document_map.write().await.insert(uri.to_string(), text);
        self.snapshot_map.write().await.remove(uri.as_str());
    }

    pub(crate) async fn remove_document(&self, uri: &Url) {
        self.document_map.write().await.remove(uri.as_str());
        self.snapshot_map.write().await.remove(uri.as_str());
    }

    pub(crate) async fn document_text(&self, uri: &Url) -> Option<String> {
        self.document_map.read().await.get(uri.as_str()).cloned()
    }

    pub(crate) async fn cache_snapshot(
        &self,
        uri: &Url,
        version: Option<i32>,
        snapshot: AnalysisSnapshot,
    ) {
        self.snapshot_map
            .write()
            .await
            .insert(uri.to_string(), CachedSnapshot { version, snapshot });
    }

    pub(crate) async fn cached_snapshot(
        &self,
        uri: &Url,
        version: Option<i32>,
    ) -> Option<AnalysisSnapshot> {
        let entry = self.snapshot_map.read().await.get(uri.as_str()).cloned()?;
        match version {
            Some(expected) if entry.version == Some(expected) => Some(entry.snapshot),
            Some(_) => None,
            None => Some(entry.snapshot),
        }
    }

    pub(crate) async fn request_context(&self, uri: &Url, version: Option<i32>) -> RequestContext {
        let document = self
            .document_text(uri)
            .await
            .map(|text| DocumentContext::new(uri.clone(), version, text));
        let snapshot = self.cached_snapshot(uri, version).await;
        let open_document_count = self.document_map.read().await.len();
        let indexed_file_count = self.workspace_index.read().await.len();

        RequestContext {
            document,
            snapshot,
            workspace: WorkspaceView { open_document_count, indexed_file_count },
        }
    }

    /// Record the workspace roots reported by the client during `initialize`.
    pub(crate) async fn set_workspace_roots(&self, roots: Vec<PathBuf>) {
        self.workspace_index.write().await.set_roots(roots);
    }

    /// Index every JSLT file under the configured workspace roots.
    pub(crate) async fn discover_workspace(&self) -> usize {
        self.workspace_index.write().await.discover()
    }

    /// Refresh this file's entry in the workspace index and module graph.
    pub(crate) async fn index_snapshot(&self, uri: &Url, snapshot: AnalysisSnapshot) {
        self.workspace_index.write().await.index_snapshot(uri, snapshot);
    }

    /// Drop unsaved buffer content for a closed document from the index.
    pub(crate) async fn refresh_index_from_disk(&self, uri: &Url) {
        self.workspace_index.write().await.refresh_from_disk(uri);
    }

    /// Look up an indexed file, reading and analyzing it from disk if needed.
    pub(crate) async fn indexed_file(&self, uri: &Url) -> Option<IndexedFile> {
        if let Some(file) = self.workspace_index.read().await.get(uri) {
            return Some(file.clone());
        }
        self.workspace_index.write().await.ensure_indexed(uri).cloned()
    }

    /// Files importing `uri`, paired with the alias each one uses for it.
    pub(crate) async fn dependents_of(&self, uri: &Url) -> Vec<(Url, String)> {
        self.workspace_index.read().await.dependents_of(uri)
    }

    pub(crate) async fn report_non_fatal(&self, err: HandlerError) {
        self.client.log_message(MessageType::WARNING, err.to_string()).await;
    }
}
