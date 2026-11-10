//! A language server driven through real JSON-RPC requests.
//!
//! Shared by the protocol and performance test binaries; each one only uses
//! part of the surface, hence the blanket dead-code allowance.
#![allow(dead_code)]

use std::path::{Path, PathBuf};

use futures::StreamExt;
use jslt_lsp::JsltLanguageServer;
use serde_json::{Value, json};
use tempfile::TempDir;
use tower::{Service, ServiceExt};
use tower_lsp::LspService;
use tower_lsp::jsonrpc::Request;
use tower_lsp::lsp_types::Url;

/// A running language server plus the temporary workspace it was pointed at.
pub struct TestServer {
    service: LspService<JsltLanguageServer>,
    dir: TempDir,
    next_id: i64,
}

impl TestServer {
    /// A spawned server that has not been sent `initialize` yet.
    pub fn uninitialized() -> Self {
        Self::spawn()
    }

    /// Start a server with no workspace root, so nothing is indexed from disk.
    pub async fn bare() -> Self {
        let mut server = Self::spawn();
        server.request("initialize", json!({ "capabilities": {} })).await;
        server.notify("initialized", json!({})).await;
        server
    }

    /// Start a server rooted at a temp directory seeded with `files`.
    pub async fn with_files(files: &[(&str, &str)]) -> Self {
        let mut server = Self::spawn();
        for (name, text) in files {
            server.write_file(name, text);
        }

        let root = Url::from_directory_path(server.dir.path()).expect("root uri");
        server
            .request(
                "initialize",
                json!({
                    "capabilities": {},
                    "workspaceFolders": [{ "uri": root, "name": "fixture" }],
                }),
            )
            .await;
        server.notify("initialized", json!({})).await;

        server
    }

    fn spawn() -> Self {
        let dir = tempfile::tempdir().expect("temp dir");
        let (service, socket) = LspService::new(JsltLanguageServer::new);
        // Nothing reads the client half in tests, and the loopback channel is
        // bounded, so drain it or notifications would block the handlers.
        tokio::spawn(async move {
            let mut socket = socket;
            while socket.next().await.is_some() {}
        });

        TestServer { service, dir, next_id: 1 }
    }

    pub fn write_file(&self, name: &str, text: &str) {
        let path = self.path(name);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).expect("create parent dir");
        }
        std::fs::write(path, text).expect("write fixture");
    }

    pub fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(name)
    }

    pub fn uri(&self, name: &str) -> Url {
        file_uri(&self.path(name))
    }

    pub async fn request(&mut self, method: &'static str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;

        let request = Request::build(method).params(params).id(id).finish();
        let response = self
            .service
            .ready()
            .await
            .expect("service ready")
            .call(request)
            .await
            .expect("call succeeded")
            .expect("response");

        let value = serde_json::to_value(response).expect("serializable response");
        assert!(value.get("error").is_none(), "{} failed: {}", method, value);
        value.get("result").cloned().unwrap_or(Value::Null)
    }

    pub async fn notify(&mut self, method: &'static str, params: Value) {
        let request = Request::build(method).params(params).finish();
        let _ = self.service.ready().await.expect("service ready").call(request).await;
    }

    pub async fn open(&mut self, name: &str, text: &str) {
        let uri = self.uri(name);
        self.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": uri,
                    "languageId": "jslt",
                    "version": 1,
                    "text": text,
                }
            }),
        )
        .await;
    }

    pub async fn position_request(
        &mut self,
        method: &'static str,
        name: &str,
        line: u32,
        ch: u32,
    ) -> Value {
        let uri = self.uri(name);
        let mut params = json!({
            "textDocument": { "uri": uri },
            "position": { "line": line, "character": ch },
        });

        if method == "textDocument/references" {
            params["context"] = json!({ "includeDeclaration": true });
        }

        self.request(method, params).await
    }
}

/// The canonical `file:` URI for `path`, matching what the server indexes.
pub fn file_uri(path: &Path) -> Url {
    Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri")
}
