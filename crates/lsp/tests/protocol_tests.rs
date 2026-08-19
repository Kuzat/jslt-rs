//! Protocol-level tests that drive the server through JSON-RPC requests over a
//! real multi-file workspace on disk.

use std::path::{Path, PathBuf};

use futures::StreamExt;
use jslt_lsp::JsltLanguageServer;
use serde_json::{Value, json};
use tempfile::TempDir;
use tower::{Service, ServiceExt};
use tower_lsp::LspService;
use tower_lsp::jsonrpc::Request;

const UTILS_JSLT: &str =
    "def double(x)\n  $x * 2\n\ndef triple(x)\n  $x * 3\n\n{\"doubled\": double(.value)}\n";

const MAIN_JSLT: &str = "import \"utils.jslt\" as utils\n\nlet n = .value\n\n{\n  \"doubled\": utils:double($n),\n  \"tripled\": utils:triple($n)\n}\n";

/// A running language server plus the temporary workspace it was pointed at.
struct TestServer {
    service: LspService<JsltLanguageServer>,
    dir: TempDir,
    next_id: i64,
}

impl TestServer {
    /// Start a server over a workspace containing `utils.jslt` and `main.jslt`.
    async fn start() -> Self {
        let dir = tempfile::tempdir().expect("temp dir");
        std::fs::write(dir.path().join("utils.jslt"), UTILS_JSLT).expect("write utils");
        std::fs::write(dir.path().join("main.jslt"), MAIN_JSLT).expect("write main");

        let (service, socket) = LspService::new(JsltLanguageServer::new);
        // Nothing reads the client half in tests, and the loopback channel is
        // bounded, so drain it or notifications would block the handlers.
        tokio::spawn(async move {
            let mut socket = socket;
            while socket.next().await.is_some() {}
        });

        let mut server = TestServer { service, dir, next_id: 1 };

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

    fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(name)
    }

    fn uri(&self, name: &str) -> Url {
        file_uri(&self.path(name))
    }

    async fn request(&mut self, method: &'static str, params: Value) -> Value {
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

    async fn notify(&mut self, method: &'static str, params: Value) {
        let request = Request::build(method).params(params).finish();
        let _ = self.service.ready().await.expect("service ready").call(request).await;
    }

    async fn open(&mut self, name: &str, text: &str) {
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

    async fn position_request(
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

use tower_lsp::lsp_types::Url;

fn file_uri(path: &Path) -> Url {
    Url::from_file_path(path.canonicalize().expect("canonical path")).expect("file uri")
}

#[tokio::test]
async fn initialize_advertises_the_implemented_capabilities() {
    let dir = tempfile::tempdir().expect("temp dir");
    let (service, socket) = LspService::new(JsltLanguageServer::new);
    tokio::spawn(async move {
        let mut socket = socket;
        while socket.next().await.is_some() {}
    });
    let mut server = TestServer { service, dir, next_id: 1 };
    let capabilities = server.request("initialize", json!({ "capabilities": {} })).await;
    let capabilities = &capabilities["capabilities"];

    assert_eq!(capabilities["hoverProvider"], json!(true));
    assert_eq!(capabilities["definitionProvider"], json!(true));
    assert_eq!(capabilities["referencesProvider"], json!(true));
    assert_eq!(capabilities["documentFormattingProvider"], json!(true));
    assert_eq!(capabilities["renameProvider"]["prepareProvider"], json!(true));
    assert_eq!(capabilities["completionProvider"]["resolveProvider"], json!(true));
    assert!(
        capabilities["signatureHelpProvider"]["triggerCharacters"]
            .as_array()
            .expect("trigger characters")
            .contains(&json!("("))
    );
}

#[tokio::test]
async fn completion_offers_local_symbols_imports_and_builtins() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    let result = server.position_request("textDocument/completion", "main.jslt", 5, 13).await;
    let labels: Vec<&str> =
        result.as_array().expect("items").iter().filter_map(|i| i["label"].as_str()).collect();

    assert!(labels.contains(&"utils"), "missing import alias: {:?}", labels);
    assert!(labels.contains(&"n"), "missing let variable: {:?}", labels);
    assert!(labels.contains(&"size"), "missing stdlib builtin: {:?}", labels);
}

#[tokio::test]
async fn hover_describes_an_import_alias() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    let result = server.position_request("textDocument/hover", "main.jslt", 0, 25).await;
    let value = result["contents"]["value"].as_str().expect("hover markdown");

    assert!(value.contains("utils.jslt"), "unexpected hover: {}", value);
}

#[tokio::test]
async fn signature_help_reports_the_active_parameter() {
    let mut server = TestServer::start().await;
    server.open("utils.jslt", UTILS_JSLT).await;

    // Inside `double(.value)` on the final line.
    let result = server.position_request("textDocument/signatureHelp", "utils.jslt", 6, 20).await;

    assert_eq!(result["activeParameter"], json!(0));
    let label = result["signatures"][0]["label"].as_str().expect("signature label");
    assert!(label.contains("double"), "unexpected signature: {}", label);
}

#[tokio::test]
async fn definition_jumps_into_the_imported_module() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    // Cursor on the `double` half of `utils:double`.
    let result = server.position_request("textDocument/definition", "main.jslt", 5, 21).await;

    assert_eq!(result["uri"], json!(server.uri("utils.jslt")));
    assert_eq!(result["range"]["start"], json!({ "line": 0, "character": 4 }));
    assert_eq!(result["range"]["end"], json!({ "line": 0, "character": 10 }));
}

#[tokio::test]
async fn definition_on_the_alias_half_opens_the_module_file() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    // Cursor on the `utils` half of `utils:double`.
    let result = server.position_request("textDocument/definition", "main.jslt", 5, 15).await;

    assert_eq!(result["uri"], json!(server.uri("utils.jslt")));
    assert_eq!(result["range"]["start"], json!({ "line": 0, "character": 0 }));
}

#[tokio::test]
async fn references_span_the_declaring_and_importing_files() {
    let mut server = TestServer::start().await;
    server.open("utils.jslt", UTILS_JSLT).await;

    // Cursor on `double` in `def double(x)`.
    let result = server.position_request("textDocument/references", "utils.jslt", 0, 5).await;
    let locations = result.as_array().expect("locations");

    let utils_uri = json!(server.uri("utils.jslt"));
    let main_uri = json!(server.uri("main.jslt"));

    let in_utils = locations.iter().filter(|loc| loc["uri"] == utils_uri).count();
    let in_main: Vec<&Value> = locations.iter().filter(|loc| loc["uri"] == main_uri).collect();

    // Declaration plus the local call in the module body.
    assert_eq!(in_utils, 2, "unexpected utils locations: {:?}", locations);
    assert_eq!(in_main.len(), 1, "unexpected main locations: {:?}", locations);
    // The whole `utils:double` token is reported at the call site.
    assert_eq!(in_main[0]["range"]["start"], json!({ "line": 5, "character": 13 }));
    assert_eq!(in_main[0]["range"]["end"], json!({ "line": 5, "character": 25 }));
}

#[tokio::test]
async fn prepare_rename_selects_just_the_alias_identifier() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    let result = server.position_request("textDocument/prepareRename", "main.jslt", 0, 25).await;

    assert_eq!(result["range"]["start"], json!({ "line": 0, "character": 23 }));
    assert_eq!(result["range"]["end"], json!({ "line": 0, "character": 28 }));
    assert_eq!(result["placeholder"], json!("utils"));
}

#[tokio::test]
async fn renaming_an_alias_leaves_the_path_and_function_names_alone() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    let uri = server.uri("main.jslt");
    let result = server
        .request(
            "textDocument/rename",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": 25 },
                "newName": "helpers",
            }),
        )
        .await;

    let changes = result["changes"].as_object().expect("changes");
    assert_eq!(changes.len(), 1, "alias rename must stay in one file: {:?}", changes);

    let edits = changes[uri.as_str()].as_array().expect("edits");
    assert_eq!(edits.len(), 3);
    // Only the alias identifier in the import statement.
    assert_eq!(
        edits[0]["range"],
        json!({
            "start": { "line": 0, "character": 23 },
            "end": { "line": 0, "character": 28 }
        })
    );
    // Only the alias half of each `utils:function` reference.
    assert_eq!(
        edits[1]["range"],
        json!({
            "start": { "line": 5, "character": 13 },
            "end": { "line": 5, "character": 18 }
        })
    );
    assert_eq!(
        edits[2]["range"],
        json!({
            "start": { "line": 6, "character": 13 },
            "end": { "line": 6, "character": 18 }
        })
    );
    assert!(edits.iter().all(|edit| edit["newText"] == json!("helpers")));
}

#[tokio::test]
async fn renaming_an_exported_def_updates_importing_files() {
    let mut server = TestServer::start().await;
    server.open("utils.jslt", UTILS_JSLT).await;

    let utils_uri = server.uri("utils.jslt");
    let main_uri = server.uri("main.jslt");
    let result = server
        .request(
            "textDocument/rename",
            json!({
                "textDocument": { "uri": utils_uri },
                "position": { "line": 0, "character": 5 },
                "newName": "twice",
            }),
        )
        .await;

    let changes = result["changes"].as_object().expect("changes");
    assert_eq!(changes.len(), 2, "expected edits in both files: {:?}", changes);

    let utils_edits = changes[utils_uri.as_str()].as_array().expect("utils edits");
    assert_eq!(utils_edits.len(), 2, "declaration plus local call");

    let main_edits = changes[main_uri.as_str()].as_array().expect("main edits");
    assert_eq!(main_edits.len(), 1);
    // Only the function half of `utils:double`, never the alias.
    assert_eq!(
        main_edits[0]["range"],
        json!({
            "start": { "line": 5, "character": 19 },
            "end": { "line": 5, "character": 25 }
        })
    );
    assert_eq!(main_edits[0]["newText"], json!("twice"));
}

#[tokio::test]
async fn renaming_from_a_call_site_renames_the_imported_definition() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    let main_uri = server.uri("main.jslt");
    let utils_uri = server.uri("utils.jslt");
    let result = server
        .request(
            "textDocument/rename",
            json!({
                "textDocument": { "uri": main_uri },
                "position": { "line": 5, "character": 21 },
                "newName": "twice",
            }),
        )
        .await;

    let changes = result["changes"].as_object().expect("changes");
    assert!(changes.contains_key(utils_uri.as_str()), "module file must be edited: {:?}", changes);
    assert!(changes.contains_key(main_uri.as_str()), "call site must be edited: {:?}", changes);
}

#[tokio::test]
async fn rename_rejects_invalid_identifiers() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", MAIN_JSLT).await;

    let uri = server.uri("main.jslt");
    let result = server
        .request(
            "textDocument/rename",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": 25 },
                "newName": "1nope",
            }),
        )
        .await;

    assert_eq!(result, Value::Null);
}

#[tokio::test]
async fn formatting_returns_edits_for_unformatted_source() {
    let mut server = TestServer::start().await;
    server.open("main.jslt", "{\"a\"   :    1}\n").await;

    let uri = server.uri("main.jslt");
    let result = server
        .request(
            "textDocument/formatting",
            json!({
                "textDocument": { "uri": uri },
                "options": { "tabSize": 2, "insertSpaces": true },
            }),
        )
        .await;

    let edits = result.as_array().expect("edits");
    assert_eq!(edits.len(), 1);
    assert!(edits[0]["newText"].as_str().expect("new text").contains("\"a\": 1"));
}

#[tokio::test]
async fn handlers_stay_quiet_on_unopened_and_malformed_documents() {
    let mut server = TestServer::start().await;

    // Never opened.
    let result = server.position_request("textDocument/hover", "main.jslt", 0, 0).await;
    assert_eq!(result, Value::Null);

    // Opened, but syntactically broken: handlers must degrade, not fail.
    server.open("main.jslt", "def broken(").await;
    for method in ["textDocument/hover", "textDocument/definition", "textDocument/prepareRename"] {
        let result = server.position_request(method, "main.jslt", 0, 5).await;
        assert!(result.is_null() || result.is_object(), "{} returned {:?}", method, result);
    }

    let completion = server.position_request("textDocument/completion", "main.jslt", 0, 5).await;
    assert!(completion.is_array(), "completion should still offer builtins");
}
