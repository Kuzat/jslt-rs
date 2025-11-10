//! JSLT Language Server binary
//!
//! This is the executable that editors will launch.
//! It communicates via stdin/stdout using the Language Server Protocol

#[tokio::main]
async fn main() {
    jslt_lsp::run_server().await;
}