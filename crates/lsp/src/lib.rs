//! JSLT Language Server Protocol implementation.

mod analysis;
mod capabilities;
mod context;
mod errors;
mod handlers;
mod naming;
mod state;
mod workspace;

pub use state::JsltLanguageServer;

use tower_lsp::{LspService, Server};

/// Start the language server over stdio.
pub async fn run_server() {
    tracing_subscriber::fmt().with_writer(std::io::stderr).with_ansi(false).init();

    let (service, socket) = LspService::new(JsltLanguageServer::new);
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket).serve(service).await;
}
