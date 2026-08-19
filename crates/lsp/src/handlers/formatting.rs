use crate::errors::HandlerError;
use formatter::format_source;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{DocumentFormattingParams, MessageType, Position, Range, TextEdit};

use crate::JsltLanguageServer;

pub(crate) async fn formatting(
    server: &JsltLanguageServer,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = params.text_document.uri;
    let context = server.request_context(&uri, None).await;

    let Some(document) = context.document else {
        server.report_non_fatal(HandlerError::MissingDocument { uri: uri.to_string() }).await;
        return Ok(None);
    };
    tracing::trace!(
        uri = %document.uri,
        open_documents = context.workspace.open_document_count,
        "processed formatting request",
    );

    match format_source(&document.text) {
        Ok(formatted) => {
            if formatted == document.text {
                return Ok(None);
            }

            let range = Range::new(Position::new(0, 0), Position::new(u32::MAX, u32::MAX));
            Ok(Some(vec![TextEdit { range, new_text: formatted }]))
        }
        Err(e) => {
            server
                .client
                .log_message(
                    MessageType::WARNING,
                    format!("Cannot format document with parse errors: {}", e),
                )
                .await;
            Ok(None)
        }
    }
}
