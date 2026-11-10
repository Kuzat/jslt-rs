use std::fmt;

#[derive(Debug)]
pub(crate) enum HandlerError {
    MissingDocument { uri: String },
}

impl fmt::Display for HandlerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingDocument { uri } => {
                write!(f, "document is not open in LSP state: {}", uri)
            }
        }
    }
}
