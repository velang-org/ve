use std::fmt;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};


#[derive(Debug)]
pub enum CompileError {
    CodegenError {
        message: String,
        span: Option<Span>,
        file_id: FileId,
    },
    IOError(std::io::Error),
    LinkingError(String),
    OptimizationError(String),
    UnsupportedOperation(String),
    TypeError { message: String, span: Option<Span>, file_id: FileId },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::CodegenError { message, .. } => write!(f, "Codegen error: {}", message),
            CompileError::IOError(e) => write!(f, "IO error: {}", e),
            CompileError::LinkingError(msg) => write!(f, "Linking error: {}", msg),
            CompileError::OptimizationError(msg) => write!(f, "Optimization error: {}", msg),
            CompileError::UnsupportedOperation(msg) => write!(f, "Unsupported operation: {}", msg),
            CompileError::TypeError { message, .. } => write!(f, "Type error: {}", message),
        }
    }
}
impl std::error::Error for CompileError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            CompileError::IOError(e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for CompileError {
    fn from(e: std::io::Error) -> Self {
        CompileError::IOError(e)
    }
}


impl CompileError {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            CompileError::CodegenError { message, span, file_id } => {
                let mut diagnostic = Diagnostic::error()
                    .with_message(message);
                if let Some(span) = span {
                    diagnostic = diagnostic.with_labels(vec![
                        Label::primary(*file_id, span.clone())
                            .with_message("codegen error occurred here")
                    ]);
                }
                diagnostic
            }
            _ => Diagnostic::error().with_message(self.to_string()),
        }
    }
}