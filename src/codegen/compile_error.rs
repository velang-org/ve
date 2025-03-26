use std::fmt;
use codespan::{FileId, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};


#[derive(Debug)]
pub enum CompileError {
    CodegenError {
        message: String,
        #[allow(dead_code)]
        span: Option<Span>,
        #[allow(dead_code)]
        file_id: FileId,
    },
    IOError(std::io::Error),
    #[allow(dead_code)]
    LinkingError(String),
    #[allow(dead_code)]
    OptimizationError(String),
    #[allow(dead_code)]
    UnsupportedOperation(String),
    #[allow(dead_code)]
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
    #[allow(dead_code)]
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            CompileError::CodegenError { message, span, file_id } => {
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![
                        span.map(|s| Label::primary(*file_id, s)).unwrap_or_else(|| Label::primary(*file_id, Span::default()))
                    ])
            }
            CompileError::LinkingError(message) => {
                Diagnostic::error()
                    .with_message(format!("Linking error: {}", message))
            }
            CompileError::OptimizationError(message) => {
                Diagnostic::error()
                    .with_message(format!("Optimization error: {}", message))
            }
            CompileError::UnsupportedOperation(message) => {
                Diagnostic::error()
                    .with_message(format!("Unsupported operation: {}", message))
            }
            CompileError::TypeError { message, span, file_id } => {
                Diagnostic::error()
                    .with_message(format!("Type error: {}", message))
                    .with_labels(vec![
                        span.map(|s| Label::primary(*file_id, s)).unwrap_or_else(|| Label::primary(*file_id, Span::default()))
                    ])
            }
            CompileError::IOError(e) => {
                Diagnostic::error()
                    .with_message(format!("IO error: {}", e))
            }
        }
    }
}