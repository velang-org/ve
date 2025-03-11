mod c;
mod compile_error;

use std::path::Path;
use codespan::FileId;
pub use compile_error::CompileError;

pub enum Target {
    Native(c::CBackend),
}

pub struct CodegenConfig {
    pub target_triple: String,
}

impl Target {
    pub fn create(config: CodegenConfig, file_id: FileId, imported_return_types: std::collections::HashMap<String, (Vec<crate::ast::Type>, crate::ast::Type)>) -> Self {
        Target::Native(c::CBackend::new(config, file_id, imported_return_types))
    }

    pub fn compile(&mut self, program: &crate::ast::Program, output_path: &Path) -> Result<(), CompileError> {
        match self {
            Target::Native(c_backend) => c_backend.compile(program, output_path),
        }
    }
}