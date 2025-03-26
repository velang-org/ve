mod c;
mod compile_error;
mod cranelift;

use std::path::Path;
use codespan::FileId;
pub use compile_error::CompileError;

pub enum Target {
    Native(c::CBackend),
    Cranelift(cranelift::CraneliftBackend),
}

pub struct CodegenConfig {
    pub target_triple: String,
    pub use_cranelift: bool,
}

impl Target {
    pub fn create(config: CodegenConfig, file_id: FileId, imported_return_types: std::collections::HashMap<String, (Vec<crate::ast::Type>, crate::ast::Type)>) -> Self {
        if config.use_cranelift {
            Target::Cranelift(cranelift::CraneliftBackend::new(config, file_id, imported_return_types))
        } else {
            Target::Native(c::CBackend::new(config, file_id, imported_return_types))
        }
    }

    pub fn compile(&mut self, program: &crate::ast::Program, output_path: &Path) -> Result<(), CompileError> {
        match self {
            Target::Native(c_backend) => c_backend.compile(program, output_path),
            Target::Cranelift(cranelift_backend) => cranelift_backend.compile(program, output_path),
        }
    }
}