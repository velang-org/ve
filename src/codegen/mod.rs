mod c;
mod compile_error;

use codespan::FileId;
pub use compile_error::CompileError;

pub enum Target {
    Native(c::CBackend),
}

pub struct CodegenConfig {
    pub optimize: bool,
    pub target_triple: String,
}

impl Target {
    pub fn create(config: CodegenConfig, file_id: FileId) -> Self {
        Target::Native(c::CBackend::new(config, file_id))
    }

    pub fn compile(&mut self, program: &crate::ast::Program) -> Result<(), CompileError> {
        match self {
            Target::Native(c_backend) => c_backend.compile(program),
        }
    }
}