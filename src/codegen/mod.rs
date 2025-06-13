mod c;
mod compile_error;

use codespan::FileId;
pub use compile_error::CompileError;
use std::path::Path;

pub enum Target {
    Native(c::CBackend),
}

pub struct CodegenConfig {
    pub target_triple: String,
}

impl Target {
    pub fn create(
        config: CodegenConfig,
        file_id: FileId,
        imported_functions: std::collections::HashMap<
            String,
            (Vec<crate::ast::Type>, crate::ast::Type),
        >,
        imported_structs: Vec<crate::ast::StructDef>,
        imported_ffi_vars: Vec<crate::ast::FfiVariable>,
        is_test_mode: bool,
    ) -> Self {
        Target::Native(c::CBackend::new(config, file_id, imported_functions, imported_structs, imported_ffi_vars, is_test_mode))
    }

    pub fn compile(
        &mut self,
        program: &crate::ast::Program,
        output_path: &Path,
    ) -> Result<(), CompileError> {
        match self {
            Target::Native(c_backend) => c_backend.compile(program, output_path),
        }
    }
}
