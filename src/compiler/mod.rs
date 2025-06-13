pub mod cache;
pub mod incremental;

use crate::ast;
use anyhow::{Result, anyhow};
use std::path::{Path, PathBuf};





#[derive(Debug, Clone)]
pub struct ResolvedImport {
    pub path: PathBuf,
    pub import_type: ImportType,
    pub module_path: String,
}

#[derive(Debug, Clone)]
pub enum ImportType {
    All { alias: Option<String> },
    Specifiers { specifiers: Vec<ast::ImportSpecifier> },
}

pub fn resolve_standard_library_path(module_path: &str) -> Result<PathBuf> {
    let base_path = std::env::var("VEIL_LIB_PATH")
        .unwrap_or_else(|_| "lib".to_string());
    
    let full_path = if module_path.starts_with("std/") {
        let module_name = &module_path[4..];
        PathBuf::from(base_path).join("std").join("src").join(format!("{}.ve", module_name))
    } else {
        PathBuf::from(base_path).join("src").join(format!("{}.ve", module_path))
    };
    
    if full_path.exists() {
        Ok(full_path)
    } else {
        Err(anyhow!("Standard library module not found: {}", module_path))
    }
}

pub fn resolve_imports_only(
    imports: &[ast::ImportDeclaration],
    base_path: &Path,
) -> Result<Vec<ResolvedImport>> {
    let mut resolved = Vec::new();

    for import in imports {
        match import {
            ast::ImportDeclaration::ImportAll { module_path, module_type, alias } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path.parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.ve", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir = std::env::var("VEIL_LIBS_PATH")
                            .unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.ve", module_path))
                    }
                };

                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::All { alias: alias.clone() },
                    module_path: module_path.clone(),
                });
            }            ast::ImportDeclaration::ImportSpecifiers { module_path, module_type, specifiers } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path.parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.ve", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir = std::env::var("VEIL_LIBS_PATH")
                            .unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.ve", module_path))
                    }
                };

                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::Specifiers { specifiers: specifiers.clone() },
                    module_path: module_path.clone(),
                });
            }
            ast::ImportDeclaration::ExportImportAll { module_path, module_type, alias } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path.parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.ve", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir = std::env::var("VEIL_LIBS_PATH")
                            .unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.ve", module_path))
                    }
                };

                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::All { alias: alias.clone() },
                    module_path: module_path.clone(),
                });
            }
            ast::ImportDeclaration::ExportImportSpecifiers { module_path, module_type, specifiers } => {
                let resolved_path = match module_type {
                    ast::ModuleType::Standard => resolve_standard_library_path(module_path)?,
                    ast::ModuleType::Local => {
                        let current_dir = base_path.parent()
                            .ok_or_else(|| anyhow!("Base path has no parent"))?;
                        current_dir.join(format!("{}.ve", module_path))
                    }
                    ast::ModuleType::External => {
                        let external_libs_dir = std::env::var("VEIL_LIBS_PATH")
                            .unwrap_or_else(|_| "libs".to_string());
                        PathBuf::from(external_libs_dir).join(format!("{}.ve", module_path))
                    }
                };                resolved.push(ResolvedImport {
                    path: resolved_path,
                    import_type: ImportType::Specifiers { specifiers: specifiers.clone() },
                    module_path: module_path.clone(),
                });
            }
        }
    }

    Ok(resolved)
}
