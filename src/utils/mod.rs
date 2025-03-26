use crate::ast::Type;
use crate::{ast, lexer, parser};
use anyhow::{anyhow, Context, Result};
use codespan::Files;
use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Stdio;

pub fn check_dependencies() -> Result<()> {
    let has_compiler = std::process::Command::new("clang")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .or_else(|_| std::process::Command::new("gcc").arg("--version").stdout(Stdio::null()).stderr(Stdio::null()).status())
        .is_ok();

    if !has_compiler {
        #[cfg(target_os = "windows")]
        return Err(anyhow!("Requires Clang. Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe"));
        #[cfg(not(target_os = "windows"))]
        return Err(anyhow!("Requires Clang or GCC. Please install a C compiler"));
    }
    Ok(())
}

pub fn prepare_windows_clang_args(output: &Path, optimize: bool, c_file: &Path) -> Result<Vec<String>> {
    let msvc_lib_paths = get_msvc_lib_paths()?;
    let mut clang_args = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

    for path in msvc_lib_paths {
        clang_args.push("-L".to_string());
        clang_args.push(path);
    }

    clang_args.extend_from_slice(&[
        "-lmsvcrt".to_string(),
        "-Xlinker".to_string(),
        "/NODEFAULTLIB:libcmt".to_string(),
    ]);

    Ok(clang_args)
}

pub fn process_imports(
    files: &mut Files<String>,
    imports: &[ast::ImportDeclaration],
    base_path: &Path,
) -> Result<(HashMap<String, (Vec<Type>, Type)>, Vec<ast::Function>)> {
    imports.iter().try_fold((HashMap::new(), Vec::new()), |(mut map, mut ast), import_decl| {
        match import_decl {
            ast::ImportDeclaration::ImportAll { module_path, alias } => {
                let path_result: Result<PathBuf, anyhow::Error> = if !module_path.starts_with("./") && !module_path.starts_with("../") {
                    resolve_library_path(module_path)
                } else {
                    let current_dir = base_path.parent()
                        .ok_or_else(|| anyhow!("Base path has no parent"))?;
                    Ok(current_dir.join(module_path))
                };

                let path = path_result
                    .with_context(|| format!("Failed to resolve import: {}", module_path))?
                    .canonicalize()
                    .with_context(|| format!("Failed to canonicalize path for: {}", module_path))?;

                let content = std::fs::read_to_string(&path)
                    .with_context(|| format!("Reading imported file {}", path.display()))?;

                let file_id = files.add(path.to_str().unwrap().to_string(), content);
                let lexer = lexer::Lexer::new(files, file_id);
                let mut parser = parser::Parser::new(lexer);
                let program = parser.parse().map_err(|error| {
                    eprintln!("Parser error: {:?}", error);
                    anyhow!("Parser failed")
                })?;

                let (nested_map, nested_ast) = process_imports(files, &program.imports, &path)?;

                for f in program.functions.iter().filter(|f| f.exported) {
                    let params: Vec<Type> = f.params.iter().map(|(_, t)| t.clone()).collect();
                    let name = match alias {
                        Some(inner_alias) => format!("{}::{}", inner_alias, f.name),
                        None => f.name.clone(),
                    };
                    map.insert(name, (params, f.return_type.clone()));
                }

                ast.extend(program.functions.into_iter().filter(|f| f.exported));
                map.extend(nested_map);
                ast.extend(nested_ast);
            }
            ast::ImportDeclaration::ImportSpecifiers { module_path, specifiers } => {
                let path_result: Result<PathBuf, anyhow::Error> = if !module_path.starts_with("./") && !module_path.starts_with("../") {
                    resolve_library_path(module_path)
                } else {
                    let current_dir = base_path.parent()
                        .ok_or_else(|| anyhow!("Base path has no parent"))?;
                    Ok(current_dir.join(module_path))
                };

                let path = path_result
                    .with_context(|| format!("Failed to resolve import: {}", module_path))?
                    .canonicalize()
                    .with_context(|| format!("Failed to canonicalize path for: {}", module_path))?;
                let content = std::fs::read_to_string(&path)
                    .with_context(|| format!("Reading imported file {}", path.display()))?;

                let file_id = files.add(path.to_str().unwrap().to_string(), content);
                let lexer = lexer::Lexer::new(files, file_id);
                let mut parser = parser::Parser::new(lexer);
                let program = parser.parse().map_err(|error| {
                    eprintln!("Parser error: {:?}", error);
                    anyhow!("Parser failed")
                })?;

                let (nested_map, nested_ast) = process_imports(files, &program.imports, &path)?;

                for specifier in specifiers {
                    let imported_name = specifier.alias.clone().unwrap_or(specifier.name.clone());
                    let original_name = &specifier.name;
                    if let Some(func) = program.functions.iter().find(|f| f.name.as_str() == original_name) {
                        let params: Vec<Type> = func.params.iter().map(|(_, t)| t.clone()).collect();
                        map.insert(imported_name, (params, func.return_type.clone()));
                    } else {
                        return Err(anyhow!("Function {} not found in module {}", original_name, module_path));
                    }
                }

                ast.extend(specifiers.iter().filter_map(|specifier| {
                    program.functions.iter().find(|f| f.name == specifier.name).cloned()
                }));

                map.extend(nested_map);
                ast.extend(nested_ast);
            }
        }
        Ok((map, ast))
    })
}

#[cfg(target_os = "windows")]
fn get_msvc_lib_paths() -> Result<Vec<String>> {
    use std::env;
    let mut paths = Vec::new();

    if let Ok(vc_dir) = env::var("VCINSTALLDIR") {
        paths.push(format!("{}\\Lib\\x64", vc_dir.trim_end_matches('\\')));
    }

    if let Ok(windows_sdk_dir) = env::var("WindowsSdkDir") {
        let version = env::var("WindowsSDKVersion").unwrap_or("10.0.22621.0".to_string());
        paths.push(format!("{}\\Lib\\{}\\um\\x64", windows_sdk_dir.trim_end_matches('\\'), version));
        paths.push(format!("{}\\Lib\\{}\\ucrt\\x64", windows_sdk_dir.trim_end_matches('\\'), version));
    }

    if paths.is_empty() {
        paths.extend(vec![
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.40.33807\\lib\\x64".to_string(),
            "C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\ucrt\\x64".to_string(),
            "C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\um\\x64".to_string(),
        ]);
    }

    for path in &paths {
        if !Path::new(path).exists() {
            return Err(anyhow!(
                "Missing library path: {}\nInstall VS Build Tools: https://aka.ms/vs/17/release/vs_BuildTools.exe",
                path
            ));
        }
    }
    Ok(paths)
}

pub fn validate_ve_file(path: &str) -> std::result::Result<PathBuf, String> {
    let path = Path::new(path);
    let path = if path.extension().is_none() {
        path.with_extension("ve")
    } else {
        path.to_path_buf()
    };

    if !path.exists() {
        let suggestions = suggest_similar_files(&*path.clone())
            .map(|s| format!("\nDid you mean:\n{}", s))
            .unwrap_or_default();

        return Err(format!(
            "File '{}' not found.{}",
            path.display(),
            suggestions
        ));
    }
    Ok(path)
}

fn suggest_similar_files(missing_path: &Path) -> Option<String> {
    let dir = missing_path.parent()?;
    let target_name = missing_path.file_stem()?.to_string_lossy();
    let target_name = target_name.as_ref();

    let matches: Vec<_> = dir.read_dir()
        .ok()?
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_stem()?.to_string_lossy();
            (name.contains(target_name) && path.extension() == Some("ve".as_ref()))
                .then_some(format!("  • {}", path.display()))
        })
        .collect();

    (!matches.is_empty()).then(|| matches.join("\n"))
}

fn resolve_library_path(module_path: &str) -> Result<PathBuf> {
    let lib_dir = get_lib_path()?;
    let components: Vec<&str> = module_path.split('/').collect();
    if components.is_empty() {
        return Err(anyhow!("Invalid module path: {}", module_path));
    }

    let mut path = lib_dir.join(components[0]).join("src");
    for component in &components[1..] {
        path.push(component);
    }

    let ve_file = path.with_extension("ve");
    if ve_file.exists() {
        Ok(ve_file)
    } else {
        let index_file = path.join("index.ve");
        if index_file.exists() {
            Ok(index_file)
        } else {
            Err(anyhow!("Module '{}' not found in library", module_path))
        }
    }
}

fn get_lib_path() -> Result<PathBuf> {
    let exe_path = env::current_exe().context("Failed to get current executable path")?;
    let project_root = exe_path.parent()
        .and_then(|p| p.parent())
        .and_then(|p| p.parent())
        .context("Failed to determine project root")?;

    let lib_dir = project_root.join("lib");

    if !lib_dir.exists() {
        return Err(anyhow::anyhow!("Library directory not found: {}", lib_dir.display()));
    }

    Ok(lib_dir)
}

#[allow(dead_code)]
pub type ImportedFunctions = HashMap<String, (Vec<Type>, Type)>;