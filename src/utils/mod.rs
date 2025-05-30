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

#[cfg(target_os = "windows")]
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
) -> Result<(HashMap<String, (Vec<Type>, Type)>, Vec<ast::Function>, Vec<ast::StructDef>, Vec<ast::FfiFunction>, Vec<ast::FfiVariable>, Vec<ast::Stmt>)> {
    imports.iter().try_fold((HashMap::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new(), Vec::new()),
                            |(mut map, mut funcs, mut structs, mut ffi_funcs, mut ffi_vars, mut stmts), import_decl| {
                                match import_decl {
                                    ast::ImportDeclaration::ImportAll { module_path, module_type, alias } => {
                                        let path_result: Result<PathBuf, anyhow::Error> = match module_type {
                                            ast::ModuleType::Standard => {
                                                resolve_standard_library_path(module_path)
                                            },
                                            ast::ModuleType::Local => {
                                                let current_dir = base_path.parent()
                                                    .ok_or_else(|| anyhow!("Base path has no parent"))?;
                                                Ok(current_dir.join(module_path))
                                            },
                                            ast::ModuleType::External => {
                                                resolve_library_path(module_path)
                                            }
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
                                            let file_path = error.labels.get(0)
                                                .map(|l| l.file_id)
                                                .and_then(|fid| Some(files.name(fid)))
                                                .map(|n| n.to_string_lossy().to_string());

                                            let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                                                codespan_reporting::term::termcolor::ColorChoice::Auto
                                            );
                                            let config = codespan_reporting::term::Config::default();
                                            let _ = codespan_reporting::term::emit(&mut writer.lock(), &config, files, &error);

                                            if let Some(path) = file_path {
                                                eprintln!("\nParser error in imported file '{}': {}", path, error.message);
                                            } else {
                                                eprintln!("\nParser error: {}", error.message);
                                            }

                                            anyhow!("Parser failed for import {}", module_path)
                                        })?;

                                        for function in program.functions.iter().filter(|f| f.exported) {
                                            let params: Vec<Type> = function.params.iter().map(|(_, t)| t.clone()).collect();

                                            let function_name = match alias {
                                                Some(a) => format!("{}::{}", a, function.name),
                                                None => {
                                                    let mod_name = module_path
                                                        .split('/')
                                                        .last()
                                                        .unwrap_or(module_path)
                                                        .replace(".ve", "");
                                                    format!("{}::{}", mod_name, function.name)
                                                }
                                            };

                                            map.insert(function_name, (params, function.return_type.clone()));
                                        }

                                        for function in &program.functions {
                                            funcs.push(function.clone());
                                        }

                                        for struct_def in program.structs.iter().filter(|s| s.exported) {
                                            structs.push(struct_def.clone());
                                        }

                                        for ffi_func in &program.ffi_functions {
                                            ffi_funcs.push(ffi_func.clone());
                                        }                                        for ffi_var in &program.ffi_variables {
                                            ffi_vars.push(ffi_var.clone());
                                        }                                        // Collect global statements (let statements) from imported modules
                                        for stmt in &program.stmts {
                                            match stmt {
                                                ast::Stmt::Let(..) => {
                                                    stmts.push(stmt.clone());
                                                }
                                                ast::Stmt::Block(block_stmts, _) => {
                                                    // Extract let statements from blocks
                                                    for block_stmt in block_stmts {
                                                        if let ast::Stmt::Let(..) = block_stmt {
                                                            stmts.push(block_stmt.clone());
                                                        }
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }

                                        Ok((map, funcs, structs, ffi_funcs, ffi_vars, stmts))
                                    },
                                    ast::ImportDeclaration::ImportSpecifiers { .. } => {
                                        Ok((map, funcs, structs, ffi_funcs, ffi_vars, stmts))
                                    }
                                }
                            })
}

fn resolve_standard_library_path(module_path: &str) -> Result<PathBuf> {
    let lib_dir = get_lib_path()?;

    if module_path.starts_with("std/") {
        let path_without_prefix = &module_path[4..];
        let mut path = lib_dir.join("std").join("src");

        if !path_without_prefix.is_empty() {
            path = path.join(path_without_prefix);
        }

        let ve_file = path.with_extension("ve");
        if ve_file.exists() {
            return Ok(ve_file);
        }

        let index_file = path.join("index.ve");
        if index_file.exists() {
            return Ok(index_file);
        }
    }

    Err(anyhow!("Standard library module '{}' not found", module_path))
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

fn get_lib_path() -> Result<PathBuf> {
    if let Ok(exe_path) = env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let lib_dir = exe_dir.join("lib");
            if lib_dir.exists() {
                return Ok(lib_dir);
            }
        }
        
        let project_root = exe_path.parent()
            .and_then(|p| p.parent())
            .and_then(|p| p.parent());
        
        if let Some(root) = project_root {
            let lib_dir = root.join("lib");
            if lib_dir.exists() {
                return Ok(lib_dir);
            }
        }
        
        let deeper_root = exe_path.parent()
            .and_then(|p| p.parent())
            .and_then(|p| p.parent())
            .and_then(|p| p.parent());
            
        if let Some(root) = deeper_root {
            let lib_dir = root.join("lib");
            if lib_dir.exists() {
                return Ok(lib_dir);
            }
        }
    }
    

    let mut potential_paths = Vec::new();
    

    if let Ok(home) = env::var("HOME") {
        potential_paths.push(PathBuf::from(home).join(".velang").join("lib"));
    }
    

    if let Ok(userprofile) = env::var("USERPROFILE") {
        potential_paths.push(PathBuf::from(userprofile).join(".velang").join("lib"));
    }
    

    potential_paths.extend(vec![
        PathBuf::from("/usr/local/share/velang/lib"),
        PathBuf::from("/opt/velang/lib"),
        PathBuf::from("/usr/share/velang/lib"),
    ]);
    

    for path in &potential_paths {
        if path.exists() {
            return Ok(path.clone());
        }
    }
    

    let cwd_lib = env::current_dir().unwrap_or_default().join("lib");
    if cwd_lib.exists() {
        return Ok(cwd_lib);
    }

    let attempted_paths: Vec<String> = potential_paths.iter()
        .map(|p| format!("  - {}", p.display()))
        .collect();

    Err(anyhow::anyhow!(
        "Library directory not found. VeLang requires the standard library to be installed.\n\
        Tried looking in:\n\
        - Directory relative to executable\n\
        {}\n\
        - ./lib (current directory)\n\
        \n\
        Please reinstall VeLang or ensure the standard library is properly installed.\n\
        You can install VeLang using the installation script from:\n\
        https://github.com/velang-org/ve",
        attempted_paths.join("\n")
    ))
}

#[allow(dead_code)]
pub type ImportedFunctions = HashMap<String, (Vec<Type>, Type)>;
