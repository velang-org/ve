use crate::ast::Type;
use crate::{ast, lexer, parser};
use anyhow::{Context, Result, anyhow};
use codespan::Files;
use std::collections::{HashMap, HashSet};
use std::env;
use std::path::{Path, PathBuf};

#[cfg(target_os = "windows")]
pub fn prepare_windows_clang_args(
    output: &Path,
    optimize: bool,
    c_file: &Path,
) -> Result<Vec<String>> {
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
) -> Result<(
    HashMap<String, (Vec<Type>, Type)>,
    Vec<ast::Function>,
    Vec<ast::StructDef>,
    Vec<ast::FfiFunction>,
    Vec<ast::FfiVariable>,
    Vec<ast::Stmt>,
)> {
    imports.iter().try_fold(
        (
            HashMap::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
        ),
        |(mut map, mut funcs, mut structs, mut ffi_funcs, mut ffi_vars, mut stmts), import_decl| {
            match import_decl {
                ast::ImportDeclaration::ImportAll {
                    module_path,
                    module_type,
                    alias,
                } => {
                    let path_result: Result<PathBuf, anyhow::Error> = match module_type {
                        ast::ModuleType::Standard => resolve_standard_library_path(module_path),
                        ast::ModuleType::Local => {
                            let current_dir = base_path
                                .parent()
                                .ok_or_else(|| anyhow!("Base path has no parent"))?;
                            Ok(current_dir.join(module_path))
                        }
                        ast::ModuleType::External => resolve_library_path(module_path),
                    };

                    let path = path_result
                        .with_context(|| format!("Failed to resolve import: {}", module_path))?
                        .canonicalize()
                        .with_context(|| {
                            format!("Failed to canonicalize path for: {}", module_path)
                        })?;

                    let content = std::fs::read_to_string(&path)
                        .with_context(|| format!("Reading imported file {}", path.display()))?;

                    let file_id = files.add(path.to_str().unwrap().to_string(), content);
                    let lexer = lexer::Lexer::new(files, file_id);
                    let mut parser = parser::Parser::new(lexer);
                    let mut program = parser.parse().map_err(|error| {
                        let file_path = error
                            .labels
                            .get(0)
                            .map(|l| l.file_id)
                            .and_then(|fid| Some(files.name(fid)))
                            .map(|n| n.to_string_lossy().to_string());

                        let writer = codespan_reporting::term::termcolor::StandardStream::stderr(
                            codespan_reporting::term::termcolor::ColorChoice::Auto,
                        );
                        let config = codespan_reporting::term::Config::default();
                        let _ = codespan_reporting::term::emit(
                            &mut writer.lock(),
                            &config,
                            files,
                            &error,
                        );

                        if let Some(path) = file_path {
                            eprintln!(
                                "\nParser error in imported file '{}': {}",
                                path, error.message
                            );
                        } else {
                            eprintln!("\nParser error: {}", error.message);
                        }

                        anyhow!("Parser failed for import {}", module_path)
                    })?;
                    analyze_and_mark_dependencies(&mut program);

                    for function in program.functions.iter().filter(|f| {
                        matches!(
                            f.visibility,
                            ast::Visibility::Public | ast::Visibility::Internal
                        )
                    }) {
                        let params: Vec<Type> =
                            function.params.iter().map(|(_, t)| t.clone()).collect();

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

                        map.insert(
                            function_name.clone(),
                            (params, function.return_type.clone()),
                        );
                        funcs.push(function.clone());
                    }

                    for struct_def in program
                        .structs
                        .iter()
                        .filter(|s| matches!(s.visibility, ast::Visibility::Public))
                    {
                        structs.push(struct_def.clone());
                    }

                    for ffi_func in &program.ffi_functions {
                        ffi_funcs.push(ffi_func.clone());
                    }
                    for ffi_var in &program.ffi_variables {
                        ffi_vars.push(ffi_var.clone());
                    }
                    for stmt in &program.stmts {
                        match stmt {
                            ast::Stmt::Let(_name, _, _, _, visibility) => {
                                if matches!(
                                    visibility,
                                    ast::Visibility::Public | ast::Visibility::Internal
                                ) {
                                    stmts.push(stmt.clone());
                                }
                            }
                            ast::Stmt::Block(block_stmts, _) => {
                                for block_stmt in block_stmts {
                                    if let ast::Stmt::Let(_name, _, _, _, visibility) = block_stmt {
                                        if matches!(
                                            visibility,
                                            ast::Visibility::Public | ast::Visibility::Internal
                                        ) {
                                            stmts.push(block_stmt.clone());
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    Ok((map, funcs, structs, ffi_funcs, ffi_vars, stmts))
                }
                ast::ImportDeclaration::ImportSpecifiers {
                    module_path,
                    module_type,
                    specifiers,
                } => {
                    let path_result: Result<PathBuf, anyhow::Error> = match module_type {
                        ast::ModuleType::Standard => resolve_standard_library_path(module_path),
                        ast::ModuleType::Local => {
                            let current_dir = base_path
                                .parent()
                                .ok_or_else(|| anyhow!("Base path has no parent"))?;
                            Ok(current_dir.join(module_path))
                        }
                        ast::ModuleType::External => resolve_library_path(module_path),
                    };

                    let path = path_result
                        .with_context(|| format!("Failed to resolve import: {}", module_path))?
                        .canonicalize()
                        .with_context(|| {
                            format!("Failed to canonicalize path for: {}", module_path)
                        })?;

                    let content = std::fs::read_to_string(&path)
                        .with_context(|| format!("Failed to read file: {}", path.display()))?;
                    let file_id = files.add(path.to_string_lossy().to_string(), content.clone());

                    let lexer = lexer::Lexer::new(files, file_id);
                    let mut parser = parser::Parser::new(lexer);
                    let mut program = parser
                        .parse()
                        .map_err(|e| anyhow!("Parsing error: {:?}", e))?;

                    analyze_and_mark_dependencies(&mut program);

                    for specifier in specifiers {
                        let item_name = &specifier.name;
                        let final_name = match &specifier.alias {
                            Some(alias) => alias.clone(),
                            None => item_name.clone(),
                        };
                        if let Some(function) = program.functions.iter().find(|f| {
                            f.name == *item_name && matches!(f.visibility, ast::Visibility::Public)
                        }) {
                            let params: Vec<Type> =
                                function.params.iter().map(|(_, t)| t.clone()).collect();
                            map.insert(final_name.clone(), (params, function.return_type.clone()));

                            let mut imported_func = function.clone();
                            imported_func.name = final_name.clone();
                            funcs.push(imported_func);
                        }

                        if let Some(struct_def) = program.structs.iter().find(|s| {
                            s.name == *item_name && matches!(s.visibility, ast::Visibility::Public)
                        }) {
                            let mut imported_struct = struct_def.clone();
                            imported_struct.name = final_name.clone();
                            structs.push(imported_struct);
                        }

                        for stmt in &program.stmts {
                            if let ast::Stmt::Let(name, _, _, _, visibility) = stmt {
                                if name == item_name
                                    && matches!(
                                        visibility,
                                        ast::Visibility::Public | ast::Visibility::Internal
                                    )
                                {
                                    let mut imported_stmt = stmt.clone();
                                    if let ast::Stmt::Let(
                                        ref mut stmt_name,
                                        ref _ty,
                                        ref _expr,
                                        ref _span,
                                        ref _vis,
                                    ) = imported_stmt
                                    {
                                        *stmt_name = final_name.clone();
                                    }
                                    stmts.push(imported_stmt);
                                }
                            }
                        }
                    }

                    Ok((map, funcs, structs, ffi_funcs, ffi_vars, stmts))
                }
            }
        },
    )
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

    Err(anyhow!(
        "Standard library module '{}' not found",
        module_path
    ))
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
    use std::fs;
    
    let mut paths = Vec::new();

    if let Ok(vc_dir) = env::var("VCINSTALLDIR") {
        let lib_path = format!("{}\\Lib\\x64", vc_dir.trim_end_matches('\\'));
        if Path::new(&lib_path).exists() {
            paths.push(lib_path);
        }
    }

    if let Ok(windows_sdk_dir) = env::var("WindowsSdkDir") {
        let version = env::var("WindowsSDKVersion").unwrap_or_else(|_| {
            let lib_dir = format!("{}\\Lib", windows_sdk_dir.trim_end_matches('\\'));
            if let Ok(entries) = fs::read_dir(&lib_dir) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .filter(|name| name.starts_with("10.0."))
                    .collect();
                versions.sort();
                versions.last().unwrap_or(&"10.0.22621.0".to_string()).clone()
            } else {
                "10.0.22621.0".to_string()
            }
        });
        
        let um_path = format!(
            "{}\\Lib\\{}\\um\\x64",
            windows_sdk_dir.trim_end_matches('\\'),
            version.trim_end_matches('\\')
        );
        let ucrt_path = format!(
            "{}\\Lib\\{}\\ucrt\\x64",
            windows_sdk_dir.trim_end_matches('\\'),
            version.trim_end_matches('\\')
        );
        
        if Path::new(&um_path).exists() {
            paths.push(um_path);
        }
        if Path::new(&ucrt_path).exists() {
            paths.push(ucrt_path);
        }
    }

    if paths.is_empty() {
        let possible_vs_paths = vec![
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise",
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional", 
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community",
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\BuildTools",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Enterprise",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools",
        ];

        for vs_path in possible_vs_paths {
            let vc_tools_path = format!("{}\\VC\\Tools\\MSVC", vs_path);
            if let Ok(entries) = fs::read_dir(&vc_tools_path) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .collect();
                versions.sort();
                
                if let Some(latest_version) = versions.last() {
                    let lib_path = format!("{}\\VC\\Tools\\MSVC\\{}\\lib\\x64", vs_path, latest_version);
                    if Path::new(&lib_path).exists() {
                        paths.push(lib_path);
                        break;
                    }
                }
            }
        }

        let sdk_paths = vec![
            "C:\\Program Files (x86)\\Windows Kits\\10\\Lib",
            "C:\\Program Files\\Windows Kits\\10\\Lib",
        ];

        for sdk_path in sdk_paths {
            if let Ok(entries) = fs::read_dir(sdk_path) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .filter(|name| name.starts_with("10.0."))
                    .collect();
                versions.sort();
                
                if let Some(latest_version) = versions.last() {
                    let um_path = format!("{}\\{}\\um\\x64", sdk_path, latest_version);
                    let ucrt_path = format!("{}\\{}\\ucrt\\x64", sdk_path, latest_version);
                    
                    if Path::new(&um_path).exists() {
                        paths.push(um_path);
                    }
                    if Path::new(&ucrt_path).exists() {
                        paths.push(ucrt_path);
                    }
                    break;
                }
            }
        }
    }

    if paths.is_empty() {
        return Err(anyhow!(
            "Could not find MSVC libraries. Please ensure Visual Studio Build Tools are installed.\n\
            Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe\n\
            Or run: vcvars64.bat to set up the environment."
        ));
    }

    for path in &paths {
        if !Path::new(path).exists() {
            eprintln!("Warning: Library path does not exist: {}", path);
        }
    }

    let existing_paths: Vec<String> = paths
        .into_iter()
        .filter(|path| Path::new(path).exists())
        .collect();

    if existing_paths.is_empty() {
        return Err(anyhow!(
            "No valid MSVC library paths found. Please install Visual Studio Build Tools.\n\
            Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe"
        ));
    }

    Ok(existing_paths)
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

    let matches: Vec<_> = dir
        .read_dir()
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

        let project_root = exe_path
            .parent()
            .and_then(|p| p.parent())
            .and_then(|p| p.parent());

        if let Some(root) = project_root {
            let lib_dir = root.join("lib");
            if lib_dir.exists() {
                return Ok(lib_dir);
            }
        }

        let deeper_root = exe_path
            .parent()
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

    let attempted_paths: Vec<String> = potential_paths
        .iter()
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

fn analyze_and_mark_dependencies(program: &mut ast::Program) {
    let mut let_statements: HashMap<String, usize> = HashMap::new();

    fn collect_let_statements_recursive(
        stmts: &[ast::Stmt],
        lets: &mut HashMap<String, usize>,
        base_index: &mut usize,
    ) {
        for stmt in stmts {
            match stmt {
                ast::Stmt::Let(name, _, _, _, _) => {
                    lets.insert(name.clone(), *base_index);
                    *base_index += 1;
                }
                ast::Stmt::Block(block_stmts, _) => {
                    collect_let_statements_recursive(block_stmts, lets, base_index);
                }
                _ => {
                    *base_index += 1;
                }
            }
        }
    }

    let mut base_index = 0;
    collect_let_statements_recursive(&program.stmts, &mut let_statements, &mut base_index);

    let mut needed_variables: HashSet<String> = HashSet::new();
    for function in &program.functions {
        if matches!(function.visibility, ast::Visibility::Public) {
            let mut function_deps = HashSet::new();
            collect_variable_dependencies_from_block(&function.body, &mut function_deps);
            needed_variables.extend(function_deps);
        }
    }
    fn mark_variables_recursive(stmts: &mut [ast::Stmt], needed_vars: &HashSet<String>) {
        for stmt in stmts {
            match stmt {
                ast::Stmt::Let(name, _ty, _expr, _span, visibility) => {
                    if needed_vars.contains(name) && matches!(visibility, ast::Visibility::Private)
                    {
                        *visibility = ast::Visibility::Internal;
                    }
                }
                ast::Stmt::Block(block_stmts, _) => {
                    mark_variables_recursive(block_stmts, needed_vars);
                }
                _ => {}
            }
        }
    }
    mark_variables_recursive(&mut program.stmts, &needed_variables);
}

fn collect_variable_dependencies_from_block(
    stmts: &[ast::Stmt],
    dependencies: &mut HashSet<String>,
) {
    for stmt in stmts {
        collect_variable_dependencies(stmt, dependencies);
    }
}

fn collect_variable_dependencies(stmt: &ast::Stmt, dependencies: &mut HashSet<String>) {
    match stmt {
        ast::Stmt::Let(_, _, expr, _, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Stmt::Return(expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Stmt::Block(stmts, _) => {
            for stmt in stmts {
                collect_variable_dependencies(stmt, dependencies);
            }
        }
        ast::Stmt::If(condition, then_stmt, else_stmt, _) => {
            collect_expr_dependencies(condition, dependencies);
            collect_variable_dependencies_from_block(then_stmt, dependencies);
            if let Some(else_stmt) = else_stmt {
                collect_variable_dependencies_from_block(else_stmt, dependencies);
            }
        }
        ast::Stmt::While(condition, body, _) => {
            collect_expr_dependencies(condition, dependencies);
            collect_variable_dependencies_from_block(body, dependencies);
        }
        ast::Stmt::For(_, iter_expr, body, _) => {
            collect_expr_dependencies(iter_expr, dependencies);
            collect_variable_dependencies_from_block(body, dependencies);
        }
        ast::Stmt::Expr(expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        _ => {}
    }
}

fn collect_expr_dependencies(expr: &ast::Expr, dependencies: &mut HashSet<String>) {
    match expr {
        ast::Expr::Var(name, _) => {
            dependencies.insert(name.clone());
        }
        ast::Expr::BinOp(left, _, right, _) => {
            collect_expr_dependencies(left, dependencies);
            collect_expr_dependencies(right, dependencies);
        }
        ast::Expr::UnaryOp(_, expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Expr::Call(_, args, _) => {
            for arg in args {
                collect_expr_dependencies(arg, dependencies);
            }
        }
        ast::Expr::FfiCall(_, args, _) => {
            for arg in args {
                collect_expr_dependencies(arg, dependencies);
            }
        }
        ast::Expr::ArrayAccess(array, index, _) => {
            collect_expr_dependencies(array, dependencies);
            collect_expr_dependencies(index, dependencies);
        }
        ast::Expr::FieldAccess(obj, _, _) => {
            collect_expr_dependencies(obj, dependencies);
        }
        ast::Expr::ArrayInit(elements, _) => {
            for element in elements {
                collect_expr_dependencies(element, dependencies);
            }
        }
        ast::Expr::StructInit(_, fields, _) => {
            for (_, expr) in fields {
                collect_expr_dependencies(expr, dependencies);
            }
        }
        ast::Expr::Cast(expr, _, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Expr::Assign(left, right, _) => {
            collect_expr_dependencies(left, dependencies);
            collect_expr_dependencies(right, dependencies);
        }
        ast::Expr::Deref(expr, _) => {
            collect_expr_dependencies(expr, dependencies);
        }
        ast::Expr::Range(start, end, _) => {
            collect_expr_dependencies(start, dependencies);
            collect_expr_dependencies(end, dependencies);
        }
        ast::Expr::EnumConstruct(_, _, args, _) => {
            for arg in args {
                collect_expr_dependencies(arg, dependencies);
            }
        }

        ast::Expr::Int(_, _)
        | ast::Expr::Int64(_, _)
        | ast::Expr::F32(_, _)
        | ast::Expr::Bool(_, _)
        | ast::Expr::Str(_, _)
        | ast::Expr::Void(_) => {}
        ast::Expr::SafeBlock(stmts, _) => {
            collect_variable_dependencies_from_block(stmts, dependencies);
        }
        ast::Expr::TemplateStr(parts, _) => {
            for part in parts {
                if let ast::TemplateStrPart::Expression(expr) = part {
                    collect_expr_dependencies(expr, dependencies);
                }
            }
        }
        ast::Expr::Match(_, arms, _) => {
            for arm in arms {
                match &arm.body {
                    ast::MatchArmBody::Expr(expr) => {
                        collect_expr_dependencies(expr, dependencies);
                    }
                    ast::MatchArmBody::Block(stmts) => {
                        for stmt in stmts {
                            collect_variable_dependencies(stmt, dependencies);
                        }
                    }
                }
            }
        }
    }
}
