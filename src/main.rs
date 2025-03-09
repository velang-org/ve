mod error;

use std::{
    collections::HashMap,
    fmt,
    path::{Path, PathBuf},
    process::Stdio,
};

use anyhow::{anyhow, Context};
use clap::Parser;
use codespan::{FileId, Files};
use codespan_reporting::diagnostic::Diagnostic;
use dunce::canonicalize;

use verve_lang::{
    ast::{self, Type},
    cli::{Args, Command},
    codegen,
    lexer,
    parser,
    typeck,
};

#[derive(Debug)]
struct MyError(Diagnostic<FileId>);

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::error::Error for MyError {}


type ImportedFunctions = HashMap<String, (Vec<Type>, Type)>;
type ParsedProgram = anyhow::Result<(ast::Program, ImportedFunctions)>;

type ProcessedArgs = (PathBuf, PathBuf, bool, String, bool);
fn check_dependencies() -> anyhow::Result<()> {
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
fn get_msvc_lib_paths() -> anyhow::Result<Vec<String>> {
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



fn resolve_args(args: Args) -> anyhow::Result<ProcessedArgs> {
    let (input, output, optimize, target_triple, verbose) = match args.command {
        Some(Command::Run { input, output, optimize, target_triple, verbose }) => {
            let validated_input = canonicalize(&input)
                .with_context(|| format!("Resolving path for '{}'", input.display()))?;
            (validated_input, output, optimize, target_triple, verbose)
        }
        None => {
            let input = args.input.unwrap();
            let validated_input = canonicalize(&input)
                .with_context(|| format!("Resolving path for '{}'", input.display()))?;
            (validated_input, args.output, args.optimize, args.target_triple, args.verbose)
        }
    };
    Ok((input, output, optimize, target_triple, verbose))
}

fn process_imports(
    files: &mut Files<String>,
    imports: &[ast::Import],
    base_path: &Path,
) -> anyhow::Result<(ImportedFunctions, Vec<ast::Function>)>{
    imports.iter().try_fold((HashMap::new(), Vec::new()), |(mut map, mut ast), import| {
        let path = base_path.parent().unwrap().join(&import.path);
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Reading imported file {}", path.display()))?;

        let file_id = files.add(path.to_str().unwrap().to_string(), content);
        let lexer = lexer::Lexer::new(files, file_id);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse().map_err(MyError)?;

        let (nested_map, nested_ast) = process_imports(files, &program.imports, &path)?;
        program.functions.iter()
            .filter(|f| f.exported)
            .for_each(|f| {
                let params = f.params.iter().map(|(_, t)| t.clone()).collect();
                map.insert(f.name.clone(), (params, f.return_type.clone()));
            });

        ast.extend(program.functions.into_iter().filter(|f| f.exported));
        map.extend(nested_map);
        ast.extend(nested_ast);
        Ok((map, ast))
    })
}

fn main() -> anyhow::Result<()> {
    check_dependencies().context("Dependency check failed")?;
    let args = Args::parse();

    let (input, output, optimize, target_triple, verbose) = resolve_args(args)?;
    
    let mut files = Files::<String>::new();
    let content = std::fs::read_to_string(&input)
        .with_context(|| format!("Reading file '{}'", input.display()))?;

    let file_id = files.add(input.to_str().unwrap().to_string(), content);
    let (mut program, imported_functions) = parse_and_process_imports(&mut files, &input)?;

    if verbose {
        println!("Parsed AST:\n{:#?}", program);
    }

    type_check(&mut program, file_id, &imported_functions)?;
    compile_to_target(program, output, optimize, target_triple, verbose, file_id, imported_functions)
}


fn parse_and_process_imports(
    files: &mut Files<String>,
    input: &Path,
) -> ParsedProgram {
    let file_id = files.add(input.to_str().unwrap().to_string(),
                            std::fs::read_to_string(input).with_context(|| format!("Reading {}", input.display()))?);

    let lexer = lexer::Lexer::new(files, file_id);
    let mut parser = parser::Parser::new(lexer);
    let mut program = parser.parse().map_err(MyError)?;

    let (imported_functions, imported_asts) = process_imports(files, &program.imports, input)?;
    program.functions.extend(imported_asts);

    Ok((program, imported_functions))
}

fn type_check(
    program: &mut ast::Program,
    file_id: FileId,
    imported_functions: &HashMap<String, (Vec<Type>, Type)>,
) -> anyhow::Result<()> {
    let mut type_checker = typeck::TypeChecker::new(file_id, imported_functions.clone());
    type_checker.check(program).map_err(|errors| {
        for error in errors {
            eprintln!("Type error: {:?}", error);
        }
        anyhow!("Type check failed")
    })
}

fn compile_to_target(
    program: ast::Program,
    output: PathBuf,
    optimize: bool,
    target_triple: String,
    verbose: bool,
    file_id: FileId,
    imported_functions: HashMap<String, (Vec<Type>, Type)>,
) -> anyhow::Result<()> {
    let config = codegen::CodegenConfig { target_triple };
    let mut target = codegen::Target::create(config, file_id, imported_functions);
    target.compile(&program)?;

    #[cfg(target_os = "windows")]
    let clang_args = prepare_windows_clang_args(&output, optimize, verbose)?;
    #[cfg(not(target_os = "windows"))]
    let clang_args = prepare_unix_clang_args(&output, optimize);

    let status = std::process::Command::new("clang")
        .args(&clang_args)
        .status()
        .context("Failed to execute C compiler")?;

    if !status.success() {
        return Err(anyhow!("C compilation failed"));
    }

    println!("Program compiled to: {}", output.display());
    Ok(())
}


fn prepare_windows_clang_args(output: &Path, optimize: bool, verbose: bool) -> anyhow::Result<Vec<String>> {
    let msvc_lib_paths = get_msvc_lib_paths()?;
    let mut clang_args = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        "output.c".to_string(),
        "-o".to_string(),
        output.to_str().unwrap().to_string(),
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

    if verbose {
        println!("Windows Clang args: {:?}", clang_args);
    }

    Ok(clang_args)
}


