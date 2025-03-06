use verve_lang::{lexer, parser, typeck, codegen, cli::{Args, Command}};

use clap::Parser;
use codespan::{FileId, Files};
use codespan_reporting::diagnostic::Diagnostic;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug)]
struct MyError(Diagnostic<FileId>);

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::error::Error for MyError {}

fn check_dependencies() -> Result<(), Box<dyn std::error::Error>> {
    let has_compiler = std::process::Command::new("clang")
        .arg("--version")
        .status()
        .or_else(|_| std::process::Command::new("gcc").arg("--version").status())
        .is_ok();

    if !has_compiler {
        #[cfg(target_os = "windows")]
        return Err("Requires Clang. Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe".into());
        #[cfg(not(target_os = "windows"))]
        return Err("Requires Clang or GCC. Please install a C compiler".into());
    }
    Ok(())
}

#[cfg(target_os = "windows")]
fn get_msvc_lib_paths() -> Result<Vec<String>, Box<dyn std::error::Error>> {
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
        if !std::path::Path::new(path).exists() {
            return Err(format!("Missing library path: {}\nInstall VS Build Tools: https://aka.ms/vs/17/release/vs_BuildTools.exe", path).into());
        }
    }

    Ok(paths)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    check_dependencies()?;
    let args = Args::parse();

    let (input, output, optimize, target_triple, verbose) = match args.command {
        Some(Command::Run {
                 input,
                 output,
                 optimize,
                 target_triple,
                 verbose,
             }) => (input, output, optimize, target_triple, verbose),
        None => (
            args.input.unwrap(),
            args.output,
            args.optimize,
            args.target_triple,
            args.verbose,
        ),
    };



    let mut files = Files::new();
    let content = std::fs::read_to_string(&input)?;
    let file_id = files.add(input.to_str().unwrap(), content);

    let lexer = lexer::Lexer::new(&files, file_id);
    let mut parser = parser::Parser::new(lexer);
    let mut program = parser.parse().map_err(MyError)?;

    if verbose {
        println!("Parsed AST:\n{:#?}", program);
    }

    let mut type_checker = typeck::TypeChecker::new(file_id);
    if let Err(errors) = type_checker.check(&mut program) {
        for error in errors {
            eprintln!("Type error: {:?}", error);
        }
        return Err("Type check failed".into());
    }

    let config = codegen::CodegenConfig {
        target_triple: target_triple.clone(),
    };
    let mut target = codegen::Target::create(config, file_id);
    target.compile(&program)?;

    #[cfg(target_os = "windows")]
    {
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
            println!("Invoking clang with args: {:?}", clang_args);
        }

        let status = std::process::Command::new("clang").args(&clang_args).status()?;
        if !status.success() {
            return Err("C compilation failed".into());
        }
    }

    println!("Program compiled to: {}", output.display());
    Ok(())
}