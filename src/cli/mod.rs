
pub mod init;
pub(crate) mod run;

use clap::{Parser, Subcommand};
use std::path::{Path, PathBuf};
use anyhow::{anyhow, Context};
use codespan::Files;
use dunce::canonicalize;
use crate::{codegen, lexer, parser, typeck};
use crate::utils::{process_imports, prepare_windows_clang_args};

#[derive(Debug)]
pub struct CliError(pub String);

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for CliError {}

#[derive(Debug)]
pub enum CliCommand {
    Build {
        input: PathBuf,
        output: PathBuf,
        optimize: bool,
        target_triple: String,
        verbose: bool,
        is_temp: bool,
    },
    Init {
        directory: PathBuf,
        project_name: String,
    },
    Run,
}



#[derive(Parser)]
#[command(
    version,
    about,
    args_conflicts_with_subcommands = true,
    subcommand_negates_reqs = true,
    disable_help_subcommand = true
)]
pub struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    #[arg(required = true, value_parser = validate_ve_file, value_name = "FILE[.ve]")]
    input: Option<PathBuf>,

    #[arg(short, long, default_value = "program.exe")]
    output: PathBuf,

    #[arg(long, action = clap::ArgAction::SetFalse)]
    optimize: bool,

    #[arg(long, default_value = "x86_64-pc-windows-msvc")]
    target_triple: String,

    #[arg(short, long)]
    verbose: bool,
}




#[derive(Subcommand)]
enum Command {
    Build {
        #[arg(value_parser = validate_ve_file)]
        input: PathBuf,

        #[arg(short, long, default_value = "build/program.exe")]
        output: PathBuf,

        #[arg(long, action = clap::ArgAction::SetFalse)]
        optimize: bool,

        #[arg(long, default_value = "x86_64-pc-windows-msvc")]
        target_triple: String,

        #[arg(short, long)]
        verbose: bool,
    },
    Init {
        project_name: String,
        #[arg(default_value = ".")]
        directory: PathBuf,
    },
    Run {

    }
}


pub fn parse_args() -> Args {
    Args::parse()
}

pub fn parse() -> anyhow::Result<CliCommand> {
    let args = Args::parse();

    match args.command {
        Some(Command::Build { input, output, optimize, target_triple, verbose }) => {
            let output_clone = output.clone();
            Ok(CliCommand::Build {
                input,
                output,
                optimize,
                target_triple,
                verbose,
                is_temp: output_clone == PathBuf::from("program.exe"),
            })
        }
        Some(Command::Init { directory, project_name }) => {
            Ok(CliCommand::Init { directory, project_name })
        },
        Some(Command::Run {}) => {
            Ok(CliCommand::Run)
        }
        None => {
            let input = args.input.ok_or_else(|| anyhow!("Input file is required"))?;
            let output = args.output.clone();
            Ok(CliCommand::Build {
                input,
                output: args.output,
                optimize: args.optimize,
                target_triple: args.target_triple,
                verbose: args.verbose,
                is_temp: output == PathBuf::from("program.exe"),
            })
        }
    }
}


pub fn process_build(
    input: PathBuf,
    output: PathBuf,
    optimize: bool,
    target_triple: String,
    verbose: bool,
    is_temp: bool,
) -> anyhow::Result<()> {
    if let Some(parent) = output.parent() {
        if !parent.exists() {
            std::fs::create_dir_all(parent)
                .context("Failed to create output directory")?;
        }
    }


    let mut files = Files::<String>::new();

    let file_id = files.add(input.to_str().unwrap().to_string(),
                            std::fs::read_to_string(input.clone()).with_context(|| format!("Reading {}", input.display()))?);

    let output = canonicalize(&output)
        .unwrap_or_else(|_| output.clone());

    if verbose {
        println!("Input file: {}", input.display());
        println!("Output file: {}", output.display());
    }

    let lexer = lexer::Lexer::new(&files, file_id);
    let mut parser = parser::Parser::new(lexer);
    let mut program = parser.parse().map_err(|error| {
        eprintln!("Parser error: {:?}", error);
        anyhow!("Parser failed")
    })?;

    let (imported_functions, imported_asts) = process_imports(&mut files, &program.imports, &*input)?;
    program.functions.extend(imported_asts);


    if verbose {
        println!("Parsed AST:\n{:#?}", program);
    }

    let mut type_checker = typeck::TypeChecker::new(file_id, imported_functions.clone());
    type_checker.check(&mut program).map_err(|errors| {
        for error in errors {
            eprintln!("Type error: {:?}", error);
        }
        anyhow!("Type check failed")
    })?;

    let config = codegen::CodegenConfig { target_triple };
    let mut target = codegen::Target::create(config, file_id, imported_functions);
    target.compile(&program)?;

    #[cfg(target_os = "windows")]
    let c_file = output.with_extension("c");
    let clang_args = prepare_windows_clang_args(&output, optimize, &c_file )?;
    #[cfg(not(target_os = "windows"))]
    let clang_args = prepare_unix_clang_args(&output, optimize);

    let status = std::process::Command::new("clang")
        .args(&clang_args)
        .status()
        .context("Failed to execute C compiler")?;

    if !status.success() {
        return Err(anyhow!("C compilation failed"));
    }

    if is_temp {
        let status = std::process::Command::new(output.clone())
            .status()
            .context("Failed to execute program")?;

        if !status.success() {
            return Err(anyhow!("Program failed with exit code: {}", status));
        }
    } else {
        println!("Program compiled to: {}", output.display());
    }

    Ok(())
}

fn validate_ve_file(path: &str) -> Result<PathBuf, String> {
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