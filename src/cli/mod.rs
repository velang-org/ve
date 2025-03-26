pub mod init;
pub(crate) mod run;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use anyhow::{anyhow, Context};
use codespan::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use crate::{codegen, lexer, parser, typeck};
use crate::utils::{process_imports, prepare_windows_clang_args, validate_ve_file};

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
    },
    Init {
        directory: PathBuf,
        project_name: String,
    },
    Run {
        verbose: bool,
    }
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
        #[arg(short, long)]
        verbose: bool,
    }
}

pub fn parse() -> anyhow::Result<CliCommand> {
    let args = Args::parse();

    match args.command {
        Some(Command::Build { input, output, optimize, target_triple, verbose }) => {
            Ok(CliCommand::Build {
                input,
                output,
                optimize,
                target_triple,
                verbose,
            })
        }
        Some(Command::Init { directory, project_name }) => {
            Ok(CliCommand::Init { directory, project_name })
        },
        Some(Command::Run { verbose}) => {
            Ok(CliCommand::Run { verbose })
        }
        None => {
            let input = args.input.ok_or_else(|| anyhow!("Input file is required"))?;
            Ok(CliCommand::Build {
                input,
                output: args.output,
                optimize: args.optimize,
                target_triple: args.target_triple,
                verbose: args.verbose,
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
) -> anyhow::Result<()> {
    let build_dir = input.parent()
        .ok_or_else(|| anyhow!("Invalid input file path"))?
        .join("build");

    if build_dir.exists() {
        if verbose {
            println!("Cleaning build directory: {}", build_dir.display());
        }
        std::fs::remove_dir_all(&build_dir)?;
    }
    std::fs::create_dir_all(&build_dir)?;

    let output = build_dir.join(output.file_name().unwrap());

    let c_file = build_dir.join("temp.c");

    let mut files = Files::<String>::new();
    let file_id = files.add(
        input.to_str().unwrap().to_string(),
        std::fs::read_to_string(input.clone())
            .with_context(|| format!("Reading input file {}", input.display()))?,
    );

    if verbose {
        println!("Input file: {}", input.display());
        println!("Output file: {}", output.display());
        println!("Build directory: {}", build_dir.display());
    }

    let lexer = lexer::Lexer::new(&files, file_id);
    let mut parser = parser::Parser::new(lexer);
    let mut program = match parser.parse() {
        Ok(program) => program,
        Err(error) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &error)?;
            return Err(anyhow!("Parser failed"));
        }
    };


    let (imported_functions, imported_asts) = process_imports(&mut files, &program.imports, &*input)?;
    program.functions.extend(imported_asts);

    if verbose {
        println!("Parsed AST:\n{:#?}", program);
    }

    let mut type_checker = typeck::TypeChecker::new(file_id, imported_functions.clone());
    match type_checker.check(&mut program) {
        Ok(()) => (),
        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            for error in errors {
                term::emit(&mut writer.lock(), &config, &files, &error)?;
            }
            return Err(anyhow!("Type check failed"));
        }
    }

    let config = codegen::CodegenConfig { target_triple };
    let mut target = codegen::Target::create(config, file_id, imported_functions);

    target.compile(&program, &c_file)?;

    let clang_args = prepare_windows_clang_args(&output, optimize, &c_file )?;

    let status = std::process::Command::new("clang")
        .args(&clang_args)
        .status()
        .context("Failed to execute C compiler")?;

    if !status.success() {
        return Err(anyhow!("C compilation failed"));
    }

    let status = std::process::Command::new(output.clone())
        .status()
        .context("Failed to execute program")?;

    if !status.success() {
        return Err(anyhow!("Program failed with exit code: {}", status));
    }

    if !verbose {
        std::fs::remove_file(&c_file)?;
    }
    Ok(())
}