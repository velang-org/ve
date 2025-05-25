pub mod init;
pub(crate) mod run;
pub mod benchmark;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use anyhow::{anyhow, Context};
use codespan::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use crate::{codegen, lexer, parser, typeck};
use crate::utils::{process_imports, validate_ve_file};
use std::process::Stdio;

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
    },
    Benchmark {
        input: PathBuf,
        iterations: usize,
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

    #[arg(long)]
    iterations: Option<usize>,
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
    },
    Benchmark {
        #[arg(value_parser = validate_ve_file)]
        input: PathBuf,
        
        #[arg(short, long, default_value_t = 10)]
        iterations: usize,
        
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
        },
        Some(Command::Benchmark { input, iterations, verbose }) => {
            Ok(CliCommand::Benchmark { input, iterations, verbose })
        }
        None => {
            let input = args.input.ok_or_else(|| anyhow!("Input file is required"))?;
            
            if let Some(iterations) = args.iterations {
                return Ok(CliCommand::Benchmark {
                    input,
                    iterations,
                    verbose: args.verbose,
                });
            }
            
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
            let file_id = error.labels.get(0).map(|l| l.file_id);
            let file_path = file_id.and_then(|fid| Some(files.name(fid))).map(|n| n.to_string_lossy().to_string());
            let module_info = file_path.as_ref().and_then(|path: &String| {
                if let Some(_idx) = path.find("lib/std") {
                    Some("standard library".to_string())
                } else if let Some(lib_start) = path.find("lib/") {
                    let rest = &path[lib_start + 4..];
                    if let Some(end) = rest.find('/') {
                        let lib_name = &rest[..end];
                        if lib_name != "std" {
                            return Some(format!("external library '{}'", lib_name));
                        }
                    }
                    None
                } else if let Some(ex_start) = path.find("examples/") {
                    let rest = &path[ex_start + 9..];
                    if let Some(end) = rest.find('/') {
                        let ex_name = &rest[..end];
                        return Some(format!("example module '{}'", ex_name));
                    }
                    None
                } else {
                    None
                }
            });

            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &error)?;

            let location = if let Some(path) = file_path {
                match module_info {
                    Some(module) => format!("in file '{}' ({})", path, module),
                    None => format!("in file '{}'", path),
                }
            } else {
                "in unknown location".to_string()
            };

            eprintln!("\nParser error {}: {}", location, error.message);

            if let Some(label) = error.labels.get(0) {
                eprintln!("  --> at {}..{}", label.range.start, label.range.end);
                eprintln!("  = detail: {}", label.message);
            }

            for note in &error.notes {
                eprintln!("  note: {}", note);
            }

            return Err(anyhow!("Parser failed"));
        }
    };

    let (imported_functions, imported_asts, imported_structs, imported_ffi_funcs, imported_ffi_vars) = 
        process_imports(&mut files, &program.imports, &input)?;
    program.functions.extend(imported_asts);
    program.ffi_functions.extend(imported_ffi_funcs);
    program.ffi_variables.extend(imported_ffi_vars.clone());

    if verbose {
        println!("Parsed AST:\n{:#?}", program);
    }

    let mut type_checker = typeck::TypeChecker::new(file_id, imported_functions.clone(), imported_structs.clone(), imported_ffi_vars.clone());
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
    let mut target = codegen::Target::create(config, file_id, imported_functions, imported_structs, program.ffi_variables.clone());

    target.compile(&program, &c_file)?;

    if verbose {
        println!("Compiling generated C code: {}", c_file.display());
    }

    #[cfg(target_os = "windows")]
    let clang_args = prepare_windows_clang_args(&output, optimize, &c_file)?;

    #[cfg(not(target_os = "windows"))]
    let clang_args = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

    let status = std::process::Command::new("clang")
        .args(&clang_args)
        .stderr(Stdio::null())
        .status()
        .or_else(|_| {
            std::process::Command::new("gcc")
                .args(&clang_args)
                .stderr(Stdio::null())
                .status()
        })
        .map_err(|e| anyhow!("Failed to compile C code: {}", e))?;

    if !status.success() {
        return Err(anyhow!("C compiler failed with status: {}", status));
    }

    if verbose {
        println!("Successfully compiled to: {}", output.display());
    }

    if verbose {
        println!("Running the compiled program...");
    }

    let status = std::process::Command::new(output.to_str().unwrap())
        .status()
        .map_err(|e| anyhow!("Failed to run program: {}", e))?;

    if verbose {
        println!("Program exited with status: {}", status);
    }

    Ok(())
}
