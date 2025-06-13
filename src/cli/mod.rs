pub mod benchmark;
pub mod init;
pub(crate) mod run;
pub mod upgrade;
pub mod test;

use crate::compiler::incremental::IncrementalCompiler;
#[cfg(target_os = "windows")]
use crate::helpers::{prepare_windows_clang_args, validate_ve_file};
#[cfg(not(target_os = "windows"))]
use crate::utils::validate_ve_file;
use crate::{codegen, typeck};
use anyhow::anyhow;
use clap::{Parser, Subcommand};
use codespan::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::path::PathBuf;
use colored::*;
use crate::cli::upgrade::Channel;

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
        input: PathBuf,
        verbose: bool,
    },
    Benchmark {
        input: PathBuf,
        iterations: usize,
        verbose: bool,
    },
    Upgrade {
        no_remind: bool,
        force: bool,
        verbose: bool,
        channel: Channel,
    },
    Test {
        input: PathBuf,
        test_name: Option<String>,
        verbose: bool,
        list: bool,
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
        #[arg(value_parser = validate_ve_file)]
        input: PathBuf,
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
    },
    Upgrade {
        #[arg(long, help = "Disable update reminder notifications")]
        no_remind: bool,
        #[arg(short, long, help = "Force upgrade without confirmation")]
        force: bool,
        #[arg(short, long, help = "Show verbose output during upgrade")]
        verbose: bool,
        #[arg(long, help = "Update channel: stable or canary", value_parser = parse_channel)]
        channel: Option<crate::cli::upgrade::Channel>,
    },
    Test {
       #[arg(value_parser = validate_ve_file)]
       input: PathBuf,
       #[arg(short, long)]
       test_name: Option<String>,
       #[arg(short, long)]
       verbose: bool,
       #[arg(long, help = "List available tests")]
       list: bool,
    }
}

fn parse_channel(s: &str) -> Result<Channel, String> {
    match s.to_lowercase().as_str() {
        "stable" => Ok(Channel::Stable),
        "canary" => Ok(Channel::Canary),
        _ => Err(format!("Invalid channel '{}'. Valid options: stable, canary", s)),
    }
}

pub fn parse() -> anyhow::Result<CliCommand> {
    let args = Args::parse();

    match args.command {
        Some(Command::Build {
            input,
            output,
            optimize,
            target_triple,
            verbose,
        }) => Ok(CliCommand::Build {
            input,
            output,
            optimize,
            target_triple,
            verbose,
        }),
        Some(Command::Init {
            directory,
            project_name,
        }) => Ok(CliCommand::Init {
            directory,
            project_name,
        }),
        Some(Command::Run { input, verbose }) => Ok(CliCommand::Run { input, verbose }),
        Some(Command::Benchmark {
            input,
            iterations,
            verbose,
        }) => Ok(CliCommand::Benchmark {
            input,
            iterations,
            verbose,
        }),
        Some(Command::Upgrade {
            no_remind,
            force,
            verbose,
            channel,
        }) => Ok(CliCommand::Upgrade {
            no_remind,
            force,
            verbose,
            channel: channel.unwrap_or_default(),
        }),
        Some(Command::Test {
            input,
            test_name,
            verbose,
            list,
        }) => Ok(CliCommand::Test {
            input,
            test_name,
            verbose,
            list,
        }),
        None => {
            let input = args
                .input
                .ok_or_else(|| anyhow!("Input file is required"))?;
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
    is_test: bool,
) -> anyhow::Result<PathBuf> {
    let build_dir = input
        .parent()
        .ok_or_else(|| anyhow!("Invalid input file path"))?
        .join("build");

    if build_dir.exists() {
        if verbose {
            println!("{}", format!("Cleaning build directory: {}", build_dir.display()).yellow());
        }
        
        let cache_dir = build_dir.join(".cache");
        let cache_backup = if cache_dir.exists() {
            let temp_cache = std::env::temp_dir().join(format!("veil_cache_backup_{}", std::process::id()));
            std::fs::rename(&cache_dir, &temp_cache)?;
            Some(temp_cache)
        } else {
            None
        };
        
        std::fs::remove_dir_all(&build_dir)?;
        std::fs::create_dir_all(&build_dir)?;
        
        if let Some(temp_cache) = cache_backup {
            std::fs::rename(&temp_cache, &cache_dir)?;
            if verbose {
                println!("{}", "Cache preserved".green());
            }
        }
    } else {
        std::fs::create_dir_all(&build_dir)?;
    }

    let output = build_dir.join(output.file_name().unwrap());
    let c_file = build_dir.join("temp.c");

    let mut files = Files::<String>::new();
    let mut module_compiler = IncrementalCompiler::new(&build_dir);

    if verbose {
        println!("{}", "Discovering modules and building dependency graph...".yellow());
    }

    module_compiler.build_dependency_graph(&input)?;

    if verbose {
        println!("{}", "Compiling modules incrementally...".yellow());
    }

    let compiled_modules = module_compiler.compile_all_modules(&mut files, verbose)?;

    if verbose && !compiled_modules.is_empty() {
        println!("{} modules were recompiled", compiled_modules.len());
    }

    if verbose {
        println!("{}", "Creating merged program...".yellow());
    }

    let mut program = module_compiler.create_merged_program(&input)?;

    if verbose {
        println!("{}", format!("Input file: {}", input.display()).cyan());
        println!("{}", format!("Output file: {}", output.display()).cyan());
        println!("{}", format!("Build directory: {}", build_dir.display()).cyan());
    }

    if verbose {
        let ast_file = build_dir.join("parsed_ast.txt");
        let ast_content = format!("Parsed AST:\n{:#?}", program);
        std::fs::write(&ast_file, ast_content)?;
        println!("{}", format!("Parsed AST saved to: {}", ast_file.display()).green());
    }

    let (imported_functions, imported_structs, imported_ffi_vars) = module_compiler.get_imported_info()?;
    let file_id = module_compiler.get_entry_file_id(&mut files, &input)?;

    let mut type_checker = typeck::TypeChecker::new(
        file_id,
        imported_functions.clone(),
        imported_structs.clone(),
        imported_ffi_vars.clone(),
    );
    match type_checker.check(&mut program) {
        Ok(()) => (),
        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();

            println!("=== TYPE CHECKER ERRORS ===");
            println!("Found {} errors", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {}", i + 1, error.message);
                
                if let Some(label) = error.labels.get(0) {
                    println!("  Location: {}..{}", label.range.start, label.range.end);
                    println!("  Detail: {}", label.message);
                }
                
                for note in &error.notes {
                    println!("  Note: {}", note);
                }
                
                if let Err(emit_err) = term::emit(&mut writer.lock(), &config, &files, &error) {
                    println!("  (Failed to emit formatted error: {})", emit_err);
                }

                let file_id = error.labels.get(0).map(|l| l.file_id);
                let file_path = file_id.map(|fid| files.name(fid).to_string_lossy().to_string());
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

                let location = match file_path {
                    Some(ref path) => {
                        match module_info {
                            Some(ref module) => format!("in file '{}' ({})", path, module),
                            None => format!("in file '{}'", path),
                        }
                    }
                    None => "in unknown location".to_string(),
                };

                eprintln!("\nType checker error {}: {}", location, error.message);

                if let Some(label) = error.labels.get(0) {
                    eprintln!("  --> at {}..{}", label.range.start, label.range.end);
                    eprintln!("  = detail: {}", label.message);
                }

                for note in &error.notes {
                    eprintln!("  note: {}", note);
                }
                
                println!(""); 
            }

            return Err(anyhow!("Type check failed"));
        }
    }

    let config = codegen::CodegenConfig { target_triple };
    let mut target = codegen::Target::create(
        config,
        file_id,
        imported_functions,
        imported_structs,
        program.ffi_variables.clone(),
        is_test,
    );

    target.compile(&program, &c_file)?;

    if verbose {
        println!("{}", format!("Compiling generated C code: {}", c_file.display()).yellow());
    }

    #[cfg(target_os = "windows")]
    let mut clang_args = prepare_windows_clang_args(&output, optimize, &c_file)?;

    #[cfg(not(target_os = "windows"))]
    let mut clang_args = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

    if verbose {
        clang_args.push("-DVE_DEBUG_MEMORY".to_string());
    }

    let output_result = std::process::Command::new("clang")
        .args(&clang_args)
        .output()
        .or_else(|_| std::process::Command::new("gcc").args(&clang_args).output())
        .map_err(|e| anyhow!("Failed to compile C code: {}", e))?;

    if !output_result.status.success() {
        let stderr = String::from_utf8_lossy(&output_result.stderr);
        for line in stderr.lines() {
            if line.contains("error:") || line.contains("fatal error:") {
                eprintln!("{}", line);
            }
        }
        return Err(anyhow!("C compiler failed with status: {}", output_result.status));
    }

    if verbose {
        println!("{}", format!("Successfully compiled to: {}", output.display()).green());
    }

    let artifacts = vec![output.clone(), c_file.clone()];
    let entry_dependencies = module_compiler.collect_module_dependencies(&program.imports, &input)?;
    module_compiler.cache_compilation_artifacts(&input, entry_dependencies, artifacts)?;

    if verbose {
        println!("{}", "Artifacts cached successfully".green());
    }

    if is_test {
        return Ok(output);
    }

    if verbose {
        println!("{}", "Running the compiled program...".bold().blue());
    }

    let status = std::process::Command::new(output.to_str().unwrap())
        .status()
        .map_err(|e| anyhow!("Failed to run program: {}", e))?;

    if verbose {
        println!("{}", format!("Program exited with status: {}", status).magenta());
    }

    Ok(output)
}
