mod cli;
mod utils;
mod ast;
mod lexer;
mod parser;
mod typeck;
mod codegen;


use anyhow::Context;

use crate::cli::parse;
use crate::cli::CliCommand;
use crate::utils::check_dependencies;

fn main() -> anyhow::Result<()> {
    check_dependencies().context("Dependency check failed")?;

    match parse()? {
        CliCommand::Build { input, output, optimize, target_triple, verbose, use_cranelift } => {
            cli::process_build(input, output, optimize, target_triple, verbose, use_cranelift)
        }
        CliCommand::Init { directory, project_name } => {
            cli::init::create_project(&directory, &project_name)
        },
        CliCommand::Run { verbose } => {
            cli::run::run_project(verbose)
        },
        CliCommand::Benchmark { input, iterations, verbose, use_cranelift } => {
            cli::benchmark::run_benchmark(input, iterations, verbose, use_cranelift)
        }
    }?;

    Ok(())
}




