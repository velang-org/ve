mod cli;
mod utils;
mod ast;
mod lexer;
mod parser;
mod typeck;
mod codegen;

use anyhow::{anyhow, Context};
use clap::Parser;


use crate::cli::parse;
use crate::cli::CliCommand;
use crate::utils::check_dependencies;

fn main() -> anyhow::Result<()> {
    check_dependencies().context("Dependency check failed")?;

    match parse()? {
        CliCommand::Build { input, output, optimize, target_triple, verbose, is_temp } => {
            cli::process_build(input, output, optimize, target_triple, verbose, is_temp)
        }
        CliCommand::Init { directory, project_name } => {
            cli::init::create_project(&directory, &project_name)
        },
        CliCommand::Run => {
            cli::run::run_project()
        }
    }?;

    Ok(())
}




