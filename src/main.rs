extern crate codespan;
extern crate codespan_reporting;

mod ast;
mod lexer;
mod parser;
mod typeck;
mod codegen;
mod utils;
mod cli;

use anyhow::Result;

fn main() -> Result<()> {
    match cli::parse() {
        Ok(cli::CliCommand::Build { input, output, optimize, target_triple, verbose }) => {
            cli::process_build(input, output, optimize, target_triple, verbose)
        }
        Ok(cli::CliCommand::Init { directory, project_name }) => {
            cli::init::create_project(&*directory, &*project_name)
        }
        Ok(cli::CliCommand::Run { verbose }) => {
            cli::run::run_project(verbose)
        }
        Ok(cli::CliCommand::Benchmark { input, iterations, verbose }) => {
            cli::benchmark::run_benchmark(input, iterations, verbose)
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}
