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
    let should_check_updates = std::env::args().nth(1).as_deref() != Some("upgrade");
    
    if should_check_updates {
        let _ = cli::upgrade::check_and_notify_updates();
    }

    match cli::parse() {
        Ok(cli::CliCommand::Build { input, output, optimize, target_triple, verbose }) => {
            cli::process_build(input, output, optimize, target_triple, verbose)
        }
        Ok(cli::CliCommand::Init { directory, project_name }) => {
            cli::init::create_project(&*directory, &*project_name)
        }
        Ok(cli::CliCommand::Run { input, verbose }) => {
            cli::run::run_project(input, verbose)
        }
        Ok(cli::CliCommand::Benchmark { input, iterations, verbose }) => {
            cli::benchmark::run_benchmark(input, iterations, verbose)
        }        Ok(cli::CliCommand::Upgrade { no_remind, force, verbose }) => {
            cli::upgrade::run_upgrade(no_remind, force, verbose)
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}
