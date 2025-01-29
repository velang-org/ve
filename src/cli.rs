use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(
    version,
    about,
    args_conflicts_with_subcommands = true,
    subcommand_negates_reqs = true
)]
pub struct Args {
    #[command(subcommand)]
    pub command: Option<Command>,

    /// Input file to compile (shorthand syntax)
    #[arg(required = true)]
    pub input: Option<PathBuf>,

    /// Output executable path
    #[arg(short, long, default_value = "program.exe")]
    pub output: PathBuf,

    /// Disable optimizations
    #[arg(long, action = clap::ArgAction::SetFalse)]
    pub optimize: bool,

    /// Target triple for code generation
    #[arg(long, default_value = "x86_64-pc-windows-msvc")]
    pub target_triple: String,

    /// Show verbose output
    #[arg(short, long)]
    pub verbose: bool,
}

#[derive(Subcommand)]
pub enum Command {
    /// Compile and run a Verve program
    Run {
        /// Input file to compile
        input: PathBuf,

        /// Output executable path
        #[arg(short, long, default_value = "program.exe")]
        output: PathBuf,

        /// Disable optimizations
        #[arg(long, action = clap::ArgAction::SetFalse)]
        optimize: bool,

        /// Target triple for code generation
        #[arg(long, default_value = "x86_64-pc-windows-msvc")]
        target_triple: String,

        /// Show verbose output
        #[arg(short, long)]
        verbose: bool,
    },
}