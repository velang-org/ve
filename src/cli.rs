use clap::{Parser, Subcommand};
use std::path::{Path, PathBuf};

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
    let target_name = target_name.as_ref(); // Convert Cow<str> to &str

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

#[derive(Parser)]
#[command(
    version,
    about,
    args_conflicts_with_subcommands = true,
    subcommand_negates_reqs = true,
    disable_help_subcommand = true,
    help_expected = false
)]
pub struct Args {
    #[command(subcommand)]
    pub command: Option<Command>,

    /// Input file to compile (shorthand syntax)
    #[arg(
        required = true,
        value_parser = validate_ve_file,
        value_name = "FILE[.ve]"
    )]
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
    Build {
        /// Input file to compile
        #[arg(value_parser = validate_ve_file)]
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
    }
}