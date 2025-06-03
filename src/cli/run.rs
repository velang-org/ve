use crate::cli::process_build;

pub fn run_project(input: std::path::PathBuf, verbose: bool) -> anyhow::Result<()> {
    process_build(
        input,
        "build/output.c".into(),
        false,
        "x86_64-pc-windows-msvc".into(),
        verbose,
    )
}
