use crate::cli::process_build;

pub fn run_project(verbose: bool) -> anyhow::Result<()> {
    process_build(
        "src/main.ve".into(),
        "build/output.c".into(),
        false,
        "x86_64-pc-windows-msvc".into(),
        verbose,
    )
}