use std::path::PathBuf;
use std::process::Command;

fn get_compiler_path() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_Ve"))
}

fn test_file_path(relative_path: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("integration")
        .join(relative_path)
}

fn run_compiler(relative_path: &str) -> (bool, String, String) {
    let compiler = get_compiler_path();
    let input_path = test_file_path(relative_path);

    let output = Command::new(compiler)
        .arg(input_path)
        .output()
        .expect("Failed to run compiler");

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();

    (output.status.success(), stdout, stderr)
}

#[test]
fn test_valid_program_compilation() {
    let (success, stdout, stderr) = run_compiler("valid/arithmetic.ve");

    assert!(
        success,
        "Compilation failed\nSTDOUT:\n{}\nSTDERR:\n{}",
        stdout, stderr
    );

    assert!(
        stdout.contains("Program compiled"),
        "Missing success message\nSTDOUT:\n{}",
        stdout
    );
}

#[test]
fn test_invalid_program() {
    let (success, stdout, stderr) = run_compiler("invalid/type_mismatch.ve");

    assert!(
        !success,
        "Invalid program compiled successfully\nSTDOUT:\n{}\nSTDERR:\n{}",
        stdout, stderr
    );

    assert!(
        stderr.contains("Type error"),
        "Missing type error message\nSTDERR:\n{}",
        stderr
    );
}