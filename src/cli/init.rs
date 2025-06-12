use anyhow::anyhow;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::Path;

pub fn create_project(directory: &Path, project_name: &str) -> anyhow::Result<()> {
    let project_path = directory.join(project_name);

    if project_path.exists() {
        return Err(anyhow!(
            "Directory '{}' already exists",
            project_path.display()
        ));
    }

    fs::create_dir_all(project_path.join("src"))?;

    generate_ve_toml(&project_path, project_name)?;
    generate_main_ve(&project_path)?;

    println!(
        "Created project at: {}\nRun `cd {}` to enter the project directory",
        project_path.display(),
        project_path.display()
    );

    Ok(())
}

fn generate_ve_toml(directory: &Path, project_name: &str) -> anyhow::Result<()> {
    let ve_toml_path = directory.join("ve.toml");

    if ve_toml_path.exists() {
        return Err(anyhow!("File '{}' already exists", ve_toml_path.display()));
    }

    const VE_TOML_TEMPLATE: &str = r#"
[package]
name = "{project_name}"
version = "{version}"
description = "{description}"

[dependencies]
# Add dependencies here
"#;

    let content = VE_TOML_TEMPLATE
        .replace("{project_name}", project_name)
        .replace("{version}", "0.1.0")
        .replace("{description}", "A new Veil project");

    let mut file =
        File::create(&ve_toml_path).map_err(|e| anyhow!("Failed to create ve.toml: {}", e))?;

    file.write_all(content.as_bytes())
        .map_err(|e| anyhow!("Failed to write to ve.toml: {}", e))?;

    Ok(())
}

fn generate_main_ve(directory: &Path) -> anyhow::Result<()> {
    let main_ve_path = directory.join("src/main.ve");

    if main_ve_path.exists() {
        return Err(anyhow!("File '{}' already exists", main_ve_path.display()));
    }

    const MAIN_VE_TEMPLATE: &str = r#"
fn main() {
    print("Hello, world!");
}
"#;

    let mut file =
        File::create(&main_ve_path).map_err(|e| anyhow!("Failed to create src/main.ve: {}", e))?;

    file.write_all(MAIN_VE_TEMPLATE.as_bytes())
        .map_err(|e| anyhow!("Failed to write to src/main.ve: {}", e))?;

    Ok(())
}
