use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};

const REPO_URL: &str = "https://github.com/velang-org/ve.git";
const CURRENT_VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Serialize, Deserialize)]
struct UpdateConfig {
    last_check: Option<String>,
    #[serde(default = "default_remind_updates")]
    remind_updates: bool,
}

impl Default for UpdateConfig {
    fn default() -> Self {
        Self {
            last_check: None,
            remind_updates: true,
        }
    }
}

fn default_remind_updates() -> bool {
    true
}

fn get_config_path() -> Result<PathBuf> {
    let config_dir = dirs::config_dir()
        .ok_or_else(|| anyhow!("Could not find config directory"))?;
    Ok(config_dir.join("velang").join("config.json"))
}

fn load_config() -> UpdateConfig {
    let config_path = match get_config_path() {
        Ok(path) => path,
        Err(_) => return UpdateConfig::default(),
    };

    if !config_path.exists() {
        return UpdateConfig::default();
    }

    match fs::read_to_string(&config_path) {
        Ok(content) => serde_json::from_str(&content).unwrap_or_default(),
        Err(_) => UpdateConfig::default(),
    }
}

fn save_config(config: &UpdateConfig) -> Result<()> {
    let config_path = get_config_path()?;
    
    if let Some(parent) = config_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let content = serde_json::to_string_pretty(config)?;
    fs::write(config_path, content)?;
    Ok(())
}

fn check_for_updates() -> Result<Option<String>> {
    let temp_dir = std::env::temp_dir().join(format!("velang_check_{}", std::process::id()));
    fs::create_dir_all(&temp_dir)?;
    
    let git_status = Command::new("git")
        .args(&["clone", "--depth", "1", REPO_URL, temp_dir.to_str().unwrap()])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()?;
    
    if !git_status.success() {
        fs::remove_dir_all(&temp_dir).ok();
        return Err(anyhow!("Failed to check for updates: cannot access repository"));
    }
    
    let cargo_toml_path = temp_dir.join("Cargo.toml");
    let cargo_content = fs::read_to_string(&cargo_toml_path)?;
    
    fs::remove_dir_all(&temp_dir).ok();
    
    for line in cargo_content.lines() {
        if line.trim().starts_with("version = ") {
            if let Some(version_str) = line.split('"').nth(1) {
                if version_str != CURRENT_VERSION {
                    return Ok(Some(version_str.to_string()));
                } else {
                    return Ok(None);
                }
            }
        }
    }
    
    Ok(None)
}

pub fn check_and_notify_updates() -> Result<()> {
    let mut config = load_config();
    
    if !config.remind_updates {
        return Ok(());
    }

    let today = chrono::Utc::now().date_naive().to_string();
    if config.last_check.as_ref() == Some(&today) {
        return Ok(());
    }

    match check_for_updates() {
        Ok(Some(new_version)) => {
            println!("🎉 New VeLang version available: v{} (current: v{})", new_version, CURRENT_VERSION);
            println!("   Run 've upgrade' to update");
            println!("   Use 've upgrade --no-remind' to disable these notifications");
        },
        Ok(None) => {

        },
        Err(_) => {
            return Ok(());
        }
    }

    config.last_check = Some(today);
    let _ = save_config(&config);

    Ok(())
}

pub fn run_upgrade(no_remind: bool, force: bool) -> Result<()> {
    if no_remind {
        let mut config = load_config();
        config.remind_updates = false;
        save_config(&config)?;
        println!("✅ Update notifications disabled");
        return Ok(());
    }

    println!("🔍 Checking for updates...");
    
    match check_for_updates()? {
        Some(new_version) => {
            println!("📦 Found new version: v{} (current: v{})", new_version, CURRENT_VERSION);
            
            if !force {
                print!("Do you want to upgrade? (y/N): ");
                std::io::Write::flush(&mut std::io::stdout())?;
                
                let mut input = String::new();
                std::io::stdin().read_line(&mut input)?;
                
                if !input.trim().to_lowercase().starts_with('y') {
                    println!("Upgrade cancelled");
                    return Ok(());
                }
            }
            
            println!("🚀 Starting upgrade...");
            upgrade_velang()?;
            println!("✅ VeLang upgraded successfully to v{}", new_version);
        },
        None => {
            println!("✅ You're already using the latest version (v{})", CURRENT_VERSION);
        }
    }

    Ok(())
}

fn upgrade_velang() -> Result<()> {
    println!("📥 Downloading latest VeLang source...");
    
    let temp_dir = std::env::temp_dir().join(format!("velang_upgrade_{}", std::process::id()));
    fs::create_dir_all(&temp_dir)?;
    
    let git_status = Command::new("git")
        .args(&["clone", REPO_URL, temp_dir.to_str().unwrap()])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()?;
    
    if !git_status.success() {
        return Err(anyhow!("Failed to clone VeLang repository"));
    }

    println!("🔨 Building new version...");
    
    let build_status = Command::new("cargo")
        .args(&["build", "--release", "--quiet"])
        .current_dir(&temp_dir)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()?;
    
    if !build_status.success() {
        fs::remove_dir_all(&temp_dir)?;
        return Err(anyhow!("Failed to build new VeLang version"));
    }

    println!("📦 Installing new version...");
    
    let install_status = Command::new("cargo")
        .args(&["install", "--path", ".", "--force", "--quiet"])
        .current_dir(&temp_dir)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()?;
    if !install_status.success() {
        fs::remove_dir_all(&temp_dir)?;
        return Err(anyhow!("Failed to install new VeLang version"));
    }
    fs::remove_dir_all(&temp_dir)?;
    Ok(())
}
