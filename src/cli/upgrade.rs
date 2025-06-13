use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};

const REPO_URL: &str = "https://github.com/veil-lang/veil.git";
const CURRENT_VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Clone, PartialEq)]
pub enum Channel {
    Stable,
    Canary,
}

impl Channel {
    pub fn branch(&self) -> &'static str {
        match self {
            Channel::Stable => "main",
            Channel::Canary => "canary",
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Channel::Stable => "stable",
            Channel::Canary => "canary",
        }
    }
}

impl Default for Channel {
    fn default() -> Self {
        Channel::Stable
    }
}

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
    let config_dir =
        dirs::config_dir().ok_or_else(|| anyhow!("Could not find config directory"))?;
    Ok(config_dir.join("veil").join("config.json"))
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

fn check_for_updates(channel: &Channel) -> Result<Option<String>> {
    let temp_dir = std::env::temp_dir().join(format!("veil_check_{}", std::process::id()));
    fs::create_dir_all(&temp_dir)?;

    let git_status = Command::new("git")
        .args(&[
            "clone",
            "--depth",
            "1",
            "--branch",
            channel.branch(),
            REPO_URL,
            temp_dir.to_str().unwrap(),
        ])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()?;

    if !git_status.success() {
        fs::remove_dir_all(&temp_dir).ok();
        return Err(anyhow!(
            "Failed to check for updates: cannot access repository"
        ));
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

    match check_for_updates(&Channel::default()) {
        Ok(Some(new_version)) => {
            println!(
                "🎉 New version available: v{} (current: v{})",
                new_version, CURRENT_VERSION
            );
            println!("   Run 've upgrade' to update");
            println!("   Use 've upgrade --no-remind' to disable these notifications");
        }
        Ok(None) => {}
        Err(_) => {
            return Ok(());
        }
    }

    config.last_check = Some(today);
    let _ = save_config(&config);

    Ok(())
}

pub fn run_upgrade(no_remind: bool, force: bool, verbose: bool, channel: Channel) -> Result<()> {
    if no_remind {
        let mut config = load_config();
        config.remind_updates = false;
        save_config(&config)?;
        println!("✅ Update notifications disabled");
        return Ok(());
    }

    println!("🔍 Checking for updates on {} channel...", channel.name());

    match check_for_updates(&channel)? {
        Some(new_version) => {
            println!(
                "📦 Found new version: v{} (current: v{}) on {} channel",
                new_version, CURRENT_VERSION, channel.name()
            );

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
            upgrade_veil(verbose, &channel)?;
            println!("✅ Veil upgraded successfully to v{}", new_version);
        }
        None => {
            println!(
                "✅ You're already using the latest version (v{}) on {} channel",
                CURRENT_VERSION, channel.name()
            );
        }
    }

    Ok(())
}

fn upgrade_veil(verbose: bool, channel: &Channel) -> Result<()> {
    println!("📥 Downloading latest Veil source...");

    let temp_dir = std::env::temp_dir().join(format!("veil_upgrade_{}", std::process::id()));
    if verbose {
        println!("   Using temporary directory: {}", temp_dir.display());
    }
    fs::create_dir_all(&temp_dir)?;

    let mut git_cmd = Command::new("git");
    git_cmd.args(&["clone", "--branch", channel.branch(), REPO_URL, temp_dir.to_str().unwrap()]);

    if verbose {
        println!("   Running: git clone --branch {} {} {}", channel.branch(), REPO_URL, temp_dir.display());
    } else {
        git_cmd.stdout(Stdio::null()).stderr(Stdio::null());
    }

    let git_status = git_cmd.status()?;

    if !git_status.success() {
        return Err(anyhow!("Failed to clone Veil repository"));
    }

    println!("🔨 Building new version...");

    let mut build_cmd = Command::new("cargo");
    if verbose {
        build_cmd.args(&["build", "--release"]);
        println!("   Running: cargo build --release");
    } else {
        build_cmd.args(&["build", "--release", "--quiet"]);
        build_cmd.stdout(Stdio::null()).stderr(Stdio::null());
    }

    let build_status = build_cmd.current_dir(&temp_dir).status()?;

    if !build_status.success() {
        fs::remove_dir_all(&temp_dir)?;
        return Err(anyhow!("Failed to build new Veil version"));
    }

    println!("📦 Installing new version...");

    let install_dir = get_veil_install_dir()?;
    if verbose {
        println!("   Installing to: {}", install_dir.display());
    }

    fs::create_dir_all(&install_dir)?;
    let source_exe = temp_dir
        .join("target")
        .join("release")
        .join(if cfg!(windows) { "ve.exe" } else { "ve" });
    let target_exe = install_dir.join(if cfg!(windows) { "ve.exe" } else { "ve" });    #[cfg(windows)]
    {
        if verbose {
            println!("   Preparing to replace executable...");
        }

        stop_other_veil_processes(verbose)?;

        if target_exe.exists() {
            let backup_exe = install_dir.join("ve_old.exe");

            if backup_exe.exists() {
                let _ = fs::remove_file(&backup_exe);
            }

            match fs::rename(&target_exe, &backup_exe) {
                Ok(_) => {
                    if verbose {
                        println!("   Moved current executable to backup");
                    }
                }
                Err(e) => {
                    if verbose {
                        println!("   Warning: Could not backup current executable: {}", e);
                    }
                }
            }
        }
    }

    #[cfg(not(windows))]
    {
        if verbose {
            println!("   Preparing to replace executable...");
        }

        stop_other_veil_processes_unix(verbose)?;

        if target_exe.exists() {
            let backup_exe = install_dir.join("ve_old");

            if backup_exe.exists() {
                let _ = fs::remove_file(&backup_exe);
            }

            match fs::rename(&target_exe, &backup_exe) {
                Ok(_) => {
                    if verbose {
                        println!("   Moved current executable to backup");
                    }
                }
                Err(e) => {
                    if verbose {
                        println!("   Warning: Could not backup current executable: {}", e);
                    }
                }
            }
        }
    }

    let mut retry_count = 0;
    const MAX_RETRIES: usize = 10;

    loop {
        match fs::copy(&source_exe, &target_exe) {
            Ok(_) => {
                if verbose {
                    println!("   Executable copied successfully");
                }
                break;
            }
            Err(e) if retry_count < MAX_RETRIES => {
                retry_count += 1;
                if verbose {
                    println!(
                        "   Copy attempt {} failed, retrying in {}ms... ({})",
                        retry_count,
                        retry_count * 500,
                        e
                    );
                }

                std::thread::sleep(std::time::Duration::from_millis((retry_count * 500) as u64));                #[cfg(windows)]
                {
                    if retry_count == 3 || retry_count == 6 {
                        let _ = stop_other_veil_processes(false);
                    }

                    if retry_count == 5 {
                        if target_exe.exists() {
                            let temp_name =
                                install_dir.join(format!("ve_temp_{}.exe", std::process::id()));
                            let _ = fs::rename(&target_exe, &temp_name);
                        }
                    }
                }

                #[cfg(not(windows))]
                {
                    if retry_count == 3 || retry_count == 6 {
                        let _ = stop_other_veil_processes_unix(false);
                    }

                    if retry_count == 5 {
                        if target_exe.exists() {
                            let temp_name =
                                install_dir.join(format!("ve_temp_{}", std::process::id()));
                            let _ = fs::rename(&target_exe, &temp_name);
                        }
                    }
                }
            }
            Err(e) => {
                fs::remove_dir_all(&temp_dir)?;
                return Err(anyhow!(
                    "Failed to copy executable after {} attempts: {}\n\nThis usually happens when Veil is still running. Please:\n1. Close all Veil processes\n2. Wait a few seconds\n3. Try the upgrade again",
                    MAX_RETRIES,
                    e
                ));
            }
        }
    }    #[cfg(windows)]
    {
        let backup_exe = install_dir.join("ve_old.exe");
        if backup_exe.exists() {
            let _ = fs::remove_file(&backup_exe);
        }
    }

    #[cfg(not(windows))]
    {
        let backup_exe = install_dir.join("ve_old");
        if backup_exe.exists() {
            let _ = fs::remove_file(&backup_exe);
        }
    }

    let lib_source = temp_dir.join("lib");
    if lib_source.exists() {
        let lib_target = install_dir.join("lib");
        if lib_target.exists() {
            fs::remove_dir_all(&lib_target)?;
        }
        copy_dir_all(&lib_source, &lib_target)?;
        if verbose {
            println!("   Standard library copied");
        }
    } else if verbose {
        println!("   Warning: Standard library not found in source");
    }

    cleanup_cargo_installation(verbose)?;

    if verbose {
        println!("   Cleaning up temporary directory: {}", temp_dir.display());
    }
    fs::remove_dir_all(&temp_dir)?;
    Ok(())
}

fn get_veil_install_dir() -> Result<PathBuf> {
    if let Ok(home) = std::env::var("USERPROFILE") {
        let veil_dir = PathBuf::from(home).join(".veil");
        if veil_dir.exists() {
            return Ok(veil_dir);
        }
    }

    if let Ok(home) = std::env::var("HOME") {
        let veil_dir = PathBuf::from(home).join(".veil");
        if veil_dir.exists() {
            return Ok(veil_dir);
        }
    }

    let user_dir = if cfg!(windows) {
        std::env::var("USERPROFILE")
    } else {
        std::env::var("HOME")
    };

    match user_dir {
        Ok(dir) => Ok(PathBuf::from(dir).join(".veil")),
        Err(_) => Err(anyhow!("Could not determine user home directory")),
    }
}

fn copy_dir_all(src: &PathBuf, dst: &PathBuf) -> Result<()> {
    fs::create_dir_all(dst)?;

    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());

        if src_path.is_dir() {
            copy_dir_all(&src_path, &dst_path)?;
        } else {
            fs::copy(&src_path, &dst_path)?;
        }
    }

    Ok(())
}

fn cleanup_cargo_installation(verbose: bool) -> Result<()> {
    if let Ok(cargo_home) = std::env::var("CARGO_HOME") {
        let cargo_bin = PathBuf::from(cargo_home)
            .join("bin")
            .join(if cfg!(windows) { "ve.exe" } else { "ve" });
        if cargo_bin.exists() {
            if verbose {
                println!(
                    "   Removing old cargo installation: {}",
                    cargo_bin.display()
                );
            }
            let _ = fs::remove_file(&cargo_bin);
        }
    } else if let Ok(home) = std::env::var(if cfg!(windows) { "USERPROFILE" } else { "HOME" }) {
        let cargo_bin = PathBuf::from(home)
            .join(".cargo")
            .join("bin")
            .join(if cfg!(windows) { "ve.exe" } else { "ve" });
        if cargo_bin.exists() {
            if verbose {
                println!(
                    "   Removing old cargo installation: {}",
                    cargo_bin.display()
                );
            }
            let _ = fs::remove_file(&cargo_bin);
        }
    }

    Ok(())
}

#[cfg(windows)]
fn stop_other_veil_processes(verbose: bool) -> Result<()> {
    if verbose {
        println!("   Stopping any running Veil processes (except current upgrade)...");
    }

    let current_pid = std::process::id();

    let ps_command = format!(
        "Get-Process -Name 've' -ErrorAction SilentlyContinue | Where-Object {{ $_.Id -ne {} }} | ForEach-Object {{ $_.Id }}",
        current_pid
    );

    let output = Command::new("powershell")
        .args(&["-Command", &ps_command])
        .output();

    if let Ok(output) = output {
        let output_str = String::from_utf8_lossy(&output.stdout);
        let pids: Vec<u32> = output_str
            .lines()
            .filter_map(|line| line.trim().parse::<u32>().ok())
            .collect();

        if !pids.is_empty() {
            if verbose {
                println!(
                    "   Found {} process(es) to terminate: {:?}",
                    pids.len(),
                    pids
                );
            }

            for pid in pids {
                let kill_result = Command::new("taskkill")
                    .args(&["/F", "/PID", &pid.to_string()])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status();

                if verbose {
                    match kill_result {
                        Ok(status) if status.success() => {
                            println!("   Successfully terminated process {}", pid);
                        }
                        _ => {
                            println!(
                                "   Could not terminate process {} (may have already exited)",
                                pid
                            );
                        }
                    }
                }
            }

            std::thread::sleep(std::time::Duration::from_millis(1000));
        } else if verbose {
            println!("   No other Veil processes found running");
        }
    } else if verbose {
        println!("   Could not check for running processes");
    }

    Ok(())
}

#[cfg(not(windows))]
fn stop_other_veil_processes_unix(verbose: bool) -> Result<()> {
    if verbose {
        println!("   Stopping any running processes (except current upgrade)...");
    }

    let current_pid = std::process::id();
    
    let ps_output = Command::new("ps")
        .args(&["-eo", "pid,comm"])
        .output();

    if let Ok(output) = ps_output {
        let output_str = String::from_utf8_lossy(&output.stdout);
        let pids: Vec<u32> = output_str
            .lines()
            .filter(|line| line.contains(" ve") || line.contains("/ve"))
            .filter_map(|line| {
                let parts: Vec<&str> = line.trim().split_whitespace().collect();
                if let Some(pid_str) = parts.get(0) {
                    if let Ok(pid) = pid_str.parse::<u32>() {
                        if pid != current_pid {
                            return Some(pid);
                        }
                    }
                }
                None
            })
            .collect();

        if !pids.is_empty() {
            if verbose {
                println!(
                    "   Found {} process(es) to terminate: {:?}",
                    pids.len(),
                    pids
                );
            }

            for pid in pids {
                let kill_result = Command::new("kill")
                    .args(&["-TERM", &pid.to_string()])
                    .output();

                if verbose {
                    match kill_result {
                        Ok(result) if result.status.success() => {
                            println!("   Successfully terminated process {}", pid);
                        }
                        _ => {
                            println!(
                                "   Could not terminate process {} (may have already exited)",
                                pid
                            );
                        }
                    }
                }
            }

            std::thread::sleep(std::time::Duration::from_millis(1000));
        } else if verbose {
            println!("   No other processes found running");
        }
    } else if verbose {
        println!("   Could not check for running processes");
    }

    Ok(())
}
