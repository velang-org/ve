use anyhow::Result;
use codespan::{FileId, Files};
use codespan_reporting;
use std::path::{Path, PathBuf};


pub fn extract_line_col_from_error(
    files: &Files<String>,
    file_id: FileId,
    error: &codespan_reporting::diagnostic::Diagnostic<FileId>,
) -> String {
    if let Some(label) = error.labels.first() {
        let source = files.source(file_id);
        let mut line_num = 1u32;
        let mut col_num = 1u32;
        
        for (idx, ch) in source.char_indices() {
            if idx >= label.range.start {
                break;
            }
            if ch == '\n' {
                line_num += 1;
                col_num = 1;
            } else {
                col_num += 1;
            }
        }
        format!("{}:{}", line_num, col_num)
    } else {
        "1:1".to_string()
    }
}


pub fn format_parse_error(
    files: &Files<String>,
    file_id: FileId,
    error: &codespan_reporting::diagnostic::Diagnostic<FileId>,
    file_path: &Path,
) -> String {
    let mut output = String::new();
    

    let file_name = file_path.file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("unknown");
    

    output.push_str(&format!("\x1b[1;31merror\x1b[0m: {}\n", error.message));
    
    if let Some(label) = error.labels.first() {
        let range = label.range.clone();
        

        let source = files.source(file_id);
        let lines: Vec<&str> = source.lines().collect();
        

        let mut line_num = 1u32;
        let mut col_num = 1u32;
        
        for (idx, ch) in source.char_indices() {
            if idx >= range.start {
                break;
            }
            if ch == '\n' {
                line_num += 1;
                col_num = 1;
            } else {
                col_num += 1;
            }
        }
        
        output.push_str(&format!(
            " \x1b[1;34m-->\x1b[0m {}:{}:{}\n",
            file_name,
            line_num,
            col_num
        ));
        

        let line_idx = (line_num.saturating_sub(1)) as usize;
        
        if line_idx < lines.len() {
            let line_content = lines[line_idx];
            let col_start = (col_num.saturating_sub(1)) as usize;
            
            let mut end_col = col_start + 1;
            if range.end > range.start {
                let error_length = (range.end - range.start).min(line_content.len() - col_start);
                end_col = col_start + error_length;
            }
            
            output.push_str(&format!(" \x1b[1;34m{:4} |\x1b[0m\n", ""));
            output.push_str(&format!(" \x1b[1;34m{:4} |\x1b[0m {}\n", line_num, line_content));
            

            output.push_str(&format!(" \x1b[1;34m{:4} |\x1b[0m ", ""));
            

            for _ in 0..col_start {
                output.push(' ');
            }
            

            let indicator_len = if end_col > col_start {
                end_col - col_start
            } else {
                1
            };
            
            output.push_str("\x1b[1;31m");
            for i in 0..indicator_len {
                if i == 0 {
                    output.push('^');
                } else {
                    output.push('~');
                }
            }
            output.push_str("\x1b[0m");
            

            if !label.message.is_empty() {
                output.push_str(&format!(" \x1b[1;31m{}\x1b[0m", label.message));
            }
            
            output.push('\n');
        }
    }
    

    for note in &error.notes {
        output.push_str(&format!(" \x1b[1;36m= note:\x1b[0m {}\n", note));
    }
    
    output
}


pub fn print_parse_error(
    files: &Files<String>,
    file_id: FileId,
    error: &codespan_reporting::diagnostic::Diagnostic<FileId>,
    file_path: &Path,
) {
    let formatted = format_parse_error(files, file_id, error, file_path);
    eprint!("{}", formatted);
}


pub fn validate_ve_file(path: &str) -> std::result::Result<PathBuf, String> {
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
    let target_name = target_name.as_ref();

    let matches: Vec<_> = dir
        .read_dir()
        .ok()?
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_stem()?.to_string_lossy();
            (name.contains(target_name) && path.extension() == Some("ve".as_ref()))
                .then_some(format!("  • {}", path.display()))
        })
        .collect();    (!matches.is_empty()).then(|| matches.join("\n"))
}

#[cfg(target_os = "windows")]
pub fn prepare_windows_clang_args(
    output: &Path,
    optimize: bool,
    c_file: &Path,
) -> Result<Vec<String>> {
    let msvc_lib_paths = get_msvc_lib_paths()?;
    let mut clang_args = vec![
        if optimize { "-O3" } else { "-O0" }.to_string(),
        "-pipe".to_string(),
        "-fno-exceptions".to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

    for path in msvc_lib_paths {
        clang_args.push("-L".to_string());
        clang_args.push(path);
    }

    clang_args.extend_from_slice(&[
        "-lmsvcrt".to_string(),
        "-Xlinker".to_string(),
        "/NODEFAULTLIB:libcmt".to_string(),
    ]);

    Ok(clang_args)
}

#[cfg(target_os = "windows")]
fn get_msvc_lib_paths() -> Result<Vec<String>> {
    use std::env;
    use std::fs;
    
    let mut paths = Vec::new();

    if let Ok(vc_dir) = env::var("VCINSTALLDIR") {
        let lib_path = format!("{}\\Lib\\x64", vc_dir.trim_end_matches('\\'));
        if Path::new(&lib_path).exists() {
            paths.push(lib_path);
        }
    }

    if let Ok(windows_sdk_dir) = env::var("WindowsSdkDir") {
        let version = env::var("WindowsSDKVersion").unwrap_or_else(|_| {
            let lib_dir = format!("{}\\Lib", windows_sdk_dir.trim_end_matches('\\'));
            if let Ok(entries) = fs::read_dir(&lib_dir) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .filter(|name| name.starts_with("10.0."))
                    .collect();
                versions.sort();
                versions.last().unwrap_or(&"10.0.22621.0".to_string()).clone()
            } else {
                "10.0.22621.0".to_string()
            }
        });
        
        let um_path = format!(
            "{}\\Lib\\{}\\um\\x64",
            windows_sdk_dir.trim_end_matches('\\'),
            version.trim_end_matches('\\')
        );
        let ucrt_path = format!(
            "{}\\Lib\\{}\\ucrt\\x64",
            windows_sdk_dir.trim_end_matches('\\'),
            version.trim_end_matches('\\')
        );
        
        if Path::new(&um_path).exists() {
            paths.push(um_path);
        }
        if Path::new(&ucrt_path).exists() {
            paths.push(ucrt_path);
        }
    }

    if paths.is_empty() {
        let possible_vs_paths = vec![
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise",
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional", 
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community",
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\BuildTools",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Enterprise",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community",
            "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools",
        ];

        for vs_path in possible_vs_paths {
            let vc_tools_path = format!("{}\\VC\\Tools\\MSVC", vs_path);
            if let Ok(entries) = fs::read_dir(&vc_tools_path) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .collect();
                versions.sort();
                
                if let Some(latest_version) = versions.last() {
                    let lib_path = format!("{}\\VC\\Tools\\MSVC\\{}\\lib\\x64", vs_path, latest_version);
                    if Path::new(&lib_path).exists() {
                        paths.push(lib_path);
                        break;
                    }
                }
            }
        }

        let sdk_paths = vec![
            "C:\\Program Files (x86)\\Windows Kits\\10\\Lib",
            "C:\\Program Files\\Windows Kits\\10\\Lib",
        ];

        for sdk_path in sdk_paths {
            if let Ok(entries) = fs::read_dir(sdk_path) {
                let mut versions: Vec<String> = entries
                    .filter_map(|entry| entry.ok())
                    .filter(|entry| entry.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
                    .filter_map(|entry| entry.file_name().to_str().map(|s| s.to_string()))
                    .filter(|name| name.starts_with("10.0."))
                    .collect();
                versions.sort();
                
                if let Some(latest_version) = versions.last() {
                    let um_path = format!("{}\\{}\\um\\x64", sdk_path, latest_version);
                    let ucrt_path = format!("{}\\{}\\ucrt\\x64", sdk_path, latest_version);
                    
                    if Path::new(&um_path).exists() {
                        paths.push(um_path);
                    }
                    if Path::new(&ucrt_path).exists() {
                        paths.push(ucrt_path);
                    }
                    break;
                }
            }
        }
    }

    if paths.is_empty() {
        return Err(anyhow::anyhow!(
            "Could not find MSVC libraries. Please ensure Visual Studio Build Tools are installed.\n\
            Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe\n\
            Or run: vcvars64.bat to set up the environment."
        ));
    }

    for path in &paths {
        if !Path::new(path).exists() {
            eprintln!("Warning: Library path does not exist: {}", path);
        }
    }

    let existing_paths: Vec<String> = paths
        .into_iter()
        .filter(|path| Path::new(path).exists())
        .collect();

    if existing_paths.is_empty() {
        return Err(anyhow::anyhow!(
            "No valid MSVC library paths found. Please install Visual Studio Build Tools.\n\
            Install from: https://aka.ms/vs/17/release/vs_BuildTools.exe"
        ));
    }

    Ok(existing_paths)
}
  