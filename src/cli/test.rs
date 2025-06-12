use crate::cli::process_build;
use anyhow::{anyhow, Context};
use colored::*;
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::Command;
use std::time::Instant;

pub fn run_test(input: PathBuf, test_name: Option<String>, verbose: bool, list: bool) -> anyhow::Result<()> {

    if list {
        return list_tests(input);
    }

    let content = std::fs::read_to_string(&input)
        .with_context(|| format!("Failed to read test file: {}", input.display()))?;
    
    let available_tests = parse_test_names(&content);
    
    if available_tests.is_empty() {
        println!("{}", "No tests found in the file.".yellow());
        return Ok(());
    }

    let tests_to_run = if let Some(ref specific_test) = test_name {
        if available_tests.contains(specific_test) {
            vec![specific_test.clone()]
        } else {
            return Err(anyhow!(
                "Test '{}' not found. Available tests: {}",
                specific_test,
                available_tests.join(", ")
            ));
        }
    } else {
        available_tests
    };    let executable_path = process_build(
        input.clone(),
        "build/program.exe".into(),
        false,
        "x86_64-pc-windows-msvc".into(),
        verbose,
        true,
    )?;

    run_tests_with_formatting(&executable_path, &tests_to_run, verbose)
}

fn parse_test_names(content: &str) -> Vec<String> {
    let mut tests = Vec::new();
    
    for line in content.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("test ") {
            let after_test = &trimmed[5..]; 
            if let Some(name_end) = after_test.find(' ').or_else(|| after_test.find('{')) {
                let test_name = after_test[..name_end].trim().to_string();
                if !test_name.is_empty() {
                    tests.push(test_name);
                }
            }
        }
    }
    
    tests
}

fn run_tests_with_formatting(
    executable_path: &PathBuf,
    tests: &[String],
    verbose: bool,
) -> anyhow::Result<()> {
    let total_tests = tests.len();
    let mut passed = 0;
    let mut failed = 0;
    
    println!("{}", "🧪 Running Tests".bold().blue());
    println!();
    
    let overall_start = Instant::now();
    
    for (index, test_name) in tests.iter().enumerate() {
        let test_number = index + 1;
        
        print!("{} ", format!("[{}/{}]", test_number, total_tests).dimmed());
        print!("{} ", "RUNNING".cyan());
        print!("{}", test_name.bold());
        io::stdout().flush().unwrap();
        
        let test_start = Instant::now();
        let result = Command::new(&executable_path)
            .arg(&test_name)
            .output()
            .with_context(|| format!("Failed to run test: {}", test_name))?;
        
        let test_duration = test_start.elapsed();
        
        if result.status.success() {
            print!("\r{} ", format!("[{}/{}]", test_number, total_tests).dimmed());
            print!("{} ", "✓ PASS".green().bold());
            print!("{}", test_name.bold());
            println!(" {}", format!("({:.2?})", test_duration).dimmed());
            passed += 1;
        } else {
            print!("\r{} ", format!("[{}/{}]", test_number, total_tests).dimmed());
            print!("{} ", "✗ FAIL".red().bold());
            print!("{}", test_name.bold());
            println!(" {}", format!("({:.2?})", test_duration).dimmed());
            failed += 1;
            
            if verbose || !result.stderr.is_empty() {
                let stderr = String::from_utf8_lossy(&result.stderr);
                if !stderr.trim().is_empty() {
                    println!("      {}", "Error output:".red());
                    for line in stderr.lines() {
                        println!("      {}", line.dimmed());
                    }
                }
            }
            
            let stdout = String::from_utf8_lossy(&result.stdout);
            if !stdout.trim().is_empty() && verbose {
                println!("      {}", "Standard output:".blue());
                for line in stdout.lines() {
                    println!("      {}", line.dimmed());
                }
            }
        }
    }
    
    let overall_duration = overall_start.elapsed();
    
    println!();
    if failed == 0 {
        println!(
            "{} {} test{} passed {}",
            "✓".green().bold(),
            passed.to_string().bold(),
            if passed == 1 { "" } else { "s" },
            format!("({:.2?})", overall_duration).dimmed()
        );
    } else {
        println!(
            "{} {} passed, {} failed {}",
            if passed > 0 { "⚠" } else { "✗" }.yellow().bold(),
            passed.to_string().green().bold(),
            failed.to_string().red().bold(),
            format!("({:.2?})", overall_duration).dimmed()
        );
    }

    
    Ok(())
}


pub fn list_tests(input: PathBuf) -> anyhow::Result<()> {
    let content = std::fs::read_to_string(&input)
        .with_context(|| format!("Failed to read test file: {}", input.display()))?;
    
    let available_tests = parse_test_names(&content);
    
    if available_tests.is_empty() {
        println!("{}", "No tests found in the file.".yellow());
        return Ok(());
    }

    println!("{}", "Available tests:".bold().blue());
    for (i, test_name) in available_tests.iter().enumerate() {
        println!("  {}. {}", i + 1, test_name.green());
    }
    println!("\nTotal: {} test{}", available_tests.len(), if available_tests.len() == 1 { "" } else { "s" });
    
    Ok(())
}