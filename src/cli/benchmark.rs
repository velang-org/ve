use std::path::PathBuf;
use std::time::{Duration, Instant};
use anyhow::{anyhow, Context};
use codespan::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, ColorSpec, Color};
use codespan_reporting::term::termcolor::WriteColor;
use std::io::Write;
use crate::{codegen, lexer, parser, typeck};
use crate::utils::{process_imports, prepare_windows_clang_args, validate_ve_file};

pub fn run_benchmark(
    input: PathBuf,
    iterations: usize,
    verbose: bool,
) -> anyhow::Result<()> {
    let build_dir = input.parent()
        .ok_or_else(|| anyhow!("Invalid input file path"))?
        .join("build");

    if !build_dir.exists() {
        std::fs::create_dir_all(&build_dir)?;
    }

    let output = build_dir.join("benchmark.exe");
    let c_file = build_dir.join("benchmark.c");

    let mut files = Files::<String>::new();
    let file_id = files.add(
        input.to_str().unwrap().to_string(),
        std::fs::read_to_string(input.clone())
            .with_context(|| format!("Error reading input file {}", input.display()))?,
    );

    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    
    if verbose {
        let mut stdout = StandardStream::stdout(ColorChoice::Auto);
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
        writeln!(&mut stdout, "\n📋 Benchmark Configuration")?;
        stdout.reset()?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Input file:     ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        writeln!(&mut stdout, "{}", input.display())?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Output file:    ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        writeln!(&mut stdout, "{}", output.display())?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Build directory: ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        writeln!(&mut stdout, "{}", build_dir.display())?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Iterations:     ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        writeln!(&mut stdout, "{}", iterations)?;
        stdout.reset()?;
    }

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    writeln!(&mut stdout, "\n🔄 Running compilation stages...")?;
    stdout.reset()?;

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
    write!(&mut stdout, "  [1/4] Parsing... ")?;
    stdout.flush()?;
    
    let parse_start = Instant::now();
    let lexer = lexer::Lexer::new(&files, file_id);
    let mut parser = parser::Parser::new(lexer);
    let mut program = match parser.parse() {
        Ok(program) => program,
        Err(error) => {
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
            writeln!(&mut stdout, "FAILED")?;
            stdout.reset()?;
            
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &error)?;
            return Err(anyhow!("Parsing failed"));
        }
    };
    let parse_time = parse_start.elapsed();
    
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    writeln!(&mut stdout, "DONE ✓ ({:.2?})", parse_time)?;
    stdout.reset()?;

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
    write!(&mut stdout, "  [2/4] Type checking... ")?;
    stdout.flush()?;
    
    let (imported_functions, imported_asts, imported_structs, imported_ffi_funcs, imported_ffi_vars) = 
        process_imports(&mut files, &program.imports, &*input)?;
    
    program.functions.extend(imported_asts);
    program.ffi_functions.extend(imported_ffi_funcs);
    program.ffi_variables.extend(imported_ffi_vars.clone());

    if verbose {
        println!("✓ AST parsed successfully");
    }

    let typeck_start = Instant::now();
    let mut type_checker = typeck::TypeChecker::new(
        file_id, 
        imported_functions.clone(), 
        imported_structs.clone(),
        imported_ffi_vars.clone()
    );
    match type_checker.check(&mut program) {
        Ok(()) => (),
        Err(errors) => {
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
            writeln!(&mut stdout, "FAILED")?;
            stdout.reset()?;
            
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();
            for error in errors {
                term::emit(&mut writer.lock(), &config, &files, &error)?;
            }
            return Err(anyhow!("Type checking failed"));
        }
    }
    let typeck_time = typeck_start.elapsed();
    
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    writeln!(&mut stdout, "DONE ✓ ({:.2?})", typeck_time)?;
    stdout.reset()?;

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
    write!(&mut stdout, "  [3/4] Generating code... ")?;
    stdout.flush()?;
    
    let codegen_start = Instant::now();
    let config = codegen::CodegenConfig { target_triple: "x86_64-pc-windows-msvc".to_string() };
    let mut target = codegen::Target::create(
        config, 
        file_id, 
        imported_functions, 
        imported_structs,
        imported_ffi_vars
    );
    target.compile(&program, &c_file)?;
    let codegen_time = codegen_start.elapsed();
    
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    writeln!(&mut stdout, "DONE ✓ ({:.2?})", codegen_time)?;
    stdout.reset()?;

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
    write!(&mut stdout, "  [4/4] Compiling C... ")?;
    stdout.flush()?;
    
    let compile_start = Instant::now();
    let clang_args = prepare_windows_clang_args(&output, false, &c_file)?;
    let status = std::process::Command::new("clang")
        .args(&clang_args)
        .status()
        .context("Failed to execute C compiler")?;

    if !status.success() {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
        writeln!(&mut stdout, "FAILED")?;
        stdout.reset()?;
        return Err(anyhow!("C compilation failed"));
    }
    let compile_time = compile_start.elapsed();
    
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    writeln!(&mut stdout, "DONE ✓ ({:.2?})", compile_time)?;
    stdout.reset()?;

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
    writeln!(&mut stdout, "\n🚀 Running benchmarks ({} iterations)...", iterations)?;
    stdout.reset()?;
    
    let mut execution_times = Vec::with_capacity(iterations);
    
    for i in 1..=iterations {
        let execution_start = Instant::now();
        let status = std::process::Command::new(output.clone())
            .status()
            .context("Failed to execute program")?;

        if !status.success() {
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
            writeln!(&mut stdout, "  Program failed with exit code: {}", status)?;
            stdout.reset()?;
            return Err(anyhow!("Program execution failed"));
        }
        let execution_time = execution_start.elapsed();
        execution_times.push(execution_time);
        
        if verbose {
            write!(&mut stdout, "  Run {:>2}: ", i)?;
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
            writeln!(&mut stdout, "{:.2?}", execution_time)?;
            stdout.reset()?;
        }
    }

    if !execution_times.is_empty() {
        let total: Duration = execution_times.iter().sum();
        let avg = total / execution_times.len() as u32;
        
        let mut min = execution_times[0];
        let mut max = execution_times[0];
        
        for &time in &execution_times {
            if time < min {
                min = time;
            }
            if time > max {
                max = time;
            }
        }
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
        writeln!(&mut stdout, "\n📊 Benchmark Results")?;
        stdout.reset()?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Average time:  ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Magenta)))?;
        writeln!(&mut stdout, "{:.2?}", avg)?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Fastest time:  ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        writeln!(&mut stdout, "{:.2?}", min)?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Slowest time:  ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
        writeln!(&mut stdout, "{:.2?}", max)?;
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(&mut stdout, "  Total time:    ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Blue)))?;
        writeln!(&mut stdout, "{:.2?}", total)?;
        
        stdout.reset()?;

        let total_time = parse_time + typeck_time + codegen_time + compile_time;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))?;
        writeln!(&mut stdout, "\n⏱️  Compilation Summary")?;
        stdout.reset()?;
        
        let bar_width = 50;
        let parse_percent = parse_time.as_nanos() as f64 / total_time.as_nanos() as f64;
        let typeck_percent = typeck_time.as_nanos() as f64 / total_time.as_nanos() as f64;
        let codegen_percent = codegen_time.as_nanos() as f64 / total_time.as_nanos() as f64;
        let compile_percent = compile_time.as_nanos() as f64 / total_time.as_nanos() as f64;
        
        let parse_width = (parse_percent * bar_width as f64).round() as usize;
        let typeck_width = (typeck_percent * bar_width as f64).round() as usize;
        let codegen_width = (codegen_percent * bar_width as f64).round() as usize;
        let compile_width = bar_width - parse_width - typeck_width - codegen_width;
        
        write!(&mut stdout, "  [")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Blue)).set_bold(true))?;
        for _ in 0..parse_width {
            write!(&mut stdout, "█")?;
        }
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
        for _ in 0..typeck_width {
            write!(&mut stdout, "█")?;
        }
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true))?;
        for _ in 0..codegen_width {
            write!(&mut stdout, "█")?;
        }
        
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
        for _ in 0..compile_width {
            write!(&mut stdout, "█")?;
        }
        
        stdout.reset()?;
        writeln!(&mut stdout, "] {:.2?}", total_time)?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Blue)))?;
        write!(&mut stdout, "  █ Parse ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        write!(&mut stdout, "█ TypeCheck ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
        write!(&mut stdout, "█ CodeGen ")?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
        writeln!(&mut stdout, "█ Compile")?;
        stdout.reset()?;
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_bold(true))?;
        writeln!(&mut stdout, "  Total compilation time: {:.2?}", total_time)?;
        stdout.reset()?;
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;
    use std::path::Path;
    
    fn create_test_file(content: &str) -> PathBuf {
        let test_dir = env::temp_dir().join("verve_benchmark_tests");
        fs::create_dir_all(&test_dir).unwrap();
        
        let test_file = test_dir.join("test_benchmark.ve");
        fs::write(&test_file, content).unwrap();
        
        test_file
    }
    
    #[test]
    fn test_benchmark_setup() {
        let test_content = r#"
            import "std/io";
            
            fn main() {
                print("Test benchmark");
            }
        "#;
        
        let test_file = create_test_file(test_content);
        let build_dir = test_file.parent().unwrap().join("build");

        if build_dir.exists() {
            fs::remove_dir_all(&build_dir).unwrap();
        }

        let result = run_benchmark(test_file, 1, true);

        assert!(result.is_err());
        assert!(build_dir.exists());
        if build_dir.exists() {
            let _ = fs::remove_dir_all(&build_dir);
        }
    }
}
