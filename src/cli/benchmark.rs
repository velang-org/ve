
#[cfg(target_os = "windows")]
use crate::helpers::prepare_windows_clang_args;
use crate::{codegen, typeck};
use anyhow::{Context, anyhow};
use codespan::Files;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::termcolor::{Color, ColorChoice, ColorSpec, StandardStream};
use std::io::Write;
use std::path::PathBuf;
use std::time::{Duration, Instant};
use crate::compiler::incremental::IncrementalCompiler;
pub fn run_benchmark(input: PathBuf, iterations: usize, verbose: bool) -> anyhow::Result<()> {
    let build_dir = input
        .parent()
        .ok_or_else(|| anyhow!("Invalid input file path"))?
        .join("build");

    if !build_dir.exists() {
        std::fs::create_dir_all(&build_dir)?;
    }

    let output = build_dir.join("benchmark.exe");
    let c_file = build_dir.join("benchmark.c");

    let mut files = Files::<String>::new();

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
    let mut module_compiler = IncrementalCompiler::new(&build_dir);
    module_compiler.build_dependency_graph(&input)?;
    let parse_time = parse_start.elapsed();

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))?;
    writeln!(&mut stdout, "DONE ✓ ({:.2?})", parse_time)?;
    stdout.reset()?;

    stdout.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
    write!(&mut stdout, "  [2/4] Type checking... ")?;
    stdout.flush()?;
    let mut module_compiler = IncrementalCompiler::new(&build_dir);
    module_compiler.build_dependency_graph(&input)?;
    
    let compiled_modules = module_compiler.compile_all_modules(&mut files, verbose)?;
    if verbose && !compiled_modules.is_empty() {
        println!("{} modules were recompiled", compiled_modules.len());
    }

    let mut program = module_compiler.create_merged_program(&input)?;
    let (imported_functions, imported_structs, imported_ffi_vars) = module_compiler.get_imported_info()?;
    let file_id = module_compiler.get_entry_file_id(&mut files, &input)?;

    if verbose {
        println!("✓ AST parsed successfully");
    }

    let typeck_start = Instant::now();
    let mut type_checker = typeck::TypeChecker::new(
        file_id,
        imported_functions.clone(),
        imported_structs.clone(),
        imported_ffi_vars.clone(),
    );
    match type_checker.check(&mut program) {
        Ok(()) => (),
        Err(errors) => {
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
            writeln!(&mut stdout, "FAILED")?;
            stdout.reset()?;

            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config::default();

            for error in &errors {
                term::emit(&mut writer.lock(), &config, &files, &error)?;

                let file_id = error.labels.get(0).map(|l| l.file_id);
                let file_path = file_id
                    .and_then(|fid| Some(files.name(fid)))
                    .map(|n| n.to_string_lossy().to_string());
                let module_info = file_path.as_ref().and_then(|path: &String| {
                    if let Some(_idx) = path.find("lib/std") {
                        Some("standard library".to_string())
                    } else if let Some(lib_start) = path.find("lib/") {
                        let rest = &path[lib_start + 4..];
                        if let Some(end) = rest.find('/') {
                            let lib_name = &rest[..end];
                            if lib_name != "std" {
                                return Some(format!("external library '{}'", lib_name));
                            }
                        }
                        None
                    } else if let Some(ex_start) = path.find("examples/") {
                        let rest = &path[ex_start + 9..];
                        if let Some(end) = rest.find('/') {
                            let ex_name = &rest[..end];
                            return Some(format!("example module '{}'", ex_name));
                        }
                        None
                    } else {
                        None
                    }
                });

                let location = if let Some(path) = file_path {
                    match module_info {
                        Some(module) => format!("in file '{}' ({})", path, module),
                        None => format!("in file '{}'", path),
                    }
                } else {
                    "in unknown location".to_string()
                };

                eprintln!("\nType checker error {}: {}", location, error.message);

                if let Some(label) = error.labels.get(0) {
                    eprintln!("  --> at {}..{}", label.range.start, label.range.end);
                    eprintln!("  = detail: {}", label.message);
                }

                for note in &error.notes {
                    eprintln!("  note: {}", note);
                }
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
    let config = codegen::CodegenConfig {
        target_triple: "x86_64-pc-windows-msvc".to_string(),
    };
    let mut target = codegen::Target::create(
        config,
        file_id,
        imported_functions,
        imported_structs,
        imported_ffi_vars,
        false,
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
    #[cfg(target_os = "windows")]
    let clang_args = prepare_windows_clang_args(&output, false, &c_file)?;

    #[cfg(not(target_os = "windows"))]
    let clang_args = vec![
        "-O0".to_string(),
        c_file.to_str().unwrap().into(),
        "-o".to_string(),
        output.to_str().unwrap().into(),
    ];

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
    writeln!(
        &mut stdout,
        "\n🚀 Running benchmarks ({} iterations)...",
        iterations
    )?;
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

