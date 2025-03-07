use std::path::Path;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::Config;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

pub fn report_error(
    title: &str,
    message: &str,
    path: &Path,
    content: &str,
    span: Option<(usize, usize)>,
) -> anyhow::Result<()> {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();

    let file = SimpleFile::new(path.to_string_lossy(), content);
    let diagnostic = Diagnostic::error()
        .with_message(title)
        .with_notes(vec![message.into()]);

    let diagnostic = if let Some((start, end)) = span {
        diagnostic.with_labels(vec![
            Label::primary((), start..end).with_message("here")
        ])
    } else {
        diagnostic
    };

    term::emit(&mut writer.lock(), &config, &file, &diagnostic)?;
    Ok(())
}

pub fn report_clap_error(e: clap::Error) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();

    let diagnostic = Diagnostic::error()
        .with_message("Invalid command line arguments")
        .with_notes(vec![e.to_string()]);

    term::emit(&mut writer.lock(), &config, &SimpleFile::new("cli", ""), &diagnostic).unwrap();
}

pub fn report_file_error(path: &Path) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config::default();

    let diagnostic = Diagnostic::error()
        .with_message("File not found")
        .with_notes(vec![format!("File '{}' not found", path.display())]);

    term::emit(&mut writer.lock(), &config, &SimpleFile::new("cli", ""), &diagnostic).unwrap();
}