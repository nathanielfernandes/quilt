use std::io::Cursor;

use ariadne::{Color, Label, Report, ReportKind, Source};

use super::Span;

/// A runtime error.
/// ### Fields
/// - `msg`: The error message.
/// - `help`: An optional help message.
/// - `span`: The span of the error.
/// - `color`: The color of the error message.
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub msg: String,
    pub help: Option<String>,
    pub span: Span,
    pub color: Option<ariadne::Color>,
}

impl RuntimeError {
    /// Creates a report for the error.
    pub fn report(self) -> ariadne::Report {
        let mut report = Report::build(ReportKind::Error, (), self.span.start).with_label(
            Label::new(self.span)
                .with_message(self.msg)
                .with_color(self.color.unwrap_or(Color::Red)),
        );

        if let Some(help) = &self.help {
            report.set_help(help);
        }

        report.finish()
    }

    /// Prints the error to stdout.
    pub fn print(self, src: &str) -> Result<(), std::io::Error> {
        self.report().print(Source::from(src))
    }

    /// Converts the error to a formatted string.
    pub fn to_string(self, src: &str) -> Result<String, std::io::Error> {
        let mut buf = vec![];
        let cursor = Cursor::new(&mut buf);
        self.report().write(Source::from(src), cursor)?;

        if let Ok(s) = String::from_utf8(buf) {
            Ok(s)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Could not convert to string",
            ))
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for RuntimeError {}
