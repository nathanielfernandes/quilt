use std::io::Cursor;

use ariadne::{Color, Label, Report, ReportKind};
use peg::error::ExpectedSet;

use super::{Span, Spanned};
use crate::prelude::{Op, SourceCache};
use thiserror::Error;

pub type ErrorS = Spanned<Error>;
pub type ValueType = &'static str;

pub trait ErrorExt {
    fn report(&self) -> Report<Span>;
    fn print(&self, sources: SourceCache) -> std::io::Result<()>;
}

pub trait NamedError {
    fn name(&self) -> &'static str;
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum Error {
    #[error("NameError: {0}")]
    NameError(NameError),

    #[error("OverflowError: {0}")]
    OverflowError(OverflowError),

    #[error("TypeError: {0}")]
    TypeError(TypeError),

    #[error("BuiltinError: {0}")]
    BuiltinError(BuiltinError),

    #[error("ImportError: {0}")]
    ImportError(ImportError),

    #[error("SyntaxError: {0}")]
    SyntaxError(SyntaxError),

    #[error("CompileError: {0}")]
    CompileError(CompileError),
}

impl ErrorExt for ErrorS {
    fn report(&self) -> Report<Span> {
        let (error, span) = &self;
        let msg = error.to_string();

        let kind = ReportKind::Custom(error.name(), Color::Red);

        let report = Report::build(kind, span.2, span.0)
            .with_label(Label::new(*span).with_message(msg).with_color(Color::Red));

        report.finish()
    }

    fn print(&self, sources: SourceCache) -> std::io::Result<()> {
        self.report().print(sources)
    }
}

impl NamedError for Error {
    fn name(&self) -> &'static str {
        match &self {
            Error::NameError(e) => e.name(),
            Error::OverflowError(e) => e.name(),
            Error::TypeError(e) => e.name(),
            Error::BuiltinError(e) => e.name(),
            Error::ImportError(e) => e.name(),
            Error::SyntaxError(e) => e.name(),
            Error::CompileError(e) => e.name(),
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum TypeError {
    #[error("expected `{0}` got `{1}`")]
    Expected(ValueType, ValueType),

    #[error("function '{name}' expected `{expected}` arguments got `{got}`")]
    MismatchedArity { name: String, expected: u8, got: u8 },

    #[error("cannot use `{0}` as a function")]
    NotCallable(ValueType),

    #[error("unsupported binary operation `{op}` on `{lhs}` and `{rhs}`")]
    UnsupportedBinaryOperation {
        op: Op,
        lhs: ValueType,
        rhs: ValueType,
    },

    #[error("cannot unpack `{0}`")]
    Unpackable(ValueType),

    #[error("cannot unpack a {0} item value into {1} values")]
    InsufficientValues(usize, u8),

    #[error("unsupported unary operation `{op}` on `{rhs}`")]
    UnsupportedUnaryOperation { op: Op, rhs: ValueType },

    #[error("cannot iterate over `{0}`")]
    NotIterable(ValueType),

    #[error("failed to iterate :(")]
    FailedToIterate,
}

impl NamedError for TypeError {
    fn name(&self) -> &'static str {
        match self {
            Self::Expected(_, _) => "Expected",
            Self::MismatchedArity { .. } => "MismatchedArity",
            Self::NotCallable(_) => "NotCallable",
            Self::UnsupportedBinaryOperation { .. } => "UnsupportedBinaryOperation",
            Self::Unpackable(_) => "Unpackable",
            Self::InsufficientValues(_, _) => "InsufficientValues",
            Self::UnsupportedUnaryOperation { .. } => "UnsupportedUnaryOperation",
            Self::NotIterable(_) => "NotIterable",
            Self::FailedToIterate => "FailedToIterate",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum NameError {
    #[error("name {0:?} is not defined")]
    Undefined(String),

    #[error("name {0:?} is already defined")]
    AlreadyDefined(String),

    #[error("builtin {0:?} is not defined")]
    UndefinedBuiltin(String),
}

impl NamedError for NameError {
    fn name(&self) -> &'static str {
        match self {
            Self::Undefined(_) => "Undefined",
            Self::AlreadyDefined(_) => "AlreadyDefined",
            Self::UndefinedBuiltin(_) => "UndefinedBuiltin",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum OverflowError {
    #[error("cannot define more than 256 parameters in a function")]
    TooManyParams,

    #[error("cannot use more than 256 arguments in a function call")]
    TooManyArgs,

    #[error("cannot unpack more than 256 values")]
    TooMuchToUnpack,

    #[error("stack overflow")]
    StackOverflow,

    #[error("stack underflow")]
    StackUnderflow,

    #[error("instruction overflow")]
    InstructionOverflow,

    #[error("jump too large")]
    JumpTooLarge,
}

impl NamedError for OverflowError {
    fn name(&self) -> &'static str {
        match self {
            Self::TooManyParams => "TooManyParams",
            Self::TooManyArgs => "TooManyArgs",
            Self::TooMuchToUnpack => "TooMuchToUnpack",
            Self::StackOverflow => "StackOverflow",
            Self::StackUnderflow => "StackUnderflow",
            Self::InstructionOverflow => "InstructionOverflow",
            Self::JumpTooLarge => "JumpTooLarge",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum ImportError {
    #[error("imports can only be at the top level of a file")]
    ImportNotTopLevel,

    #[error("unresolved import: {0:?}")]
    UnresolvedImport(String),

    #[error("could not resolve import: {0:?}")]
    CouldNotResolve(String),

    #[error("circular import: {0:?}")]
    CircularImport(String),
}

impl NamedError for ImportError {
    fn name(&self) -> &'static str {
        match self {
            ImportError::ImportNotTopLevel => "ImportNotTopLevel",
            ImportError::UnresolvedImport(_) => "UnresolvedImport",
            ImportError::CouldNotResolve(_) => "CouldNotResolve",
            ImportError::CircularImport(_) => "CircularImport",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum SyntaxError {
    #[error("unexpected token at line {line}, column {column}, expected one of {expected:?}")]
    UnexpectedToken {
        line: usize,
        column: usize,
        expected: ExpectedSet,
    },
}

impl NamedError for SyntaxError {
    fn name(&self) -> &'static str {
        match self {
            SyntaxError::UnexpectedToken { .. } => "UnexpectedToken",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum CompileError {
    // #[error("functions can only be defined at the top level")]
    // NestedFunction(String),
    #[error("statements cannot be used as expressions")]
    StatementAsExpression,
}

impl NamedError for CompileError {
    fn name(&self) -> &'static str {
        match self {
            // CompileError::NestedFunction(_) => "NestedFunction",
            CompileError::StatementAsExpression => "StatementAsExpression",
        }
    }
}

macro_rules! impl_from_error {
    ($($error:ident),+) => {$(
        impl From<$error> for Error {
            fn from(e: $error) -> Self {
                Error::$error(e)
            }
        }
    )+};
}

impl_from_error!(
    NameError,
    OverflowError,
    TypeError,
    BuiltinError,
    ImportError,
    SyntaxError,
    CompileError
);

/// A runtime error.
/// ### Fields
/// - `msg`: The error message.
/// - `help`: An optional help message.
/// - `span`: The span of the error.
/// - `color`: The color of the error message.
#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub struct BuiltinError {
    pub msg: String,
    pub help: Option<String>,
    pub span: Span,
}

impl BuiltinError {
    pub fn help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    /// Creates a report for the error.
    pub fn report(self) -> ariadne::Report<Span> {
        let mut report = Report::build(ReportKind::Error, self.span.2, self.span.0).with_label(
            Label::new(self.span)
                .with_message(self.msg)
                .with_color(Color::Red),
        );

        if let Some(help) = &self.help {
            report.set_help(help);
        }

        report.finish()
    }

    /// Prints the error to stdout.
    pub fn print(self, cache: SourceCache) -> Result<(), std::io::Error> {
        self.report().print(cache)
    }

    /// Converts the error to a formatted string.
    pub fn to_string(self, cache: SourceCache) -> Result<String, std::io::Error> {
        let mut buf = vec![];
        let cursor = Cursor::new(&mut buf);
        self.report().write(cache, cursor)?;

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

impl NamedError for BuiltinError {
    fn name(&self) -> &'static str {
        "BuiltinError"
    }
}

impl From<BuiltinError> for ErrorS {
    fn from(e: BuiltinError) -> Self {
        let span = e.span;
        (Error::BuiltinError(e), span)
    }
}

impl std::fmt::Display for BuiltinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
