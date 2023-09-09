use std::{io::Cursor, time::Duration};

use ariadne::{Color, Label, ReportKind};

use thiserror::Error;

pub use ariadne::Report;

use crate::{
    sourcecache::SourceCache,
    span::{Span, Spanned},
};

pub type ErrorS = Spanned<Error>;
pub type ValueType = &'static str;
pub type TraceBack = Vec<Spanned<String>>;

pub trait ErrorExt {
    fn report(&self) -> Report<Span>;
    fn print(&self, sources: &SourceCache) -> std::io::Result<()>;
    fn to_string(self, cache: &SourceCache) -> Result<String, std::io::Error>;
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

    #[error("IncludeError: {0}")]
    IncludeError(IncludeError),

    #[error("SyntaxError: {0}")]
    SyntaxError(SyntaxError),

    #[error("index `{0}` out of bounds")]
    IndexError(i64),

    #[error("Division by zero")]
    ZeroDivisionError,

    #[error("Runtime limit exceeded: {0:?}")]
    RuntimeLimitExceeded(Duration),

    //TODO: add memory limit
    #[error("Memory limit exceeded")]
    MemoryLimitExceeded,

    #[error("Halt")]
    Halt,
}

impl ErrorExt for ErrorS {
    fn report(&self) -> Report<Span> {
        let (error, span) = &self;
        let msg = error.to_string();

        let kind = ReportKind::Custom(error.name(), Color::Red);

        let mut report = Report::build(kind, span.2, span.0)
            .with_label(Label::new(*span).with_message(msg).with_color(Color::Red));

        if let Error::BuiltinError(e) = error {
            if let Some(help) = &e.help {
                report.set_help(help)
            }
        }

        report.finish()
    }

    fn print(&self, sources: &SourceCache) -> std::io::Result<()> {
        self.report().print(sources)
    }

    fn to_string(self, cache: &SourceCache) -> Result<String, std::io::Error> {
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

impl ErrorExt for TraceBack {
    fn report(&self) -> Report<Span> {
        let Some((_, span)) = self.first() else {
            return Report::build(ReportKind::Custom("Traceback", Color::Red), 0usize, 0).finish();
        };
        let kind = ReportKind::Custom("Traceback", Color::Red);
        Report::build(kind, span.2, span.0).finish()
    }

    fn print(&self, sources: &SourceCache) -> std::io::Result<()> {
        println!("Traceback (most recent call last):");
        // iterate over the first 10 frames
        for (fn_name, span) in self.iter().rev() {
            if fn_name.starts_with("...") {
                println!("   {}", fn_name);
                continue;
            }

            let Some(src) = sources.get(span.2) else {
                continue;
            };

            if src.source.len() == 0 {
                continue;
            }

            let Some(lineno) = src.source.get_offset_line(span.0).map(|(_, l, _)| l) else {
                continue;
            };

            let Some(line) = src.source.line(lineno) else {
                continue;
            };

            let line = line.chars().collect::<String>();

            println!("   File \"{}\", line {}, in {}", src.name, lineno, fn_name);
            println!("      {}", line.trim_start());
        }

        Ok(())
    }

    fn to_string(self, cache: &SourceCache) -> Result<String, std::io::Error> {
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

impl NamedError for Error {
    fn name(&self) -> &'static str {
        match &self {
            Error::NameError(e) => e.name(),
            Error::OverflowError(e) => e.name(),
            Error::TypeError(e) => e.name(),
            Error::BuiltinError(e) => e.name(),
            Error::IncludeError(e) => e.name(),
            Error::SyntaxError(e) => e.name(),
            Error::IndexError(_) => "IndexError",
            Error::ZeroDivisionError => "ZeroDivisionError",
            Error::RuntimeLimitExceeded(_) => "RuntimeLimitExceeded",
            Error::MemoryLimitExceeded => "MemoryLimitExceeded",
            Error::Halt => "Halt",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum TypeError {
    #[error("expected `{0}` got `{1}`")]
    Expected(ValueType, ValueType),

    // error for trying to convert types
    #[error("cannot convert `{0}` to `{1}`")]
    CannotConvert(ValueType, ValueType),

    #[error("cannot compare `{0}` and `{1}`")]
    CannotCompare(ValueType, ValueType),

    #[error("cannot parse '{0}' as `{1}`")]
    ParseError(String, ValueType),

    #[error("function '{name}' expected `{expected}` arguments got `{got}`")]
    MismatchedArity { name: String, expected: u8, got: u8 },

    #[error("cannot use `{0}` as a function")]
    NotCallable(ValueType),

    #[error("unsupported binary operation `{op}` on `{lhs}` and `{rhs}`")]
    UnsupportedBinaryOperation {
        op: &'static str,
        lhs: ValueType,
        rhs: ValueType,
    },

    #[error("cannot unpack `{0}`")]
    Unpackable(ValueType),

    #[error("cannot unpack a {0} item value into {1} values")]
    InsufficientValues(usize, u8),

    #[error("unsupported unary operation `{op}` on `{rhs}`")]
    UnsupportedUnaryOperation { op: &'static str, rhs: ValueType },

    #[error("cannot iterate over `{0}`")]
    NotIterable(ValueType),

    #[error("failed to iterate :(")]
    FailedToIterate,

    #[error("cannot hash `{0}`")]
    CannotHash(ValueType),
}

impl NamedError for TypeError {
    fn name(&self) -> &'static str {
        match self {
            Self::Expected(_, _) => "Expected",
            Self::CannotConvert(_, _) => "CannotConvert",
            Self::CannotCompare(_, _) => "CannotCompare",
            Self::ParseError(_, _) => "ParseError",
            Self::MismatchedArity { .. } => "MismatchedArity",
            Self::NotCallable(_) => "NotCallable",
            Self::UnsupportedBinaryOperation { .. } => "UnsupportedBinaryOperation",
            Self::Unpackable(_) => "Unpackable",
            Self::InsufficientValues(_, _) => "InsufficientValues",
            Self::UnsupportedUnaryOperation { .. } => "UnsupportedUnaryOperation",
            Self::NotIterable(_) => "NotIterable",
            Self::FailedToIterate => "FailedToIterate",
            Self::CannotHash(_) => "CannotHash",
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
    #[error("an array cannot have more than 256 items")]
    TooManyItems,

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

    #[error("string too large")]
    StringTooLarge,

    #[error("array too large")]
    ArrayTooLarge,
}

impl NamedError for OverflowError {
    fn name(&self) -> &'static str {
        match self {
            Self::TooManyItems => "TooManyItems",
            Self::TooManyParams => "TooManyParams",
            Self::TooManyArgs => "TooManyArgs",
            Self::TooMuchToUnpack => "TooMuchToUnpack",
            Self::StackOverflow => "StackOverflow",
            Self::StackUnderflow => "StackUnderflow",
            Self::InstructionOverflow => "InstructionOverflow",
            Self::JumpTooLarge => "JumpTooLarge",
            Self::StringTooLarge => "StringTooLarge",
            Self::ArrayTooLarge => "ArrayTooLarge",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum IncludeError {
    #[error("includes can only be at the top level of a file")]
    IncludeNotTopLevel,

    #[error("unresolved include: {0:?}")]
    UnresolvedInclude(String),

    #[error("could not resolve include: {0:?}")]
    CouldNotResolve(String),

    #[error("circular include: {0:?}")]
    CircularInclude(String),
}

impl NamedError for IncludeError {
    fn name(&self) -> &'static str {
        match self {
            Self::IncludeNotTopLevel => "IncludeNotTopLevel",
            Self::UnresolvedInclude(_) => "UnresolvedInclude",
            Self::CouldNotResolve(_) => "CouldNotResolve",
            Self::CircularInclude(_) => "CircularInclude",
        }
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub struct Expected(pub Vec<&'static str>);

impl std::fmt::Display for Expected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
            for item in iter {
                write!(f, ", {}", item)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Error, Clone, Eq, PartialEq)]
pub enum SyntaxError {
    #[error("unexpected token at line {line}, column {column}, expected one of {expected}")]
    UnexpectedToken {
        line: usize,
        column: usize,
        expected: Expected,
    },

    #[error("statement was used in place of expression")]
    StatementAsExpression,

    #[error("return outside of function")]
    ReturnOutsideFunction,
}

impl NamedError for SyntaxError {
    fn name(&self) -> &'static str {
        match self {
            SyntaxError::UnexpectedToken { .. } => "UnexpectedToken",
            SyntaxError::StatementAsExpression => "StatementAsExpression",
            SyntaxError::ReturnOutsideFunction => "ReturnOutsideFunction",
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
    IncludeError,
    SyntaxError
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
}

impl BuiltinError {
    pub fn help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }
}

impl NamedError for BuiltinError {
    fn name(&self) -> &'static str {
        "BuiltinError"
    }
}

impl std::fmt::Display for BuiltinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
