pub mod error;
pub mod value;

pub use error::*;
pub use value::*;

/// A type alias for a span
pub type Span = std::ops::Range<usize>;
/// A type alias for a spanned value
pub type Spanned<T> = (T, Span);
