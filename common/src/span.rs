/// A span in a source file
///
/// ### Fields
/// * `0`: The start of the span
/// * `1`: The end of the span
/// * `2`: The source id
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span(
    /// The start of the span
    pub usize,
    /// The end of the span
    pub usize,
    /// src id
    pub usize,
);

impl ariadne::Span for Span {
    fn start(&self) -> usize {
        self.0
    }

    fn end(&self) -> usize {
        self.1
    }

    fn len(&self) -> usize {
        self.1.saturating_sub(self.0)
    }

    type SourceId = usize;

    fn source(&self) -> &Self::SourceId {
        &self.2
    }
}

/// A type alias for a spanned value
pub type Spanned<T> = (T, Span);
