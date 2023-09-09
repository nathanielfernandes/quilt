use crate::span::Span;
use ariadne::{Cache, Source};

pub struct Src {
    pub name: String,
    pub source: Source,
}
pub struct SourceCache {
    sources: Vec<Src>,
}

impl Cache<usize> for &SourceCache {
    fn fetch(&mut self, id: &usize) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        self.sources
            .get(*id)
            .ok_or_else(|| Box::new("Source not found") as _)
            .map(|src| &src.source)
    }

    fn display<'a>(&self, id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.sources
            .get(*id)
            .map(|src| Box::new(src.name.clone()) as _)
    }
}

impl SourceCache {
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    pub fn get_lineno(&self, span: &Span) -> Option<usize> {
        self.sources
            .get(span.2)
            .map(|src| {
                if src.source.len() == 0 {
                    return None;
                }
                src.source.get_offset_line(span.0).map(|(_, l, _)| l + 1)
            })
            .flatten()
    }

    pub fn get(&self, id: usize) -> Option<&Src> {
        self.sources.get(id)
    }

    pub fn add(&mut self, name: String, src: &str) -> usize {
        let id = self.sources.len();
        self.sources.push(Src {
            name,
            source: Source::from(src),
        });
        id
    }
}
