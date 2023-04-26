use crate::prelude::*;

pub type AST = Vec<Spanned<Expr>>;

/// Parse a string into a Quilt ast which can then be evaluated in a [`VM`].
///
/// ### Parameters
/// * `src`: The source code to parse.
pub fn parse(src: &str) -> Result<AST, String> {
    match parse_code(src, 0) {
        Ok(ast) => Ok(ast),
        Err(e) => {
            let line = e.location.line;
            let col = e.location.column;

            Err(format!(
                "Error at line {}, column {}: expected {}",
                line, col, e.expected
            ))
        }
    }
}

use ariadne::{Cache, Source};
use fxhash::FxHashSet;

pub struct Src {
    pub name: String,
    pub source: Source,
}
pub struct SourceCache {
    sources: Vec<Src>,
    resolved: FxHashSet<String>,
}

impl Cache<usize> for SourceCache {
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
            resolved: FxHashSet::default(),
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

    pub fn parse_with_includes<R: IncludeResolver>(
        &mut self,
        name: &str,
        src: &str,
        resolver: &mut R,
    ) -> Result<AST, ErrorS> {
        let id = self.add(name.to_string(), src);

        match parse_code(src, id) {
            Ok(mut ast) => {
                self.resolve_includes(&mut ast, resolver)?;
                Ok(ast)
            }
            Err(e) => {
                let line = e.location.line;
                let column = e.location.column;
                let offset = e.location.offset;

                Err((
                    SyntaxError::UnexpectedToken {
                        line,
                        column,
                        expected: e.expected,
                    }
                    .into(),
                    Span(offset, offset, id),
                ))
            }
        }
    }

    pub fn resolve_includes<R: IncludeResolver>(
        &mut self,
        ast: &mut AST,
        resolver: &mut R,
    ) -> Result<(), ErrorS> {
        for (expr, _) in ast.iter_mut() {
            match expr {
                Expr::Include((path, span), body) => {
                    if self.resolved.contains(path) {
                        Err((
                            IncludeError::CircularInclude(path.clone()).into(),
                            span.clone(),
                        ))?;
                    }

                    self.resolved.insert(path.clone());

                    let mut included_ast = resolver.resolve(self, (&path, span.clone()))?;

                    self.resolve_includes(&mut included_ast, resolver)?;

                    *body = Some(included_ast);
                }
                _ => {}
            }
        }

        Ok(())
    }
}
