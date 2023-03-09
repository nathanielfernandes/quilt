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
use fxhash::{FxHashMap, FxHashSet};

pub struct Src {
    pub name: String,
    pub source: Source,
}
pub struct SourceCache {
    sources: FxHashMap<usize, Src>,
    resolved: FxHashSet<String>,
}

impl Cache<usize> for SourceCache {
    fn fetch(&mut self, id: &usize) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        self.sources
            .get(id)
            .ok_or_else(|| Box::new("Source not found") as _)
            .map(|src| &src.source)
    }

    fn display<'a>(&self, id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.sources
            .get(id)
            .map(|src| Box::new(src.name.clone()) as _)
    }
}

impl SourceCache {
    pub fn new() -> Self {
        Self {
            sources: FxHashMap::default(),
            resolved: FxHashSet::default(),
        }
    }

    pub fn add(&mut self, name: String, src: &str) -> usize {
        let id = self.sources.len();
        self.sources.insert(
            id,
            Src {
                name,
                source: Source::from(src),
            },
        );
        id
    }

    pub fn parse_with_imports<R: ImportResolver>(
        &mut self,
        name: &str,
        src: &str,
        resolver: &mut R,
    ) -> Result<AST, Spanned<String>> {
        let id = self.add(name.to_string(), src);

        match parse_code(src, id) {
            Ok(mut ast) => {
                self.resolve_imports(&mut ast, resolver)?;
                Ok(ast)
            }
            Err(e) => {
                let line = e.location.line;
                let col = e.location.column;

                Err((
                    format!(
                        "Error at line {}, column {}: expected {}",
                        line, col, e.expected
                    ),
                    Span(0, 0, id),
                ))
            }
        }
    }

    pub fn resolve_imports<R: ImportResolver>(
        &mut self,
        ast: &mut AST,
        resolver: &mut R,
    ) -> Result<(), Spanned<String>> {
        for (expr, _) in ast.iter_mut() {
            match expr {
                Expr::Import((path, span), block) => {
                    if self.resolved.contains(path) {
                        Err((
                            format!("Duplicate or Circular import: {}", path),
                            span.clone(),
                        ))?;
                    }

                    self.resolved.insert(path.clone());

                    let mut imported_ast = resolver.resolve(self, (&path, span.clone()))?;

                    self.resolve_imports(&mut imported_ast, resolver)?;

                    *block = Some(imported_ast);
                }
                _ => {}
            }
        }

        Ok(())
    }
}