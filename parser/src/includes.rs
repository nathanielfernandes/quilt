use common::{
    error::{ErrorS, IncludeError},
    sourcecache::SourceCache,
    span::Spanned,
};
use fxhash::FxHashSet;

use crate::{node::Node, parse_code};

pub type AST = Vec<Spanned<Node>>;

pub trait IncludeResolver {
    fn resolve(&mut self, cache: &mut SourceCache, name: Spanned<&str>) -> Result<AST, ErrorS>;
    fn was_resolved(&self, name: &str) -> bool;
}

#[derive(Default)]
pub struct DefaultIncludeResolver {
    resolved: FxHashSet<String>,
}

impl IncludeResolver for DefaultIncludeResolver {
    fn resolve(&mut self, cache: &mut SourceCache, name: Spanned<&str>) -> Result<AST, ErrorS> {
        match std::fs::read_to_string(name.0) {
            Ok(src) => match cache.parse_with_includes(name.0, &src, self) {
                Ok(ast) => {
                    self.resolved.insert(name.0.to_string());
                    Ok(ast)
                }
                Err(e) => Err(e),
            },
            Err(_) => Err((
                IncludeError::CouldNotResolve(name.0.to_string()).into(),
                name.1,
            )),
        }
    }

    fn was_resolved(&self, name: &str) -> bool {
        self.resolved.contains(name)
    }
}

pub trait Cache {
    fn parse_with_includes<R: IncludeResolver>(
        &mut self,
        name: &str,
        src: &str,
        resolver: &mut R,
    ) -> Result<AST, ErrorS>;

    fn resolve_includes<R: IncludeResolver>(
        &mut self,
        ast: &mut AST,
        resolver: &mut R,
    ) -> Result<(), ErrorS>;
}

impl Cache for SourceCache {
    fn parse_with_includes<R: IncludeResolver>(
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
            Err(e) => Err(e),
        }
    }

    fn resolve_includes<R: IncludeResolver>(
        &mut self,
        ast: &mut AST,
        resolver: &mut R,
    ) -> Result<(), ErrorS> {
        for (expr, _) in ast.iter_mut() {
            match expr {
                Node::Include((name, span), body) => {
                    if resolver.was_resolved(name) {
                        Err((
                            IncludeError::CircularInclude(name.clone()).into(),
                            span.clone(),
                        ))?;
                    }

                    let mut included_ast = resolver.resolve(self, (&name, span.clone()))?;

                    self.resolve_includes(&mut included_ast, resolver)?;

                    *body = Some(included_ast);
                }
                _ => {}
            }
        }

        Ok(())
    }
}
