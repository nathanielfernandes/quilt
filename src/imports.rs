use crate::prelude::{SourceCache, Spanned, AST};

pub trait ImportResolver {
    fn resolve(
        &mut self,
        cache: &mut SourceCache,
        name: Spanned<&str>,
    ) -> Result<AST, Spanned<String>>;
}

pub struct DefaultImportResolver;

impl ImportResolver for DefaultImportResolver {
    fn resolve(
        &mut self,
        cache: &mut SourceCache,
        name: Spanned<&str>,
    ) -> Result<AST, Spanned<String>> {
        match std::fs::read_to_string(name.0) {
            Ok(src) => Ok(cache.parse_with_imports(name.0, &src, self)?),
            Err(_) => Err((format!("Could not find import '{}'", name.0), name.1)),
        }
    }
}
