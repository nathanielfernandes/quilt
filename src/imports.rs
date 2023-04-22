use crate::prelude::{ErrorS, ImportError, SourceCache, Spanned, AST};

pub trait ImportResolver {
    fn resolve(&mut self, cache: &mut SourceCache, name: Spanned<&str>) -> Result<AST, ErrorS>;
}

// pub const STD_MATH: &str = include_str!("../std/math.ql");

pub struct DefaultImportResolver;

impl ImportResolver for DefaultImportResolver {
    fn resolve(&mut self, cache: &mut SourceCache, name: Spanned<&str>) -> Result<AST, ErrorS> {
        // match name.0 {
        // "math" => Ok(cache.parse_with_imports(name.0, STD_MATH, self)?),
        // _ =>
        match std::fs::read_to_string(name.0) {
            Ok(src) => Ok(cache.parse_with_imports(name.0, &src, self)?),
            Err(_) => Err((
                ImportError::CouldNotResolve(name.0.to_string()).into(),
                name.1,
            )),
        }
        // ,
        // }
    }
}
