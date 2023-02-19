use crate::prelude::{parse, Expr, Spanned, AST};
use fxhash::FxHashSet;

pub trait ImportResolver {
    fn resolve(&mut self, name: Spanned<&str>) -> Result<AST, Spanned<String>>;
}

pub fn resolve_imports_inner<R: ImportResolver>(
    ast: &mut AST,
    resolver: &mut R,
    resolved: &mut FxHashSet<String>,
) -> Result<(), Spanned<String>> {
    for (expr, _) in ast.iter_mut() {
        match expr {
            Expr::Import((path, span), block) => {
                if resolved.contains(path) {
                    Err((
                        format!("Duplicate or Circular import: {}", path),
                        span.clone(),
                    ))?;
                }

                let mut imported_ast = resolver.resolve((&path, span.clone()))?;

                resolved.insert(path.clone());
                resolve_imports_inner(&mut imported_ast, resolver, resolved)?;

                *block = Some(imported_ast);
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn resolve_imports(
    ast: &mut AST,
    resolver: &mut impl ImportResolver,
) -> Result<(), Spanned<String>> {
    let mut resolved = FxHashSet::default();
    resolve_imports_inner(ast, resolver, &mut resolved)
}

pub struct DefaultImportResolver;

impl ImportResolver for DefaultImportResolver {
    fn resolve(&mut self, name: Spanned<&str>) -> Result<AST, Spanned<String>> {
        match std::fs::read_to_string(name.0) {
            Ok(src) => parse(&src).map_err(|e| (e, name.1)),
            Err(_) => Err((format!("Could not find import '{}'", name.0), name.1)),
        }
    }
}
