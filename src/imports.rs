use crate::prelude::{parse, Expr, AST};
use fxhash::FxHashSet;

pub trait ImportResolver {
    fn resolve(&mut self, name: &str) -> Result<AST, String>;
}

pub fn resolve_imports_inner<R: ImportResolver>(
    ast: &mut AST,
    resolver: &mut R,
    resolved: &mut FxHashSet<String>,
) -> Result<(), String> {
    for (expr, _) in ast.iter_mut() {
        match expr {
            Expr::Import(path, block) => {
                if resolved.contains(path) {
                    Err(format!("Duplicate or Circular import: {}", path))?;
                }

                let mut imported_ast = resolver.resolve(&path)?;

                resolved.insert(path.clone());
                resolve_imports_inner(&mut imported_ast, resolver, resolved)?;

                *block = Some(imported_ast);
            }
            _ => {}
        }
    }

    Ok(())
}

pub fn resolve_imports(ast: &mut AST, resolver: &mut impl ImportResolver) -> Result<(), String> {
    let mut resolved = FxHashSet::default();
    resolve_imports_inner(ast, resolver, &mut resolved)
}

pub struct DefaultImportResolver;

impl ImportResolver for DefaultImportResolver {
    fn resolve(&mut self, name: &str) -> Result<AST, String> {
        let src = std::fs::read_to_string(name)
            .map_err(|_| format!("Could not find module '{}'", name))?;
        parse(&src)
    }
}
