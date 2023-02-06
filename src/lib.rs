pub mod builtins;
pub mod debug;
pub mod eval;
pub mod parser;
pub mod std;
pub mod value;

/// The prelude module contains all the functions and types that are available to use.
pub mod prelude {
    pub use crate::builtins::*;

    pub use crate::generic_builtins;
    pub use crate::specific_builtins;
    pub use paste::paste;

    pub use crate::debug::*;
    pub use crate::eval::*;
    pub use crate::parser::*;
    pub use crate::std::*;
    pub use crate::value::*;

    use self::parser::parse_code;

    pub type AST = Vec<Spanned<Expr>>;

    /// Parse a string into a Quilt ast which can then be evaluated in a [`VM`].
    ///
    /// ### Parameters
    /// * `src`: The source code to parse.
    pub fn parse(src: &str) -> Result<AST, String> {
        match parse_code(src) {
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
}
