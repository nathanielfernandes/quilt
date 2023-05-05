mod tests;

pub mod prelude {
    pub use parser::includes::*;
    pub use parser::node::Node;
    pub use parser::{parse_code, parse_literal};

    pub use compiler::{literal_to_value, Compiler};

    pub use interpreter::builtins::{Consumable, VmData};
    pub use interpreter::qstd;
    pub use interpreter::value::Value;
    pub use interpreter::vm::VM;
    pub use interpreter::Script;

    pub use interpreter::context_builtins;
    pub use interpreter::generic_builtins;
    pub use interpreter::specific_builtins;

    pub use common::error::{BuiltinError, Error, ErrorExt, ErrorS};
    pub use common::sourcecache::SourceCache;
    pub use common::span::{Span, Spanned};

    pub use disassembler::Disassembler;

    pub mod builtins {
        pub use interpreter::builtins::*;
    }
}
