mod tests;

pub mod prelude {
    pub use parser::includes::*;
    pub use parser::node::Node;
    pub use parser::{parse_code, parse_literal};

    pub use compiler::{literal_to_value, Compiler};

    pub use interpreter::builtins::{error, error_with_help, expected, Consumable, VmData};
    pub use interpreter::value::Value;
    pub use interpreter::vm::{VmOptions, VM};
    pub use interpreter::Script;
    pub use qstd;

    pub use interpreter::context_builtins;
    pub use interpreter::generic_builtins;
    pub use interpreter::specific_builtins;

    pub use common::error::*;
    pub use common::sourcecache::SourceCache;
    pub use common::span::{Span, Spanned};
    pub use common::vecc::{GetSize, Vecc};

    #[cfg(feature = "disassembler")]
    pub use disassembler::{
        structure::FunctionDisassembly, structure::Hint, structure::Line, Disassembler,
    };

    pub mod builtins {
        pub use interpreter::builtins::*;
    }
}
