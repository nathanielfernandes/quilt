pub mod builtins;
pub mod debug;

pub mod std;

pub mod backend;
pub mod code;
pub mod frontend;
pub mod imports;
pub mod shared;
pub mod treewalk;

/// The prelude module contains all the functions and types that are available to use.
pub mod prelude {
    pub use crate::builtins::*;

    pub use crate::context_builtins;
    pub use crate::generic_builtins;
    pub use crate::specific_builtins;

    pub use paste::paste;

    pub use crate::debug::*;

    pub use crate::code::*;
    pub use crate::frontend::*;
    pub use crate::imports::*;
    pub use crate::shared::*;
    pub use crate::std::*;
    pub use crate::treewalk::*;

    pub use self::parser::parser::parse_code;
}
