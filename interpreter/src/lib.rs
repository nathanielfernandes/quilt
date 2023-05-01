use builtins::BuiltinFnMap;
use value::Function;

pub mod arith;
pub mod builtins;
pub mod qstd;
pub mod value;
pub mod vm;

pub struct Script<Data> {
    pub global_symbols: Vec<String>,
    pub function: Function,
    pub builtins: BuiltinFnMap<Data>,
}
