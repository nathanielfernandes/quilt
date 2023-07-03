use common::pool::Pool;
use value::Function;

pub mod arith;
pub mod builtins;
pub mod qstd;
pub mod value;
pub mod vm;

#[derive(Clone, Debug)]
pub struct Script {
    pub global_symbols: Pool<String, u16>,
    pub function: Function,
}
