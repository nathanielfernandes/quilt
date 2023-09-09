use common::pool::Pool;
use value::Function;

pub mod arith;
pub mod builtins;
pub mod value;
pub mod vm;

#[derive(Clone, Debug)]
pub struct Script {
    pub global_symbols: Pool<String, u16>,
    pub function: Function,
}

impl Script {
    pub fn blank(name: String) -> Self {
        Self {
            global_symbols: Pool::new(),
            function: Function::blank(name),
        }
    }
}
