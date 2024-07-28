use common::pool::Pool;
use fxhash::FxHashSet;
use value::Function;

pub mod arith;
pub mod builtins;
pub mod value;
pub mod vm;

#[derive(Clone, Debug)]
pub struct Script {
    pub externals: FxHashSet<u16>,
    pub global_symbols: Pool<String, u16>,
    pub function: Function,
}

impl Script {
    pub fn blank(name: String) -> Self {
        Self {
            externals: FxHashSet::default(),
            global_symbols: Pool::new(),
            function: Function::blank(name),
        }
    }

    // check if an external is valid
    pub fn check_external(&self, symbol: &String) -> bool {
        let Some(id) = self.global_symbols.id(symbol) else {
            return false;
        };

        self.externals.contains(&id)
    }
}
