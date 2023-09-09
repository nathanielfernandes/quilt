use common::error::Error;
use interpreter::{builtins::*, generic_builtins, value::Value, vm::*};

generic_builtins! {
    [export=testing]
    [vm_options=options]

    fn @assert(value: bool) {
        if !value {
            return Err(error("assertion failed"))?
        };

        Value::None
    }

    fn @assert_eq(a: any, b: any) {
        if a != b {
            return Err(error(format!("assertion failed: `(left == right)`\n  left: `{}`\n right: `{}`", a, b)))?
        };

        Value::None
    }
}