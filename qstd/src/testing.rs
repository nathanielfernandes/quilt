use common::error::Error;
use interpreter::{builtins::*, generate_builtins, value::Value, vm::*};

generate_builtins! {
    ///Testing asserts
    [export=testing]

    ///Assert that a value is true
    fn @assert(value: bool) {
        if !value {
            return Err(error("assertion failed"))?
        };

        Value::None
    }

    ///Assert that two values are equal
    fn @assert_eq(a: any, b: any) {
        if a != b {
            return Err(error(format!("assertion failed: `(left == right)`\n  left: `{}`\n right: `{}`", a, b)))?
        };

        Value::None
    }

    ///Assert that two values are not equal
    fn @assert_ne(a: any, b: any) {
        if a == b {
            return Err(error(format!("assertion failed: `(left != right)`\n  left: `{}`\n right: `{}`", a, b)))?
        };

        Value::None
    }

    ///Panic that this code is unreachable
    fn @unreachable() {
        Err(error("unreachable code reached"))?
    }

    ///Panic with a message
    fn @panic(message: str) {
        Err(error(message))?
    }

    ///Panic that this code is not yet implemented
    fn @todo() {
        Err(error("not yet implemented"))?
    }
}
