use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use common::error::{Error, TypeError};
use interpreter::{builtins::*, generate_builtins, value::Value, vm::*};
use rand::Rng;

generate_builtins! {
    ///Math functions.
    [export=math]

    ///Compute the log10 of a number.
    fn @log(arg: any) -> float {
        match arg {
            Value::Int(i) => Value::Float((i as f64).log10()),
            Value::Float(f) => Value::Float(f.log10()),
            _ => Err(TypeError::CannotConvert(arg.ntype(), "float"))?,
        }
    }

    ///Compute the log2 of a number.
    fn @log2(arg: any) -> float {
        match arg {
            Value::Int(i) => Value::Float((i as f64).log2()),
            Value::Float(f) => Value::Float(f.log2()),
            _ => Err(TypeError::CannotConvert(arg.ntype(), "float"))?,
        }
    }

    ///Compute the log10 of a number.
    fn @log10(arg: any) -> float {
        match arg {
            Value::Int(i) => Value::Float((i as f64).log10()),
            Value::Float(f) => Value::Float(f.log10()),
            _ => Err(TypeError::CannotConvert(arg.ntype(), "float"))?,
        }
    }

    ///Compute an arbitrary hash of a value.
    fn @hash(arg: any) -> int {
        use Value::*;
        match arg {
            Special(_) | Array(_) | LoopCtx(_) | Function(_) | Closure(_) => {
                Err(TypeError::CannotHash(arg.ntype()))?
            },
             _ => {
                let mut hasher = DefaultHasher::new();
                arg.hash(&mut hasher);

                let hash = hasher.finish();

                // make value positive u32
                let hash = hash as i64 & 0x7FFFFFFF;

                Value::Int(hash as i64)
             }
        }
    }

    ///Get the min of two numbers.
    fn @min(a: any, b: any) -> num {
        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => Value::Int(*a.min(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.min(*b)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f64).min(*b) ),
            (Value::Float(a), Value::Int(b)) => Value::Float(a.min(*b as f64)),
            _ => Err(TypeError::CannotCompare(a.ntype(), b.ntype()))?,
        }
    }

    ///Get the max of two numbers.
    fn @max(a: any, b: any) -> num {
        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => Value::Int(*a.max(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.max(*b)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f64).max(*b) ),
            (Value::Float(a), Value::Int(b)) => Value::Float(a.max(*b as f64)),
            _ => Err(TypeError::CannotCompare(a.ntype(), b.ntype()))?,
        }
    }

    ///Round a number to the nearest whole number.
    fn @round(a: any) -> int {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.round() as i64),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    ///Round a number down to the nearest whole number.
    fn @floor(a: any) -> int {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.floor() as i64),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    ///Round a number up to the nearest whole number.
    fn @ceil(a: any) -> int {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.ceil() as i64),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    ///Fix number to n digits after the decimal point.
    fn @fix(a: any, digits: int) -> num {
        match (&a, digits) {
            (Value::Int(i), 0) => Value::Int(*i),
            (Value::Float(f), 0) => Value::Int(*f as i64),
            (Value::Int(i), d) => Value::Float(((*i as f64) * 10.0f64.powi(d as i32)).round() / 10.0f64.powi(d as i32)),
            (Value::Float(f), d) => Value::Float((*f * 10.0f64.powi(d as i32)).round() / 10.0f64.powi(d as i32)),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    ///Clamp a number between two values.
    fn @clamp(a: num, m: num, mm: num) -> float {
        if m > mm {
            Err(error("min must be less than max"))?
        }

        if m.is_nan() || mm.is_nan() {
            Err(error("min and max must be numbers"))?
        }

        Value::Float(a.clamp(m, mm))
    }

    ///Linearly interpolate between two numbers.
    fn @interp(a: num, b: num, t: num) -> float {
        Value::Float(a + (b - a) * t)
    }

    ///Get the absolute value of a number.
    fn @abs(a: any) -> num {
        match a {
            Value::Int(a) => Value::Int(a.abs()),
            Value::Float(a) => Value::Float(a.abs()),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    ///Compute the sin of a number in radians.
    fn @sin(a: num) -> float {
        Value::Float(a.sin())
    }

    ///Compute the cos of a number in radians.
    fn @cos(a: num) -> float {
        Value::Float(a.cos())
    }

    ///Compute the tan of a number in radians.
    fn @tan(a: num) -> float {
        Value::Float(a.tan())
    }

    ///Compute the asin of a number in radians.
    fn @asin(a: num) -> float {
        Value::Float(a.asin())
    }

    ///Compute the acos of a number in radians.
    fn @acos(a: num) -> float {
        Value::Float(a.acos())
    }

    ///Compute the atan of a number in radians.
    fn @atan(a: num) -> float {
        Value::Float(a.atan())
    }

    ///Compute the atan2 of a number in radians.
    fn @atan2(a: num, b: num) -> float {
        Value::Float(a.atan2(b))
    }

    ///Compute the sqrt of a number.
    fn @sqrt(a: num) -> float {
        Value::Float(a.sqrt())
    }

    ///Get a random number between 0 and 1.
    fn @random() -> float {
        Value::Float(rand::random())
    }

    ///Get a random boolean.
    fn @randbool() -> bool {
        Value::Bool(rand::random())
    }

    ///Get a random integer between two values.
    fn @randint(start: int, end: int) -> int {
        if start >= end {
            Err(error("start must be less than end"))?
        }

        Value::Int(rand::thread_rng().gen_range(start..end))
    }

    ///Get a random float between two values.
    fn @randfloat(start: double, end: double) -> float {
        if start >= end {
            Err(error("start must be less than end"))?
        }

        Value::Float(rand::thread_rng().gen_range(start..end))
    }

    ///Get a random value from a list.
    fn @randchoice(list: list) -> any {
        let mut list = list;
        if list.is_empty() {
            Err(error("list must not be empty"))?
        }

        let idx = rand::thread_rng().gen_range(0..list.len());
        list.swap_remove(idx)
    }
}
