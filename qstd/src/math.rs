use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use common::error::{Error, TypeError};
use interpreter::{builtins::*, generate_builtins, value::Value, vm::*};
use rand::Rng;

generate_builtins! {
    [export=math]

    fn @log(arg: any) {
        match arg {
            Value::Int(i) => Value::Float((i as f64).log10()),
            Value::Float(f) => Value::Float(f.log10()),
            _ => Err(TypeError::CannotConvert(arg.ntype(), "float"))?,
        }
    }

    fn @log2(arg: any) {
        match arg {
            Value::Int(i) => Value::Float((i as f64).log2()),
            Value::Float(f) => Value::Float(f.log2()),
            _ => Err(TypeError::CannotConvert(arg.ntype(), "float"))?,
        }
    }

    fn @log10(arg: any) {
        match arg {
            Value::Int(i) => Value::Float((i as f64).log10()),
            Value::Float(f) => Value::Float(f.log10()),
            _ => Err(TypeError::CannotConvert(arg.ntype(), "float"))?,
        }
    }

    fn @hash(arg: any) {
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

    fn @min(a: any, b: any) {
        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => Value::Int(*a.min(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.min(*b)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f64).min(*b) ),
            (Value::Float(a), Value::Int(b)) => Value::Float(a.min(*b as f64)),
            _ => Err(TypeError::CannotCompare(a.ntype(), b.ntype()))?,
        }
    }

    fn @max(a: any, b: any) {
        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => Value::Int(*a.max(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.max(*b)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f64).max(*b) ),
            (Value::Float(a), Value::Int(b)) => Value::Float(a.max(*b as f64)),
            _ => Err(TypeError::CannotCompare(a.ntype(), b.ntype()))?,
        }
    }

    fn @round(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.round() as i64),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    fn @floor(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.floor() as i64),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    fn @ceil(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.ceil() as i64),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    // fix number to n digits after the decimal point
    fn @fix(a: any, digits: int) {
        match (&a, digits) {
            (Value::Int(i), 0) => Value::Int(*i),
            (Value::Float(f), 0) => Value::Int(*f as i64),
            (Value::Int(i), d) => Value::Float(((*i as f64) * 10.0f64.powi(d as i32)).round() / 10.0f64.powi(d as i32)),
            (Value::Float(f), d) => Value::Float((*f * 10.0f64.powi(d as i32)).round() / 10.0f64.powi(d as i32)),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    fn @clamp(a: num, m: num, mm: num) {
        if m > mm {
            Err(error("min must be less than max"))?
        }

        if m.is_nan() || mm.is_nan() {
            Err(error("min and max must be numbers"))?
        }

        Value::Float(a.clamp(m, mm))
    }

    fn @abs(a: any) {
        match a {
            Value::Int(a) => Value::Int(a.abs()),
            Value::Float(a) => Value::Float(a.abs()),
            _ => Err(TypeError::Expected("num", a.ntype()))?,
        }
    }

    fn @sin(a: num) {
        Value::Float(a.sin())
    }

    fn @cos(a: num) {
        Value::Float(a.cos())
    }

    fn @tan(a: num) {
        Value::Float(a.tan())
    }

    fn @asin(a: num) {
        Value::Float(a.asin())
    }

    fn @acos(a: num) {
        Value::Float(a.acos())
    }

    fn @atan(a: num) {
        Value::Float(a.atan())
    }

    fn @atan2(a: num, b: num) {
        Value::Float(a.atan2(b))
    }

    fn @sqrt(a: num) {
        Value::Float(a.sqrt())
    }

    fn @random() {
        Value::Float(rand::random())
    }

    fn @randbool() {
        Value::Bool(rand::random())
    }

    fn @randint(start: int, end: int) {
        if start >= end {
            Err(error("start must be less than end"))?
        }

        Value::Int(rand::thread_rng().gen_range(start..end))
    }

    fn @randfloat(start: double, end: double) {
        if start >= end {
            Err(error("start must be less than end"))?
        }

        Value::Float(rand::thread_rng().gen_range(start..end))
    }

    fn @randchoice(list: list) {
        let mut list = list;
        if list.is_empty() {
            Err(error("list must not be empty"))?
        }

        let idx = rand::thread_rng().gen_range(0..list.len());
        list.swap_remove(idx)
    }

    fn @luma(c: color) {
        Value::Float(c[0] as f64 * 0.2126 + c[1] as f64  * 0.7152 + c[2] as f64  * 0.0722)
    }

    fn @hue(c: color) {
        Value::Float(c[0] as f64 * 0.299 + c[1] as f64  * 0.587 + c[2] as f64  * 0.114)
    }

    fn @saturation(c: color) {
        Value::Float((c[0] as f64 - 0.5).abs() + (c[1] as f64 - 0.5).abs() + (c[2] as f64 - 0.5).abs())
    }

    fn @brightness(c: color) {
        Value::Float(c[0] as f64 * 0.299 + c[1] as f64  * 0.587 + c[2] as f64  * 0.114)
    }

    fn @contrast(c: color) {
        Value::Float((c[0] as f64 - 0.5).abs() + (c[1] as f64 - 0.5).abs() + (c[2] as f64 - 0.5).abs())
    }
}
