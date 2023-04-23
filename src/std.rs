use std::{io::Write, rc::Rc};

use crate::{generic_builtins, prelude::Value, shared::BuiltinError};

generic_builtins! {
    [export=std]

    fn @err(msg: str) {
        error!(msg)?
    }

    fn @get(list: list, index: int) {
        if let Some(v) = list.get(index as usize) {
            v.clone()
        } else {
            error!(
                format!("index {} out of bounds", index)
            )?
        }
    }

    fn @str(arg: any) {
        Value::String(Rc::new(arg.to_string()))
    }

    fn @int(arg: any) {
        match arg {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f as i32),
            Value::String(s) => match s.parse::<i32>() {
                Ok(i) => Value::Int(i),
                Err(_) => error!(format!("could not parse '{}' as `{}`", s, "int"))?,
            },
            _ => error!(format!("type `{}` cannot be converted to `{}`",  arg.ntype(), "int"))?,
        }
    }

    fn @float(arg: any) {
        match arg {
            Value::Int(i) => Value::Float(i as f32),
            Value::Float(f) => Value::Float(f),
            Value::String(s) => match s.parse::<f32>() {
                Ok(f) => Value::Float(f),
                Err(_) => error!(format!("could not parse '{}' as `{}`", s, "float").to_string())?,
            },
            _ => error!(format!("type `{}` cannot be converted to `{}`",  arg.ntype(), "float"))?,
        }
    }

    fn @bool(arg: any) {
        match arg {
            Value::Bool(b) => Value::Bool(b),
            Value::Int(i) => Value::Bool(i != 0),
            Value::Float(f) => Value::Bool(f != 0.0),
            Value::String(s) => match s.parse::<bool>() {
                Ok(b) => Value::Bool(b),
                Err(_) => error!(format!("could not parse '{}' as `{}`", s, "bool").to_string())?,
            },
            _ => error!(format!("type `{}` cannot be converted to `{}`",  arg.ntype(), "bool"))?,
        }
    }

    fn @len(arg: any) {
        match arg {
            Value::List(l) => Value::Int(l.len() as i32),
            Value::String(s) => Value::Int(s.len() as i32),
            _ => error!(format!("type `{}` cannot be converted to `{}`", arg.ntype(), "len"))?,
        }
    }

    fn @list(start: u8, end: u8) {
        Value::List(Rc::new((start..end).map(|i| Value::Int(i as i32)).collect()))
    }

    fn @rgba(r: u8, g: u8, b: u8, a: u8) {
        Value::Color([r, g, b, a])
    }

    fn @rgb(r: u8, g: u8, b: u8) {
        Value::Color([r, g, b, 255])
    }
}

generic_builtins! {
    [export=math]

    fn @min(a: any, b: any) {
        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => Value::Int(*a.min(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.min(*b)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f32).min(*b) ),
            (Value::Float(a), Value::Int(b)) => Value::Float(a.min(*b as f32)),
            _ => error!(format!("type `{}` cannot be compared to `{}`",  a.ntype(), b.ntype()))?,
        }
    }

    fn @max(a: any, b: any) {
        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => Value::Int(*a.max(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.max(*b)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f32).max(*b) ),
            (Value::Float(a), Value::Int(b)) => Value::Float(a.max(*b as f32)),
            _ => error!(format!("type `{}` cannot be compared to `{}`",  a.ntype(), b.ntype()))?,
        }
    }

    fn @round(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.round() as i32),
            _ => error!(format!("type `{}` cannot be converted to `{}`",  a.ntype(), "int"))?,
        }
    }

    fn @floor(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.floor() as i32),
            _ => error!(format!("type `{}` cannot be converted to `{}`",  a.ntype(), "int"))?,
        }
    }

    fn @ceil(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.ceil() as i32),
            _ => error!(format!("type `{}` cannot be converted to `{}`",  a.ntype(), "int"))?,
        }
    }

    // fix number to n digits after the decimal point
    fn @fix(a: any, digits: int) {
        match (&a, digits) {
            (Value::Int(i), 0) => Value::Int(*i),
            (Value::Float(f), 0) => Value::Int(*f as i32),
            (Value::Int(i), d) => Value::Float(((*i as f32) * 10.0f32.powi(d)).round() / 10.0f32.powi(d)),
            (Value::Float(f), d) => Value::Float((*f * 10.0f32.powi(d)).round() / 10.0f32.powi(d)),
            _ => error!(format!("type `{}` cannot be converted to `{}`",  a.ntype(), "int"))?,

        }
    }

    fn @clamp(a: any, m: any, mm: any) {
        match (&a, &m, &mm) {
            (Value::Int(a), Value::Int(min), Value::Int(max)) => Value::Int(*a.min(max).min(min)),
            (Value::Float(a), Value::Float(min), Value::Float(max)) => Value::Float(a.min(*max).min(*min)),
            (Value::Int(a), Value::Float(min), Value::Float(max)) => Value::Float((*a as f32).min(*max).min(*min)),
            (Value::Float(a), Value::Int(min), Value::Int(max)) => Value::Float(a.min(*max as f32).min(*min as f32)),
            (Value::Int(a), Value::Int(min), Value::Float(max)) => Value::Float((*a as f32).min(*max).min(*min as f32)),
            (Value::Int(a), Value::Float(min), Value::Int(max)) => Value::Float((*a as f32).min(*max as f32).min(*min)),
            (Value::Float(a), Value::Int(min), Value::Float(max)) => Value::Float(a.min(*max).min(*min as f32)),
            (Value::Float(a), Value::Float(min), Value::Int(max)) => Value::Float(a.min(*max as f32).min(*min)),

            _ => error!(format!("type `{}` cannot be compared with given range",  a.ntype()))?,
        }
    }

    fn @abs(a: any) {
        match a {
            Value::Int(a) => Value::Int(a.abs()),
            Value::Float(a) => Value::Float(a.abs()),
            _ => error!(format!("type `{}` cannot be compared to `{}`",  a.ntype(), "abs"))?,
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

    fn @randint(start: int, end: int) {
        if start >= end {
            error!("start must be less than end".to_string())?
        }

        Value::Int(rand::random::<i32>() % (end - start) + start)
    }

    fn @randfloat(start: float, end: float) {
        if start >= end {
            error!("start must be less than end".to_string())?
        }

        Value::Float(rand::random::<f32>() % (end - start) + start)
    }
}

generic_builtins! {
    [export=io]

    fn @print(to_print: rest) {
        for arg in to_print {
            print!("{} ", arg);
        }
        println!();
        Value::None
    }

    fn @debug(to_print: rest) {
        // orange
        print!("\x1b[38;5;208m");

        for arg in to_print {
            print!("{} ", arg);
        }
        // reset
        println!("\x1b[0m");
        Value::None
    }

    fn @input(prompt: str) {
        print!("{}", prompt);
        if let Ok(_) = std::io::stdout().flush() {
            let mut input = String::new();
            if let Ok(_) = std::io::stdin().read_line(&mut input) {
                Value::String(Rc::new(input.trim().to_string()))
            } else {
                error!("could not read input".to_string())?
            }
        } else {
            error!("could not flush stdout".to_string())?
        }
    }
}
