use std::io::Write;

use colored::Colorize;

use crate::{generic_builtins, shared::RuntimeError, shared::Value};

generic_builtins! {
    [export=std]

    fn @get(list: list, index: int) {
        if let Some(v) = list.get(index as usize) {
            v.clone()
        } else {
            error!(
                format!("index {} out of bounds", index).red().to_string()
            )?
        }
    }

    fn @str(arg: any) {
        Value::Str(arg.to_string())
    }

    fn @int(arg: any) {
        match arg {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f as i32),
            Value::Str(s) => match s.parse::<i32>() {
                Ok(i) => Value::Int(i),
                Err(_) => error!(format!("could not parse '{}' as {}", s, "int").red().to_string())?,
            },
            _ => error!(format!("type {} cannot be converted to {}",  arg.ntype().cyan(), "int".cyan()).yellow().to_string())?,
        }
    }

    fn @float(arg: any) {
        match arg {
            Value::Int(i) => Value::Float(i as f32),
            Value::Float(f) => Value::Float(f),
            Value::Str(s) => match s.parse::<f32>() {
                Ok(f) => Value::Float(f),
                Err(_) => error!(format!("could not parse '{}' as {}", s, "float").red().to_string())?,
            },
            _ => error!(format!("type {} cannot be converted to {}",  arg.ntype().cyan(), "float".cyan()).yellow().to_string())?,
        }
    }

    fn @bool(arg: any) {
        match arg {
            Value::Bool(b) => Value::Bool(b),
            Value::Int(i) => Value::Bool(i != 0),
            Value::Float(f) => Value::Bool(f != 0.0),
            Value::Str(s) => match s.parse::<bool>() {
                Ok(b) => Value::Bool(b),
                Err(_) => error!(format!("could not parse '{}' as {}", s, "bool").red().to_string())?,
            },
            _ => error!(format!("type {} cannot be converted to {}",  arg.ntype().cyan(), "bool".cyan()).yellow().to_string())?,
        }
    }

    fn @len(arg: any) {
        match arg {
            Value::List(l) => Value::Int(l.len() as i32),
            Value::Str(s) => Value::Int(s.len() as i32),
            _ => error!(format!("type {} cannot be converted to {}",  arg.ntype().cyan(), "len".cyan()).yellow().to_string())?,
        }
    }

    fn @list(start: u8, end: u8) {
        Value::List((start..end).map(|i| Value::Int(i as i32)).collect())
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
            _ => error!(format!("type {} cannot be compared to {}",  a.ntype().cyan(), b.ntype().cyan()).yellow().to_string())?,
        }
    }

    fn @max(a: any, b: any) {
        match (&a, &b) {
            (Value::Int(a), Value::Int(b)) => Value::Int(*a.max(b)),
            (Value::Float(a), Value::Float(b)) => Value::Float(a.max(*b)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f32).max(*b) ),
            (Value::Float(a), Value::Int(b)) => Value::Float(a.max(*b as f32)),
            _ => error!(format!("type {} cannot be compared to {}",  a.ntype().cyan(), b.ntype().cyan()).yellow().to_string())?,
        }
    }

    fn @round(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.round() as i32),
            _ => error!(format!("type {} cannot be converted to {}",  a.ntype().cyan(), "int".cyan()).yellow().to_string())?,
        }
    }

    fn @floor(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.floor() as i32),
            _ => error!(format!("type {} cannot be converted to {}",  a.ntype().cyan(), "int".cyan()).yellow().to_string())?,
        }
    }

    fn @ceil(a: any) {
        match a {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f.ceil() as i32),
            _ => error!(format!("type {} cannot be converted to {}",  a.ntype().cyan(), "int".cyan()).yellow().to_string())?,
        }
    }

    // fix number to n digits after the decimal point
    fn @fix(a: any, digits: int) {
        match (&a, digits) {
            (Value::Int(i), 0) => Value::Int(*i),
            (Value::Float(f), 0) => Value::Int(*f as i32),
            (Value::Int(i), d) => Value::Float(((*i as f32) * 10.0f32.powi(d)).round() / 10.0f32.powi(d)),
            (Value::Float(f), d) => Value::Float((*f * 10.0f32.powi(d)).round() / 10.0f32.powi(d)),
            _ => error!(format!("type {} cannot be converted to {}",  a.ntype().cyan(), "int".cyan()).yellow().to_string())?,

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

            _ => error!(format!("type {} cannot be compared with given range",  a.ntype().cyan()))?,
        }
    }

    fn @abs(a: any) {
        match a {
            Value::Int(a) => Value::Int(a.abs()),
            Value::Float(a) => Value::Float(a.abs()),
            _ => error!(format!("type {} cannot be compared to {}",  a.ntype().cyan(), "abs".cyan()).yellow().to_string())?,
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

    fn @input(prompt: str) {
        print!("{}", prompt);
        if let Ok(_) = std::io::stdout().flush() {
            let mut input = String::new();
            if let Ok(_) = std::io::stdin().read_line(&mut input) {
                Value::Str(input.trim().to_string())
            } else {
                error!("could not read input".red().to_string())?
            }
        } else {
            error!("could not flush stdout".red().to_string())?
        }
    }
}
