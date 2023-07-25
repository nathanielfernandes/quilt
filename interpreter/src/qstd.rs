use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    io::Write,
    rc::Rc,
};

use common::error::{Error, TypeError};
use rand::Rng;

use crate::{
    arith::fix_index,
    builtins::{error, VmData},
    generic_builtins,
    value::{make_value_array, Value},
    vm::VM,
};

#[inline]
pub fn std<Data>(vm: &mut VM<Data>)
where
    Data: VmData,
{
    vm.add_builtins(core);
    vm.add_builtins(math);
    vm.add_builtins(strings);
}

#[inline]
pub fn stdio<Data>(vm: &mut VM<Data>)
where
    Data: VmData,
{
    vm.add_builtins(std);
    vm.add_builtins(io);
}

generic_builtins! {
    [export=core]
    [vm_options=options]

    fn @get(list: list, index: int) {
        let idx = fix_index(index, list.len())?;

        if let Some(v) = list.get(idx) {
            v.clone()
        } else {
            Err(Error::IndexError(index))?
        }
    }

    fn @set(list: list, index: int, value: any) {
        let idx = fix_index(index, list.len())?;

        let mut list = list;
        if let Some(v) = list.get_mut(idx) {
            *v = value;
            make_value_array(list, options.array_max_size)?
        } else {
            Err(Error::IndexError(index))?
        }
    }

    fn @slice(list: list, start: int, end: int) {
        let len = list.len() as i64;

        // only allow negative indices if they are in bounds
        let start = if start < 0 {
            if -start > len {
                Err(Error::IndexError(start))?
            } else {
                len + start
            }
        } else {
            start
        };

        let end = if end < 0 {
            if -end > len {
                Err(Error::IndexError(start))?
            } else {
                len + end
            }
        } else {
            end
        };

        if start > end {
            Err(error("start index is greater than end index"))?
        }

        list[start as usize..end as usize].to_vec().into()
    }

    fn @str(arg: any) {
        Value::String(Rc::new(arg.to_string()))
    }

    fn @int(arg: any) {
        match arg {
            Value::Int(i) => Value::Int(i),
            Value::Float(f) => Value::Int(f as i64),
            Value::String(s) => match s.parse::<i64>() {
                Ok(i) => Value::Int(i),
                Err(_) => Err(TypeError::ParseError(s.to_string(), "int"))?,
            },
            _ => Err(TypeError::CannotConvert(arg.ntype(), "int"))?,
        }
    }

    fn @float(arg: any) {
        match arg {
            Value::Int(i) => Value::Float(i as f64),
            Value::Float(f) => Value::Float(f),
            Value::String(s) => match s.parse::<f64>() {
                Ok(f) => Value::Float(f),
                Err(_) => Err(TypeError::ParseError(s.to_string(), "float"))?,
            },
            _ => Err(TypeError::CannotConvert(arg.ntype(), "int"))?,
        }
    }

    fn @bool(arg: any) {
        match arg {
            Value::Bool(b) => Value::Bool(b),
            Value::Int(i) => Value::Bool(i != 0),
            Value::Float(f) => Value::Bool(f != 0.0),
            Value::String(s) => match s.parse::<bool>() {
                Ok(b) => Value::Bool(b),
                Err(_) => Err(TypeError::ParseError(s.to_string(), "bool"))?,
            },
            _ => Err(TypeError::CannotConvert(arg.ntype(), "bool"))?,
        }
    }

    fn @len(arg: any) {
        match arg {
            Value::Array(l) => Value::Int(l.len() as i64),
            Value::String(s) => Value::Int(s.len() as i64),
            _ => Err(error(format!("type `{}` has no length", arg.ntype())))?,
        }
    }

    fn @rgba(r: u8, g: u8, b: u8, a: u8) {
        Value::Color([r, g, b, a])
    }

    fn @rgb(r: u8, g: u8, b: u8) {
        Value::Color([r, g, b, 255])
    }

    fn @hsla(h: num, s: float, l: float, a: float) {
        Value::Color(hsla_to_rgba(h, s, l, a))
    }

    fn @hsl(h: num, s: float, l: float) {
        Value::Color(hsla_to_rgba(h, s, l, 1.0))
    }

    fn @hsva(h: num, s: float, v: float, a: float) {
        Value::Color(hsva_to_rgba(h, s, v, a))
    }

    fn @hsv(h: num, s: float, v: float) {
        Value::Color(hsva_to_rgba(h, s, v, 1.0))
    }

    fn @gettype(arg: any) {
        arg.ntype().into()
    }

    fn @r(c: color) {
        Value::Int(c[0] as i64)
    }

    fn @g(c: color) {
        Value::Int(c[1] as i64)
    }

    fn @b(c: color) {
        Value::Int(c[2] as i64)
    }

    fn @a(c: color) {
        Value::Int(c[3] as i64)
    }
}

#[inline]
fn hsla_to_rgba(h: f64, s: f64, l: f64, a: f64) -> [u8; 4] {
    let c = (1.0 - (2.0 * l - 1.0).abs()) * s;
    let h_ = h / 60.0;
    let x = c * (1.0 - (h_ % 2.0 - 1.0).abs());
    let m = l - c / 2.0;

    let (r, g, b) = if h_ < 1.0 {
        (c, x, 0.0)
    } else if h_ < 2.0 {
        (x, c, 0.0)
    } else if h_ < 3.0 {
        (0.0, c, x)
    } else if h_ < 4.0 {
        (0.0, x, c)
    } else if h_ < 5.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };

    [
        ((r + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((g + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((b + m).max(0.0).min(1.0) * 255.0).round() as u8,
        (a * 255.0).round() as u8,
    ]
}

#[inline]
fn hsva_to_rgba(h: f64, s: f64, v: f64, a: f64) -> [u8; 4] {
    let c = v * s;
    let h_ = h / 60.0;
    let x = c * (1.0 - (h_ % 2.0 - 1.0).abs());
    let m = v - c;

    let (r, g, b) = if h_ < 1.0 {
        (c, x, 0.0)
    } else if h_ < 2.0 {
        (x, c, 0.0)
    } else if h_ < 3.0 {
        (0.0, c, x)
    } else if h_ < 4.0 {
        (0.0, x, c)
    } else if h_ < 5.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };

    [
        ((r + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((g + m).max(0.0).min(1.0) * 255.0).round() as u8,
        ((b + m).max(0.0).min(1.0) * 255.0).round() as u8,
        (a * 255.0).round() as u8,
    ]
}

generic_builtins! {
    [export=math]
    [vm_options=options]

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

    fn @clamp(a: any, m: any, mm: any) {
        match (&a, &m, &mm) {
            (Value::Int(a), Value::Int(min), Value::Int(max)) => Value::Int(*a.min(max).min(min)),
            (Value::Float(a), Value::Float(min), Value::Float(max)) => Value::Float(a.min(*max).min(*min)),
            (Value::Int(a), Value::Float(min), Value::Float(max)) => Value::Float((*a as f64).min(*max).min(*min)),
            (Value::Float(a), Value::Int(min), Value::Int(max)) => Value::Float(a.min(*max as f64).min(*min as f64)),
            (Value::Int(a), Value::Int(min), Value::Float(max)) => Value::Float((*a as f64).min(*max).min(*min as f64)),
            (Value::Int(a), Value::Float(min), Value::Int(max)) => Value::Float((*a as f64).min(*max as f64).min(*min)),
            (Value::Float(a), Value::Int(min), Value::Float(max)) => Value::Float(a.min(*max).min(*min as f64)),
            (Value::Float(a), Value::Float(min), Value::Int(max)) => Value::Float(a.min(*max as f64).min(*min)),

            _ => Err(error(format!("type `{}` cannot be clamped with the given range", a.ntype())))?,
        }
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

    fn @randint(start: int, end: int) {
        if start >= end {
            Err(error("start must be less than end"))?
        }

        Value::Int(rand::thread_rng().gen_range(start..end))
    }

    fn @randfloat(start: float, end: float) {
        if start >= end {
            Err(error("start must be less than end"))?
        }

        Value::Float(rand::thread_rng().gen_range(start..end))
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

generic_builtins! {
    [export=strings]
    [vm_options=options]

    fn @truncate(s: str, len: int, ellipsis: str) {
        // factor in the length of the ellipsis
        let len = len as usize;

        // get the byte index of the nth character
        match s.char_indices().nth(len) {
            Some((i, _)) => {
                let i = i.saturating_sub(ellipsis.chars().count());
                // move the ellipsis so that it touches the last non-whitespace character
                let mut i = i;
                let mut cs = s[..i].chars().rev();
                while let Some(c) = cs.next() {
                    if !c.is_whitespace() {
                        break;
                    }
                    i = i.saturating_sub(c.len_utf8());
                }

                let mut s = s[..i].to_string();
                s.push_str(&ellipsis);
                s
            }
            None => s.to_string(),
        }.into()
    }

    fn @capitalize(s: str) {
        let mut c = s.chars();
        match c.next() {
            None => "".to_string(),
            Some(f) => f.to_uppercase().chain(c).collect(),
        }.into()
    }

    fn @title(s: str) {
        fn capitalize(s: &str) -> String {
            let mut c = s.chars();
            match c.next() {
                None => "".to_string(),
                Some(f) => f.to_uppercase().chain(c).collect(),
            }
        }

        s.split_whitespace().map(capitalize).collect::<Vec<String>>().join(" ").into()
    }

    fn @uppercase(s: str) {
        s.to_uppercase().into()
    }

    fn @lowercase(s: str) {
        s.to_lowercase().into()
    }

    fn @split(s: str, sep: str) {
        make_value_array(s.split(&sep).map(|s| s.into()).collect::<Vec<Value>>(), options.array_max_size)?
    }

    fn @join(s: list, sep: str) {
        s.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(&sep).into()
    }

    fn @replace(s: str, old: str, new: str) {
        s.replace(&old, &new).into()
    }

    fn @trim(s: str) {
        s.trim().into()
    }

    fn @trim_start(s: str) {
        s.trim_start().into()
    }

    fn @trim_end(s: str) {
        s.trim_end().into()
    }

    fn @starts_with(s: str, start: str) {
        s.starts_with(&start).into()
    }

    fn @ends_with(s: str, end: str) {
        s.ends_with(&end).into()
    }

    fn @contains(s: str, contains: str) {
        s.contains(&contains).into()
    }

    fn @chars(s: str) {
        s.chars().map(|c| c.to_string()).collect::<Vec<String>>().into()
    }
}

generic_builtins! {
    [export=io]
    [vm_options=_options]

    fn @print(to_print: rest) {
        for arg in to_print {
            print!("{} ", arg);
        }
        println!();
        Value::None
    }

    fn @debug(to_print: rest) {
        // orange
        print!("\x1b[38;5;208mDEBUG: ");

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
                Err(error("could not read input".to_string()))?
            }
        } else {
            Err(error("could not flush stdout".to_string()))?
        }
    }
}
