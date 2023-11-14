use common::error::{Error, TypeError};
use interpreter::{
    arith::fix_index,
    builtins::*,
    generate_builtins,
    value::{make_value_array, Value},
    vm::*,
};

generate_builtins! {
    ///Basic builtin functions.
    [export=core]

    ///Get an item from a list at a given index.
    fn @get(list: list, index: int) -> any {
        let idx = fix_index(index, list.len())?;

        if let Some(v) = list.get(idx) {
            v.clone()
        } else {
            Err(Error::IndexError(index))?
        }
    }

    [options]
    ///Set an item in a list at a given index.
    fn @set(list: list, index: int, value: any) -> list {
        let idx = fix_index(index, list.len())?;

        let mut list = list;
        if let Some(v) = list.get_mut(idx) {
            *v = value;
            make_value_array(list, options.array_max_size)?
        } else {
            Err(Error::IndexError(index))?
        }
    }

    ///Get a slice of a list.
    fn @slice(list: list, start: int, end: int) -> list {
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

    ///Convert a value to a string.
    fn @str(arg: any) -> str {
        arg.to_string().into()
    }

    ///Convert an integer to its binary representation as a string.
    fn @bin(val: int) -> str {
        format!("{:b}", val).into()
    }

    ///Convert an integer to its octal representation as a string.
    fn @oct(val: int) -> str {
        format!("{:o}", val).into()
    }

    ///Convert an integer to its hexadecimal representation as a string.
    fn @hex(val: int) -> str {
        format!("{:x}", val).into()
    }

    ///Convert a character code to a string.
    fn @char(val: int) -> str {
        if let Some(c) = std::char::from_u32(val as u32) {
            c.to_string().into()
        } else {
            Err(error(format!("invalid character code `{}`", val)))?
        }
    }

    ///Convert a string to an integer with a given radix.
    fn @int_from_radix(val: str, radix: int32) -> int {
        // if radix is not from 2 to 36, return an error
        if radix < 2 || radix > 36 {
            Err(error(format!(
                "radix must be between 2 and 36, got `{}`",
                radix
            )))?
        }

        match i64::from_str_radix(&val, radix as u32) {
            Ok(i) => Value::Int(i),
            Err(_) => Err(TypeError::ParseError(val.to_string(), "int"))?,
        }
    }

    ///Convert a value to an integer.
    fn @int(arg: any) -> int {
        match &arg {
            Value::Int(i) => Value::Int(*i),
            Value::Float(f) => Value::Int(*f as i64),
            Value::String(s) => match s.parse::<i64>() {
                Ok(i) => Value::Int(i),
                Err(_) => Err(TypeError::ParseError(s.to_string(), "int"))?,
            },
            _ => Err(TypeError::CannotConvert(arg.ntype(), "int"))?,
        }
    }

    ///Convert a value to a float.
    fn @float(arg: any) -> float {
        match &arg {
            Value::Int(i) => Value::Float(*i as f64),
            Value::Float(f) => Value::Float(*f),
            Value::String(s) => match s.parse::<f64>() {
                Ok(f) => Value::Float(f),
                Err(_) => Err(TypeError::ParseError(s.to_string(), "float"))?,
            },
            _ => Err(TypeError::CannotConvert(arg.ntype(), "int"))?,
        }
    }

    ///Convert a value to a boolean.
    fn @bool(arg: any) -> bool {
        match &arg {
            Value::Bool(b) => Value::Bool(*b),
            Value::Int(i) => Value::Bool(*i != 0),
            Value::Float(f) => Value::Bool(*f != 0.0),
            Value::String(s) => match s.parse::<bool>() {
                Ok(b) => Value::Bool(b),
                Err(_) => Err(TypeError::ParseError(s.to_string(), "bool"))?,
            },
            _ => Err(TypeError::CannotConvert(arg.ntype(), "bool"))?,
        }
    }

    ///Convert a value to a color
    fn @color(arg: any) -> color {
        match &arg {
            Value::Color(c) => Value::Color(*c),
            // decimal to rgba
            Value::Int(i) => {
                let r = (i >> 24) as u8;
                let g = (i >> 16) as u8;
                let b = (i >> 8) as u8;
                let a = *i as u8;

                Value::Color([r, g, b, a])
            }
            Value::String(ss) => {
                let s = ss.trim_matches('#');
                let mut color = [0, 0, 0, 255];

                // take only the first 8 characters
                let chars = s.chars().take(8).collect::<Vec<char>>();

                if chars.len() == 8 || chars.len() == 6 {
                    for (i, c) in chars.chunks(2).enumerate() {
                        let c = c.iter().collect::<String>();
                        let c = u8::from_str_radix(&c, 16).unwrap_or(0);
                        color[i] = c;
                    }
                } else if chars.len() == 3 {
                    for (i, c) in chars.iter().enumerate() {
                        let c = c.to_string().repeat(2);
                        let c = u8::from_str_radix(&c, 16).unwrap_or(0);
                        color[i] = c;
                    }
                } else {
                    Err(error(format!("invalid color `{}`", ss)))?
                }

                Value::Color(color)
            }
            _ => Err(TypeError::CannotConvert(arg.ntype(), "color"))?,
        }
    }

    ///Get the length of an array or string.
    fn @len(arg: any) -> int {
        match &arg {
            Value::Array(l) => Value::Int(l.len() as i64),
            Value::String(s) => Value::Int(s.len() as i64),
            _ => Err(error(format!("type `{}` has no length", arg.ntype())))?,
        }
    }

    ///Get the type of a value as a string.
    fn @gettype(arg: any) -> str {
        arg.ntype().into()
    }
}
