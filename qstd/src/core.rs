use common::error::{Error, TypeError};
use interpreter::{
    arith::fix_index,
    builtins::*,
    generic_builtins,
    value::{make_value_array, Value},
    vm::*,
};

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
        arg.to_string().into()
    }

    fn @int(arg: any) {
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

    fn @float(arg: any) {
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

    fn @bool(arg: any) {
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

    fn @len(arg: any) {
        match &arg {
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

    fn @hsla(h: double, s: double, l: double, a: double) {
        Value::Color(hsla_to_rgba(h, s, l, a))
    }

    fn @hsl(h: double, s: double, l: double) {
        Value::Color(hsla_to_rgba(h, s, l, 1.0))
    }

    fn @hsva(h: double, s: double, v: double, a: double) {
        Value::Color(hsva_to_rgba(h, s, v, a))
    }

    fn @hsv(h: double, s: double, v: double) {
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
