pub mod docs;
pub mod macros;

pub use docs::*;

use common::error::{BuiltinError, Error, TypeError};
use fxhash::FxHashMap;

use crate::{value::Value, vm};

pub use paste::paste;

pub trait VmData {
    #[inline(always)]
    fn on_enter_function(&mut self) {}
    #[inline(always)]
    fn on_exit_function(&mut self) {}
}

macro_rules! impl_vmdata {
    ($($t:ty),*) => {
        $(
            impl VmData for $t {}
        )*
    };
}

impl_vmdata!(
    (),
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize
);

/// a type alias representing args passed to a builtin function
// pub type BuiltinArgs = Vec<Value>;
pub struct BuiltinArgsContainer<'a> {
    args: &'a [Value],
    idx: usize,
}

impl<'a> BuiltinArgsContainer<'a> {
    #[inline]
    pub fn new(args: &'a [Value]) -> Self {
        Self { args, idx: 0 }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.args.len() - self.idx
    }

    #[inline]
    pub fn next(&mut self) -> Option<&'a Value> {
        let idx = self.idx;
        self.idx += 1;
        self.args.get(idx)
    }

    #[inline]
    pub fn remaining(&mut self) -> &'a [Value] {
        let idx = self.idx;
        self.idx = self.args.len();
        if idx == self.args.len() {
            &[]
        } else {
            &self.args[idx..]
        }
    }
}

pub type BuiltinArgs<'a> = &'a [Value];
/// a type alias representing a builtin function
pub type BuiltinFnReg<Data> = fn(&mut Data, &BuiltinArgs, &vm::VmOptions) -> Result<Value, Error>;

pub type ContextEntry<Data> = BuiltinFnReg<Data>;
pub type ContextExit<Data> = fn(&mut Data) -> Result<(), Error>;

#[derive(Clone, Copy)]
pub enum BuiltinFn<Data> {
    /// a builtin function that takes a [`BuiltinArgs`] and returns a [`Value`]
    Fn(BuiltinFnReg<Data>),
    Context(ContextEntry<Data>, ContextExit<Data>),
}

/// a type alias representing a map of builtin functions
pub type BuiltinFnMap<Data> = FxHashMap<u16, BuiltinFn<Data>>;
pub type BuiltinList<const N: usize, Data> = [(String, BuiltinFn<Data>); N];
pub type BuiltinAdderFn<Data> = fn(&mut vm::VM<Data>);

/// a trait that allows a type to be consumed from [`BuiltinArgs`] by a builtin function
pub trait Consumable {
    fn consume(&mut self, expected: &str) -> Result<&Value, Error>;
    fn stop(&mut self) -> Result<(), Error>;

    fn int(&mut self) -> Result<i64, Error>;
    fn int32(&mut self) -> Result<i32, Error>;
    fn u8(&mut self) -> Result<u8, Error>;
    fn bool(&mut self) -> Result<bool, Error>;
    fn float(&mut self) -> Result<f32, Error>;
    fn double(&mut self) -> Result<f64, Error>;
    fn num(&mut self) -> Result<f64, Error>;
    fn str(&mut self) -> Result<String, Error>;
    fn color(&mut self) -> Result<[u8; 4], Error>;
    fn list(&mut self) -> Result<Vec<Value>, Error>;
    fn pair(&mut self) -> Result<(Value, Value), Error>;
    fn rest(&mut self) -> Result<Vec<Value>, Error>;
    fn any(&mut self) -> Result<Value, Error>;

    fn special(&mut self, id: &'static str) -> Result<usize, Error>;

    fn optional<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<R, Error>,
    ) -> Result<Option<R>, Error>;
}

/// helper function to create a [`RuntimeError`] for when an unexpected value is encountered
#[inline]
pub fn expected(got: &'static str, expected: &'static str) -> Error {
    TypeError::Expected(expected, got).into()
}

impl<'a> Consumable for BuiltinArgsContainer<'a> {
    fn optional<R>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<R, Error>,
    ) -> Result<Option<R>, Error> {
        if self.len() == 0 {
            Ok(None)
        } else {
            Ok(Some(f(self)?))
        }
    }

    fn consume(&mut self, expected: &str) -> Result<&'a Value, Error> {
        if let Some(value) = self.next() {
            Ok(value)
        } else {
            Err(BuiltinError {
                msg: format!(
                    "too litte arguments supplied to builtin, expected `{}`",
                    expected
                ),
                help: None,
            }
            .into())
        }
    }

    fn special(&mut self, id: &'static str) -> Result<usize, Error> {
        let value = self.consume(id)?;

        match value {
            Value::Special(special) => {
                if id == special.0 {
                    Ok(special.1)
                } else {
                    Err(expected(special.0, id))
                }
            }
            _ => Err(expected(value.ntype(), id)),
        }
    }

    /// consumes the next argument as an integer, errors if the next argument is not an integer
    fn int(&mut self) -> Result<i64, Error> {
        let value = self.consume("int")?;
        match value {
            Value::Int(i) => Ok(*i),
            Value::Float(f) => Ok(*f as i64),
            _ => Err(expected(value.ntype(), "int")),
        }
    }

    /// consumes the next argument as an integer, errors if the next argument is not an integer
    fn int32(&mut self) -> Result<i32, Error> {
        let value = self.consume("int")?;
        match value {
            Value::Int(i) => Ok(*i as i32),
            Value::Float(f) => Ok(*f as i32),
            _ => Err(expected(value.ntype(), "int")),
        }
    }

    /// consumes the next argument as a float or int, errors if the next argument is not a float or int
    fn num(&mut self) -> Result<f64, Error> {
        let value = self.consume("num")?;
        match value {
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(expected(value.ntype(), "num")),
        }
    }

    /// consumes the next argument as a u8 value, errors if the next argument is not a u8
    ///
    /// if the next argument is a float, it will be clamped to the range [0, 255] and rounded
    fn u8(&mut self) -> Result<u8, Error> {
        let value = self.consume("u8")?;
        match value {
            Value::Int(i) => Ok((*i).min(255).max(0) as u8),
            Value::Float(f) => Ok((f * 255.0).min(255.0).max(0.0) as u8),
            _ => Err(expected(value.ntype(), "u8")),
        }
    }

    /// consumes the next argument as a bool value, errors if the next argument is not a bool
    fn bool(&mut self) -> Result<bool, Error> {
        let value = self.consume("bool")?;
        match value {
            Value::Bool(b) => Ok(*b),
            _ => Err(expected(value.ntype(), "bool")),
        }
    }

    /// consume the next argument as a float, errors if the next argument cannot be a float
    fn float(&mut self) -> Result<f32, Error> {
        let value = self.consume("float")?;
        match value {
            Value::Int(i) => Ok(*i as f32),
            Value::Float(f) => Ok(*f as f32),
            _ => Err(expected(value.ntype(), "float")),
        }
    }

    /// consume the next argument as a float, errors if the next argument cannot be a float
    fn double(&mut self) -> Result<f64, Error> {
        let value = self.consume("float")?;
        match value {
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(expected(value.ntype(), "float")),
        }
    }

    /// consumes the next argument as a string, errors if the next argument is not a string
    fn str(&mut self) -> Result<String, Error> {
        let value = self.consume("str")?;
        match value {
            Value::String(s) => Ok(s.to_string()),
            _ => Err(expected(value.ntype(), "str")),
        }
    }

    /// consumes the next argument as a color, errors if the next argument is not a color
    fn color(&mut self) -> Result<[u8; 4], Error> {
        let value = self.consume("color")?;
        match value {
            Value::Color(c) => Ok(*c),
            _ => Err(expected(value.ntype(), "color")),
        }
    }

    /// consumes the next argument as a list, errors if the next argument is not a list
    fn list(&mut self) -> Result<Vec<Value>, Error> {
        let value = self.consume("list")?;
        match value {
            Value::Array(l) => Ok(l.to_vec()),
            Value::Pair(pair) => Ok(vec![pair.0.clone(), pair.1.clone()]),
            _ => Err(expected(value.ntype(), "list")),
        }
    }

    fn pair(&mut self) -> Result<(Value, Value), Error> {
        let value = self.consume("pair")?;
        match value {
            Value::Pair(pair) => Ok((pair.0.clone(), pair.1.clone())),
            _ => Err(expected(value.ntype(), "pair")),
        }
    }

    /// consumes the rest of the arguments as a list
    fn rest(&mut self) -> Result<Vec<Value>, Error> {
        Ok(self.remaining().to_vec())
    }

    /// errors if there are any arguments left
    fn stop(&mut self) -> Result<(), Error> {
        if let Some(item) = self.next() {
            Err(BuiltinError {
                msg: format!("too many arguments, got extra {}", item.ntype()).to_string(),
                help: None,
            }
            .into())
        } else {
            Ok(())
        }
    }

    /// consumes the next argument as a any value, errors if there are no arguments left
    fn any(&mut self) -> Result<Value, Error> {
        self.consume("any").cloned()
    }
}

#[inline]
pub fn error<S: Into<String>>(msg: S) -> Error {
    BuiltinError {
        msg: msg.into(),
        help: None,
    }
    .into()
}

#[inline]
pub fn error_with_help<S: Into<String>, H: Into<String>>(msg: S, help: H) -> Error {
    BuiltinError {
        msg: msg.into(),
        help: Some(help.into()),
    }
    .into()
}
