use common::error::BuiltinError;
use fxhash::FxHashMap;

use crate::{value::Value, vm};

pub use paste::paste;

pub trait VmData {
    fn on_enter_function(&mut self) {}
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
pub struct BuiltinArgs<'a> {
    args: &'a [Value],
    idx: usize,
}

impl<'a> BuiltinArgs<'a> {
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

/// a type alias representing a builtin function
pub type BuiltinFnReg<Data> = fn(&mut Data, &mut BuiltinArgs) -> Result<Value, BuiltinError>;

pub type ContextEntry<Data> = BuiltinFnReg<Data>;
pub type ContextExit<Data> = fn(&mut Data) -> Result<(), BuiltinError>;

#[derive(Clone, Copy)]
pub enum BuiltinFn<Data> {
    /// a builtin function that takes a [`BuiltinArgs`] and returns a [`Value`]
    Fn(BuiltinFnReg<Data>),
    Context(ContextEntry<Data>, ContextExit<Data>),
}

/// a type alias representing a map of builtin functions
pub type BuiltinFnMap<Data> = FxHashMap<u16, BuiltinFn<Data>>;

pub type BuiltinList<const N: usize, Data> = [(String, BuiltinFn<Data>); N];
pub type BuiltinAdderFn<const SS: usize, const CSS: usize, Data> = fn(&mut vm::VM<SS, CSS, Data>);

/// a trait that allows a type to be consumed from [`BuiltinArgs`] by a builtin function
pub trait Consumable {
    fn consume(&mut self, expected: &str) -> Result<&Value, BuiltinError>;
    fn stop(&mut self) -> Result<(), BuiltinError>;

    fn int(&mut self) -> Result<i32, BuiltinError>;
    fn u8(&mut self) -> Result<u8, BuiltinError>;
    fn bool(&mut self) -> Result<bool, BuiltinError>;
    fn float(&mut self) -> Result<f32, BuiltinError>;
    fn num(&mut self) -> Result<f32, BuiltinError>;
    fn str(&mut self) -> Result<String, BuiltinError>;
    fn color(&mut self) -> Result<[u8; 4], BuiltinError>;
    fn list(&mut self) -> Result<Vec<Value>, BuiltinError>;
    fn pair(&mut self) -> Result<(Value, Value), BuiltinError>;
    fn rest(&mut self) -> Result<Vec<Value>, BuiltinError>;
    fn any(&mut self) -> Result<Value, BuiltinError>;

    fn special(&mut self, id: &str) -> Result<usize, BuiltinError>;
}

/// helper function to create a [`RuntimeError`] for when an unexpected value is encountered
#[inline]
pub fn expected(got: &str, expected: &str) -> BuiltinError {
    BuiltinError {
        msg: format!("expected `{}`, got `{}`", expected, got),
        help: None,
    }
}

impl<'a> Consumable for BuiltinArgs<'a> {
    fn consume(&mut self, expected: &str) -> Result<&'a Value, BuiltinError> {
        if let Some(value) = self.next() {
            Ok(value)
        } else {
            Err(BuiltinError {
                msg: format!(
                    "too litte arguments supplied to builtin, expected `{}`",
                    expected
                ),
                help: None,
            })
        }
    }

    fn special(&mut self, id: &str) -> Result<usize, BuiltinError> {
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
    fn int(&mut self) -> Result<i32, BuiltinError> {
        let value = self.consume("int")?;
        match value {
            Value::Int(i) => Ok(*i),
            Value::Float(f) => Ok(*f as i32),
            _ => Err(expected(value.ntype(), "int")),
        }
    }

    /// consumes the next argument as a float or int, errors if the next argument is not a float or int
    fn num(&mut self) -> Result<f32, BuiltinError> {
        let value = self.consume("num")?;
        match value {
            Value::Int(i) => Ok(*i as f32),
            Value::Float(f) => Ok(*f),
            _ => Err(expected(value.ntype(), "num")),
        }
    }

    /// consumes the next argument as a u8 value, errors if the next argument is not a u8
    ///
    /// if the next argument is a float, it will be clamped to the range [0, 255] and rounded
    fn u8(&mut self) -> Result<u8, BuiltinError> {
        let value = self.consume("u8")?;
        match value {
            Value::Int(i) => Ok((*i).min(255).max(0) as u8),
            Value::Float(f) => Ok((f * 255.0).min(255.0).max(0.0) as u8),
            _ => Err(expected(value.ntype(), "u8")),
        }
    }

    /// consumes the next argument as a bool value, errors if the next argument is not a bool
    fn bool(&mut self) -> Result<bool, BuiltinError> {
        let value = self.consume("bool")?;
        match value {
            Value::Bool(b) => Ok(*b),
            _ => Err(expected(value.ntype(), "bool")),
        }
    }

    /// consume the next argument as a float, errors if the next argument cannot be a float
    fn float(&mut self) -> Result<f32, BuiltinError> {
        let value = self.consume("float")?;
        match value {
            Value::Int(i) => Ok(*i as f32),
            Value::Float(f) => Ok(*f),
            _ => Err(expected(value.ntype(), "float")),
        }
    }

    /// consumes the next argument as a string, errors if the next argument is not a string
    fn str(&mut self) -> Result<String, BuiltinError> {
        let value = self.consume("str")?;
        match value {
            Value::String(s) => Ok(s.to_string()),
            _ => Err(expected(value.ntype(), "str")),
        }
    }

    /// consumes the next argument as a color, errors if the next argument is not a color
    fn color(&mut self) -> Result<[u8; 4], BuiltinError> {
        let value = self.consume("color")?;
        match value {
            Value::Color(c) => Ok(*c),
            _ => Err(expected(value.ntype(), "color")),
        }
    }

    /// consumes the next argument as a list, errors if the next argument is not a list
    fn list(&mut self) -> Result<Vec<Value>, BuiltinError> {
        let value = self.consume("list")?;
        match value {
            Value::Array(l) => Ok(l.to_vec()),
            Value::Pair(pair) => Ok(vec![pair.0.clone(), pair.1.clone()]),
            _ => Err(expected(value.ntype(), "list")),
        }
    }

    fn pair(&mut self) -> Result<(Value, Value), BuiltinError> {
        let value = self.consume("pair")?;
        match value {
            Value::Pair(pair) => Ok((pair.0.clone(), pair.1.clone())),
            _ => Err(expected(value.ntype(), "pair")),
        }
    }

    /// consumes the rest of the arguments as a list
    fn rest(&mut self) -> Result<Vec<Value>, BuiltinError> {
        Ok(self.remaining().to_vec())
    }

    /// errors if there are any arguments left
    fn stop(&mut self) -> Result<(), BuiltinError> {
        if let Some(item) = self.next() {
            Err(BuiltinError {
                msg: format!("too many arguments, got extra {}", item.ntype()).to_string(),
                help: None,
            })
        } else {
            Ok(())
        }
    }

    /// consumes the next argument as a any value, errors if there are no arguments left
    fn any(&mut self) -> Result<Value, BuiltinError> {
        self.consume("any").cloned()
    }
}

/// a macro to define builtin functions
#[macro_export]
macro_rules! generic_builtins {
    {
        [export=$group:ident]
        $(
            fn @$name:ident($($arg:ident: $type:ident),*) $body:block)*
    } => {

           $(
                fn $name<Data>(_: &mut Data, args: &mut crate::builtins::BuiltinArgs) -> Result<crate::value::Value, common::error::BuiltinError> {
                    use crate::builtins::Consumable;

                    #[allow(unused_macros)]
                    macro_rules! error {
                        ($msg:expr) => {
                            Err(common::error::BuiltinError {
                                msg: $msg,
                                help: None,
                            })
                        };
                    }


                    $(
                        let $arg = args.$type()?;
                    )*

                    args.stop()?;
                    Ok($body)
                }
           )*

            pub fn $group<const SS: usize, const CSS: usize, Data>(vm: &mut crate::vm::VM<SS, CSS, Data>)
            where Data : crate::builtins::VmData
            {
                vm.add_builtins_list(
                    [
                        $(
                            (String::from(stringify!($name)), crate::builtins::BuiltinFn::Fn($name)),
                        )*
                    ]
                );
            }
    };
}

/// a macro to define builtin functions that modify a specific type
#[macro_export]
macro_rules! specific_builtins {
    {

        [type=$datatype:ty]
        [export=$group:ident]
        $(

            $(#[doc = $doc:expr])*
            fn @$name:ident($data:ident, $($arg:ident: $type:ident),*) $body:block)*
    } => {
            use quilt::prelude::*;
            use quilt::prelude::builtins::*;

           $(
               $(#[doc = $doc])*
               pub fn $name($data: &mut $datatype, args: &mut BuiltinArgs) -> Result<Value, BuiltinError> {

                    #[allow(unused_macros)]
                    macro_rules! error {
                        ($msg:expr) => {
                            Err(BuiltinError {
                                msg: $msg,
                                help: None,
                            })
                        };
                    }

                    #[allow(unused_macros)]
                    macro_rules! err {
                        ($msg:expr) => {
                            BuiltinError {
                                msg: $msg,
                                help: None,
                            }
                        };
                    }

                   $(
                       let $arg = args.$type()?;
                   )*

                   args.stop()?;
                   Ok($body)
               }
           )*

           pub fn $group<const SS: usize, const CSS: usize>(vm: &mut VM<SS, CSS, $datatype>)
           {

                vm.add_builtins_list(
                    [
                        $(
                            (String::from(stringify!($name)), BuiltinFn::Fn($name)),
                        )*
                    ]
                );
            }

           paste! {
               pub fn [< $group _doc >]<S: AsRef<str>>(fn_name: S) -> Option<&'static str> {
                   match fn_name.as_ref() {
                       $(
                           stringify!($name) => Some(concat!($($doc, '\n'), *)),
                       )*
                       _ => None,
                   }
               }
           }
    };
}

#[macro_export]
macro_rules! context_builtins {
    {
        [type=$datatype:ty]
        [export=$group:ident]
        $(

            {
                [enter]
                $(#[doc = $start_doc:expr])*
                fn @$start_name:ident($start_data:ident, $($arg:ident: $type:ident),*) $start_body:block

                [exit]
                $(#[doc = $end_doc:expr])*
                fn @$end_name:ident($end_data:ident) $end_body:block
            }

        )*
    } => {
            use quilt::prelude::*;

           $(

                $(#[doc = $start_doc])*
                pub fn $start_name($start_data: &mut $datatype, args: &mut BuiltinArgs) -> Result<Value, BuiltinError> {

                    #[allow(unused_macros)]
                    macro_rules! error {
                        ($msg:expr) => {
                            Err(BuiltinError {
                                msg: $msg,
                                help: None,
                            })?
                        };
                    }

                   $(
                       let $arg = args.$type()?;
                   )*

                   args.stop()?;
                   Ok($start_body)
                }

                $(#[doc = $end_doc])*
                pub fn $end_name($end_data: &mut $datatype) -> Result<(), BuiltinError> {
                    #[allow(unused_macros)]
                    macro_rules! error {
                        ($msg:expr) => {
                            Err(BuiltinError {
                                msg: $msg,
                                help: None,
                            })?
                        };
                    }
                    $end_body
                    Ok(())
                }
           )*

           pub fn $group<const SS: usize, const CSS: usize>(vm: &mut VM<SS, CSS, $datatype>)
           {
                vm.add_builtins_list(
                    [
                        $(
                            (String::from(stringify!($start_name)), BuiltinFn::Context($start_name, $end_name)),
                        )*
                    ]
                )
           }

           paste! {
               pub fn [< $group _doc >]<S: AsRef<str>>(fn_name: S) -> Option<&'static str> {
                   match fn_name.as_ref() {
                       $(
                           stringify!($start_name) => Some(concat!($($start_doc, '\n'), *)),
                       )*
                       _ => None,
                   }
               }
           }
    };
}
