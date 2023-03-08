use crate::shared::{RuntimeError, Span, Spanned, Value};
use ariadne::Color;
use colored::Colorize;
use fxhash::FxHashMap;

/// a type alias representing args passed to a builtin function
pub type BuiltinArgs = Spanned<Vec<Spanned<Value>>>;

/// a type alias representing a builtin function
pub type BuiltinFnReg<Data> = fn(&mut Data, &mut BuiltinArgs) -> Result<Value, RuntimeError>;

pub enum BuiltinFn<Data> {
    /// a builtin function that takes a [`BuiltinArgs`] and returns a [`Value`]
    Fn(BuiltinFnReg<Data>),
    Context(BuiltinFnReg<Data>, BuiltinFnReg<Data>),
}

/// a type alias representing a map of builtin functions
pub type BuiltinFnMap<Data> = FxHashMap<String, BuiltinFn<Data>>;

/// a type alias representing a function that adds builtin functions to a map
pub type BuiltinAdder<Data> = fn(&mut BuiltinFnMap<Data>);

/// a trait that allows a type to be consumed from [`BuiltinArgs`] by a builtin function
pub trait Consumable {
    fn consume(&mut self, expected: &str) -> Result<Spanned<Value>, RuntimeError>;
    fn stop(&mut self) -> Result<(), RuntimeError>;

    fn int(&mut self) -> Result<i32, RuntimeError>;
    fn u8(&mut self) -> Result<u8, RuntimeError>;
    fn bool(&mut self) -> Result<bool, RuntimeError>;
    fn float(&mut self) -> Result<f32, RuntimeError>;
    fn num(&mut self) -> Result<f32, RuntimeError>;
    fn str(&mut self) -> Result<String, RuntimeError>;
    fn color(&mut self) -> Result<[u8; 4], RuntimeError>;
    fn list(&mut self) -> Result<Vec<Value>, RuntimeError>;
    fn pair(&mut self) -> Result<(Value, Value), RuntimeError>;
    fn rest(&mut self) -> Result<Vec<Value>, RuntimeError>;
    fn any(&mut self) -> Result<Value, RuntimeError>;

    fn special(&mut self, id: &str) -> Result<usize, RuntimeError>;
}

/// helper function to create a [`RuntimeError`] for when an unexpected value is encountered
pub fn expected(got: &str, expected: &str, span: Span) -> RuntimeError {
    RuntimeError {
        msg: format!("expected {}, got {}", expected.cyan(), got.cyan())
            .yellow()
            .to_string(),
        span,
        help: None,
        color: Some(Color::Yellow),
    }
}

impl Consumable for BuiltinArgs {
    fn consume(&mut self, expected: &str) -> Result<Spanned<Value>, RuntimeError> {
        if self.0.len() != 0 {
            Ok(self.0.remove(0))
        } else {
            Err(RuntimeError {
                msg: format!(
                    "too litte arguments supplied to builtin, expected {}",
                    expected
                )
                .red()
                .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    fn special(&mut self, id: &str) -> Result<usize, RuntimeError> {
        let (value, span) = self.consume(id)?;

        match value {
            Value::Special(sid, value) => {
                if id == sid {
                    Ok(value)
                } else {
                    Err(expected(sid, id, span))
                }
            }
            _ => Err(expected(value.ntype(), id, span)),
        }
    }

    /// consumes the next argument as an integer, errors if the next argument is not an integer
    fn int(&mut self) -> Result<i32, RuntimeError> {
        let (value, span) = self.consume("int")?;
        match value {
            Value::Int(i) => Ok(i),
            Value::Float(f) => Ok(f as i32),
            _ => Err(expected(value.ntype(), "int", span)),
        }
    }

    /// consumes the next argument as a float or int, errors if the next argument is not a float or int
    fn num(&mut self) -> Result<f32, RuntimeError> {
        let (value, span) = self.consume("num")?;
        match value {
            Value::Int(i) => Ok(i as f32),
            Value::Float(f) => Ok(f),
            _ => Err(expected(value.ntype(), "num", span)),
        }
    }

    /// consumes the next argument as a u8 value, errors if the next argument is not a u8
    ///
    /// if the next argument is a float, it will be clamped to the range [0, 255] and rounded
    fn u8(&mut self) -> Result<u8, RuntimeError> {
        let (value, span) = self.consume("u8")?;
        match value {
            Value::Int(i) => Ok(i.min(255).max(0) as u8),
            Value::Float(f) => Ok((f * 255.0).min(255.0).max(0.0) as u8),
            _ => Err(expected(value.ntype(), "u8", span)),
        }
    }

    /// consumes the next argument as a bool value, errors if the next argument is not a bool
    fn bool(&mut self) -> Result<bool, RuntimeError> {
        let (value, span) = self.consume("bool")?;
        match value {
            Value::Bool(b) => Ok(b),
            _ => Err(expected(value.ntype(), "bool", span)),
        }
    }

    /// consume the next argument as a float, errors if the next argument cannot be a float
    fn float(&mut self) -> Result<f32, RuntimeError> {
        let (value, span) = self.consume("float")?;
        match value {
            Value::Int(i) => Ok(i as f32),
            Value::Float(f) => Ok(f),
            _ => Err(expected(value.ntype(), "float", span)),
        }
    }

    /// consumes the next argument as a string, errors if the next argument is not a string
    fn str(&mut self) -> Result<String, RuntimeError> {
        let (value, span) = self.consume("str")?;
        match value {
            Value::Str(s) => Ok(s),
            _ => Err(expected(value.ntype(), "str", span)),
        }
    }

    /// consumes the next argument as a color, errors if the next argument is not a color
    fn color(&mut self) -> Result<[u8; 4], RuntimeError> {
        let (value, span) = self.consume("color")?;
        match value {
            Value::Color(c) => Ok(c),
            _ => Err(expected(value.ntype(), "color", span)),
        }
    }

    /// consumes the next argument as a list, errors if the next argument is not a list
    fn list(&mut self) -> Result<Vec<Value>, RuntimeError> {
        let (value, span) = self.consume("list")?;
        match value {
            Value::List(l) => Ok(l),
            Value::Pair(l, r) => Ok(vec![*l, *r]),
            _ => Err(expected(value.ntype(), "list", span)),
        }
    }

    fn pair(&mut self) -> Result<(Value, Value), RuntimeError> {
        let (value, span) = self.consume("pair")?;
        match value {
            Value::Pair(l, r) => Ok((*l, *r)),
            _ => Err(expected(value.ntype(), "pair", span)),
        }
    }

    /// consumes the rest of the arguments as a list
    fn rest(&mut self) -> Result<Vec<Value>, RuntimeError> {
        Ok(self.0.drain(..).map(|(v, _)| v).collect())
    }

    /// errors if there are any arguments left
    fn stop(&mut self) -> Result<(), RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            Err(RuntimeError {
                msg: format!("too many arguments, got extra {}", item.0.ntype().cyan())
                    .yellow()
                    .to_string(),
                span: item.1,
                help: None,
                color: Some(Color::Yellow),
            })
        } else {
            Ok(())
        }
    }

    /// consumes the next argument as a any value, errors if there are no arguments left
    fn any(&mut self) -> Result<Value, RuntimeError> {
        let (value, _) = self.consume("any")?;
        Ok(value)
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
                pub fn $name<Data>(_: &mut Data, args: &mut crate::builtins::BuiltinArgs) -> Result<Value, RuntimeError> {
                        use crate::builtins::Consumable;

                        #[allow(unused_macros)]
                        macro_rules! error {
                            ($msg:expr) => {
                                Err(RuntimeError {
                                    msg: $msg,
                                    span: args.1.clone(),
                                    help: None,
                                    color: None,
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

            pub fn $group<Data>(map: &mut fxhash::FxHashMap<String, crate::builtins::BuiltinFn<Data>>) {
                $(
                    map.insert(String::from(stringify!($name)), crate::builtins::BuiltinFn::Fn($name));
                )*
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

           $(
               $(#[doc = $doc])*
               pub fn $name($data: &mut $datatype, args: &mut BuiltinArgs) -> Result<Value, RuntimeError> {

                    #[allow(unused_macros)]
                    macro_rules! error {
                        ($msg:expr) => {
                            Err(RuntimeError {
                                msg: $msg,
                                span: args.1.clone(),
                                help: None,
                                color: None,
                            })
                        };
                    }

                    #[allow(unused_macros)]
                    macro_rules! err {
                        ($msg:expr) => {
                            RuntimeError {
                                msg: $msg,
                                span: args.1.clone(),
                                help: None,
                                color: None,
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

           pub fn $group(map: &mut fxhash::FxHashMap<String, BuiltinFn<$datatype>>) {
               $(
                   map.insert(String::from(stringify!($name)), BuiltinFn::Fn($name));
               )*
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
                fn @$end_name:ident($end_data:ident, $end_arg:ident: $end_type:ident) $end_body:block


            }

        )*
    } => {
            use crate::prelude::*;

           $(

                $(#[doc = $start_doc])*
                pub fn $start_name($start_data: &mut $datatype, args: &mut BuiltinArgs) -> Result<Value, RuntimeError> {

                    #[allow(unused_macros)]
                    macro_rules! error {
                        ($msg:expr) => {
                            Err(RuntimeError {
                                msg: $msg,
                                span: args.1.clone(),
                                help: None,
                                color: None,
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
                pub fn $end_name($end_data: &mut $datatype, args: &mut BuiltinArgs) -> Result<Value, RuntimeError> {

                    #[allow(unused_macros)]
                    macro_rules! error {
                        ($msg:expr) => {
                            Err(RuntimeError {
                                msg: $msg,
                                span: args.1.clone(),
                                help: None,
                                color: None,
                            })?
                        };
                    }

                   let $end_arg = args.$end_type()?;

                   args.stop()?;
                   Ok($end_body)
                }
           )*

           pub fn $group(map: &mut fxhash::FxHashMap<String, BuiltinFn<$datatype>>) {
               $(
                   map.insert(String::from(stringify!($start_name)), BuiltinFn::Context($start_name, $end_name));
               )*
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
