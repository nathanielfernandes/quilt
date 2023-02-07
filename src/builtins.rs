use crate::{
    eval::RuntimeError,
    parser::{Span, Spanned},
    value::Value,
};
use ariadne::Color;
use colored::Colorize;
use fxhash::FxHashMap;

/// a type alias representing args passed to a builtin function
pub type BuiltinArgs = Spanned<Vec<Spanned<Value>>>;

/// a type alias representing a builtin function
pub type BuiltinFn<Data> = fn(&mut Data, &mut BuiltinArgs) -> Result<Value, RuntimeError>;

/// a type alias representing a map of builtin functions
pub type BuiltinFnMap<Data> = FxHashMap<String, BuiltinFn<Data>>;

/// a type alias representing a function that adds builtin functions to a map
pub type BuiltinAdder<Data> = fn(&mut BuiltinFnMap<Data>);

/// a trait that allows a type to be consumed from [`BuiltinArgs`] by a builtin function
pub trait Consumable {
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
    fn stop(&mut self) -> Result<(), RuntimeError>;
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
    /// consumes the next argument as an integer, errors if the next argument is not an integer
    fn int(&mut self) -> Result<i32, RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Int(i) => Ok(i),
                Value::Float(f) => Ok(f as i32),
                _ => Err(expected(item.0.ntype(), "int", item.1)),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected int"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    /// consumes the next argument as a float or int, errors if the next argument is not a float or int
    fn num(&mut self) -> Result<f32, RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Int(i) => Ok(i as f32),
                Value::Float(f) => Ok(f),
                _ => Err(expected(item.0.ntype(), "num", item.1)),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected num"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    /// consumes the next argument as a u8 value, errors if the next argument is not a u8
    ///
    /// if the next argument is a float, it will be clamped to the range [0, 255] and rounded
    fn u8(&mut self) -> Result<u8, RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Int(i) => Ok(i.min(255).max(0) as u8),
                Value::Float(f) => Ok((f * 255.0).min(255.0).max(0.0) as u8),
                _ => Err(RuntimeError {
                    msg: format!("expected {}, got {}", "u8".cyan(), item.0.ntype().cyan())
                        .yellow()
                        .to_string(),
                    span: item.1,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected int"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    /// consumes the next argument as a bool value, errors if the next argument is not a bool
    fn bool(&mut self) -> Result<bool, RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Bool(b) => Ok(b),
                _ => Err(RuntimeError {
                    msg: format!("expected {}, got {}", "bool".cyan(), item.0.ntype().cyan())
                        .yellow()
                        .to_string(),
                    span: item.1,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected bool"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    /// consume the next argument as a float, errors if the next argument cannot be a float
    fn float(&mut self) -> Result<f32, RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Int(i) => Ok(i as f32),
                Value::Float(f) => Ok(f),
                _ => Err(RuntimeError {
                    msg: format!("expected {}, got {}", "float".cyan(), item.0.ntype().cyan())
                        .yellow()
                        .to_string(),
                    span: item.1,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected float"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    /// consumes the next argument as a string, errors if the next argument is not a string
    fn str(&mut self) -> Result<String, RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Str(s) => Ok(s),
                _ => Err(RuntimeError {
                    msg: format!("expected {}, got {}", "str".cyan(), item.0.ntype().cyan())
                        .yellow()
                        .to_string(),
                    span: item.1,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected str"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    /// consumes the next argument as a color, errors if the next argument is not a color
    fn color(&mut self) -> Result<[u8; 4], RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Color(c) => Ok(c),
                _ => Err(RuntimeError {
                    msg: format!("expected {}, got {}", "color".cyan(), item.0.ntype().cyan())
                        .yellow()
                        .to_string(),
                    span: item.1,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected color"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    /// consumes the next argument as a list, errors if the next argument is not a list
    fn list(&mut self) -> Result<Vec<Value>, RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::List(l) => Ok(l),
                _ => Err(RuntimeError {
                    msg: format!("expected list, got {}", item.0.ntype().cyan())
                        .yellow()
                        .to_string(),
                    span: item.1,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected list"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
    }

    fn pair(&mut self) -> Result<(Value, Value), RuntimeError> {
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            match item.0 {
                Value::Pair(l, r) => Ok((*l, *r)),
                _ => Err(RuntimeError {
                    msg: format!("expected pair, got {}", item.0.ntype().cyan())
                        .yellow()
                        .to_string(),
                    span: item.1,
                    help: None,
                    color: Some(Color::Yellow),
                }),
            }
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected pair"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
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
        if self.0.len() != 0 {
            let item = self.0.remove(0);
            Ok(item.0)
        } else {
            Err(RuntimeError {
                msg: "too litte arguments supplied to builtin, expected any"
                    .red()
                    .to_string(),
                span: self.1.clone(),
                help: None,
                color: None,
            })
        }
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
                            })?
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
                   map.insert(String::from(stringify!($name)), $name);
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
                            })?
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
                   map.insert(String::from(stringify!($name)), $name);
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
