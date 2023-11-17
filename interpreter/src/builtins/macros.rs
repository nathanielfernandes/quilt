/// a macro to define builtin functions
#[macro_export]
macro_rules! generate_builtins {
    {
        $(#[doc = $groupdoc:expr])*
        [export=$group:ident]

        $(
            $([$options:ident])?
            $(#[doc = $doc:expr])*
            fn @$name:ident($($arg:ident: $type:ident),*$(,)? $(?$oparg:ident: $optype:ident $(= $opdef:expr)?),*) $(-> $($ret:ident),+)?  $body:block
        )*
    } => {
            #[cfg(any(THIS))]
            use quilt::prelude::*;

            $(
                $(#[doc = $doc])*
                fn $name<Data>(_: &mut Data, args: &BuiltinArgs, opts: &VmOptions) -> Result<Value, Error> {
                    let mut args = BuiltinArgsContainer::new(args);

                    #[allow(unused_variables)]
                    $(
                        let $options = opts;
                    )?

                    $(
                        let $arg = args.$type()?;
                    )*

                    $(
                        let $oparg = args.optional(BuiltinArgsContainer::$optype)?$(.unwrap_or_else(|| $opdef))?;
                    )*

                    args.stop()?;
                    Ok($body)
                }
            )*

            $(
                paste! {
                    #[allow(non_upper_case_globals, unused_variables)]
                    pub const [<$name _DOC>]: BuiltinDoc = BuiltinDoc {
                        name: stringify!($name),
                        description: concat!($($doc, '\n'), *),
                        args: &[ $(Arg {
                            name: stringify!($arg),
                            r#type: stringify!($type),
                            optional: false,
                            default: None,
                        },)*
                        $(Arg {
                            name: stringify!($oparg),
                            r#type: stringify!($optype),
                            optional: true,
                            $(default: Some(stringify!($opdef)),)?

                            ..DEFAULT_ARG
                        }),*
                        ],
                        returns: &[ $($(stringify!($ret),)*)? ],
                    };
                }
            )*

            paste! {
                #[allow(non_upper_case_globals, unused_variables)]
                pub const [<$group _DOCS>]: BuiltinGroup = BuiltinGroup {
                    name: stringify!($group),
                    description: concat!($($groupdoc, '\n'), *),
                    builtins: &[
                        $(
                            [<$name _DOC>],
                        )*
                    ],
                };
            }

            pub fn $group<Data>(vm: &mut VM<Data>)
            where Data : VmData
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
                pub fn [<get_ $group _doc>](name: &str) -> Option<&'static BuiltinDoc> {
                    match name {
                        $(
                            stringify!($name) => Some(&[<$name _DOC>]),
                        )*
                        _ => None,
                    }
                }
            }
    };


    {
        $(#[doc = $groupdoc:expr])*
        [data=$datatype:ty]
        [export=$group:ident]
        $(
            $([$options:ident])?
            $(#[doc = $doc:expr])*
            fn @$name:ident($data:ident, $($arg:ident: $type:ident),*$(,)? $(?$oparg:ident: $optype:ident $(= $opdef:expr)?),*) $(-> $($ret:ident),+)?  $body:block
        )*
    } => {
            use quilt::prelude::*;

            $(
                $(#[doc = $doc])*
                fn $name($data: &mut $datatype, args: &BuiltinArgs, opts: &VmOptions) -> Result<Value, Error> {
                    let mut args = BuiltinArgsContainer::new(args);

                    #[allow(unused_variables)]
                    $(
                        let $options = opts;
                    )?

                    $(
                        let $arg = args.$type()?;
                    )*

                    $(
                        let $oparg = args.optional(BuiltinArgsContainer::$optype)?$(.unwrap_or_else(|| $opdef))?;
                    )*

                    args.stop()?;
                    Ok($body)
                }
            )*

            $(
                paste! {
                    #[allow(non_upper_case_globals, unused_variables)]
                    pub const [<$name _DOC>]: BuiltinDoc = BuiltinDoc {
                        name: stringify!($name),
                        description: concat!($($doc, '\n'), *),
                        args: &[ $(Arg {
                            name: stringify!($arg),
                            r#type: stringify!($type),
                            optional: false,
                            default: None,
                        },)*
                        $(Arg {
                            name: stringify!($oparg),
                            r#type: stringify!($optype),
                            optional: true,
                            $(default: Some(stringify!($opdef)),)?

                            ..DEFAULT_ARG
                        }),*
                        ],
                        returns: &[ $($(stringify!($ret),)*)? ],
                    };
                }
            )*

            paste! {
                #[allow(non_upper_case_globals, unused_variables)]
                pub const [<$group _DOCS>]: BuiltinGroup = BuiltinGroup {
                    name: stringify!($group),
                    description: concat!($($groupdoc, '\n'), *),
                    builtins: &[
                        $(
                            [<$name _DOC>],
                        )*
                    ],
                };
            }

            pub fn $group(vm: &mut VM<$datatype>) {
                vm.add_builtins_list(
                    [
                        $(
                            (String::from(stringify!($name)), BuiltinFn::Fn($name)),
                        )*
                    ]
                );
            }

            paste! {
                pub fn [<get_ $group _doc>](name: &str) -> Option<&'static BuiltinDoc> {
                    match name {
                        $(
                            stringify!($name) => Some(&[<$name _DOC>]),
                        )*
                        _ => None,
                    }
                }
            }
    };


    {
        $(#[doc = $groupdoc:expr])*
        [data=$datatype:ty]
        [export=$group:ident]
        $(
            $([$options:ident])?
            $(#[doc = $doc:expr])*
            fn @$name:ident($data:ident, $($arg:ident: $type:ident),*$(,)? $(?$oparg:ident: $optype:ident $(= $opdef:expr)?),*) $(-> $($ret:ident),+)?

            $start_body:block => $end_body:block
        )*
    } => {
            use quilt::prelude::*;

            $(
                $(#[doc = $doc])*
                fn $name($data: &mut $datatype, args: &BuiltinArgs, opts: &VmOptions) -> Result<Value, Error> {
                    let mut args = BuiltinArgsContainer::new(args);

                    #[allow(unused_variables)]
                    $(
                        let $options = opts;
                    )?

                    $(
                        let $arg = args.$type()?;
                    )*

                    args.stop()?;
                    Ok($start_body)
                }

                paste! {
                    fn [<__ $name _end__ >]($data: &mut $datatype) -> Result<(), Error> {
                        $end_body
                        Ok(())
                    }
                }
            )*

            $(
                paste! {
                    #[allow(non_upper_case_globals, unused_variables)]
                    pub const [<$name _DOC>]: BuiltinDoc = BuiltinDoc {
                        name: stringify!($name),
                        description: concat!($($doc, '\n'), *),
                        args: &[ $(Arg {
                            name: stringify!($arg),
                            r#type: stringify!($type),
                            optional: false,
                            default: None,
                        },)*
                        $(Arg {
                            name: stringify!($oparg),
                            r#type: stringify!($optype),
                            optional: true,
                            $(default: Some(stringify!($opdef)),)?

                            ..DEFAULT_ARG
                        }),*
                        ],
                        returns: &[ $($(stringify!($ret),)*)? ],
                    };
                }
            )*


            paste! {
                #[allow(non_upper_case_globals, unused_variables)]
                pub const [<$group _DOCS>]: BuiltinGroup = BuiltinGroup {
                    name: stringify!($group),
                    description: concat!($($groupdoc, '\n'), *),
                    builtins: &[
                        $(
                            [<$name _DOC>],
                        )*
                    ],
                };
            }

            pub fn $group(vm: &mut VM<$datatype>) {
                vm.add_builtins_list(
                    [
                        $(
                            paste! {
                                (String::from(stringify!($name)), BuiltinFn::Context($name, [<__ $name _end__ >]))
                            },
                        )*
                    ]
                );
            }

            paste! {
                pub fn [<get_ $group _doc>](name: &str) -> Option<&'static BuiltinDoc> {
                    match name {
                        $(
                            stringify!($name) => Some(&[<$name _DOC>]),
                        )*
                        _ => None,
                    }
                }
            }
    };
}
