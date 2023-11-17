#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "serde_docs", derive(serde::Serialize))]
pub struct BuiltinGroup {
    pub name: &'static str,
    pub description: &'static str,
    pub builtins: &'static [BuiltinDoc],
}

#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "serde_docs", derive(serde::Serialize))]
pub struct BuiltinDoc {
    pub name: &'static str,
    pub description: &'static str,
    pub args: &'static [Arg],
    pub returns: &'static [&'static str],
}

#[derive(Debug, Clone, Copy, Default)]
#[cfg_attr(feature = "serde_docs", derive(serde::Serialize))]
pub struct Arg {
    pub name: &'static str,
    pub r#type: &'static str,
    pub optional: bool,
    pub default: Option<&'static str>,
}

pub const DEFAULT_ARG: Arg = Arg {
    name: "",
    r#type: "",
    optional: false,
    default: None,
};
