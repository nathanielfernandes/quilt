#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde_docs", derive(serde::Serialize))]
pub struct BuiltinDoc {
    pub name: &'static str,
    pub description: &'static str,
    pub args: &'static [(&'static str, &'static str)],
    pub returns: &'static [&'static str],
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde_docs", derive(serde::Serialize))]
pub struct BuiltinGroup {
    pub name: &'static str,
    pub description: &'static str,
    pub builtins: &'static [BuiltinDoc],
}
