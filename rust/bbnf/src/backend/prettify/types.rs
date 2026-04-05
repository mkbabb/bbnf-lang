//! Backend-agnostic types for prettify planning.

/// How a rule's formatter wraps its body.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum WrapperPolicy {
    #[default]
    None,
    Group,
    GroupIndent,
    Block,
    BlockIndent,
    Off,
}

/// What separator the formatter emits between repeated/sequenced children.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum SeparatorPolicy {
    #[default]
    None,
    Space,
    Softline,
    Hardline,
    Blankline,
    Custom(String),
}

/// Per-rule formatting policy derived from `@pretty` directives.
#[derive(Clone, Debug, Default)]
pub struct PrettyPolicy {
    pub wrapper: WrapperPolicy,
    pub separator: SeparatorPolicy,
    pub is_ws_rule: bool,
    pub split: Option<String>,
}

/// Planning result for a single rule's prettify codegen.
#[derive(Clone, Debug, Default)]
pub struct PrettyRulePlan {
    pub inline: bool,
    pub policy: PrettyPolicy,
}
