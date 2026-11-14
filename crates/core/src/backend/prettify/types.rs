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
    /// The rule's text ends in a token that reads to its line end (a regex
    /// ending in `.*`), so nothing may follow it on its output line
    /// (RULINGS 2026-11-14a S3CPP-L).
    pub reads_to_line_end: bool,
}

/// Planning result for a single rule's prettify codegen.
#[derive(Clone, Debug, Default)]
pub struct PrettyRulePlan {
    pub inline: bool,
    pub policy: PrettyPolicy,
}
