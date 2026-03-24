//! Type definitions for code generation.

/// Container-level parser attributes parsed from `#[parser(...)]`.
#[derive(Clone, Debug, Default)]
pub struct ParserAttributes {
    pub paths: Vec<std::path::PathBuf>,
    pub ignore_whitespace: bool,
    pub debug: bool,
    pub use_string: bool,
    pub remove_left_recursion: bool,
    pub prettify: bool,
    pub skip_recover: bool,
    pub arena: bool,
    /// Span-only monolithic parse mode: all rules return `Span<'a>`, zero allocations.
    /// Requires that the grammar has no custom Map functions (all rules are span-compatible).
    pub span: bool,
}
