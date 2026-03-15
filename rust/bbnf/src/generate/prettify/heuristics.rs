//! Heuristic mode for auto-inferring `@pretty` hints.
//!
//! A grammar-level `@pretty * <mode>` meta-directive controls the mode.

/// Heuristic mode for auto-inferring `@pretty` hints.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeuristicMode {
    /// No heuristics at all — raw codegen (backward compat).
    Off,
    /// Only apply wrapped-pattern fix (Step 1.3).
    Minimal,
    /// Full structural inference (default).
    Auto,
}

impl HeuristicMode {
    /// Parse a mode string from a `@pretty * <mode>` directive.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "off" => Some(Self::Off),
            "minimal" => Some(Self::Minimal),
            "auto" => Some(Self::Auto),
            _ => None,
        }
    }
}
