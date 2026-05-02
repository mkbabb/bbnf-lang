pub mod analysis;
pub mod directives;
pub mod features;
pub mod state;

pub use analysis::*;
pub use state::*;

/// Canonical [`lsp_types::Diagnostic::source`] identifier for every
/// diagnostic surfaced by `bbnf-analysis`. Single source of truth so
/// per-feature diagnostic builders no longer duplicate the literal
/// (per `feedback_no-workarounds-arch` DRY edict).
pub const DIAGNOSTIC_SOURCE: &str = "bbnf";
