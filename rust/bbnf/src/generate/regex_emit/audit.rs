//! Regex tier audit — diagnoses which emission tier handles each pattern.
//!
//! Used for coverage tracking: every regex pattern in a grammar should
//! resolve to a tier without falling back to LazyLock.

use super::hir_walk;
use crate::generate::fast_paths;
use crate::generate::fast_paths::detect;

/// Which emission tier handles a regex pattern.
#[derive(Debug, Clone)]
pub enum RegexTier {
    /// Tier 1: Known fast-path scanner (JSON number, CSS ident, etc.).
    FastPath(&'static str),
    /// Tier 1b: Known fast-path scanner with fused number conversion.
    FastPathFused(&'static str),
    /// Tier 2: HIR-based inline byte operations.
    HirInline,
    /// Tier 3: DFA-compiled inline state machine or table.
    DfaCompiled { states: usize, classes: usize },
    /// Tier 4: Unsupported — no tier can compile this pattern (compile-time error).
    Unsupported,
}

/// Audit a regex pattern to determine which emission tier handles it.
///
/// This does NOT emit code — it just classifies the pattern.
pub fn audit_regex_pattern(pattern: &str) -> RegexTier {
    // Tier 1: fast paths (non-fused).
    if fast_paths::emit_regex_direct_call(pattern).is_some() {
        return RegexTier::FastPath(classify_fast_path(pattern));
    }

    // Tier 2: HIR-based inline.
    if hir_walk::try_emit_regex_inline(pattern).is_some() {
        return RegexTier::HirInline;
    }

    // Tier 3: DFA.
    if let Some(dfa) = parse_that::regex_engine::Dfa::compile(pattern) {
        return RegexTier::DfaCompiled {
            states: dfa.state_count(),
            classes: dfa.class_count(),
        };
    }

    // Tier 4: unsupported.
    RegexTier::Unsupported
}

/// Classify which fast-path scanner handles a pattern (for diagnostic display).
fn classify_fast_path(pattern: &str) -> &'static str {
    if detect::is_json_number_pattern(pattern) {
        return "json_number";
    }
    if detect::is_json_string_pattern(pattern) {
        return "json_string";
    }
    if detect::is_ws_block_comment_pattern(pattern) {
        return "ws_block_comment";
    }
    if detect::is_ident_pattern(pattern) {
        return "ident";
    }
    if detect::is_quoted_string_pattern(pattern) {
        return "quoted_string";
    }
    "other"
}
