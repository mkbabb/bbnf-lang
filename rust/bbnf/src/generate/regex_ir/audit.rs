//! Regex tier audit — diagnoses which emission tier handles each pattern.
//!
//! Used for coverage tracking: every regex pattern in a grammar should
//! resolve to a tier without falling back to LazyLock.

use super::fast_paths;
use super::hir;

use super::classify::{RegexClass, classify_regex};

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
    if hir::try_emit_regex_inline(pattern).is_some() {
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
    match classify_regex(pattern) {
        RegexClass::JsonNumber => "json_number",
        RegexClass::JsonString => "json_string",
        RegexClass::WsBlockComment => "ws_block_comment",
        RegexClass::CssIdent => "ident",
        RegexClass::CssQuotedString => "quoted_string",
        RegexClass::Identifier => "ident",
        RegexClass::Numeric { .. } => "numeric",
        _ => "other",
    }
}
