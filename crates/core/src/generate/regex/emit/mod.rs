//! Regex code emission — tiered fallback via pattern registry.
//!
//! The `RegexPattern` trait defines a pluggable pattern recognizer + emitter.
//! The registry is priority-ordered: each pattern is tried in order until one
//! matches. Adding a new pattern = implement the trait + add one line to the registry.

mod dfa;
mod generalized;
pub mod hir;
mod negated_class;
pub(crate) mod scanner_plan;
pub mod simd;

// Re-export the negated class detection (used by fast_paths below)
pub use negated_class::{is_negated_char_class_regex, NegCharClassQuantifier};

// Phase 6: DFA canonical hashing for cross-rule deduplication.
pub use dfa::canonical_dfa_hash;

use parse_that::regex::classify::{ClassRangeInfo, RegexClass};
use crate::generate::regex::cost_model::{CostModel, EmitOpts};

use proc_macro2::TokenStream;
use quote::quote;

// ── Unified entry point ──────────────────────────────────────────────────

/// Unified regex emission entry point.
///
/// Walks the tier ladder in order and returns the first emitter that
/// succeeds. There is no decide-then-re-emit dance: each emitter is the
/// sole authority on whether it can handle the pattern, so the planner
/// and emitter cannot disagree. The [`RegexStrategy`] enum remains as a
/// pure diagnostic type for `solve_regex_strategy`'s callers (regex
/// audit, debug output) and is not consulted here.
pub fn emit_regex(pattern: &str, opts: &EmitOpts) -> TokenStream {
    // Tier 1: fast-path scanner — shared helpers, SIMD positive classes,
    // generalized char ranges, negated char-class memchr/nibble-LUT.
    if let Some(tokens) = emit_regex_fast_path(pattern, opts) {
        return tokens;
    }
    // Tier 2: HIR-based inline byte operations.
    if let Some(tokens) = hir::try_emit_regex_inline(pattern) {
        return tokens;
    }
    // Tier 3: DFA-compiled decision tree or static transition table.
    if let Some(tokens) = dfa::try_emit_dfa_inline(pattern, opts) {
        return tokens;
    }
    // Tier 4: unsupported — compile-time error.
    emit_regex_unsupported(pattern)
}

/// Emit a compile-time error for regex patterns unsupported by all tiers.
pub fn emit_regex_unsupported(pattern: &str) -> TokenStream {
    let msg = format!(
        "regex pattern not compilable by HIR walker or DFA compiler: {}",
        pattern
    );
    quote! { compile_error!(#msg) }
}

/// Check if a regex pattern returns a fused `(Span, f64)` instead of plain `Span`.
/// Used by type inference to determine the correct enum variant type.
///
/// The fused number path is keyed on the JSON-style integer alternation
/// (`reject_leading_zero`) plus an exponent — i.e. patterns whose
/// canonical shape is the JSON `Number` production.
///
/// Prefer passing an `EmitOpts` with `ir` set so the classification is
/// resolved from the cache. This shim exists for call sites where the
/// caller only has the pattern string; it pays a full HIR parse.
pub fn is_fused_number_regex(pattern: &str) -> bool {
    use parse_that::regex::classify::classify_regex;
    matches!(
        classify_regex(pattern),
        RegexClass::Numeric {
            allows_sign: true,
            allows_fraction: true,
            allows_exponent: true,
            reject_leading_zero: true,
            ..
        }
    )
}

/// Cached variant of [`is_fused_number_regex`] — resolves via
/// `ir.regex_info[sid].classification` when the pattern is interned.
pub fn is_fused_number_regex_cached(ir: &bbnf_ir::GrammarIR, pattern: &str) -> bool {
    let opts = EmitOpts::new(&CostModel::DEFAULT).with_ir(ir);
    matches!(
        opts.classify_regex(pattern),
        RegexClass::Numeric {
            allows_sign: true,
            allows_fraction: true,
            allows_exponent: true,
            reject_leading_zero: true,
            ..
        }
    )
}

// ── Fast-path emission (Tier 1) ──────────────────────────────────────────

/// Emit a direct scanner call, without fused number conversion.
pub fn emit_regex_direct_call(pattern: &str) -> Option<TokenStream> {
    let opts = EmitOpts::new(&CostModel::DEFAULT);
    emit_regex_fast_path(pattern, &opts)
}

/// Emit a direct scanner call with optional fused number conversion.
fn emit_regex_fast_path(pattern: &str, opts: &EmitOpts) -> Option<TokenStream> {
    if let Some(plan) = scanner_plan::plan_regex_scanner(pattern, opts) {
        return Some(plan.into_tokens());
    }

    // Comma-or-whitespace separator: ,|\s+
    // This is a two-branch alternation not captured by a single RegexClass
    // variant. Kept as exact string comparison (not regex re-parsing).
    if pattern == r",|\s+" || pattern == r"\s+|," {
        return Some(quote! {
            {
                let __start = state.offset;
                if __start < state.src_bytes.len() {
                    if unsafe { *state.src_bytes.get_unchecked(__start) } == b',' {
                        state.offset = __start + 1;
                        Some(::parse_that::Span::new(__start, __start + 1, state.src))
                    } else {
                        let mut __pos = __start;
                        while __pos < state.src_bytes.len()
                            && unsafe { *state.src_bytes.get_unchecked(__pos) }.is_ascii_whitespace()
                        {
                            __pos += 1;
                        }
                        if __pos > __start {
                            state.offset = __pos;
                            Some(::parse_that::Span::new(__start, __pos, state.src))
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
            }
        });
    }

    // SIMD-accelerated positive char class scanning (Phase 2.1/2.2).
    // Before falling through to scalar generalized emission, check if
    // the pattern is a quantified char class that could use memchr/nibble-LUT.
    if let Some(ts) = try_emit_simd_positive_class(pattern, opts) {
        return Some(ts);
    }

    // Generalized regex patterns (char ranges, small char sets).
    if let Some(ts) = generalized::emit_generalized_regex_direct(pattern, opts) {
        return Some(ts);
    }

    // Negated character class → ws-interleaved loop, memchr (1-3), or nibble-LUT (4-8).
    if let Some((excluded, quantifier)) = is_negated_char_class_regex(pattern, opts) {
        // When the grammar has @ws with block-comment-aware whitespace,
        // emit a ws-interleaved byte loop instead of raw memchr/LUT.
        // This ensures block comments like `/*!*/` embedded inside the
        // negated char class span are consumed transparently before the
        // terminator byte check. Only activates for
        // WhitespaceWithBlockComment @ws patterns; non-@ws and
        // non-comment grammars keep the SIMD path.
        if opts.has_ws_block_comment() {
            return Some(match quantifier {
                NegCharClassQuantifier::Plus => {
                    simd::emit_ws_interleaved_negated_scan_plus(&excluded)
                }
                NegCharClassQuantifier::Star => {
                    simd::emit_ws_interleaved_negated_scan_star(&excluded)
                }
            });
        }

        // Tranche AU.2.7 v2 — single structural-bitmap kernel path.
        // Subsumes memchr1/2/3 (1–3 needles) and nibble-LUT (4–8
        // needles) in one emitter; no hybrid, no fallback.
        let result = match quantifier {
            NegCharClassQuantifier::Plus => simd::emit_negated_scan_plus(&excluded),
            NegCharClassQuantifier::Star => simd::emit_negated_scan_star(&excluded),
        };
        if result.is_some() {
            return result;
        }
    }

    None
}

// ── Strategy planner ─────────────────────────────────────────────────────

/// Which emission strategy handles a regex pattern.
///
/// Returned by [`solve_regex_strategy`] and consumed by [`emit_regex`].
/// The DFA tier split (A vs B) is driven by `CostModel`, replacing the
/// hardcoded 12-state threshold previously baked into the emitter.
#[derive(Debug, Clone, PartialEq)]
pub enum RegexStrategy {
    /// Tier 1: Known fast-path scanner.
    FastPath(&'static str),
    /// Tier 1b: Known fast-path scanner with fused number conversion.
    FastPathFused(&'static str),
    /// Tier 2: HIR-based inline byte operations.
    HirInline,
    /// Tier 3a: DFA-compiled decision tree
    /// (`states <= cost.decision_tree_max_states`).
    DfaTierA { states: usize, classes: usize },
    /// Tier 3b: DFA-compiled static transition table
    /// (`states > cost.decision_tree_max_states`).
    DfaTierB { states: usize, classes: usize },
    /// Tier 4: Unsupported — compile-time error.
    Unsupported,
}

/// Solve the emission strategy for a regex pattern.
///
/// Probes each tier in order with the caller's `opts`; the first that
/// applies determines the strategy. The DFA tier is split into A
/// (decision-tree) and B (table-lookup) by
/// `opts.cost.decision_tree_max_states`.
///
/// This is a pure classifier for the regex audit test and debug output
/// paths. [`emit_regex`] does NOT consult it — the emitter walks the
/// same tier ladder independently and is the sole authority on whether
/// it can handle a pattern. The planner and emitter share tier
/// predicates (`emit_regex_fast_path` / `hir::try_emit_regex_inline` /
/// DFA compilation) so they cannot disagree on any pattern.
pub fn solve_regex_strategy(pattern: &str, opts: &EmitOpts) -> RegexStrategy {
    if emit_regex_fast_path(pattern, opts).is_some() {
        let kind = classify_fast_path(pattern, opts);
        let is_json_number = matches!(
            opts.classify_regex(pattern),
            RegexClass::Numeric {
                allows_sign: true,
                allows_fraction: true,
                allows_exponent: true,
                reject_leading_zero: true,
                ..
            }
        );
        return if opts.fuse_numbers && is_json_number {
            RegexStrategy::FastPathFused(kind)
        } else {
            RegexStrategy::FastPath(kind)
        };
    }
    if hir::try_emit_regex_inline(pattern).is_some() {
        return RegexStrategy::HirInline;
    }
    if let Some(dfa) = parse_that::regex::Dfa::compile(pattern) {
        let states = dfa.state_count();
        let classes = dfa.class_count();
        return if states <= opts.cost.decision_tree_max_states {
            RegexStrategy::DfaTierA { states, classes }
        } else {
            RegexStrategy::DfaTierB { states, classes }
        };
    }
    RegexStrategy::Unsupported
}

fn classify_fast_path(pattern: &str, opts: &EmitOpts) -> &'static str {
    match opts.classify_regex(pattern) {
        RegexClass::Numeric {
            reject_leading_zero: true,
            ..
        } => "json_number",
        RegexClass::Numeric { .. } => "numeric",
        RegexClass::QuotedString {
            allows_u_escapes: true,
            ..
        } => "json_string",
        RegexClass::QuotedString { .. } => "quoted_string",
        RegexClass::WhitespaceWithBlockComment => "ws_block_comment",
        RegexClass::Identifier { .. } => "ident",
        _ => "other",
    }
}

// ── SIMD positive class emission (Phase 2.1/2.2) ────────────────────────

/// Try to emit a SIMD-accelerated scan for positive character classes.
///
/// Uses the structural classifier to detect quantified char classes and
/// reads the `ClassRangeInfo.chars` bitset to compute excluded bytes
/// for memchr/nibble-LUT emission.
///
/// Falls through to `None` for classes with too many excluded bytes
/// (handled by scalar loops in `generalized.rs`).
fn try_emit_simd_positive_class(pattern: &str, opts: &EmitOpts) -> Option<TokenStream> {
    let class = opts.classify_regex(pattern);

    let (chars, negated, min, max) = match &class {
        RegexClass::CharClassQuantified(ClassRangeInfo {
            chars,
            negated,
            min,
            max,
        }) => (chars, *negated, *min, *max),
        _ => return None,
    };

    // Only handle positive, unbounded classes (+ or *).
    if negated || max.is_some() {
        return None;
    }

    let is_plus = min >= 1;

    // Compute excluded bytes from the complement of the accepted set.
    // The `chars` bitset holds the positive-form accepted bytes (ASCII).
    let excluded: Vec<u8> = (0u8..128).filter(|b| !chars.has(*b)).collect();

    // Tranche AU.2.7 v2: single structural-bitmap kernel handles
    // 1..=8 excluded bytes. The nibble-LUT window is the upper
    // bound — wider sets fall through to the generalized emitter.
    if excluded.is_empty() || excluded.len() > 8 {
        return None;
    }

    if is_plus {
        simd::emit_negated_scan_plus(&excluded)
    } else {
        simd::emit_negated_scan_star(&excluded)
    }
}
