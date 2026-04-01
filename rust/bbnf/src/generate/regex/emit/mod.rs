//! Regex code emission — tiered fallback via pattern registry.
//!
//! The `RegexPattern` trait defines a pluggable pattern recognizer + emitter.
//! The registry is priority-ordered: each pattern is tried in order until one
//! matches. Adding a new pattern = implement the trait + add one line to the registry.

mod dfa;
mod generalized;
mod hir;
mod inline_scanners;
mod negated_class;
pub mod simd;

// Re-export the negated class detection (used by fast_paths below)
pub use negated_class::{NegCharClassQuantifier, is_negated_char_class_regex};

// Phase 6: DFA canonical hashing for cross-rule deduplication.
pub use dfa::canonical_dfa_hash;

use crate::generate::regex::classify::{RegexClass, classify_regex};
use crate::generate::regex::cost_model::EmitOpts;

use proc_macro2::TokenStream;
use quote::quote;

// ── Unified entry point ──────────────────────────────────────────────────

/// Unified regex emission entry point.
///
/// Tries tiers in fallback order: fast_paths → hir → dfa → compile_error.
/// All callers should use this instead of calling individual tiers directly.
pub fn emit_regex(pattern: &str, opts: &EmitOpts) -> TokenStream {
    // Tier 1: Fast paths (known patterns, classified scanners, generalized).
    if let Some(ts) = emit_regex_fast_path(pattern, opts.fuse_numbers) {
        return ts;
    }

    // Tier 2: HIR-based inline byte operations.
    if let Some(ts) = hir::try_emit_regex_inline(pattern) {
        return ts;
    }

    // Tier 3: DFA-compiled inline state machine.
    if let Some(ts) = dfa::try_emit_dfa_inline(pattern) {
        return ts;
    }

    // Tier 4: Unsupported — compile-time error.
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
pub fn is_fused_number_regex(pattern: &str) -> bool {
    matches!(classify_regex(pattern), RegexClass::JsonNumber)
}

// ── Fast-path emission (Tier 1) ──────────────────────────────────────────

/// Emit a direct scanner call, without fused number conversion.
pub fn emit_regex_direct_call(pattern: &str) -> Option<TokenStream> {
    emit_regex_fast_path(pattern, false)
}

/// Emit a direct scanner call with optional fused number conversion.
fn emit_regex_fast_path(pattern: &str, fuse_numbers: bool) -> Option<TokenStream> {
    match classify_regex(pattern) {
        RegexClass::JsonString => {
            return Some(quote! { ::parse_that::scan_json_string(state) });
        }
        RegexClass::JsonNumber => {
            if fuse_numbers {
                return Some(quote! { ::parse_that::scan_number_convert_json(state) });
            } else {
                return Some(quote! { ::parse_that::scan_number_span_json(state) });
            }
        }
        RegexClass::WsBlockComment => {
            return Some(inline_scanners::emit_inline_ws_comment_scanner());
        }
        RegexClass::CssIdent => {
            return Some(inline_scanners::emit_inline_ident_scanner());
        }
        RegexClass::CssQuotedString => {
            return Some(inline_scanners::emit_inline_string_scanner());
        }
        RegexClass::Numeric { allows_sign, .. } => {
            if !allows_sign {
                return Some(quote! { ::parse_that::scan_number_span_json(state) });
            }
        }
        RegexClass::Identifier => {
            return Some(inline_scanners::emit_inline_ident_scanner());
        }
        _ => {}
    }

    // Comma-or-whitespace separator: ,|\s+
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
    if let Some(ts) = try_emit_simd_positive_class(pattern) {
        return Some(ts);
    }

    // Generalized regex patterns (char ranges, small char sets).
    if let Some(ts) = generalized::emit_generalized_regex_direct(pattern) {
        return Some(ts);
    }

    // Negated character class → memchr (1-3) or nibble-LUT (4-8).
    if let Some((excluded, quantifier)) = is_negated_char_class_regex(pattern) {
        let bytes = excluded.as_bytes();

        // Try memchr first (1-3 needles).
        let result = match quantifier {
            NegCharClassQuantifier::Plus => simd::emit_negated_scan_plus(bytes),
            NegCharClassQuantifier::Star => simd::emit_negated_scan_star(bytes),
        };
        if result.is_some() {
            return result;
        }

        // Try nibble-LUT for 4-8 excluded bytes (Phase 2.3).
        if bytes.len() >= 4 && bytes.len() <= 8 {
            if let Some(lut_scan) = simd::emit_nibble_lut_scan(bytes) {
                let ts = match quantifier {
                    NegCharClassQuantifier::Plus => {
                        quote! {
                            {
                                let __start = state.offset;
                                if __start >= state.src_bytes.len() { None } else {
                                    let __scan = (#lut_scan).unwrap_or(state.src_bytes.len() - __start);
                                    if __scan == 0 { None } else {
                                        state.offset = __start + __scan;
                                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                                    }
                                }
                            }
                        }
                    }
                    NegCharClassQuantifier::Star => {
                        quote! {
                            {
                                let __start = state.offset;
                                let __scan = if __start >= state.src_bytes.len() { 0 } else {
                                    (#lut_scan).unwrap_or(state.src_bytes.len() - __start)
                                };
                                state.offset = __start + __scan;
                                Some(::parse_that::Span::new(__start, state.offset, state.src))
                            }
                        }
                    }
                };
                return Some(ts);
            }
        }
    }

    None
}

// ── Audit support ────────────────────────────────────────────────────────

/// Which emission tier handles a regex pattern (diagnostic only).
#[derive(Debug, Clone)]
pub enum RegexTier {
    /// Tier 1: Known fast-path scanner.
    FastPath(&'static str),
    /// Tier 1b: Known fast-path scanner with fused number conversion.
    FastPathFused(&'static str),
    /// Tier 2: HIR-based inline byte operations.
    HirInline,
    /// Tier 3: DFA-compiled inline state machine or table.
    DfaCompiled { states: usize, classes: usize },
    /// Tier 4: Unsupported — compile-time error.
    Unsupported,
}

/// Audit a regex pattern to determine which emission tier handles it.
pub fn audit_regex_pattern(pattern: &str) -> RegexTier {
    if emit_regex_direct_call(pattern).is_some() {
        return RegexTier::FastPath(classify_fast_path(pattern));
    }
    if hir::try_emit_regex_inline(pattern).is_some() {
        return RegexTier::HirInline;
    }
    if let Some(dfa) = parse_that::regex_engine::Dfa::compile(pattern) {
        return RegexTier::DfaCompiled {
            states: dfa.state_count(),
            classes: dfa.class_count(),
        };
    }
    RegexTier::Unsupported
}

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

// ── SIMD positive class emission (Phase 2.1/2.2) ────────────────────────

/// Try to emit a SIMD-accelerated scan for positive character classes.
///
/// Detects patterns like `[a-z]+`, `\d+`, `\s+`, `\w+` and emits
/// inverted memchr/nibble-LUT scans when the excluded byte count is small.
///
/// Falls through to `None` for classes with too many excluded bytes
/// (handled by scalar loops in `generalized.rs`).
fn try_emit_simd_positive_class(pattern: &str) -> Option<TokenStream> {
    // Parse the quantified class pattern.
    let (class_pattern, is_plus) = if let Some(s) = pattern.strip_suffix('+') {
        (s, true)
    } else if let Some(s) = pattern.strip_suffix('*') {
        (s, false)
    } else {
        return None;
    };

    // Detect shorthand classes: \d, \s, \w
    let excluded: Vec<u8> = match class_pattern {
        r"\d" => {
            // Digits: 0-9 included, everything else excluded
            simd::compute_excluded_bytes(&[(b'0', b'9')])
        }
        r"\s" => {
            // Whitespace: \t \n \x0B \x0C \r space
            simd::compute_excluded_bytes(&[(0x09, 0x0D), (0x20, 0x20)])
        }
        r"\w" => {
            // Word chars: [0-9A-Za-z_] — 63 included, 65 excluded in ASCII 0-127
            // Too many excluded for memchr; nibble-LUT won't help either.
            return None;
        }
        _ => {
            // Try single char range: [a-z]
            if let Some(ranges) = parse_positive_class_ranges(class_pattern) {
                simd::compute_excluded_bytes(&ranges)
            } else {
                return None;
            }
        }
    };

    // Only emit SIMD for patterns with few excluded bytes (memchr/nibble-LUT territory).
    // CostModel.memchr_max_needles=3, nibble_lut_max_excluded=8.
    if excluded.is_empty() || excluded.len() > 8 {
        return None;
    }

    // Emit the SIMD scan.
    if excluded.len() <= 3 {
        // Use memchr on excluded bytes (inverted scan).
        if is_plus {
            simd::emit_negated_scan_plus(&excluded)
        } else {
            simd::emit_negated_scan_star(&excluded)
        }
    } else {
        // Use nibble-LUT on excluded bytes (4-8).
        let lut_scan = simd::emit_nibble_lut_scan(&excluded)?;
        if is_plus {
            Some(quote! {
                {
                    let __start = state.offset;
                    if __start >= state.src_bytes.len() { None } else {
                        let __scan = (#lut_scan).unwrap_or(state.src_bytes.len() - __start);
                        if __scan == 0 { None } else {
                            state.offset = __start + __scan;
                            Some(::parse_that::Span::new(__start, state.offset, state.src))
                        }
                    }
                }
            })
        } else {
            Some(quote! {
                {
                    let __start = state.offset;
                    let __scan = if __start >= state.src_bytes.len() { 0 } else {
                        (#lut_scan).unwrap_or(state.src_bytes.len() - __start)
                    };
                    state.offset = __start + __scan;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                }
            })
        }
    }
}

/// Parse a positive character class (e.g., `[a-z]`, `[0-9a-fA-F]`) into byte ranges.
fn parse_positive_class_ranges(class_str: &str) -> Option<Vec<(u8, u8)>> {
    let inner = class_str.strip_prefix('[')?.strip_suffix(']')?;
    if inner.starts_with('^') || inner.is_empty() {
        return None;
    }

    let mut ranges = Vec::new();
    let mut chars = inner.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Shorthand in class context.
            match chars.next()? {
                'd' => ranges.push((b'0', b'9')),
                'w' => {
                    ranges.push((b'0', b'9'));
                    ranges.push((b'A', b'Z'));
                    ranges.push((b'_', b'_'));
                    ranges.push((b'a', b'z'));
                }
                's' => {
                    ranges.push((0x09, 0x0D));
                    ranges.push((0x20, 0x20));
                }
                esc if esc.is_ascii() => {
                    let b = esc as u8;
                    ranges.push((b, b));
                }
                _ => return None,
            }
        } else if c.is_ascii() {
            // Check for range: a-z
            if chars.peek() == Some(&'-') {
                chars.next(); // consume '-'
                let hi = chars.next()?;
                if !hi.is_ascii() {
                    return None;
                }
                ranges.push((c as u8, hi as u8));
            } else {
                let b = c as u8;
                ranges.push((b, b));
            }
        } else {
            return None;
        }
    }

    if ranges.is_empty() { None } else { Some(ranges) }
}
