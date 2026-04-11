//! Char-class quantified kernel emission — Tranche W phase 5d, expanded
//! in Tranche X phase 1.
//!
//! Maps a `CharSet128` + quantifier bounds onto the appropriate hoisted
//! helper from `parse_that::parsers::scan::digits`. Recognizes the
//! common shapes the cargo-expand audit flagged as duplicated 86 times
//! in the CSS L4 generated parser:
//!
//! - `[0-9]+` → `scan_digits_mut`
//! - `[0-9]*` → `scan_digits_star_mut`
//! - `[a-zA-Z0-9]+` → `scan_alnum_mut`
//! - `[0-9a-fA-F]+` → `scan_hex_mut`
//!
//! Other shapes return `None` so the generalized emitter continues to
//! handle them inline.
//!
//! Two output shapes are exposed (Tranche X phase 1):
//! 1. **Expression shape** — `emit_call_opt` — produces a bare
//!    `Option<Span>` expression suitable for the generalized emitter
//!    paths (`generalized/mod.rs`, `generalized/class_segments.rs`)
//!    where the surrounding emitter expects an `Option<Span>`-typed
//!    block expression.
//! 2. **Statement shape** — `emit_stmt_opt` — produces a `{ … }` block
//!    of statements that advance `state.offset` and `return None;` on
//!    failure, suitable for the HIR repetition context
//!    (`hir/repetition.rs::emit_class_loop`) where the enclosing
//!    closure has an `Option<()>` return type.
//!
//! Tranche AF.1 Wave 3E: the `DIGITS` / `ALNUM` / `HEX` shape
//! constants and the `matches_set` classifier moved to the shared
//! `kernels::charset_shapes` helper (also consumed by `prefix_class`).
//! The legacy `emit_call` wrapper that fell back to
//! `scan_digits_mut` on shape miss was deleted — it had zero
//! production callers and its fallback path was unreachable, since
//! the production consumers (`scanner_plan`, `generalized`) all
//! route through `emit_call_opt` and handle `None` explicitly.

use bbnf_ir::CharSet128;
use proc_macro2::TokenStream;
use quote::quote;

use super::charset_shapes::{matches_set, ALNUM, DIGITS, HEX};

/// Try to map `(chars, negated, lo, hi)` onto a hoisted scanner.
/// Returns `Some(call)` if a hoisted helper applies; `None` otherwise.
///
/// The returned token stream is an **expression** of type
/// `Option<Span<'a>>`. Use `emit_stmt_opt` for the HIR/repetition
/// statement context that expects `return None;` on failure.
pub fn emit_call_opt(
    chars: &CharSet128,
    negated: bool,
    lo: u32,
    hi: Option<u32>,
) -> Option<TokenStream> {
    if negated {
        return None;
    }
    let unbounded_or_one_plus = lo == 1 && hi.is_none();
    let zero_plus = lo == 0 && hi.is_none();

    if matches_set(chars, &DIGITS) {
        if unbounded_or_one_plus {
            return Some(quote! { ::parse_that::scan_digits_mut(state) });
        }
        if zero_plus {
            return Some(quote! { Some(::parse_that::scan_digits_star_mut(state)) });
        }
    }
    if matches_set(chars, &ALNUM) && unbounded_or_one_plus {
        return Some(quote! { ::parse_that::scan_alnum_mut(state) });
    }
    if matches_set(chars, &HEX) && unbounded_or_one_plus {
        return Some(quote! { ::parse_that::scan_hex_mut(state) });
    }
    None
}

/// Tranche X phase 1: emit a HIR-context **statement block** that
/// advances `state.offset` and `return None;` on failure.
///
/// Used by `generate/regex/emit/hir/repetition.rs::emit_repetition`
/// to short-circuit the inline `is_ascii_*` while-loops in favor of
/// the hoisted scanners for the recognized shapes.
///
/// For the `*` (zero-or-more) shape, the helper always succeeds, so
/// the emitted block is unconditional. For `+` (one-or-more), it
/// `return None`s on failure.
pub fn emit_stmt_opt(
    chars: &CharSet128,
    negated: bool,
    lo: u32,
    hi: Option<u32>,
) -> Option<TokenStream> {
    if negated {
        return None;
    }
    let unbounded_or_one_plus = lo == 1 && hi.is_none();
    let zero_plus = lo == 0 && hi.is_none();

    if matches_set(chars, &DIGITS) {
        if unbounded_or_one_plus {
            return Some(quote! {
                {
                    if ::parse_that::scan_digits_mut(state).is_none() {
                        return None;
                    }
                }
            });
        }
        if zero_plus {
            return Some(quote! {
                {
                    let _ = ::parse_that::scan_digits_star_mut(state);
                }
            });
        }
    }
    if matches_set(chars, &ALNUM) && unbounded_or_one_plus {
        return Some(quote! {
            {
                if ::parse_that::scan_alnum_mut(state).is_none() {
                    return None;
                }
            }
        });
    }
    if matches_set(chars, &HEX) && unbounded_or_one_plus {
        return Some(quote! {
            {
                if ::parse_that::scan_hex_mut(state).is_none() {
                    return None;
                }
            }
        });
    }
    None
}

// ── CharSet128 builders for the routing call sites ──────────────────
//
// Each builder takes the input form a particular emit site has on hand
// (regex pattern string fragment, byte ranges from a HIR class, etc.)
// and produces a CharSet128 + (negated, lo, hi) tuple suitable for
// `emit_call_opt` / `emit_stmt_opt`. Shared so the three Tranche X
// phase 1 routing sites stay consistent and the structural gate from
// §3 rule 16 has one canonical entry point per input shape.

/// Build a `CharSet128` from a regex character-class body string (the
/// part inside `[ … ]`, without the brackets). Returns the set plus a
/// `negated` flag.
///
/// Returns `None` if the body contains anything the simple parser
/// can't handle: nested brackets, `]` literals, non-ASCII, an
/// unrecognized escape, etc. The kernel routing falls back to the
/// inline emitter on `None`.
///
/// Recognized:
/// - Plain bytes: `abc` → {a, b, c}
/// - Ranges: `a-z` → {a..=z}
/// - Shorthand inside the class: `\d` `\w` `\s` (canonical ranges)
/// - Leading `^` flips negated
/// - Common escape literals (`\n`, `\r`, `\t`, ASCII punctuation)
pub fn charset_from_class_body(body: &str) -> Option<(CharSet128, bool)> {
    let mut chars = body.chars().peekable();
    let negated = if chars.peek() == Some(&'^') {
        chars.next();
        true
    } else {
        false
    };

    let mut cs = CharSet128::new();
    while let Some(c) = chars.next() {
        if c == '\\' {
            let esc = chars.next()?;
            match esc {
                'd' => cs.add_range(b'0', b'9'),
                'w' => {
                    cs.add_range(b'0', b'9');
                    cs.add_range(b'A', b'Z');
                    cs.add(b'_');
                    cs.add_range(b'a', b'z');
                }
                's' => {
                    // Canonical \s ranges: \t\n\v\f\r and space
                    cs.add(b'\t');
                    cs.add(b'\n');
                    cs.add(0x0B);
                    cs.add(0x0C);
                    cs.add(b'\r');
                    cs.add(b' ');
                }
                'n' => cs.add(b'\n'),
                'r' => cs.add(b'\r'),
                't' => cs.add(b'\t'),
                other if other.is_ascii() => cs.add(other as u8),
                _ => return None,
            }
        } else if c.is_ascii() {
            // Range form: a-z (peek for `-` followed by another char)
            if chars.peek() == Some(&'-') {
                chars.next();
                let hi = chars.next()?;
                if !hi.is_ascii() {
                    return None;
                }
                cs.add_range(c as u8, hi as u8);
            } else {
                cs.add(c as u8);
            }
        } else {
            return None;
        }
    }

    if cs.is_empty() {
        return None;
    }
    Some((cs, negated))
}

/// Build a `CharSet128` from a bare shorthand escape (`\d`, `\w`,
/// `\s`). Returns `None` for any other shorthand. Used by
/// `generalized/mod.rs::emit_shorthand_class_loop` for the bare
/// shorthand-plus-quantifier path (e.g., `\d+`, `\w*`).
pub fn charset_from_shorthand(shorthand: &str) -> Option<CharSet128> {
    let mut cs = CharSet128::new();
    match shorthand {
        r"\d" => cs.add_range(b'0', b'9'),
        r"\w" => {
            cs.add_range(b'0', b'9');
            cs.add_range(b'A', b'Z');
            cs.add(b'_');
            cs.add_range(b'a', b'z');
        }
        r"\s" => {
            cs.add(b'\t');
            cs.add(b'\n');
            cs.add(0x0B);
            cs.add(0x0C);
            cs.add(b'\r');
            cs.add(b' ');
        }
        _ => return None,
    }
    Some(cs)
}

/// Build a `CharSet128` from HIR `ByteRange`s, used by the HIR
/// repetition routing path in `generate/regex/emit/hir/repetition.rs`.
///
/// `negated` is the flag from `CharClass::Bytes { negated, .. }`.
pub fn charset_from_byte_ranges(
    ranges: &[parse_that::regex::hir::ByteRange],
) -> CharSet128 {
    let mut cs = CharSet128::new();
    for r in ranges {
        cs.add_range(r.start, r.end);
    }
    cs
}

