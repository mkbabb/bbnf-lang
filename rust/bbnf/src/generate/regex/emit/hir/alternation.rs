//! Alternation HIR emitters: `a|b|c` patterns.
//!
//! Three strategies tried in order:
//! 1. All-single-byte compact `matches!` expression.
//! 2. First-byte dispatch via `match` when branches have disjoint leading bytes.
//! 3. Cascading if/else with checkpoint/restore per branch.

use proc_macro2::TokenStream;
use quote::quote;
use parse_that::regex::hir::{CharClass, Hir};

use super::emit_hir;

// ── Alternation ─────────────────────────────────────────────────────────────

/// Emit inline code for an alternation (`a|b|c`).
///
/// Three strategies, tried in order:
/// 1. All-single-byte: compact `matches!` expression.
/// 2. First-byte dispatch: `match` on first byte when all branches have
///    disjoint leading bytes. O(1) branch selection.
/// 3. Cascading if/else with checkpoint/restore per branch.
pub(super) fn emit_alternation(alts: &[Hir]) -> Option<TokenStream> {
    if alts.is_empty() {
        return Some(quote! { return None; });
    }
    if alts.len() == 1 {
        return emit_hir(&alts[0]);
    }

    // Strategy 1: all-single-byte compact match.
    if let Some(ts) = try_emit_byte_match_alt(alts) {
        return Some(ts);
    }

    // Strategy 2: first-byte dispatch for multi-byte branches.
    if let Some(ts) = try_emit_first_byte_dispatch_alt(alts) {
        return Some(ts);
    }

    let mut branches: Vec<TokenStream> = Vec::new();
    for (i, alt) in alts.iter().enumerate() {
        let body = emit_hir(alt)?;
        if i == 0 {
            branches.push(quote! {
                let __save_alt = state.offset;
                let __alt_ok = (|| -> Option<()> {
                    #body
                    Some(())
                })();
            });
        } else {
            branches.push(quote! {
                let __alt_ok = if __alt_ok.is_none() {
                    state.offset = __save_alt;
                    (|| -> Option<()> {
                        #body
                        Some(())
                    })()
                } else {
                    __alt_ok
                };
            });
        }
    }

    Some(quote! {
        {
            #(#branches)*
            if __alt_ok.is_none() {
                return None;
            }
        }
    })
}

/// Try to emit a compact single-byte match for alternations where every
/// branch matches exactly one byte (single-byte literal or single-char class).
///
/// Returns `None` if any branch is more complex.
fn try_emit_byte_match_alt(alts: &[Hir]) -> Option<TokenStream> {
    // Collect all possible byte values from each branch.
    let mut all_bytes: Vec<u8> = Vec::new();
    for alt in alts {
        match alt {
            Hir::Literal(bytes) if bytes.len() == 1 => {
                all_bytes.push(bytes[0]);
            }
            Hir::Class(CharClass::Bytes { ranges, negated }) => {
                if *negated {
                    return None;
                }
                for r in ranges {
                    for b in r.start..=r.end {
                        all_bytes.push(b);
                    }
                    // If the range is too large, bail.
                    if r.end as u32 - r.start as u32 > 64 {
                        return None;
                    }
                }
            }
            Hir::Class(CharClass::Unicode { ranges, negated }) => {
                if *negated {
                    return None;
                }
                for r in ranges {
                    if r.end > '\x7F' {
                        return None;
                    }
                    for c in r.start..=r.end {
                        all_bytes.push(c as u8);
                    }
                    if r.end as u32 - r.start as u32 > 64 {
                        return None;
                    }
                }
            }
            _ => return None,
        }
    }

    if all_bytes.is_empty() {
        return None;
    }

    let byte_lits: Vec<proc_macro2::Literal> = all_bytes
        .iter()
        .map(|b| proc_macro2::Literal::byte_character(*b))
        .collect();

    Some(quote! {
        {
            let __b = *state.src_bytes.get(state.offset)?;
            if !matches!(__b, #(#byte_lits)|*) {
                return None;
            }
            state.offset += 1;
        }
    })
}

/// Try to emit O(1) first-byte dispatch for alternation branches whose
/// leading bytes are all disjoint.
///
/// For `from|to|\d+%`, where branches start with `f`, `t`, `[0-9]` respectively,
/// emits a `match __b { b'f' => ..., b't' => ..., b'0'..=b'9' => ..., _ => None }`.
fn try_emit_first_byte_dispatch_alt(alts: &[Hir]) -> Option<TokenStream> {
    // Extract first-byte sets for all branches and check disjointness.
    let mut all_first_bytes: Vec<Vec<u8>> = Vec::new();
    let mut seen = [false; 256];

    for alt in alts {
        let first = extract_first_bytes(alt)?;
        if first.is_empty() {
            return None;
        }
        for &b in &first {
            if seen[b as usize] {
                return None; // overlap -- can't dispatch
            }
            seen[b as usize] = true;
        }
        all_first_bytes.push(first);
    }

    // All disjoint -- emit match-arm dispatch.
    let mut match_arms: Vec<TokenStream> = Vec::new();
    for (idx, alt) in alts.iter().enumerate() {
        let body = emit_hir(alt)?;
        let byte_lits: Vec<proc_macro2::Literal> = all_first_bytes[idx]
            .iter()
            .map(|b| proc_macro2::Literal::byte_character(*b))
            .collect();
        match_arms.push(quote! {
            #(#byte_lits)|* => {
                let __ok = (|| -> Option<()> {
                    #body
                    Some(())
                })();
                if __ok.is_none() {
                    state.offset = __save_dispatch;
                    return None;
                }
            }
        });
    }

    Some(quote! {
        {
            let __save_dispatch = state.offset;
            let __dispatch_b = *state.src_bytes.get(state.offset)?;
            match __dispatch_b {
                #(#match_arms)*
                _ => { return None; }
            }
        }
    })
}

/// Extract the definite first-byte set from an HIR node.
///
/// Returns the set of all possible first bytes, or `None` if the first byte
/// cannot be determined statically (nullable expressions, complex constructs).
fn extract_first_bytes(hir: &Hir) -> Option<Vec<u8>> {
    match hir {
        Hir::Literal(bytes) => {
            if bytes.is_empty() {
                return None;
            }
            Some(vec![bytes[0]])
        }
        Hir::Class(class) => {
            let bytes = class_to_byte_vec(class)?;
            if bytes.is_empty() || bytes.len() > 64 {
                return None;
            }
            Some(bytes)
        }
        Hir::Concat(subs) => {
            if subs.is_empty() {
                return None;
            }
            extract_first_bytes(&subs[0])
        }
        Hir::Group(sub) => extract_first_bytes(sub),
        Hir::Repetition(rep) => {
            if rep.min == 0 {
                return None; // nullable -- first byte is ambiguous
            }
            extract_first_bytes(&rep.sub)
        }
        _ => None,
    }
}

/// Convert a character class to a flat list of matching bytes.
/// Returns `None` for non-ASCII Unicode classes, negated classes, or classes with > 64 bytes.
fn class_to_byte_vec(class: &CharClass) -> Option<Vec<u8>> {
    match class {
        CharClass::Bytes { ranges, negated } => {
            if *negated {
                return None; // negated classes expand to too many bytes
            }
            let mut bytes = Vec::new();
            for r in ranges {
                let span = r.end as u32 - r.start as u32;
                if span > 64 {
                    return None;
                }
                for b in r.start..=r.end {
                    bytes.push(b);
                }
            }
            Some(bytes)
        }
        CharClass::Unicode { ranges, negated } => {
            if *negated {
                return None;
            }
            let mut bytes = Vec::new();
            for r in ranges {
                if r.end > '\x7F' {
                    return None;
                }
                let span = r.end as u32 - r.start as u32;
                if span > 64 {
                    return None;
                }
                for c in r.start..=r.end {
                    bytes.push(c as u8);
                }
            }
            Some(bytes)
        }
    }
}
