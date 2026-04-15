//! Repetition HIR emitters: quantifiers (`+`, `*`, `?`, `{n,m}`).
//!
//! Handles both tight byte-predicate loops for character classes and
//! general-purpose checkpoint loops for arbitrary sub-expressions.

use parse_that::regex::hir::{CharClass, Hir, Repetition};
use proc_macro2::TokenStream;
use quote::quote;

use super::emit_hir;
use super::leaf::emit_class_predicate;
use crate::backend::kernels;

// ── Repetition ──────────────────────────────────────────────────────────────

/// Emit inline code for a repetition (quantifier).
pub(super) fn emit_repetition(rep: &Repetition) -> Option<TokenStream> {
    let min = rep.min;
    let max = rep.max;

    // Lazy quantifiers (`*?`, `+?`, `{n,m}?`) cannot be handled correctly
    // by the HIR walker -- greedy loops would over-consume. Bail to the
    // DFA emitter which handles lazy semantics properly.
    // Exception: lazy patterns fused with a following literal in emit_concat
    // use memmem-based scan (handled there, not here).
    if !rep.greedy {
        return None;
    }

    // Special case: optional `?` (min=0, max=1) of a class or literal.
    if min == 0 && max == Some(1) {
        return emit_optional(&rep.sub);
    }

    // Special case: class-based loops (`[a-z]+`, `\d*`, etc.)
    // For a tight byte-predicate loop without per-iteration IIFE overhead.
    //
    // Tranche X phase 1: route through `kernels::charclass::emit_stmt_opt`
    // first. The kernel collapses the four hot shapes
    // (`[0-9]+`, `[0-9]*`, `[a-zA-Z0-9]+`, `[0-9a-fA-F]+`) onto the
    // hoisted `parse_that::scan_*_mut` helpers, eliminating the
    // ~16-line inline `is_ascii_digit()` loop the cargo-expand audit
    // measured 86 times in CSS L4. Falls through to the inline
    // `emit_class_loop` for shapes the kernel doesn't recognize
    // (negated, bounded, `\s`, multi-range mixes).
    if let Hir::Class(class) = &*rep.sub {
        if let Some(stmt) = try_kernel_class_loop(class, min, max) {
            return Some(stmt);
        }
        if let Some(predicate) = emit_class_predicate(class) {
            return emit_class_loop(&predicate, min, max);
        }
    }

    // General repetition: emit a counted loop with checkpoint per iteration.
    emit_general_loop(&rep.sub, min, max)
}

/// Emit an optional match (`?` quantifier): try, succeed either way.
fn emit_optional(sub: &Hir) -> Option<TokenStream> {
    let body = emit_hir(sub)?;
    Some(quote! {
        {
            let __save = state.offset;
            let __ok = (|| -> Option<()> {
                #body
                Some(())
            })();
            if __ok.is_none() {
                state.offset = __save;
            }
        }
    })
}

/// Emit a tight byte-predicate loop for a character class quantifier.
///
/// No checkpoint save/restore per iteration -- just a while loop with
/// direct byte checks.
fn emit_class_loop(predicate: &TokenStream, min: u32, max: Option<u32>) -> Option<TokenStream> {
    match max {
        None => {
            // Unbounded: `+` or `*`
            if min >= 1 {
                let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
                Some(quote! {
                    {
                        let __loop_start = state.offset;
                        let __end = state.src_bytes.len();
                        let mut __pos = state.offset;
                        while __pos < __end {
                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                            if #predicate { __pos += 1; } else { break; }
                        }
                        if __pos < __loop_start + #min_lit as usize {
                            return None;
                        }
                        state.offset = __pos;
                    }
                })
            } else {
                // Zero-or-more: always succeeds.
                Some(quote! {
                    {
                        let __end = state.src_bytes.len();
                        let mut __pos = state.offset;
                        while __pos < __end {
                            let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                            if #predicate { __pos += 1; } else { break; }
                        }
                        state.offset = __pos;
                    }
                })
            }
        }
        Some(max_val) => {
            // Bounded: `{n,m}`
            let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
            let max_lit = proc_macro2::Literal::u32_unsuffixed(max_val);
            Some(quote! {
                {
                    let __end = state.src_bytes.len();
                    let mut __pos = state.offset;
                    let mut __count: u32 = 0;
                    while __pos < __end && __count < #max_lit {
                        let __b = unsafe { *state.src_bytes.get_unchecked(__pos) };
                        if #predicate { __pos += 1; __count += 1; } else { break; }
                    }
                    if __count < #min_lit {
                        return None;
                    }
                    state.offset = __pos;
                }
            })
        }
    }
}

/// Tranche X phase 1: try routing a `Hir::Class` repetition through
/// `kernels::charclass::emit_stmt_opt`. Returns `Some(block)` only for
/// the recognized hot shapes; falls through to `emit_class_loop` on
/// `None`. Per §3 rule 16 this is the **structural** kernel routing
/// gate for the HIR repetition path.
fn try_kernel_class_loop(class: &CharClass, min: u32, max: Option<u32>) -> Option<TokenStream> {
    // The kernel only handles unbounded `+` (lo == 1) and `*` (lo == 0)
    // shapes today. Bounded ranges fall back to the inline emitter.
    if !(max.is_none() && (min == 0 || min == 1)) {
        return None;
    }
    // Only byte-mode classes — Unicode classes go through the DFA tier.
    let CharClass::Bytes { ranges, negated } = class else {
        return None;
    };
    if *negated {
        return None;
    }
    let chars = kernels::charclass::charset_from_byte_ranges(ranges);
    kernels::charclass::emit_stmt_opt(&chars, false, min, max)
}

/// Emit a general-purpose loop for repetition of arbitrary sub-expressions.
///
/// Uses checkpoint save/restore per iteration. More overhead than the
/// class-specific loop, but handles any HIR sub-expression.
fn emit_general_loop(
    sub: &Hir,
    min: u32,
    max: Option<u32>,
) -> Option<TokenStream> {
    let body = emit_hir(sub)?;

    match max {
        None => {
            // Unbounded: `+` or `*`
            let min_check = if min == 0 {
                quote! {}
            } else {
                let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
                quote! {
                    if __rep_count < #min_lit {
                        return None;
                    }
                }
            };
            Some(quote! {
                {
                    let mut __rep_count: u32 = 0;
                    loop {
                        let __save = state.offset;
                        let __ok = (|| -> Option<()> {
                            #body
                            Some(())
                        })();
                        if __ok.is_none() {
                            state.offset = __save;
                            break;
                        }
                        // Guard against zero-width matches causing infinite loops.
                        if state.offset == __save {
                            break;
                        }
                        __rep_count += 1;
                    }
                    #min_check
                }
            })
        }
        Some(max_val) => {
            // Bounded: `{n,m}`
            let max_lit = proc_macro2::Literal::u32_unsuffixed(max_val);
            let min_check = if min == 0 {
                quote! {}
            } else {
                let min_lit = proc_macro2::Literal::u32_unsuffixed(min);
                quote! {
                    if __rep_count < #min_lit {
                        return None;
                    }
                }
            };
            Some(quote! {
                {
                    let mut __rep_count: u32 = 0;
                    while __rep_count < #max_lit {
                        let __save = state.offset;
                        let __ok = (|| -> Option<()> {
                            #body
                            Some(())
                        })();
                        if __ok.is_none() {
                            state.offset = __save;
                            break;
                        }
                        if state.offset == __save {
                            break;
                        }
                        __rep_count += 1;
                    }
                    #min_check
                }
            })
        }
    }
}
