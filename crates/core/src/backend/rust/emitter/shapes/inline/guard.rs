//! Negate / Minus guard-only emission.
//!
//! - `Negate(inner)` — try inner; on success, fail with Syntax. No
//!   tape record pushed. Mirrors walker's NotFollowedBy.
//! - `Minus(primary, excluded)` — first check the excluded pattern;
//!   if it would succeed at `*p`, fail. Otherwise parse the primary,
//!   emitting its records. Mirrors walker's `emit_minus_arm`.
//!
//! The shared sub-helpers `emit_guard_attempt_tape`
//! (rewind-on-success guard pattern) and `emit_primary_tape`
//! (record-producing primary emission) keep guard handling local to
//! the production inline path.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::dfa_codegen::regex_scan_adapter_ident;
use super::super::dispatcher::emit_ref_call_shape;
use super::super::sanitise_grammar;
use super::branch_analysis::unwrap_trivia;
use super::emit_inline_position_tape;
use super::regex::emit_regex_tape;

/// `Negate(inner)` — try inner; on success, fail with Syntax. No tape
/// record pushed. Mirrors walker's NotFollowedBy. On inner failure,
/// preserve `*p` and continue.
pub(super) fn emit_negate_tape(
    inner: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let inner_attempt = emit_guard_attempt_tape(inner, support_mod, grammar_suffix, ir);
    quote! {
        {
            let save_p = *p;
            let attempt: ::core::result::Result<(), ()> = (|| {
                #inner_attempt
                Ok(())
            })();
            *p = save_p;
            if attempt.is_ok() {
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    },
                );
            }
        }
    }
}

/// `Minus(primary, excluded)` — first check the excluded pattern; if
/// it would succeed at `*p`, fail. Otherwise parse the primary,
/// emitting its records. Mirrors walker's `emit_minus_arm`.
pub(super) fn emit_minus_tape(
    primary: &IrNode,
    excluded: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let excluded_attempt = emit_guard_attempt_tape(excluded, support_mod, grammar_suffix, ir);
    let primary_emit = emit_primary_tape(primary, variant_idx, support_mod, grammar_suffix, ir);
    quote! {
        {
            let save_p = *p;
            let excluded_result: ::core::result::Result<(), ()> = (|| {
                #excluded_attempt
                Ok(())
            })();
            *p = save_p;
            if excluded_result.is_ok() {
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: save_p as u32,
                    },
                );
            }
            #primary_emit
        }
    }
}

/// Emit a guard-mode attempt for a node — returns `Ok(())` on match,
/// `Err(())` on failure. Used by Negate / Minus. No tape records are
/// committed; the caller wraps this in a rewind block.
fn emit_guard_attempt_tape(
    node: &IrNode,
    _support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return Err(());
                }
                *p = end;
            }
        }
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
            quote! {
                let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                    return Err(());
                };
                *p += match_len as usize;
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_shape(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                if (#call).is_err() {
                    return Err(());
                }
            },
            None => quote! { return Err(()); },
        },
        _ => quote! { return Err(()); },
    }
}

/// Emit the primary-side of a Minus — a full-record-producing inline
/// position. Delegates back through [`emit_inline_position_tape`] for
/// non-leaf nodes (Alt / Regex / …) or emits direct byte matches for
/// Literal / Ref leaves.
pub(super) fn emit_primary_tape(
    node: &IrNode,
    variant_idx: u8,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match unwrap_trivia(node) {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
            let variant_lit = variant_idx;
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return ::core::result::Result::Err(
                        crate::runtime::DtaError::Syntax {
                            offset: at as u32,
                        },
                    );
                }
                *p = end;
                let _ = builder.push_leaf_with(
                    ::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    #variant_lit,
                    0,
                    ::tape::PayloadData::None,
                );
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_shape(grammar_suffix, *rid, ir) {
            // Walker-parity: on Ref-call failure inside a Minus-primary
            // we are already in a failure-commit state (the caller
            // propagates `?`), so the enclosing rule will itself fail
            // and its caller's truncation takes effect. No per-site
            // truncation needed here because the failure is terminal
            // at this position.
            Some(call) => quote! { let _ = (#call)?; },
            None => quote! {
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    },
                );
            },
        },
        IrNode::Regex(sid) => emit_regex_tape(*sid, variant_idx, grammar_suffix, ir),
        inner @ (IrNode::Alt(_, _)
        | IrNode::Negate(_)
        | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. }) => {
            emit_inline_position_tape(inner, variant_idx, support_mod, grammar_suffix, ir)
        }
        _ => quote! {},
    }
}
