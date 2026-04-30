//! Structural Seq Alt-branch emission — per-position tape walk.
//!
//! When an Alt branch is a structural Seq containing Refs, inline
//! Alts, Regex scans, nested Seqs, and so on, the records cannot be
//! collapsed into a single `TapeKind::Literal` span. The emission
//! must walk position-by-position through the branch body, mirroring
//! Flat's `emit_tape_position_core` contract.
//!
//! [`emit_branch_position_core`] handles the per-position recursion
//! (dispatching to `Literal`/`Ref`/`Regex`/`Seq`/`OptionalWhitespace`/
//! `Map`/`Repeat`/`Alt`/`Negate`/`Minus`/`TokenDispatch`); the
//! Negate/Minus/Alt/TokenDispatch cases re-enter the rule-level
//! emitters and convert their `DtaError` rejection into the
//! per-position attempt closure's `Err(())` via
//! [`wrap_dta_err_to_unit`].
//!
//! [`emit_seq_branch_structural_struct_direct`] is the sibling entry
//! consumed by the Keyword emitter's Seq-branch arm — it bypasses the
//! outer attempt wrapper so the Keyword emitter can package matched
//! branch text through the StructBuilder surface.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::dfa_codegen::regex_scan_adapter_ident;
use super::super::dispatcher::emit_ref_call_shape;
use super::super::sanitise_grammar;
use super::guard::{emit_minus_tape, emit_negate_tape};
use super::token_dispatch::emit_token_dispatch_tape;

/// Emit a structural Seq Alt-branch attempt — one position per
/// child, recursing through the standard per-position tape emitter
/// (`emit_branch_position_core`), with full rollback on failure.
///
/// The emission mirrors Flat's `emit_tape_position_core` contract:
/// each Ref position calls its target's shape fn; each Literal /
/// Regex / Alt / inline position emits the walker-parity record
/// stream for that node. Unlike the pure-literal-chain path, the
/// records are NOT compressed into a single Literal leaf — the
/// branch's records land directly in the outer Alt compound.
///
/// AX.W0a.2.h — must preserve `OptionalWhitespace` trivia between
/// positions. `flatten()` strips OW wrappers (historical contract
/// for pure-literal-chain emission); the structural branch instead
/// descends Seq / Next / Skip / Map wrappers directly, keeping OW
/// as an `emit_branch_position_core` case so `skip_space` lands
/// between positions. Without this, `@import { a } from "foo"`'s
/// branch loses the space between `}` and `from` and rejects.
pub(super) fn emit_structural_branch_tape(
    seq: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let body = emit_branch_position_core(seq, support_mod, grammar_suffix, ir);
    quote! {
        {
            let attempt_p = *p;
            let attempt_len = builder.position();
            let attempt: ::core::result::Result<(), ()> = (|| {
                #body
                Ok(())
            })();
            match attempt {
                Ok(_) => break 'try_branches,
                Err(_) => {
                    *p = attempt_p;
                    builder.rollback_to(attempt_len);
                }
            }
        }
    }
}

/// StructDirect sibling for Keyword Seq branches.
///
/// The caller owns attempt rollback and final payload emission. This
/// helper only validates the branch body position-by-position, using
/// the same grammar-general structural walk as Flat StructDirect
/// positions: literals and regexes advance input, refs delegate to
/// their shape fns, and speculative operators restore both input and
/// builder state through `StructBuilder` checkpoints.
pub(in crate::backend::rust::emitter::shapes) fn emit_seq_branch_structural_struct_direct(
    seq: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    emit_branch_position_core_struct_direct(seq, support_mod, grammar_suffix, ir)
}

/// Emit a single position inside a structural Seq Alt-branch attempt
/// closure. The closure returns `Err(())` on failure and `Ok(())` on
/// the terminating position; each per-position emission propagates
/// failures via `?` → `Err(())` conversion or early `return Err(())`.
///
/// Mirrors the walker's per-state lowering for the corresponding
/// `IrNode`, with rollback handled by the outer attempt wrapper.
///
/// AX.W0a.2.h — matches `node` directly (NOT `unwrap_trivia(node)`):
/// `OptionalWhitespace` must reach its dedicated arm to emit
/// `skip_space` bookends around the inner position. Stripping OW
/// here would silently drop the whitespace-between-positions
/// emission (bug observed on BBNF's `import_directive` structural
/// branch, where `import_items ?w "from" ?w import_path` lost the
/// skip_space between `import_items` and `"from"`).
fn emit_branch_position_core(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
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
                let _ = builder.push_leaf_with(
                    crate::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    0,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
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
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
            quote! {
                let span_lo = *p as u32;
                let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                    return Err(());
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    crate::runtime::tape::TapeKind::Span,
                    span_lo,
                    span_hi,
                    0,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
            }
        }
        IrNode::Epsilon => quote! {},
        IrNode::Seq(children) => {
            let inner: Vec<TokenStream> = children
                .iter()
                .map(|c| emit_branch_position_core(c, support_mod, grammar_suffix, ir))
                .collect();
            quote! { #(#inner)* }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_branch_position_core(lhs, support_mod, grammar_suffix, ir);
            let r = emit_branch_position_core(rhs, support_mod, grammar_suffix, ir);
            quote! { #l #r }
        }
        IrNode::OptionalWhitespace(inner) => {
            let i = emit_branch_position_core(inner, support_mod, grammar_suffix, ir);
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #i
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Map { inner, .. } => {
            emit_branch_position_core(inner, support_mod, grammar_suffix, ir)
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_emit = emit_branch_position_core(inner, support_mod, grammar_suffix, ir);
            let lo_lit = *lo;
            let hi_is_finite = *hi != u32::MAX;
            let hi_lit = *hi;
            let bound_check = if hi_is_finite {
                quote! {
                    if iter_count >= #hi_lit as usize {
                        break;
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                {
                    let mut iter_count: usize = 0;
                    loop {
                        #bound_check
                        let iter_p = *p;
                        let iter_len = builder.position();
                        let iter_res: ::core::result::Result<(), ()> = (|| {
                            #inner_emit
                            Ok(())
                        })();
                        if iter_res.is_err() || *p == iter_p {
                            *p = iter_p;
                            builder.rollback_to(iter_len);
                            break;
                        }
                        iter_count += 1;
                    }
                    if iter_count < #lo_lit as usize {
                        return Err(());
                    }
                }
            }
        }
        // AY.W2.6b — Negate / Minus / Alt / TokenDispatch can appear
        // at position level inside a Keyword-shape Seq branch. EBNF's
        // `terminal` rule body `"'" , character - "'" , { character -
        // "'" } , "'"` places Minus inline between literal positions;
        // the keyword detector correctly admits the branch on the
        // leading `'` / `"` byte. The per-position emission delegates
        // to the rule-level helpers, wrapped through an inner closure
        // that converts `DtaError` rejections into the attempt
        // closure's `Err(())`. Variant index is 0 at position level —
        // the owning rule's Alt / Keyword compound stamps the rule
        // discriminant on the outer record.
        IrNode::Alt(branches, Some(_)) => {
            let inner =
                super::alt::emit_alt_byte_dispatch_tape(branches, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
        IrNode::Alt(branches, None) => {
            let inner = super::alt::emit_alt_tape(branches, 0, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
        IrNode::Negate(inner_node) => {
            let inner = emit_negate_tape(inner_node, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
        IrNode::Minus(primary, excluded) => {
            let inner = emit_minus_tape(primary, excluded, 0, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            let inner =
                emit_token_dispatch_tape(token, arms, fallback, 0, support_mod, grammar_suffix, ir);
            wrap_dta_err_to_unit(inner)
        }
    }
}

fn emit_branch_position_core_struct_direct(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
            quote! {
                {
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
                }
            }
        }
        IrNode::Ref(rid) => match emit_ref_call_shape(grammar_suffix, *rid, ir) {
            Some(call) => quote! {
                let _ = (#call)?;
            },
            None => quote! {
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    },
                );
            },
        },
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid).to_string();
            let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
            quote! {
                {
                    let __scan_start = *p;
                    let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                        return ::core::result::Result::Err(
                            crate::runtime::DtaError::Syntax {
                                offset: __scan_start as u32,
                            },
                        );
                    };
                    *p += match_len as usize;
                }
            }
        }
        IrNode::Epsilon => quote! {},
        IrNode::Seq(children) => {
            let inner: Vec<TokenStream> = children
                .iter()
                .map(|c| {
                    emit_branch_position_core_struct_direct(c, support_mod, grammar_suffix, ir)
                })
                .collect();
            quote! { #(#inner)* }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_branch_position_core_struct_direct(lhs, support_mod, grammar_suffix, ir);
            let r = emit_branch_position_core_struct_direct(rhs, support_mod, grammar_suffix, ir);
            quote! { #l #r }
        }
        IrNode::OptionalWhitespace(inner) => {
            let i = emit_branch_position_core_struct_direct(inner, support_mod, grammar_suffix, ir);
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #i
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Map { inner, .. } => {
            emit_branch_position_core_struct_direct(inner, support_mod, grammar_suffix, ir)
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_emit =
                emit_branch_position_core_struct_direct(inner, support_mod, grammar_suffix, ir);
            let lo_lit = *lo;
            let hi_is_finite = *hi != u32::MAX;
            let hi_lit = *hi;
            let bound_check = if hi_is_finite {
                quote! {
                    if __iter_count >= #hi_lit {
                        break;
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                {
                    let mut __iter_count: u32 = 0;
                    loop {
                        #bound_check
                        let __iter_save_p = *p;
                        if input.get(*p).is_none() {
                            break;
                        }
                        let __iter_builder_checkpoint = builder.checkpoint();
                        let __iter_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            #inner_emit
                            ::core::result::Result::Ok(())
                        })();
                        match __iter_result {
                            ::core::result::Result::Ok(()) => {
                                if *p == __iter_save_p {
                                    builder.rollback(__iter_builder_checkpoint);
                                    break;
                                }
                                builder.commit(__iter_builder_checkpoint);
                                __iter_count += 1;
                            }
                            ::core::result::Result::Err(_) => {
                                *p = __iter_save_p;
                                builder.rollback(__iter_builder_checkpoint);
                                break;
                            }
                        }
                    }
                    if __iter_count < #lo_lit {
                        return ::core::result::Result::Err(
                            crate::runtime::DtaError::Syntax {
                                offset: *p as u32,
                            },
                        );
                    }
                }
            }
        }
        IrNode::Alt(branches, _) => {
            let arms: Vec<TokenStream> = branches
                .iter()
                .map(|branch| {
                    let body = emit_branch_position_core_struct_direct(
                        &branch.node,
                        support_mod,
                        grammar_suffix,
                        ir,
                    );
                    quote! {
                        {
                            let __alt_save_p = *p;
                            let __alt_builder_checkpoint = builder.checkpoint();
                            let __alt_result: ::core::result::Result<
                                (),
                                crate::runtime::DtaError,
                            > = (|| {
                                #body
                                ::core::result::Result::Ok(())
                            })();
                            match __alt_result {
                                ::core::result::Result::Ok(()) => {
                                    builder.commit(__alt_builder_checkpoint);
                                    break 'try_branches;
                                }
                                ::core::result::Result::Err(_) => {
                                    *p = __alt_save_p;
                                    builder.rollback(__alt_builder_checkpoint);
                                }
                            }
                        }
                    }
                })
                .collect();
            quote! {
                'try_branches: loop {
                    #(#arms)*
                    return ::core::result::Result::Err(
                        crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        },
                    );
                }
            }
        }
        IrNode::Negate(inner) => {
            let inner_emit =
                emit_branch_position_core_struct_direct(inner, support_mod, grammar_suffix, ir);
            quote! {
                {
                    let __neg_save_p = *p;
                    let __neg_builder_checkpoint = builder.checkpoint();
                    let __neg_result: ::core::result::Result<
                        (),
                        crate::runtime::DtaError,
                    > = (|| {
                        #inner_emit
                        ::core::result::Result::Ok(())
                    })();
                    *p = __neg_save_p;
                    builder.rollback(__neg_builder_checkpoint);
                    if __neg_result.is_ok() {
                        return ::core::result::Result::Err(
                            crate::runtime::DtaError::Syntax {
                                offset: *p as u32,
                            },
                        );
                    }
                }
            }
        }
        IrNode::Minus(primary, excluded) => {
            let primary_emit =
                emit_branch_position_core_struct_direct(primary, support_mod, grammar_suffix, ir);
            let excluded_emit =
                emit_branch_position_core_struct_direct(excluded, support_mod, grammar_suffix, ir);
            quote! {
                {
                    let __minus_save_p = *p;
                    let __minus_builder_checkpoint = builder.checkpoint();
                    let __minus_excl: ::core::result::Result<
                        (),
                        crate::runtime::DtaError,
                    > = (|| {
                        #excluded_emit
                        ::core::result::Result::Ok(())
                    })();
                    *p = __minus_save_p;
                    builder.rollback(__minus_builder_checkpoint);
                    if __minus_excl.is_ok() {
                        return ::core::result::Result::Err(
                            crate::runtime::DtaError::Syntax {
                                offset: *p as u32,
                            },
                        );
                    }
                    #primary_emit
                }
            }
        }
        IrNode::TokenDispatch { .. } => quote! {
            return ::core::result::Result::Err(
                crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                },
            );
        },
    }
}

/// Wrap a rule-level tape-emit block (whose early-return uses
/// `DtaError`) inside an inner closure that converts any
/// `DtaError` rejection into the per-position attempt closure's
/// `Err(())`. Used by [`emit_branch_position_core`] to delegate the
/// Minus / Negate / Alt / TokenDispatch emit helpers without
/// duplicating ~300 LOC of emission logic.
///
/// The inner closure isolates the rule-level `return Err(DtaError)`
/// exits: on success the outer attempt continues with the records
/// already pushed to `builder`; on rejection the outer attempt
/// returns `Err(())`, and the caller (`emit_structural_branch_tape`)
/// handles rollback of `*p` + `builder.rollback_to(...)`.
fn wrap_dta_err_to_unit(rule_emit: TokenStream) -> TokenStream {
    quote! {
        {
            let __pos_attempt: ::core::result::Result<
                (),
                crate::runtime::DtaError,
            > = (|| {
                #rule_emit
                ::core::result::Result::Ok(())
            })();
            if __pos_attempt.is_err() {
                return ::core::result::Result::Err(());
            }
        }
    }
}
