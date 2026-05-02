//! Structural Seq Alt-branch emission for StructDirect keyword branches.
//!
//! When an Alt branch is a structural Seq containing Refs, inline
//! Alts, Regex scans, nested Seqs, and so on, the emission must walk
//! position-by-position through the branch body.
//!
//! [`emit_seq_branch_structural_struct_direct`] is the sibling entry
//! consumed by the Keyword emitter's Seq-branch arm — it bypasses the
//! outer attempt wrapper so the Keyword emitter can package matched
//! branch text through the StructBuilder surface.

use bbnf_ir::{GrammarIR, IrNode};
use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::dfa_codegen::regex_scan_adapter_ident;
use super::super::alt_dispatch::branches::branch_payload_push;
use super::super::dispatcher::emit_ref_call_shape;
use super::super::sanitise_grammar;

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
/// failures via `?` or early `return Err(DtaError)`.
///
/// AX.W0a.2.h — matches `node` directly (NOT `unwrap_trivia(node)`):
/// `OptionalWhitespace` must reach its dedicated arm to emit
/// `skip_space` bookends around the inner position. Stripping OW
/// here would silently drop the whitespace-between-positions
/// emission (bug observed on BBNF's `import_directive` structural
/// branch, where `import_items ?w "from" ?w import_path` lost the
/// skip_space between `import_items` and `"from"`).
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
            // AZ-IV.W1-CLOSE.B — descend into nested Alt children and
            // emit per-inner-arm typed payload pushes for prefix-tree-
            // factored Seqs. The factor pass at
            // `crates/ir/src/passes/prefix.rs:178-291` rewrites
            // `Map(Literal("aliceblue"), fn_aliceblue) | Map(Literal("antiquewhite"), fn_aw) | …`
            // into `Seq(Literal("a"), Alt([Map(Literal("liceblue"), fn_aliceblue), Map(Literal("ntiquewhite"), fn_aw), …]))`.
            // The outer `branch_payload_push` walker in
            // `alt_dispatch/branches.rs::find_map_fn` returns `None`
            // for the inner Alt because there are N candidate fn_ids;
            // the per-arm push happens here, immediately after the
            // inner literal match commits.
            //
            // Each inner `branch.node` carries its own
            // `Map { fn_id, … }`; `branch_payload_push` resolves it
            // exactly as the outer dispatch arm would. Non-Map inner
            // arms fall through to `push_leaf_with_unit()` (the helper's
            // default), preserving the discriminator-without-payload
            // contract for unannotated arms.
            let arms: Vec<TokenStream> = branches
                .iter()
                .map(|branch| {
                    let body = emit_branch_position_core_struct_direct(
                        &branch.node,
                        support_mod,
                        grammar_suffix,
                        ir,
                    );
                    let payload_push = branch_payload_push(&branch.node, ir);
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
                                    #payload_push
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
