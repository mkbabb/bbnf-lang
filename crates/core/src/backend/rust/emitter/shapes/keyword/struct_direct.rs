//! AZ-I.W2.RD — struct-direct Keyword-shape body.
//!
//! Routes JSON's keyword payload projections through the
//! [`StructBuilder`] surface:
//!
//! - `null = "null" -> 0u8` (single-literal, `TypeDesc::U8`) →
//!   `builder.push_leaf_with_unit()` per the JsonStructBuilder
//!   contract documented at `crates/core/src/runtime/json/builder.rs`
//!   §wire-contract.
//! - `bool = "true" -> true | "false" -> false` (Alt-of-literal,
//!   `TypeDesc::Bool`) → per-branch `builder.push_leaf_with_bool(true)` /
//!   `builder.push_leaf_with_bool(false)`.
//!
//! Keyword rules outside JSON's surface (CSS `dirKeyword` and friends
//! whose `TypeDesc::U8` carries a non-zero discriminator) are
//! orchestrator-deferred to W3 — the StructDirect resolver does not
//! admit those grammars in W2; this module's StructDirect emission
//! covers JSON's two known shapes.

use bbnf_ir::registry::EmitStrategy;
use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{emit_ref_call_shape, shape_fn_ident};
use super::super::substrate::{builder_ty_elided, builder_ty_with_lifetime};
use super::payload::{alt_branch_bool_payload, alt_branch_payload_value, leading_literal_bytes};
use super::unwrap_trivia;

/// Resolve the rule's projected `TypeDesc`. Returns `None` when the
/// rule is absent from `ir.types` (untyped) — the caller treats this as
/// the unit-emission fallback (the `null` case is the canonical
/// untyped-keyword shape but defensive handling here keeps the
/// emitter total).
fn rule_type_desc(rule: &IrRule, ir: &GrammarIR) -> Option<TypeDesc> {
    ir.types.iter().find_map(|(rid, t)| {
        if *rid == rule.id {
            Some(t.clone())
        } else {
            None
        }
    })
}

/// Per-branch StructDirect emission: pick `push_leaf_with_bool` /
/// `push_leaf_with_unit` based on the rule's projected `TypeDesc` plus
/// the branch payload.
fn struct_direct_leaf_emit_for(
    rule_ty: Option<&TypeDesc>,
    branch_bool_payload: Option<bool>,
    branch_payload: Option<&TokenStream>,
) -> TokenStream {
    if let Some(value) = branch_bool_payload {
        return quote! {
            builder.push_leaf_with_bool(#value);
        };
    }

    match (rule_ty, branch_payload) {
        // `bool` rule branches carry `Some(0u32)` / `Some(1u32)` per
        // `payload_from_fn`; convert to the matching `bool` literal.
        (Some(TypeDesc::Bool), Some(payload)) => quote! {
            builder.push_leaf_with_bool(((#payload) as u32) != 0u32);
        },
        // Single-literal `bool` rules (no Alt) reach here without a
        // branch payload; fall back to true (the lone literal is the
        // truthy sense by W3.1's keyword classification convention).
        (Some(TypeDesc::Bool), None) => quote! {
            builder.push_leaf_with_bool(true);
        },
        // `null = "null" -> 0u8` and other `TypeDesc::U8` keywords
        // whose value is zero project to the unit (null) marker per
        // `JsonStructBuilder::push_leaf_with_unit` (see runtime docs).
        // Sheets / CSS U8 keywords with non-zero discriminators are
        // not yet admitted under W2; W3 lights them up with the
        // matching `push_leaf_with_u64` shape.
        (Some(TypeDesc::U8), _) => quote! {
            builder.push_leaf_with_unit();
        },
        // Untyped keyword (no `->` annotation) — push unit; the
        // record carries no payload.
        (None, _) => quote! {
            builder.push_leaf_with_unit();
        },
        // Other typed keywords (defensive — JSON has only U8 / Bool;
        // future grammars admitted by `for_grammar` will widen this
        // arm with the matching `push_leaf_with_*` call).
        (Some(_), _) => quote! {
            builder.push_leaf_with_unit();
        },
    }
}

/// Emit the StructDirect Keyword-shape body. Mirrors the tape-path
/// dispatch (single-literal vs Alt-of-literal vs Alt-of-Ref-or-Seq) but
/// routes the leaf push through `builder.push_leaf_with_*`.
pub(super) fn emit_parse_keyword_struct_direct(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("keyword", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_ty = rule_type_desc(rule, ir);
    // Single-literal bodies are self-contained (no delegate calls) so
    // the elided builder type suffices; Alt bodies that may carry
    // Ref-led branches need a named lifetime so the delegated call's
    // `Builder<'p>` parameter unifies with the caller's input. We pick
    // per-arm below.
    let builder_ty_e = builder_ty_elided(strategy);
    let p_lt = format_ident!("p");
    let builder_ty_p = builder_ty_with_lifetime(strategy, &p_lt);

    let body = unwrap_trivia(&rule.body);
    match body {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes
                .iter()
                .map(|b| {
                    let lit = *b;
                    quote! { #lit }
                })
                .collect();
            let leaf_emit = struct_direct_leaf_emit_for(rule_ty.as_ref(), None, None);
            quote! {
                /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
                /// (single-literal body).
                ///
                /// Matches the literal byte sequence and routes the
                /// rule's projected payload through the `StructBuilder`
                /// trait surface. Returns unit on success
                /// for compositional uniformity with the tape-path
                /// emission; the offset is unused by struct-direct
                /// callers (the dispatcher discards `Ok(_)` payloads).
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    _first_byte: u8,
                    _state: &mut #support_mod::ScanState,
                    builder: &mut #builder_ty_e,
                ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
                    use crate::runtime::builder::StructBuilder as _;
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return ::core::result::Result::Err(
                            crate::runtime::tape::DtaError::Syntax {
                                offset: at as u32,
                                failing_state:
                                    crate::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    crate::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        );
                    }
                    *p = end;
                    #leaf_emit
                    ::core::result::Result::Ok(())
                }
            }
        }
        IrNode::Alt(branches, _) => {
            use std::collections::BTreeMap;

            // Mirror the tape-path partition: collect literal-led,
            // Ref-led, and Seq-led branches per-position. The
            // Keyword-shape detector admits Alt-of-literal-led
            // (JSON `bool`), Alt-of-Ref-led (CSS `pseudoClass` whose
            // branches reference rules whose bodies are literal-prefix
            // Seqs), and Alt-of-Seq-led (BBNF `literal = "\"" Regex
            // "\""`). The struct-direct path mirrors the tape-path
            // BranchKind dispatch so each branch routes to the matching
            // emission template:
            //
            //   - Literal — byte-match + StructBuilder leaf push.
            //   - Ref     — prefix-check + delegate to target shape fn
            //               (the target's record bubbles up via the
            //               inner `builder.push_*` call).
            //   - Seq     — emit per-position structural body inside
            //               an attempt closure (handled identically to
            //               Ref for now — sub-rule's emitter carries
            //               the records; the keyword body just routes).
            enum BranchKind<'a> {
                Literal,
                Ref(bbnf_ir::RuleId),
                Seq(&'a IrNode),
            }
            let per_branch: Vec<(Vec<u8>, BranchKind<'_>, usize, &bbnf_ir::AltBranch)> = branches
                .iter()
                .enumerate()
                .filter_map(|(branch_idx, branch)| {
                    let body = unwrap_trivia(&branch.node);
                    match body {
                        IrNode::Literal(sid) => {
                            let bytes = ir.get_string(*sid).as_bytes().to_vec();
                            if bytes.is_empty() {
                                return None;
                            }
                            Some((bytes, BranchKind::Literal, branch_idx, branch))
                        }
                        IrNode::Ref(rid) => {
                            let bytes = leading_literal_bytes(body, ir)?;
                            if bytes.is_empty() {
                                return None;
                            }
                            Some((bytes, BranchKind::Ref(*rid), branch_idx, branch))
                        }
                        IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
                            let bytes = leading_literal_bytes(body, ir)?;
                            if bytes.is_empty() {
                                return None;
                            }
                            Some((bytes, BranchKind::Seq(body), branch_idx, branch))
                        }
                        _ => None,
                    }
                })
                .collect();

            let mut by_first: BTreeMap<
                u8,
                Vec<&(Vec<u8>, BranchKind<'_>, usize, &bbnf_ir::AltBranch)>,
            > = BTreeMap::new();
            for entry in &per_branch {
                by_first.entry(entry.0[0]).or_default().push(entry);
            }

            let arms: Vec<TokenStream> =
                by_first
                    .iter()
                    .map(|(first, group)| {
                        // Descending prefix length — longer literals try first.
                        let mut group_sorted: Vec<&(
                            Vec<u8>,
                            BranchKind<'_>,
                            usize,
                            &bbnf_ir::AltBranch,
                        )> = group.iter().copied().collect();
                        group_sorted
                            .sort_by_key(|entry| (std::cmp::Reverse(entry.0.len()), entry.2));
                        let tries: Vec<TokenStream> = group_sorted
                        .iter()
                        .map(|(bytes, kind, _branch_idx, branch)| {
                            let len = bytes.len();
                            let byte_lits: Vec<TokenStream> = bytes
                                .iter()
                                .map(|b| {
                                    let lit = *b;
                                    quote! { #lit }
                                })
                                .collect();
                            match kind {
                                BranchKind::Literal => {
                                    let bool_payload =
                                        alt_branch_bool_payload(branch, ir);
                                    let payload = alt_branch_payload_value(branch, ir);
                                    let leaf_emit = struct_direct_leaf_emit_for(
                                        rule_ty.as_ref(),
                                        bool_payload,
                                        payload.as_ref(),
                                    );
                                    quote! {
                                        if input.len() >= *p + #len
                                            && input[*p..*p + #len] == [#(#byte_lits),*]
                                        {
                                            let at = *p;
                                            let end = at + #len;
                                            *p = end;
                                            #leaf_emit
                                            return ::core::result::Result::Ok(());
                                        }
                                    }
                                }
                                BranchKind::Ref(target_rid) => {
                                    // AZ-II.cutover.L Phase 3a — delegate
                                    // to the target shape fn. The Ref's
                                    // body owns its record emission via
                                    // `builder.begin_compound` / `push_*`
                                    // calls; on Err we rollback `*p` and
                                    // fall through to the next candidate
                                    // in this first-byte group (CSS L4
                                    // `pseudoClass` — `:has` and `:not`
                                    // share leading `:`; on `:hover`
                                    // input every named-pseudo fails and
                                    // the fallback `classicPseudo` Ref
                                    // claims it).
                                    //
                                    // `emit_ref_call_shape` returns the
                                    // call expression irrespective of
                                    // strategy; the only difference at
                                    // the codegen level is the concrete
                                    // `builder` type, threaded through
                                    // by the caller's signature.
                                    let ref_call = emit_ref_call_shape(
                                        grammar_suffix, *target_rid, ir,
                                    ).unwrap_or_else(|| quote! {
                                        ::core::result::Result::Err(
                                            crate::runtime::tape::DtaError::Syntax {
                                                offset: *p as u32,
                                                failing_state:
                                                    crate::runtime::tape::DtaStateId::NONE,
                                                failing_rule:
                                                    crate::runtime::tape::DtaRuleId(u32::MAX),
                                            },
                                        )
                                    });
                                    quote! {
                                        if input.len() >= *p + #len
                                            && input[*p..*p + #len] == [#(#byte_lits),*]
                                        {
                                            let __ref_save_p = *p;
                                            let __ref_builder_checkpoint = builder.checkpoint();
                                            match (#ref_call) {
                                                ::core::result::Result::Ok(__off) => {
                                                    builder.commit(__ref_builder_checkpoint);
                                                    return ::core::result::Result::Ok(__off);
                                                }
                                                ::core::result::Result::Err(_) => {
                                                    *p = __ref_save_p;
                                                    builder.rollback(__ref_builder_checkpoint);
                                                }
                                            }
                                        }
                                    }
                                }
                                BranchKind::Seq(seq_body) => {
                                    let inner_emit = super::super::inline::
                                        emit_seq_branch_structural_struct_direct(
                                            seq_body,
                                            &support_mod,
                                            grammar_suffix,
                                            ir,
                                        );
                                    quote! {
                                        if input.len() >= *p + #len
                                            && input[*p..*p + #len] == [#(#byte_lits),*]
                                        {
                                            let __seq_span_lo = *p;
                                            let __seq_builder_checkpoint = builder.checkpoint();
                                            let __seq_result: ::core::result::Result<
                                                (),
                                                crate::runtime::tape::DtaError,
                                            > = (|| {
                                                #inner_emit
                                                ::core::result::Result::Ok(())
                                            })();
                                            match __seq_result {
                                                ::core::result::Result::Ok(()) => {
                                                    let __seq_span_hi = *p;
                                                    builder.rollback(__seq_builder_checkpoint);
                                                    let __seq_text = unsafe {
                                                        ::core::str::from_utf8_unchecked(
                                                            &input[__seq_span_lo..__seq_span_hi],
                                                        )
                                                    };
                                                    builder.push_leaf_with_str(__seq_text);
                                                    return ::core::result::Result::Ok(());
                                                }
                                                ::core::result::Result::Err(__err) => {
                                                    *p = __seq_span_lo;
                                                    builder.rollback(__seq_builder_checkpoint);
                                                    return ::core::result::Result::Err(__err);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        })
                        .collect();
                        quote! {
                            #first => {
                                #(#tries)*
                                return ::core::result::Result::Err(
                                    crate::runtime::tape::DtaError::Syntax {
                                        offset: *p as u32,
                                        failing_state:
                                            crate::runtime::tape::DtaStateId::NONE,
                                        failing_rule:
                                            crate::runtime::tape::DtaRuleId(u32::MAX),
                                    },
                                );
                            }
                        }
                    })
                    .collect();
            quote! {
                /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
                /// (Alt of literal-led, Ref-led, or Seq-led branches).
                ///
                /// Literal branches push leaves through
                /// `builder.push_leaf_with_bool` (TypeDesc::Bool) or
                /// `builder.push_leaf_with_unit` (TypeDesc::U8 /
                /// untyped). Ref branches delegate to the target shape
                /// fn so the target writes directly into the same
                /// builder. Returns unit for StructDirect composition.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<'p>(
                    input: &'p [u8],
                    p: &mut usize,
                    first_byte: u8,
                    state: &mut #support_mod::ScanState,
                    builder: &mut #builder_ty_p,
                ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
                    use crate::runtime::builder::StructBuilder as _;
                    let _ = state;
                    match first_byte {
                        #(#arms)*
                        _ => ::core::result::Result::Err(
                            crate::runtime::tape::DtaError::Syntax {
                                offset: *p as u32,
                                failing_state:
                                    crate::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    crate::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        ),
                    }
                }
            }
        }
        _ => quote! {},
    }
}
