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
use super::payload::{
    alt_branch_bool_payload, alt_branch_payload_value, leading_literal_bytes,
    rule_root_bool_payload, rule_root_payload_value,
};
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
/// `push_branch_tag` / `push_leaf_with_str` (Span synthesis) /
/// `push_leaf_with_unit` based on the rule's projected `TypeDesc`
/// plus the branch payload and a literal-span capture.
///
/// `TypeDesc::U8` Alt-of-literals (Sheets `add_op`, `mul_op`,
/// `unary_prefix`, `compare_op`; CSS `dirKeyword`-style discriminators)
/// route through `push_branch_tag(payload as u32)` so the parsed
/// branch's typed discriminator reaches the `StructBuilder`'s
/// per-grammar tag surface (`SheetsValue::Tag` / equivalent). The
/// `null = "null" -> 0u8` JSON pattern, by contrast, has no usable
/// discriminator (the rule is single-literal, payload value is
/// `0u8`, and JSON's `JsonStructBuilder::push_branch_tag` is a no-op
/// audit hook); JSON's `null` rule continues to land via
/// `push_leaf_with_unit()` because there is no other branch to
/// distinguish.
///
/// The discriminator between "tag emission" and "unit emission" for
/// `TypeDesc::U8` is whether the rule has a `branch_payload` (a
/// per-branch typed value): an Alt-of-literals carries one; JSON's
/// single-literal `null` does not.
///
/// AZ-III.W2.4.u — content-only literal-led keyword branches (no
/// `bool_payload`, no `branch_payload`, no rule-level `TypeDesc`
/// override) capture the matched literal slice into a synthetic
/// `BbnfValue::Span` via `push_leaf_with_str` instead of pushing
/// `Unit`. This restores the source contract `bootstrap_parser`
/// met for BBNF rules like `modifier = "?w" | "?" | "*" | "+"` and
/// `binary_operators = "<<" | ">>" | "-"`, where the trimmed span
/// of the modifier child is exactly the punctuator text — the
/// signal `lower_factor`'s span-text classification consumes
/// directly without needing the source-gap recovery.
///
/// `span_capture` is a TokenStream that, when spliced into the
/// emitted body, evaluates to a `&str` carrying the matched literal
/// bytes (typically `unsafe { ::core::str::from_utf8_unchecked(
/// &input[at..end]) }`). When `None`, the catch-all falls back to
/// the legacy `push_leaf_with_unit()` for callers that have not
/// yet plumbed the capture (e.g. a future StructDirect path where
/// the literal bytes are not yet known to the emitter site).
fn struct_direct_leaf_emit_for(
    rule_ty: Option<&TypeDesc>,
    branch_bool_payload: Option<bool>,
    branch_payload: Option<&TokenStream>,
    span_capture: Option<&TokenStream>,
) -> TokenStream {
    if let Some(value) = branch_bool_payload {
        return quote! {
            builder.push_leaf_with_bool(#value);
        };
    }

    // Per-branch typed payload wins over rule-level `TypeDesc`. The
    // `branch_payload` carries the typed value the grammar's `->`
    // declaration produced for THIS branch — when present, it is the
    // finer-grained truth; the rule-level `rule_ty` is the coarse
    // unification of all branches and may be `None` / `Tuple` /
    // unsolved when the type-inference pass under-typed the rule.
    //
    // `IntLit` payloads route through `push_branch_tag(value)` so
    // grammar-specific `StructBuilder` implementations (Sheets's
    // `Tag(b)` / `Error(b)` deposits, JSON's no-op audit hook) observe
    // the declared discriminator. Bool-rule rule_ty + Bool payloads
    // continue to route through `push_leaf_with_bool`.
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
        // Per-branch IntLit / typed payload — route through
        // `push_branch_tag(payload)` regardless of rule-level type.
        // Sheets `add_op`, `mul_op`, `compare_op`, `unary_prefix`,
        // `error_literal`, `sheet_prefix` all land here when their
        // branches declare `-> Nu8`. JSON's `null = "null" -> 0u8`
        // also lands here (single-literal: per-branch payload is
        // `0u32`); JSON's `JsonStructBuilder::push_branch_tag` is a
        // no-op audit hook so the call is signature-compatible.
        (_, Some(payload)) => quote! {
            builder.push_branch_tag(#payload);
        },
        // No payload, no rule-level type override — synthesise a
        // `BbnfValue::Span` carrying the matched literal slice when
        // the caller plumbed `span_capture`. The lower-side
        // classification path (`lower_factor` /
        // `lower_mapped_factor`) reads the span text directly,
        // eliminating the W2.4.t source-gap recovery for keyword
        // branches whose grammar projection is content-only.
        (_, None) => match span_capture {
            Some(capture) => quote! {
                builder.push_leaf_with_str(#capture);
            },
            None => quote! {
                builder.push_leaf_with_unit();
            },
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
            // AZ-IV.W1.9 — for single-literal bodies wrapped in
            // `Map { fn_id }` (e.g. JSON's `null = "null" -> 0u8`), the
            // typed payload lives at the rule root. Extract it from
            // `rule.body` (BEFORE `unwrap_trivia` strips the Map
            // wrapper) so the leaf-emit dispatch routes to
            // `push_leaf_with_unit` (the IntLit-as-null-marker pattern).
            // Without this, single-literal Map'd rules fall through to
            // `push_leaf_with_str("null")` and the runtime surfaces
            // `JsonValue::String("null")` instead of `JsonValue::Null`.
            let rule_bool_payload = rule_root_bool_payload(&rule.body, ir);
            let single_literal_is_null_marker =
                rule_bool_payload.is_none() && rule_root_payload_value(&rule.body, ir).is_some();
            // AZ-III.W2.4.u — splice the matched literal bytes into a
            // `&str` slice that `struct_direct_leaf_emit_for` routes
            // into `push_leaf_with_str` when no typed payload is set.
            // The slice comes from the input under SAFETY guarded by
            // the immediately-preceding equality check against the
            // literal byte sequence (which was UTF-8 by construction
            // — every byte in `bytes` came from `ir.get_string(*sid)`,
            // a `&str`).
            let span_capture: TokenStream = quote! {
                unsafe { ::core::str::from_utf8_unchecked(&input[at..end]) }
            };
            // For the null-marker pattern, suppress span_capture so
            // the catch-all routes through `push_leaf_with_unit()`.
            let leaf_emit = if single_literal_is_null_marker {
                struct_direct_leaf_emit_for(rule_ty.as_ref(), None, None, None)
            } else {
                struct_direct_leaf_emit_for(
                    rule_ty.as_ref(),
                    rule_bool_payload,
                    None,
                    Some(&span_capture),
                )
            };
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
                #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
                pub fn #fn_ident<'p, __P>(
                    input: &'p [u8],
                    p: &mut usize,
                    _first_byte: u8,
                    _state: &mut #support_mod::ScanState,
                    builder: &mut #builder_ty_e,
                    cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
                ) -> ::core::result::Result<(), crate::runtime::DtaError>
                where
                    __P: for<'__c> crate::path::schema::PathSchema<'__c>,
                {
                    use crate::runtime::builder::StructBuilder as _;
                    let _ = cursor;
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

            let arms: Vec<TokenStream> = by_first
                .iter()
                .map(|(first, group)| {
                    // Descending prefix length — longer literals try first.
                    let mut group_sorted: Vec<&(
                        Vec<u8>,
                        BranchKind<'_>,
                        usize,
                        &bbnf_ir::AltBranch,
                    )> = group.iter().copied().collect();
                    group_sorted.sort_by_key(|entry| (std::cmp::Reverse(entry.0.len()), entry.2));
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
                                    let bool_payload = alt_branch_bool_payload(branch, ir);
                                    let payload = alt_branch_payload_value(branch, ir);
                                    // AZ-III.W2.4.u — splice the matched
                                    // literal bytes for Span synthesis
                                    // when this branch carries no typed
                                    // payload. Mirrors the single-literal
                                    // case above; routes the branch's
                                    // matched bytes (`input[at..end]`)
                                    // through `push_leaf_with_str` so
                                    // `lower_factor`'s span-text path
                                    // reads the modifier punctuator
                                    // directly without source-gap
                                    // recovery.
                                    let span_capture: TokenStream = quote! {
                                        unsafe {
                                            ::core::str::from_utf8_unchecked(&input[at..end])
                                        }
                                    };
                                    let leaf_emit = struct_direct_leaf_emit_for(
                                        rule_ty.as_ref(),
                                        bool_payload,
                                        payload.as_ref(),
                                        Some(&span_capture),
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
                                    let ref_call =
                                        emit_ref_call_shape(grammar_suffix, *target_rid, ir)
                                            .unwrap_or_else(|| {
                                                quote! {
                                                    ::core::result::Result::Err(
                                                        crate::runtime::DtaError::Syntax {
                                                            offset: *p as u32,
                                                        },
                                                    )
                                                }
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
                                                crate::runtime::DtaError,
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
                                crate::runtime::DtaError::Syntax {
                                    offset: *p as u32,
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
                #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
                pub fn #fn_ident<'p, __P>(
                    input: &'p [u8],
                    p: &mut usize,
                    first_byte: u8,
                    state: &mut #support_mod::ScanState,
                    builder: &mut #builder_ty_p,
                    cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
                ) -> ::core::result::Result<(), crate::runtime::DtaError>
                where
                    __P: for<'__c> crate::path::schema::PathSchema<'__c>,
                {
                    use crate::runtime::builder::StructBuilder as _;
                    let _ = state;
                    let _ = cursor;
                    match first_byte {
                        #(#arms)*
                        _ => ::core::result::Result::Err(
                            crate::runtime::DtaError::Syntax {
                                offset: *p as u32,
                            },
                        ),
                    }
                }
            }
        }
        _ => quote! {},
    }
}
