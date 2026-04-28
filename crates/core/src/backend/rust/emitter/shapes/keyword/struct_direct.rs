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

use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::payload::{alt_branch_payload_value, leading_literal_bytes};
use super::unwrap_trivia;
use super::super::dispatcher::shape_fn_ident;

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
    branch_payload: Option<&TokenStream>,
) -> TokenStream {
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
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("keyword", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_ty = rule_type_desc(rule, ir);

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
            let leaf_emit = struct_direct_leaf_emit_for(rule_ty.as_ref(), None);
            quote! {
                /// AZ-I.W2.RD — struct-direct Keyword-shape parse fn
                /// (single-literal body).
                ///
                /// Matches the literal byte sequence and routes the
                /// rule's projected payload through the `StructBuilder`
                /// trait surface. Returns `TapeOffset::NONE` on success
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
                    builder: &mut crate::runtime::JsonStructBuilder<'_>,
                ) -> ::core::result::Result<
                    crate::runtime::tape::TapeOffset,
                    crate::runtime::tape::DtaError,
                > {
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
                    ::core::result::Result::Ok(
                        crate::runtime::tape::TapeOffset::NONE,
                    )
                }
            }
        }
        IrNode::Alt(branches, _) => {
            use std::collections::BTreeMap;

            // Mirror the tape-path partition: collect literal-led
            // branches with their payload. Ref-led / Seq-led branches
            // are not admitted on JSON's keyword grammars, but to keep
            // the StructDirect body total we route them through a
            // trailing syntax error — the resolver gates JSON-only
            // admission, so this arm only fires for malformed input.
            let per_branch: Vec<(Vec<u8>, Option<TokenStream>, usize)> = branches
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
                            let payload = alt_branch_payload_value(branch, ir);
                            Some((bytes, payload, branch_idx))
                        }
                        IrNode::Ref(_) | IrNode::Seq(_)
                        | IrNode::Next(_, _) | IrNode::Skip(_, _) => {
                            // Reach the leading literal so the dispatch
                            // arm still routes — but on JSON these
                            // shapes don't appear at keyword level.
                            let bytes = leading_literal_bytes(body, ir)?;
                            if bytes.is_empty() {
                                return None;
                            }
                            let payload = alt_branch_payload_value(branch, ir);
                            Some((bytes, payload, branch_idx))
                        }
                        _ => None,
                    }
                })
                .collect();

            let mut by_first: BTreeMap<u8, Vec<&(Vec<u8>, Option<TokenStream>, usize)>> =
                BTreeMap::new();
            for entry in &per_branch {
                by_first.entry(entry.0[0]).or_default().push(entry);
            }

            let arms: Vec<TokenStream> = by_first
                .iter()
                .map(|(first, group)| {
                    // Descending prefix length — longer literals try first.
                    let mut group_sorted: Vec<&(Vec<u8>, Option<TokenStream>, usize)> =
                        group.iter().copied().collect();
                    group_sorted.sort_by_key(|entry| {
                        (std::cmp::Reverse(entry.0.len()), entry.2)
                    });
                    let tries: Vec<TokenStream> = group_sorted
                        .iter()
                        .map(|(bytes, payload, _branch_idx)| {
                            let len = bytes.len();
                            let byte_lits: Vec<TokenStream> = bytes
                                .iter()
                                .map(|b| {
                                    let lit = *b;
                                    quote! { #lit }
                                })
                                .collect();
                            let leaf_emit = struct_direct_leaf_emit_for(
                                rule_ty.as_ref(),
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
                                    return ::core::result::Result::Ok(
                                        crate::runtime::tape::TapeOffset::NONE,
                                    );
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
                /// (Alt of literal-led branches).
                ///
                /// Each branch's typed payload routes through
                /// `builder.push_leaf_with_bool` (TypeDesc::Bool) or
                /// `builder.push_leaf_with_unit` (TypeDesc::U8 /
                /// untyped). Returns `TapeOffset::NONE` for
                /// compositional uniformity.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    first_byte: u8,
                    state: &mut #support_mod::ScanState,
                    builder: &mut crate::runtime::JsonStructBuilder<'_>,
                ) -> ::core::result::Result<
                    crate::runtime::tape::TapeOffset,
                    crate::runtime::tape::DtaError,
                > {
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
