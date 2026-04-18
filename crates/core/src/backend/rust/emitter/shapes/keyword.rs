//! Keyword-shape emitter — `parse_keyword_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Keyword-shape parse function. Handles two
//! admitted sub-cases per W3.1's keyword detector:
//!
//! 1. **Single-literal body** — e.g. JSON's `null = "null" -> 0u8`.
//!    Emits a direct byte-sequence match + a Literal leaf push with
//!    the rule's `-> <value>` payload.
//! 2. **Alt of literal-led branches** — e.g. JSON's
//!    `bool = "true" -> true | "false" -> false`. Emits a byte-dispatch
//!    over the discriminator byte + per-branch match + literal leaf
//!    push carrying the branch-specific payload.
//!
//! The payload inference reads the rule's `-> <expr>` annotation from
//! the IR's `FnDescriptor` list indirectly (via the existing
//! `MapExpr::Const` / `MapExpr::BoolLit` paths). For JSON the two
//! known keyword payloads are:
//!
//! - `null = "null" -> 0u8` → [`PayloadData::InlineScalar(0u32)`]
//! - `bool = "true" -> true | "false" -> false` →
//!   [`PayloadData::InlineScalar(1u32)`] and
//!   [`PayloadData::InlineScalar(0u32)`] respectively (per
//!   [`NumberVisitor`]'s `bool(value as u32)` convention in
//!   `bbnf-tape::visitor`).

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::shape_fn_ident;

/// Emit `pub fn parse_keyword_<grammar>_<rule>(input, p, first_byte,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_keyword(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("keyword", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let _ = ir;

    // Detect the two sub-cases by body shape.
    let body = unwrap_trivia(&rule.body);
    match body {
        IrNode::Literal(sid) => {
            // Single-literal body — emit a byte-sequence match.
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            // Default payload: the rule's `-> 0u8` annotation; when
            // the rule doesn't carry a `-> const` payload, fall back
            // to pushing an empty Literal leaf (no payload).
            let payload = literal_payload_for(rule, ir);
            // Payload: Aggregate(&[byte]) for 1-byte arena slot; the
            // reader's `payload_bytes(rec, 1)` pulls the byte back
            // out. For a missing annotation we default to 0.
            let payload_byte = match payload.as_ref() {
                Some(_) => {
                    // Cast the u32 literal to u8 for the 1-byte arena
                    // slot. `payload` is always 0 / 1 for bool/null.
                    quote! { #payload }
                }
                None => quote! { 0u32 },
            };
            let payload_push = quote! {
                ::bbnf::runtime::tape::PayloadData::Aggregate(&[(#payload_byte) as u8])
            };
            let byte_lits: Vec<TokenStream> = bytes
                .iter()
                .map(|b| {
                    let lit = *b;
                    quote! { #lit }
                })
                .collect();
            quote! {
                /// AW-V.W3.2 — per-grammar Keyword-shape parse function
                /// (single-literal body).
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    _first_byte: u8,
                    builder: &mut ::bbnf::runtime::tape::TapeBuilder,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::tape::TapeOffset,
                    ::bbnf::runtime::tape::DtaError,
                > {
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: at as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                    *p = end;
                    // Span kind + Aggregate(&[byte]) matches the
                    // existing walker's emission — the bench's
                    // `tape.payload_bytes(rec, 1)` reader consumes
                    // the 1-byte arena slot.
                    let off = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        at as u32,
                        end as u32,
                        #variant_idx,
                        0,
                        #payload_push,
                    );
                    Ok(off)
                }
            }
        }
        IrNode::Alt(branches, _) => {
            // Alt-of-literals — emit a per-branch byte-dispatch.
            let arms: Vec<TokenStream> = branches
                .iter()
                .enumerate()
                .filter_map(|(branch_idx, branch)| {
                    let body = unwrap_trivia(&branch.node);
                    let IrNode::Literal(sid) = body else { return None };
                    let bytes = ir.get_string(*sid).as_bytes();
                    if bytes.is_empty() { return None; }
                    let first = bytes[0];
                    let len = bytes.len();
                    let byte_lits: Vec<TokenStream> = bytes
                        .iter()
                        .map(|b| {
                            let lit = *b;
                            quote! { #lit }
                        })
                        .collect();
                    let branch_payload = alt_branch_payload(rule, branch, branch_idx, ir);
                    let branch_meta = branch_idx as u8;
                    Some(quote! {
                        #first => {
                            let at = *p;
                            let end = at + #len;
                            if input.len() < end
                                || input[at..end] != [#(#byte_lits),*]
                            {
                                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                    offset: at as u32,
                                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                            *p = end;
                            // Span + Aggregate — see null arm.
                            let off = builder.push_leaf_with(
                                ::bbnf::runtime::tape::TapeKind::Span,
                                at as u32,
                                end as u32,
                                #variant_idx,
                                #branch_meta,
                                #branch_payload,
                            );
                            Ok(off)
                        }
                    })
                })
                .collect();
            quote! {
                /// AW-V.W3.2 — per-grammar Keyword-shape parse function
                /// (Alt of literal-led branches).
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    first_byte: u8,
                    builder: &mut ::bbnf::runtime::tape::TapeBuilder,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::tape::TapeOffset,
                    ::bbnf::runtime::tape::DtaError,
                > {
                    match first_byte {
                        #(#arms)*
                        _ => Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        }),
                    }
                }
            }
        }
        _ => quote! {},
    }
}

/// Extract the rule's `-> <const>` scalar payload if present. Returns
/// `Some(u32)` for `InlineScalar` form; `None` when the rule has no
/// `-> ` annotation or carries a non-scalar payload.
fn literal_payload_for(rule: &IrRule, ir: &GrammarIR) -> Option<TokenStream> {
    // Walk the rule body looking for Map { inner: Literal, fn_id }.
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let fn_id = find_map_fn(&rule.body)?;
    let fn_desc = ir.fns.get(fn_id as usize)?;
    payload_from_fn(fn_desc, ir)
}

/// Per-branch payload for an Alt-of-literals. Branch index is passed
/// so we can discriminate (e.g. `true`→1, `false`→0).
fn alt_branch_payload(
    _rule: &IrRule,
    branch: &bbnf_ir::AltBranch,
    _branch_idx: usize,
    ir: &GrammarIR,
) -> TokenStream {
    // Walk the branch body for a Map { fn_id } annotation.
    fn find_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, .. } => Some(*fn_id),
            IrNode::OptionalWhitespace(inner) => find_map_fn(inner),
            _ => None,
        }
    }
    let payload = find_map_fn(&branch.node)
        .and_then(|fid| ir.fns.get(fid as usize))
        .and_then(|fd| payload_from_fn(fd, ir));
    let payload_byte = match payload {
        Some(value) => quote! { #value },
        None => quote! { 0u32 },
    };
    quote! {
        ::bbnf::runtime::tape::PayloadData::Aggregate(&[(#payload_byte) as u8])
    }
}

/// Extract a `u32` payload value from a `FnDescriptor` when possible.
///
/// Handles the simple cases W3.2 admits (single-literal + 2-branch
/// bool). More nuanced payload typing (F64 / U32 / Aggregate) is
/// out-of-scope for Keyword-shape (numbers route through
/// Number-shape; strings route through String-shape).
fn payload_from_fn(fn_desc: &bbnf_ir::FnDescriptor, ir: &GrammarIR) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, MapExpr};
    let FnDescriptor::Expr { expr, .. } = fn_desc else {
        return None;
    };
    match expr {
        MapExpr::BoolLit(b) => {
            let v = if *b { 1u32 } else { 0u32 };
            Some(quote! { #v })
        }
        MapExpr::IntLit(n) => {
            let v = *n as u32;
            Some(quote! { #v })
        }
        MapExpr::StringLit(sid) => {
            // `"null" -> 0u8` can also lower as IntLit; if it lowers
            // as StringLit we conservatively pick 0.
            let _ = sid;
            let _ = ir;
            Some(quote! { 0u32 })
        }
        _ => None,
    }
}

/// Strip `Map` / `OptionalWhitespace` trivia wrappers.
fn unwrap_trivia(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_trivia(inner.as_ref()),
        IrNode::OptionalWhitespace(inner) => unwrap_trivia(inner.as_ref()),
        _ => node,
    }
}
