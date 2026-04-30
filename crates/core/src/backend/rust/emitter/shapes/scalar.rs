//! Scalar-shape emitter — `parse_scalar_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits per-grammar Scalar-shape parse functions for rules that
//! reduce to a single-leaf body after stripping trivia wrappers
//! (Literal / Regex / Ref) and were NOT admitted by the primary
//! detectors (Object / Array / String / Number / Keyword).
//!
//! For JSON the Scalar-shape catches rules like `comma = "," ?w` and
//! `colon = ":" ?w` — the structural separator literals the object /
//! array shapes consume inline. The emitted parse function is a thin
//! delegator that advances past the expected byte sequence and
//! returns a Literal leaf. Unlike Keyword-shape the Scalar has no
//! meaningful `-> <payload>`; it is a span-only marker.
//!
//! Scalar-shape rules are rarely the direct target of a dispatcher
//! arm — they're typically consumed in the middle of an Object /
//! Array body (the `,` / `:` reads). The shape fn exists for rules
//! that ARE at a dispatch position; currently that is the
//! fallback path (grammars with Scalar-shape rules that land at the
//! top level).

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{
    emit_ref_call_shape, emit_ref_call_visitor, shape_fn_ident, visitor_shape_fn_ident,
};
use bbnf_ir::registry::EmitStrategy;

/// Emit `pub fn parse_scalar_<grammar>_<rule>(input, p, state, builder)
/// -> Result<(), DtaError>`.
///
/// AZ-I.W2.RC / AZ-II.cutover.O4 — emits the struct-builder body. The
/// literal's bytes select `builder.push_leaf_with_unit()`,
/// `builder.push_leaf_with_bool(value)`, or no value emission for
/// structural separators consumed by the enclosing compound.
pub fn emit_parse_scalar(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("scalar", grammar_suffix, rule_name);
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let EmitStrategy::StructDirect { rust, .. } = strategy;
    let builder_ty: syn::Path = syn::parse_str(rust.builder_path)
        .expect("EmitStrategy::StructDirect.rust.builder_path must parse as a Rust path");

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
            // AZ-I.W2.RC — codegen-time literal dispatch. The
            // emitter inspects the literal's bytes once and splices
            // the matching builder call, or none for structural
            // separators.
            let builder_emit: TokenStream = match bytes {
                b"null" => quote! { builder.push_leaf_with_unit(); },
                b"true" => quote! { builder.push_leaf_with_bool(true); },
                b"false" => quote! { builder.push_leaf_with_bool(false); },
                _ => quote! {},
            };
            quote! {
                /// AZ-I.W2.RC — per-grammar Scalar-shape parse
                /// function (single-literal body, struct-direct
                /// substrate). The literal's bytes select the
                /// matching `builder.push_leaf_with_*` call at
                /// codegen time; structural separators emit no
                /// builder call.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
                pub fn #fn_ident<'p>(
                    input: &'p [u8],
                    p: &mut usize,
                    _state: &mut #support_mod::ScanState,
                    builder: &mut #builder_ty<'p>,
                ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                    use crate::runtime::builder::StructBuilder as _;
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return Err(crate::runtime::DtaError::Syntax {
                            offset: at as u32,
                        });
                    }
                    *p = end;
                    #builder_emit
                    Ok(())
                }
            }
        }
        IrNode::Ref(rid) => {
            // AX.W0a.2.b — transparent-alias Scalar. Delegate to
            // the target rule's shape fn; no compound push — the
            // alias shares the target's offset. The delegate call
            // expression names `builder` as its target so the same
            // call splice composes in the struct-builder signature.
            let call = match emit_ref_call_shape(grammar_suffix, *rid, ir) {
                Some(c) => c,
                None => {
                    // Detector should have rejected — defensive empty body.
                    return quote! {};
                }
            };
            quote! {
                /// AZ-I.W2.RC — per-grammar Scalar-shape parse
                /// function (transparent-Ref body, struct-direct
                /// substrate). Delegates to the target's
                /// strategy-resolved shape fn; the inner call
                /// expression names `builder` against the concrete
                /// struct-builder.
                #[inline]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<'p>(
                    input: &'p [u8],
                    p: &mut usize,
                    state: &mut #support_mod::ScanState,
                    builder: &mut #builder_ty<'p>,
                ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                    #call
                }
            }
        }
        other => {
            unreachable!(
                "Scalar-shape emitter invoked on unexpected body {:?} for rule {}",
                std::mem::discriminant(other),
                rule_name,
            );
        }
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

// ─────────────────────────────────────────────────────────────────────
// AW-V.W3-bench-fix — visitor-path Scalar emitter.
//
// Scalar-shape rules at dispatch position are rare (JSON has comma /
// colon marker rules that get consumed inline). The visitor-path
// emits a no-op that advances past the literal — no visitor method
// fires because the scalar is a structural separator, not a value.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_scalar_visitor_<grammar>_<rule><V>(input, p, state,
/// visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_scalar_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("scalar", grammar_suffix, rule_name);
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);

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
            quote! {
                /// AW-V.W3-bench-fix — visitor-path Scalar-shape parse
                /// function (literal body).
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<V>(
                    input: &[u8],
                    p: &mut usize,
                    _state: &mut #support_mod::ScanState,
                    _visitor: &mut V,
                ) -> ::core::result::Result<(), crate::runtime::ParseErr>
                where
                    V: ::tape::ObjectVisitor
                        + ::tape::ArrayVisitor
                        + ::tape::StringVisitor
                        + ::tape::NumberVisitor
                        + ::tape::KeywordVisitor,
                {
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return Err(crate::runtime::ParseErr::Syntax {
                            offset: at as u32, rule: None,
                        });
                    }
                    *p = end;
                    Ok(())
                }
            }
        }
        IrNode::Ref(rid) => {
            let call = match emit_ref_call_visitor(grammar_suffix, *rid, ir) {
                Some(c) => c,
                None => return quote! {},
            };
            quote! {
                /// AX.W0a.2.b — visitor-path Scalar-shape parse function
                /// (transparent-Ref body).
                ///
                /// AX.W0a.2.f — compound; plain `#[inline]`.
                #[inline]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<V>(
                    input: &[u8],
                    p: &mut usize,
                    state: &mut #support_mod::ScanState,
                    visitor: &mut V,
                ) -> ::core::result::Result<(), crate::runtime::ParseErr>
                where
                    V: ::tape::ObjectVisitor
                        + ::tape::ArrayVisitor
                        + ::tape::StringVisitor
                        + ::tape::NumberVisitor
                        + ::tape::KeywordVisitor,
                {
                    #call.map(|_| ())
                }
            }
        }
        _ => quote! {},
    }
}
