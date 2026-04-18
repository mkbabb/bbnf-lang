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

use super::dispatcher::shape_fn_ident;

/// Emit `pub fn parse_scalar_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_scalar(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("scalar", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;

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
            let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
            quote! {
                /// AW-V.W3.2 — per-grammar Scalar-shape parse function.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    _state: &mut #support_mod::ScanState,
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
                    let off = builder.push_leaf(
                        ::bbnf::runtime::tape::TapeKind::Literal,
                        at as u32,
                        end as u32,
                        #variant_idx,
                        0,
                    );
                    Ok(off)
                }
            }
        }
        _ => {
            // Non-literal Scalar — rare; emit an empty stub so the
            // emitter's fn reference compiles, but route through the
            // walker fallback at the call site. W4 may extend this
            // with Regex / Ref lowering per detector extension.
            let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
            quote! {
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
                pub fn #fn_ident(
                    input: &[u8],
                    p: &mut usize,
                    _state: &mut #support_mod::ScanState,
                    _builder: &mut ::bbnf::runtime::tape::TapeBuilder,
                ) -> ::core::result::Result<
                    ::bbnf::runtime::tape::TapeOffset,
                    ::bbnf::runtime::tape::DtaError,
                > {
                    Err(::bbnf::runtime::tape::DtaError::InvalidState {
                        state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    })
                }
            }
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
