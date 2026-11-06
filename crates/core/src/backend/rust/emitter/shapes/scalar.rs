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

use super::alt_dispatch::branches::payload_push_from_fn;
use super::dispatcher::{emit_ref_call_shape, shape_fn_ident};
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
    let body = emit_scalar_body(&rule.body, grammar_suffix, ir, &support_mod);
    quote! {
        /// StructDirect Scalar parser. OptionalWhitespace retains its
        /// skip-space bookends, constant Maps replace inner builder effects,
        /// and Ref aliases call only functions the shape driver emits.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty<'p>,
            cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            __P: for<'__c> crate::path::schema::PathSchema<'__c>,
        {
            use crate::runtime::builder::StructBuilder as _;
            #body
            ::core::result::Result::Ok(())
        }
    }
}

fn emit_scalar_body(
    node: &IrNode,
    grammar_suffix: &str,
    ir: &GrammarIR,
    support_mod: &proc_macro2::Ident,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|byte| quote! { #byte }).collect();
            let payload = match bytes {
                b"null" => quote! { builder.push_leaf_with_unit(); },
                b"true" => quote! { builder.push_leaf_with_bool(true); },
                b"false" => quote! { builder.push_leaf_with_bool(false); },
                _ => quote! {},
            };
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
                    #payload
                }
            }
        }
        IrNode::Ref(rule_id) => match emit_ref_call_shape(grammar_suffix, *rule_id, ir) {
            Some(call) => quote! {
                let _ = (#call)?;
            },
            None => quote! {
                compile_error!(
                    "Scalar Ref target has no emitted per-shape driver"
                );
            },
        },
        IrNode::OptionalWhitespace(inner) => {
            let inner = emit_scalar_body(inner, grammar_suffix, ir, support_mod);
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #inner
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Map { inner, fn_id } => {
            let inner = emit_scalar_body(inner, grammar_suffix, ir, support_mod);
            let payload = ir.fns.get(*fn_id as usize).and_then(|descriptor| {
                match payload_push_from_fn(descriptor, ir) {
                    Ok(payload) => payload,
                    Err(message) => Some(quote! { compile_error!(#message); }),
                }
            });
            match payload {
                Some(payload) => quote! {
                    {
                        let __map_builder_checkpoint = builder.checkpoint();
                        let __map_result: ::core::result::Result<
                            (),
                            crate::runtime::DtaError,
                        > = (|| {
                            #inner
                            ::core::result::Result::Ok(())
                        })();
                        match __map_result {
                            ::core::result::Result::Ok(()) => {
                                builder.rollback(__map_builder_checkpoint);
                                #payload
                            }
                            ::core::result::Result::Err(__err) => {
                                builder.rollback(__map_builder_checkpoint);
                                return ::core::result::Result::Err(__err);
                            }
                        }
                    }
                },
                None => quote! {
                    compile_error!(
                        "Scalar Map has no emitted constant replacement"
                    );
                },
            }
        }
        _ => quote! {
            compile_error!(
                "Scalar shape has no emitted path for this IR node"
            );
        },
    }
}
