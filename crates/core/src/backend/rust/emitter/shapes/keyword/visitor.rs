//! AW-V.W3-bench-fix — visitor-path Keyword emitter.
//!
//! Mirrors the prototype's `expect_keyword` + `visitor.bool(...)` /
//! `visitor.null()` invocations at the dispatcher arm. Two sub-cases:
//!
//! - `null = "null" -> 0u8` → `visitor.null()`
//! - `bool = "true" -> true | "false" -> false` →
//!   `visitor.bool(true)` / `visitor.bool(false)` per branch.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{emit_ref_call_visitor, visitor_shape_fn_ident};
use super::payload::leading_literal_bytes;
use super::unwrap_trivia;

/// Emit `pub fn parse_keyword_visitor_<grammar>_<rule><V: JsonVisitor>(
/// input, p, first_byte, state, visitor) -> Result<(), ParseErr>`.
///
/// AX.W0a.2.g — mirrors tape-path signature extension: `state` parameter
/// threaded through so Ref-led Alt branches can delegate to their
/// target's visitor-path shape fn via [`emit_ref_call_visitor`]. For
/// single-literal and pure-Literal Alt forms, `state` is unused — the
/// parameter is present only so the signature stays uniform across
/// sub-cases.
pub fn emit_parse_keyword_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("keyword", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

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
            // For `null = "null"` the semantic is `visitor.null()`.
            // Single-literal keyword emit: if the literal is `null`
            // call `visitor.null()`; if it's `true` / `false` the
            // bool visitor method; otherwise default to `null()`
            // (best-effort — rules without a `-> const` lose the
            // discriminator).
            let literal_str = std::str::from_utf8(bytes).unwrap_or("");
            let emit = match literal_str {
                "true" => quote! {
                    visitor.bool(true).map_err(|_| crate::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    })
                },
                "false" => quote! {
                    visitor.bool(false).map_err(|_| crate::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    })
                },
                _ => quote! {
                    visitor.null().map_err(|_| crate::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    })
                },
            };
            quote! {
                /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
                /// function (single-literal body).
                ///
                /// AX.W0a.2.g — `state` parameter unused for single-
                /// literal form.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<V>(
                    input: &[u8],
                    p: &mut usize,
                    _first_byte: u8,
                    _state: &mut #support_mod::ScanState,
                    visitor: &mut V,
                ) -> ::core::result::Result<(), crate::runtime::ParseErr>
                where
                    V: crate::runtime::tape::KeywordVisitor
                       + crate::runtime::tape::ObjectVisitor
                       + crate::runtime::tape::ArrayVisitor
                       + crate::runtime::tape::StringVisitor
                       + crate::runtime::tape::NumberVisitor,
                {
                    let at = *p;
                    let end = at + #len;
                    if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                        return Err(crate::runtime::ParseErr::Syntax {
                            offset: at as u32, rule: None,
                        });
                    }
                    *p = end;
                    #emit
                }
            }
        }
        IrNode::Alt(branches, _) => {
            use std::collections::BTreeMap;

            // Collect `(leading_literal_bytes, branch_ref_or_literal)`
            // for each branch; mirrors the tape-path collection logic.
            let per_branch: Vec<(Vec<u8>, Option<bbnf_ir::RuleId>, usize, &bbnf_ir::AltBranch)> =
                branches
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
                                Some((bytes, None, branch_idx, branch))
                            }
                            IrNode::Ref(rid) => {
                                let bytes = leading_literal_bytes(body, ir)?;
                                if bytes.is_empty() {
                                    return None;
                                }
                                Some((bytes, Some(*rid), branch_idx, branch))
                            }
                            _ => None,
                        }
                    })
                    .collect();

            let mut by_first: BTreeMap<u8, Vec<&(Vec<u8>, Option<bbnf_ir::RuleId>, usize, &bbnf_ir::AltBranch)>> =
                BTreeMap::new();
            for entry in &per_branch {
                by_first.entry(entry.0[0]).or_default().push(entry);
            }

            let arms: Vec<TokenStream> = by_first
                .iter()
                .map(|(first, group)| {
                    let tries: Vec<TokenStream> = group
                        .iter()
                        .map(|(bytes, target_ref, _branch_idx, _branch)| {
                            let len = bytes.len();
                            let byte_lits: Vec<TokenStream> =
                                bytes.iter().map(|b| {
                                    let lit = *b;
                                    quote! { #lit }
                                }).collect();
                            if let Some(target_rid) = target_ref {
                                let ref_call =
                                    emit_ref_call_visitor(grammar_suffix, *target_rid, ir)
                                        .unwrap_or_else(|| quote! {
                                            ::core::result::Result::Err(
                                                crate::runtime::ParseErr::Syntax {
                                                    offset: *p as u32, rule: None,
                                                },
                                            )
                                        });
                                quote! {
                                    if input.len() >= *p + #len
                                        && input[*p..*p + #len] == [#(#byte_lits),*]
                                    {
                                        return (#ref_call);
                                    }
                                }
                            } else {
                                let literal_str =
                                    std::str::from_utf8(bytes).unwrap_or("");
                                let emit = match literal_str {
                                    "true" => quote! {
                                        visitor.bool(true).map_err(
                                            |_| crate::runtime::ParseErr::Syntax {
                                                offset: at as u32, rule: None,
                                            })
                                    },
                                    "false" => quote! {
                                        visitor.bool(false).map_err(
                                            |_| crate::runtime::ParseErr::Syntax {
                                                offset: at as u32, rule: None,
                                            })
                                    },
                                    _ => quote! {
                                        visitor.null().map_err(
                                            |_| crate::runtime::ParseErr::Syntax {
                                                offset: at as u32, rule: None,
                                            })
                                    },
                                };
                                quote! {
                                    if input.len() >= *p + #len
                                        && input[*p..*p + #len] == [#(#byte_lits),*]
                                    {
                                        let at = *p;
                                        let end = at + #len;
                                        *p = end;
                                        return #emit;
                                    }
                                }
                            }
                        })
                        .collect();
                    quote! {
                        #first => {
                            #(#tries)*
                            return Err(crate::runtime::ParseErr::Syntax {
                                offset: *p as u32, rule: None,
                            });
                        }
                    }
                })
                .collect();
            quote! {
                /// AW-V.W3-bench-fix — visitor-path Keyword-shape parse
                /// function (Alt of literal-led or Ref-led branches).
                ///
                /// AX.W0a.2.g — admits Ref-led branches; threads `state`
                /// for downstream visitor-path Ref calls.
                #[inline(always)]
                #[allow(non_snake_case, clippy::too_many_arguments)]
                pub fn #fn_ident<V>(
                    input: &[u8],
                    p: &mut usize,
                    first_byte: u8,
                    state: &mut #support_mod::ScanState,
                    visitor: &mut V,
                ) -> ::core::result::Result<(), crate::runtime::ParseErr>
                where
                    V: crate::runtime::tape::KeywordVisitor
                       + crate::runtime::tape::ObjectVisitor
                       + crate::runtime::tape::ArrayVisitor
                       + crate::runtime::tape::StringVisitor
                       + crate::runtime::tape::NumberVisitor,
                {
                    let _ = state;
                    match first_byte {
                        #(#arms)*
                        _ => Err(crate::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        }),
                    }
                }
            }
        }
        _ => quote! {},
    }
}
