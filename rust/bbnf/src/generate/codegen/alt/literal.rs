//! All-literal Alt detection and emission.
//!
//! Handles alternations where every branch is a `Literal` or `Map(Literal, Constant)`.
//! Two strategies:
//! - Small sets (<=8 for mapped, <=16 for bare): sequential inline byte comparison.
//! - Large bare sets (>16): first-byte trie dispatch via `match` on the leading byte.

use bbnf_ir::AltBranch;

use proc_macro2::TokenStream;
use quote::quote;

use super::super::ir_types::IrCodegenCtx;
use super::super::unescape_literal;
use super::super::MonoCtx;
use super::{extract_literal_through_map, LitThroughMap};

/// Try to emit an all-literal Alt. Returns `None` if branches aren't all literal-like.
pub(in super::super) fn try_emit_all_literal_alt(
    branches: &[AltBranch],
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> Option<TokenStream> {
    let lit_infos: Vec<Option<LitThroughMap>> = branches
        .iter()
        .map(|b| extract_literal_through_map(&b.node, ctx))
        .collect();

    let all_literal_like = lit_infos.iter().all(|x| x.is_some());
    if !all_literal_like {
        return None;
    }

    let entries: Vec<LitThroughMap> = lit_infos.into_iter().map(|x| x.unwrap()).collect();

    let any_mapped = entries.iter().any(|e| e.constant_value.is_some());
    let all_bare = !any_mapped;

    // All-bare-literal path: Span return (any_span for large sets, sequential for small).
    // All-mapped-literal path: constant return values via sequential byte comparison.
    // Mixed (some bare, some mapped) also uses sequential when <= threshold.

    // Sequential byte comparison threshold: for bare literals use 8 (above that,
    // any_span with Aho-Corasick is faster). For mapped literals, sequential is the
    // only option since each branch has a unique return value.
    let use_sequential = if any_mapped {
        true // No any_span alternative for mapped branches.
    } else {
        entries.len() <= 8
    };

    if use_sequential {
        return Some(emit_sequential_literal_alt(&entries, all_bare, mctx, ctx));
    }

    // Large all-bare sets: first-byte trie dispatch for >16 entries,
    // sequential inline byte matching for <=16.
    debug_assert!(all_bare, "mapped literals should always use sequential path");
    Some(emit_bare_literal_alt(&entries, mctx, ctx))
}

/// Emit sequential byte comparison for small literal sets (or any mapped-literal set).
fn emit_sequential_literal_alt(
    entries: &[LitThroughMap],
    all_bare: bool,
    mctx: &mut MonoCtx,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    let cp_var = mctx.fresh("lit_cp");
    let mut arms: Vec<TokenStream> = Vec::new();
    for (i, info) in entries.iter().enumerate() {
        let raw = ctx.ir.get_string(info.lit_sid);
        let s = unescape_literal(raw);
        let bytes = s.as_bytes();
        let len = bytes.len();
        let byte_lits: Vec<proc_macro2::Literal> =
            bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();

        // The return expression: Span for bare literals, constant value for mapped.
        let ret_expr = if let Some(const_sid) = info.constant_value {
            let val_src = ctx.ir.get_string(const_sid);
            let val_expr: syn::Expr = syn::parse_str(val_src).unwrap();
            quote! { #val_expr }
        } else {
            // Bare literal -- return Span.
            if len == 1 {
                quote! { ::parse_that::Span::new(#cp_var, #cp_var + 1, state.src) }
            } else {
                quote! { ::parse_that::Span::new(#cp_var, __end, state.src) }
            }
        };

        let check = if len == 1 {
            quote! {
                if state.src_bytes.get(state.offset).copied() == Some(#(#byte_lits)*) {
                    state.offset += 1;
                    return Some(#ret_expr);
                }
            }
        } else {
            quote! {
                {
                    let __end = state.offset + #len;
                    if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*] as &[u8]) {
                        state.offset = __end;
                        return Some(#ret_expr);
                    }
                }
            }
        };

        if i < entries.len() - 1 {
            arms.push(check);
        } else {
            arms.push(quote! {
                #check
                None
            });
        }
    }

    // Return type: Span for all-bare, the constant type for all-mapped,
    // or `_` for mixed (compiler infers).
    let return_type = if all_bare {
        quote! { ::parse_that::Span<'a> }
    } else {
        quote! { _ }
    };

    quote! {
        (|| -> Option<#return_type> {
            let #cp_var = state.offset;
            #(#arms)*
        })()
    }
}

/// Emit bare-literal Alt for sets of size 9..=16 (sequential) or >16 (first-byte trie).
fn emit_bare_literal_alt(
    entries: &[LitThroughMap],
    mctx: &mut MonoCtx,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    let cp_var = mctx.fresh("lit_cp");

    // Collect (unescaped string, byte array) for each literal.
    let lit_strings: Vec<String> = entries
        .iter()
        .map(|info| unescape_literal(ctx.ir.get_string(info.lit_sid)))
        .collect();

    // For large sets (>16), group by first byte and emit match dispatch.
    if lit_strings.len() > 16 {
        return emit_first_byte_trie(&lit_strings, &cp_var, mctx);
    }

    // Small sets (<=16): sequential inline byte matching.
    emit_sequential_bare(&lit_strings, &cp_var)
}

/// Emit first-byte trie dispatch for large (>16) bare-literal sets.
fn emit_first_byte_trie(
    lit_strings: &[String],
    cp_var: &syn::Ident,
    _mctx: &mut MonoCtx,
) -> TokenStream {
    // Group literals by first byte.
    let mut groups: std::collections::BTreeMap<u8, Vec<usize>> =
        std::collections::BTreeMap::new();
    for (i, s) in lit_strings.iter().enumerate() {
        if let Some(&b) = s.as_bytes().first() {
            groups.entry(b).or_default().push(i);
        }
    }

    let mut match_arms: Vec<TokenStream> = Vec::new();
    for (byte, indices) in &groups {
        let b_lit = proc_macro2::Literal::byte_character(*byte);
        let mut checks: Vec<TokenStream> = Vec::new();
        for &idx in indices {
            let s = &lit_strings[idx];
            let bytes = s.as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<proc_macro2::Literal> =
                bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
            if len == 1 {
                checks.push(quote! {
                    state.offset += 1;
                    return Some(::parse_that::Span::new(#cp_var, #cp_var + 1, state.src));
                });
            } else {
                checks.push(quote! {
                    let __end = #cp_var + #len;
                    if state.src_bytes.get(#cp_var..__end) == Some(&[#(#byte_lits),*] as &[u8]) {
                        state.offset = __end;
                        return Some(::parse_that::Span::new(#cp_var, __end, state.src));
                    }
                });
            }
        }
        match_arms.push(quote! { #b_lit => { #(#checks)* } });
    }
    match_arms.push(quote! { _ => {} });

    quote! {
        (|| -> Option<::parse_that::Span<'a>> {
            let #cp_var = state.offset;
            let __byte = *state.src_bytes.get(state.offset)?;
            match __byte {
                #(#match_arms,)*
            }
            None
        })()
    }
}

/// Emit sequential inline byte matching for bare-literal sets (<=16).
fn emit_sequential_bare(
    lit_strings: &[String],
    cp_var: &syn::Ident,
) -> TokenStream {
    let mut arms: Vec<TokenStream> = Vec::new();
    for (i, s) in lit_strings.iter().enumerate() {
        let bytes = s.as_bytes();
        let len = bytes.len();
        let byte_lits: Vec<proc_macro2::Literal> =
            bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();

        let check = if len == 1 {
            quote! {
                if state.src_bytes.get(state.offset).copied() == Some(#(#byte_lits)*) {
                    state.offset += 1;
                    return Some(::parse_that::Span::new(#cp_var, #cp_var + 1, state.src));
                }
            }
        } else {
            quote! {
                {
                    let __end = state.offset + #len;
                    if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*] as &[u8]) {
                        state.offset = __end;
                        return Some(::parse_that::Span::new(#cp_var, __end, state.src));
                    }
                }
            }
        };

        if i < lit_strings.len() - 1 {
            arms.push(check);
        } else {
            arms.push(quote! {
                #check
                None
            });
        }
    }

    quote! {
        (|| -> Option<::parse_that::Span<'a>> {
            let #cp_var = state.offset;
            #(#arms)*
        })()
    }
}
