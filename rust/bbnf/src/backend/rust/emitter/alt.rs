//! Alternation emission for the shared-driver Rust emitter.

use bbnf_ir::{AltDispatch, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::key_dispatch::{KeyClass, KeyDispatchConfig};
use crate::backend::{AllocStrategy, AltBranchInfo, KeyDispatchBranch};

use super::RustEmitter;
use super::RustEmitCtx;

/// Check if Alt branches are heterogeneous (mixed types). If so, each non-Enum
/// branch must be coerced to BoxedEnum via sub-variant wrapping + slab alloc.
fn needs_coercion(branches: &[(AltBranchInfo, TokenStream)], fallback: Option<&(AltBranchInfo, TokenStream)>) -> bool {
    let all_types: Vec<&TypeDesc> = branches
        .iter()
        .map(|(info, _)| &info.ty)
        .chain(fallback.iter().map(|(info, _)| &info.ty))
        .collect();
    if all_types.len() <= 1 {
        return false;
    }
    all_types.iter().any(|t| **t != *all_types[0])
}

/// Coerce a branch body to the common Alt result type (BoxedEnum).
/// Heterogeneous Alts ALWAYS produce BoxedEnum — every branch gets
/// sub-variant wrapped + slab allocated, matching the monolithic path's
/// `effective_elide_box=false` for mixed-type Alts.
fn coerce_branch(
    info: &AltBranchInfo,
    body: &TokenStream,
    ctx: &RustEmitCtx,
    enum_ident: &syn::Ident,
) -> TokenStream {
    // If the branch already produces BoxedEnum (slab ref), no coercion needed.
    if info.ty == TypeDesc::BoxedEnum {
        return body.clone();
    }

    let ir_ctx = ctx.ir_ctx();

    // If the branch produces Enum (unboxed), slab-allocate it.
    if info.ty == TypeDesc::Enum {
        let alloc_expr = ir_ctx.emit_alloc(&quote! { __coerce_v });
        return quote! { (#body).map(|__coerce_v| #alloc_expr) };
    }

    // Branch produces a simpler type (Span, Tuple, etc.) — wrap in sub-variant + alloc.
    // Try exact match first, then normalized (BoxedEnum→Enum in nested positions).
    let normalized = normalize_type(&info.ty);
    if let Some(variant_name) = ir_ctx.global_sub_variants.get(&info.ty)
        .or_else(|| ir_ctx.global_sub_variants.get(&normalized))
    {
        let variant = format_ident!("{}", variant_name);
        let alloc_expr = ir_ctx.emit_alloc(&quote! { #enum_ident::#variant(__sv) });
        quote! { (#body).map(|__sv| #alloc_expr) }
    } else {
        // No sub-variant — just slab-allocate the value as-is.
        let alloc_expr = ir_ctx.emit_alloc(&quote! { __coerce_v });
        quote! { (#body).map(|__coerce_v| #alloc_expr) }
    }
}

/// Normalize BoxedEnum → Enum in nested type positions for sub-variant lookup.
fn normalize_type(ty: &TypeDesc) -> TypeDesc {
    match ty {
        TypeDesc::BoxedEnum => TypeDesc::Enum,
        TypeDesc::Tuple(elems) => TypeDesc::Tuple(elems.iter().map(normalize_type).collect()),
        TypeDesc::Vec(inner) => TypeDesc::Vec(Box::new(normalize_type(inner))),
        TypeDesc::Option(inner) => TypeDesc::Option(Box::new(normalize_type(inner))),
        other => other.clone(),
    }
}

impl RustEmitter {
    pub(super) fn emit_alt_dispatch_impl(
        &mut self,
        table: &AltDispatch,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let do_coerce = needs_coercion(&branches, fallback.as_ref());

        let mut arms = Vec::new();

        for (branch_idx, (info, body)) in branches.iter().enumerate() {
            let byte_patterns: Vec<u8> = table
                .table
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b as usize == branch_idx)
                .map(|(byte_val, _)| byte_val as u8)
                .collect();

            if byte_patterns.is_empty() {
                continue;
            }

            let patterns: Vec<_> = byte_patterns.iter().map(|b| quote! { #b }).collect();
            let coerced = if do_coerce {
                coerce_branch(info, body, ctx, &self.enum_ident)
            } else {
                body.clone()
            };
            arms.push(quote! {
                #( #patterns )|* => { #coerced }
            });
        }

        let fallback_expr = if let Some((info, fb_body)) = fallback {
            let coerced = if do_coerce {
                coerce_branch(&info, &fb_body, ctx, &self.enum_ident)
            } else {
                fb_body
            };
            quote! { _ => { #coerced } }
        } else {
            quote! { _ => None }
        };
        arms.push(fallback_expr);

        quote! {
            if state.offset < state.src.len() {
                match state.src.as_bytes()[state.offset] {
                    #( #arms ),*
                }
            } else {
                None
            }
        }
    }

    pub(super) fn emit_alt_checkpoint_impl(
        &mut self,
        branches: Vec<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        if branches.len() == 1 {
            let (_, body) = &branches[0];
            return body.clone();
        }

        let do_coerce = needs_coercion(&branches, None);

        let mut chain = Vec::new();
        for (info, body) in &branches {
            let coerced = if do_coerce {
                coerce_branch(info, body, ctx, &self.enum_ident)
            } else {
                body.clone()
            };
            chain.push(quote! {
                {
                    let __cp = state.offset;
                    let __result = #coerced;
                    if __result.is_some() {
                        return __result;
                    }
                    state.offset = __cp;
                }
            });
        }

        quote! {
            (|| {
                #( #chain )*
                None
            })()
        }
    }

    pub(super) fn emit_alt_all_literal_impl(
        &mut self,
        literals: Vec<(String, TokenStream)>,
        _alloc: AllocStrategy,
        _ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let mut chain = Vec::new();
        for (_value, body) in &literals {
            chain.push(quote! {
                let __r = #body;
                if __r.is_some() { return __r; }
            });
        }
        quote! {
            (|| {
                #( #chain )*
                None
            })()
        }
    }

    pub(super) fn emit_key_dispatch_impl(
        &mut self,
        config: &KeyDispatchConfig,
        branches: Vec<KeyDispatchBranch<TokenStream>>,
        fallback: Option<(AltBranchInfo, TokenStream)>,
        _alloc: AllocStrategy,
        ctx: &mut RustEmitCtx,
    ) -> TokenStream {
        let cp = ctx.fresh("kd_cp");
        let scanner = match config.key_class {
            KeyClass::Identifier => quote! { ::parse_that::scan_ident(state) },
            KeyClass::QuotedString { .. } => quote! { ::parse_that::scan_string_quoted(state) },
        };
        let arm_checks: Vec<TokenStream> = branches
            .into_iter()
            .map(|kd| {
                let comparisons: Vec<TokenStream> = kd
                    .key_bytes
                    .iter()
                    .map(|key| {
                        let byte_lits: Vec<proc_macro2::Literal> =
                            key.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
                        let len = key.len();
                        quote! { (__kd_len == #len && __kd_bytes == &[#(#byte_lits),*]) }
                    })
                    .collect();
                let body = kd.body;
                quote! {
                    if #(#comparisons)||* {
                        state.offset = #cp;
                        return #body;
                    }
                }
            })
            .collect();
        let fallback_expr = if let Some((_, fb)) = fallback {
            fb
        } else {
            quote! { None }
        };
        quote! {
            {
                let #cp = state.offset;
                if let Some(ref __kd_s) = #scanner {
                    let __kd_bytes = &state.src_bytes[__kd_s.start..__kd_s.end];
                    let __kd_len = __kd_bytes.len();
                    #(#arm_checks)*
                }
                state.offset = #cp;
                #fallback_expr
            }
        }
    }
}
