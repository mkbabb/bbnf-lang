//! Monolithic Alt emission: dispatch-table match and flat checkpoint chain.

use bbnf_ir::{AltBranch, IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::ir_types::{self, IrCodegenCtx};
use super::super::infer::{infer_node_type, infer_node_type_elide_box};
use super::super::unescape_literal;
use super::{emit_mono_expr, MonoCtx};

/// Emit a monolithic Alt.
pub(super) fn emit_mono_alt(
    branches: &[AltBranch],
    dispatch: Option<&bbnf_ir::AltDispatch>,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    if branches.is_empty() {
        return quote! { None };
    }
    if branches.len() == 1 {
        return emit_mono_expr(&branches[0].node, ctx, mctx, elide_box);
    }

    // Check for all-literal → direct byte matching (small sets) or any_span (large sets).
    let all_literal = branches
        .iter()
        .all(|b| matches!(&b.node, IrNode::Literal(_)));
    if all_literal {
        let lit_strings: Vec<String> = branches
            .iter()
            .map(|b| {
                let IrNode::Literal(sid) = &b.node else {
                    unreachable!()
                };
                let raw = ctx.ir.get_string(*sid);
                unescape_literal(raw)
            })
            .collect();

        // For small sets (≤8 literals), emit a checkpoint + sequential byte comparison
        // chain instead of building an Aho-Corasick automaton at runtime.
        if lit_strings.len() <= 8 {
            let cp_var = mctx.fresh("lit_cp");
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
                            return Some(::parse_that::Span::new(#cp_var, state.offset, state.src));
                        }
                    }
                } else {
                    quote! {
                        {
                            let __end = state.offset + #len;
                            if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*]) {
                                state.offset = __end;
                                return Some(::parse_that::Span::new(#cp_var, __end, state.src));
                            }
                        }
                    }
                };

                if i < lit_strings.len() - 1 {
                    arms.push(check);
                } else {
                    // Last arm: no need for early return, just return None on failure.
                    arms.push(quote! {
                        #check
                        None
                    });
                }
            }

            return quote! {
                (|| -> Option<::parse_that::Span<'a>> {
                    let #cp_var = state.offset;
                    #(#arms)*
                })()
            };
        }

        // Large sets: fall back to any_span (Aho-Corasick).
        let lits: Vec<proc_macro2::Literal> = lit_strings
            .iter()
            .map(|s| proc_macro2::Literal::string(s))
            .collect();
        let name = mctx.hoist(quote! { ::parse_that::any_span(&[#(#lits),*]) });
        return quote! { #name.call(state) };
    }

    // Check branch homogeneity for elide_box propagation.
    // When elide_box=true (from Vec/Repeat parent), propagate it even for
    // heterogeneous branches — coerce to Enum (by value) instead of BoxedEnum
    // (arena ref). This eliminates arena allocations in Vec contexts.
    let effective_elide_box = elide_box;

    let branch_tys: Vec<TypeDesc> = if effective_elide_box {
        branches
            .iter()
            .map(|b| infer_node_type_elide_box(&b.node, ctx))
            .collect()
    } else {
        branches
            .iter()
            .map(|b| infer_node_type(&b.node, ctx))
            .collect()
    };
    let all_same = branch_tys.windows(2).all(|w| w[0] == w[1]);
    let needs_coercion = !all_same;

    if let Some(disp) = dispatch {
        emit_mono_dispatch(
            branches,
            disp,
            &branch_tys,
            needs_coercion,
            ctx,
            mctx,
            effective_elide_box,
        )
    } else {
        emit_mono_flat_alt(
            branches,
            &branch_tys,
            needs_coercion,
            ctx,
            mctx,
            effective_elide_box,
        )
    }
}

/// Emit a monolithic dispatch-table Alt — direct `match` on first byte.
fn emit_mono_dispatch(
    branches: &[AltBranch],
    disp: &bbnf_ir::AltDispatch,
    branch_tys: &[TypeDesc],
    needs_coercion: bool,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    let mut match_arms: Vec<TokenStream> = Vec::new();
    let mut used = vec![false; branches.len()];

    for (idx, branch) in branches.iter().enumerate() {
        if used[idx] {
            continue;
        }
        used[idx] = true;
        let bytes: Vec<u8> = (0u8..128)
            .filter(|&c| disp.table.get(c as usize).copied() == Some(idx as u8))
            .collect();
        if bytes.is_empty() {
            continue;
        }

        let byte_patterns: Vec<TokenStream> = bytes
            .iter()
            .map(|&b| {
                let b_lit = proc_macro2::Literal::byte_character(b);
                quote! { #b_lit }
            })
            .collect();

        // Dispatch guaranteed byte: when the arm matches exactly one byte,
        // the next single-byte literal check for that byte can skip bounds checking.
        // This eliminates the redundant open-delimiter check in wrap patterns
        // (e.g., dispatch matches `{` → object body re-checks `{`).
        mctx.dispatch_guaranteed_byte = if bytes.len() == 1 {
            Some(bytes[0])
        } else {
            None
        };

        let branch_expr = emit_mono_expr(&branch.node, ctx, mctx, elide_box);
        mctx.dispatch_guaranteed_byte = None;

        // Apply coercion if branches are heterogeneous.
        let coerced = if needs_coercion {
            if elide_box {
                coerce_mono_branch_by_value(branch_expr, &branch_tys[idx], ctx)
            } else {
                coerce_mono_branch(branch_expr, &branch_tys[idx], ctx)
            }
        } else {
            branch_expr
        };

        match_arms.push(quote! { #(#byte_patterns)|* => { #coerced }, });
    }

    match_arms.push(quote! { _ => None, });

    quote! {
        {
            let __byte = *state.src_bytes.get(state.offset)?;
            match __byte {
                #(#match_arms)*
            }
        }
    }
}

/// Emit a monolithic flat Alt — checkpoint/restore chain.
fn emit_mono_flat_alt(
    branches: &[AltBranch],
    branch_tys: &[TypeDesc],
    needs_coercion: bool,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    let cp_var = mctx.fresh("alt_cp");
    // Return type must match what branches actually produce.
    // - Homogeneous BoxedEnum → &'a ArenaEnum (boxed_enum_type)
    // - Homogeneous Enum → ArenaEnum (enum_type)
    // - Heterogeneous + elide_box → ArenaEnum (enum_type, by value)
    // - Heterogeneous + !elide_box → &'a ArenaEnum (boxed_enum_type, arena ref)
    let return_type = if needs_coercion {
        if elide_box {
            ctx.enum_type.clone()
        } else {
            ctx.boxed_enum_type.clone()
        }
    } else if branch_tys[0] == TypeDesc::BoxedEnum {
        ctx.boxed_enum_type.clone()
    } else {
        ir_types::type_desc_to_syn(&branch_tys[0], ctx)
    };

    let mut arms: Vec<TokenStream> = Vec::new();
    for (i, branch) in branches.iter().enumerate() {
        let branch_expr = emit_mono_expr(&branch.node, ctx, mctx, elide_box);
        let coerced = if needs_coercion {
            if elide_box {
                coerce_mono_branch_by_value(branch_expr, &branch_tys[i], ctx)
            } else {
                coerce_mono_branch(branch_expr, &branch_tys[i], ctx)
            }
        } else {
            branch_expr
        };

        if i < branches.len() - 1 {
            arms.push(quote! {
                if let Some(__v) = (|| -> Option<_> { #coerced })() {
                    return Some(__v);
                }
                state.furthest_offset = state.furthest_offset.max(state.offset);
                state.offset = #cp_var;
            });
        } else {
            arms.push(quote! { (|| -> Option<_> { #coerced })() });
        }
    }

    quote! {
        (|| -> Option<#return_type> {
            let #cp_var = state.offset;
            #(#arms)*
        })()
    }
}

/// Coerce a branch expression to Enum (`ArenaEnum`) by value for heterogeneous Alt
/// in elide_box context. No arena allocation — Span branches get wrapped in a
/// sub-variant enum constructor, Enum branches pass through.
fn coerce_mono_branch_by_value(
    expr: TokenStream,
    branch_ty: &TypeDesc,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    if *branch_ty == TypeDesc::Enum {
        return expr;
    }

    let enum_ident = &ctx.enum_ident;

    let all_sub_variants: Vec<(&str, &TypeDesc)> = ctx
        .ir
        .rules
        .iter()
        .flat_map(|r| {
            r.meta.sub_variants.iter().map(|sv| {
                let name = ctx.ir.get_string(sv.variant_name);
                (name, &sv.ty)
            })
        })
        .collect();

    if let Some((variant_name, _)) = all_sub_variants
        .iter()
        .find(|(_, vty)| **vty == *branch_ty)
    {
        let variant_ident = format_ident!("{}", variant_name);
        quote! { #expr.map(|__x| #enum_ident::#variant_ident(__x)) }
    } else {
        // No sub-variant match — wrap directly (shouldn't happen in practice).
        expr
    }
}

/// Coerce a branch expression to BoxedEnum (`&'a ArenaEnum`) for heterogeneous Alt.
///
/// All branches must produce the same type. BoxedEnum branches pass through.
/// Enum branches get arena.alloc'd. Sub-variant branches get wrapped + alloc'd.
fn coerce_mono_branch(
    expr: TokenStream,
    branch_ty: &TypeDesc,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    // Already BoxedEnum (&ArenaEnum) — no coercion needed.
    if *branch_ty == TypeDesc::BoxedEnum {
        return expr;
    }

    let enum_ident = &ctx.enum_ident;
    let helper = ctx.arena_helper_ident();

    // Look for a matching sub-variant (Span/other non-Enum types from
    // heterogeneous branches get wrapped in a sub-variant + arena.alloc).
    let all_sub_variants: Vec<(&str, &TypeDesc)> = ctx
        .ir
        .rules
        .iter()
        .flat_map(|r| {
            r.meta.sub_variants.iter().map(|sv| {
                let name = ctx.ir.get_string(sv.variant_name);
                (name, &sv.ty)
            })
        })
        .collect();

    if let Some((variant_name, _)) = all_sub_variants
        .iter()
        .find(|(_, vty)| **vty == *branch_ty)
    {
        let variant_ident = format_ident!("{}", variant_name);
        quote! {
            #expr.map(|__x| {
                let __alloc = #helper(state).alloc(#enum_ident::#variant_ident(__x));
                &*__alloc
            })
        }
    } else if *branch_ty == TypeDesc::Enum {
        // Enum (ArenaEnum) → arena.alloc → &ArenaEnum (BoxedEnum).
        quote! {
            #expr.map(|__x| {
                let __alloc = #helper(state).alloc(__x);
                &*__alloc
            })
        }
    } else if *branch_ty == TypeDesc::Span {
        // Span without sub-variant — arena.alloc directly.
        // (Shouldn't happen in practice; IR always generates sub-variants
        // for heterogeneous Alt branches.)
        quote! {
            #expr.map(|__x| {
                let __alloc = #helper(state).alloc(__x);
                &*__alloc
            })
        }
    } else {
        expr
    }
}
