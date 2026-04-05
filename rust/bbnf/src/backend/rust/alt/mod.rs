//! Monolithic Alt emission: dispatch-table match and flat checkpoint chain.

mod key_dispatch;
mod literal;

use bbnf_ir::{AltBranch, FnDescriptor, IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::ir_types::{self, IrCodegenCtx};
use super::{MonoCtx, emit_mono_expr};

use key_dispatch::try_emit_key_dispatch;

/// Result of extracting a literal through a Map wrapper.
/// `lit_sid` is the StringId of the literal; `constant_fn_id` is `Some(FnId)`
/// when the node is `Map(Literal, Expr { constant_expr, .. })`.
#[derive(Clone, Copy)]
struct LitThroughMap {
    lit_sid: bbnf_ir::StringId,
    constant_fn_id: Option<bbnf_ir::FnId>,
}

/// Extract a literal StringId from a node, looking through Map(_, Constant)
/// wrappers. Returns the literal's StringId and the constant value StringId
/// (if wrapped), allowing the caller to emit the constant value directly
/// instead of constructing a Span.
fn extract_literal_through_map(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> Option<LitThroughMap> {
    match node {
        IrNode::Literal(sid) => Some(LitThroughMap {
            lit_sid: *sid,
            constant_fn_id: None,
        }),
        IrNode::Map { inner, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::Expr { expr, .. } if expr.is_constant() => {
                    // Recurse through nested Maps, but flag that a constant expr exists.
                    let inner_info = extract_literal_through_map(inner, ctx)?;
                    // For constant MapExpr, we use the fn_id to look up the constant later.
                    Some(LitThroughMap {
                        lit_sid: inner_info.lit_sid,
                        constant_fn_id: Some(*fn_id),
                    })
                }
                _ => None,
            }
        }
        _ => None,
    }
}

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

    // Check for all-literal (or all Map(Literal, Constant)) -> direct byte matching.
    if let Some(ts) = literal::try_emit_all_literal_alt(branches, ctx, mctx) {
        return ts;
    }

    // Check branch homogeneity for elide_box propagation.
    // Only propagate elide_box when branches are homogeneous in the elide_box
    // context. Heterogeneous branches require sub-variant coercion to BoxedEnum
    // (slab ref), which must match the enum variant type from `project_types`.
    let effective_elide_box = if elide_box {
        let elide_tys: Vec<TypeDesc> = branches
            .iter()
            .map(|b| ctx.vec_elem_type(&b.node))
            .collect();
        elide_tys.windows(2).all(|w| w[0] == w[1])
    } else {
        false
    };

    let branch_tys: Vec<TypeDesc> = if effective_elide_box {
        branches
            .iter()
            .map(|b| ctx.vec_elem_type(&b.node))
            .collect()
    } else {
        branches.iter().map(|b| ctx.node_type(&b.node)).collect()
    };
    let all_same = branch_tys.windows(2).all(|w| w[0] == w[1]);
    let needs_coercion = !all_same;

    // Build a local sub-variant map for THIS specific Alt's branches.
    // Non-Span types are globally unique (validated at compile time), so
    // the precomputed HashMap gives O(1) lookup. For Span types, search
    // the current rule first to pick the correct variant name.
    let local_sub_variants: Vec<Option<String>> = if needs_coercion {
        branch_tys
            .iter()
            .map(|ty| {
                if *ty == TypeDesc::BoxedEnum || *ty == TypeDesc::Enum {
                    return None;
                }
                // For Span types, prefer the current rule's sub-variant
                // (multiple rules may have Span sub-variants with different names).
                if *ty == TypeDesc::Span {
                    if let Some(ref name) = mctx.current_rule_name {
                        if let Some(rule) = ctx.ir.find_rule(name) {
                            for sv in &rule.meta.sub_variants {
                                if sv.ty == *ty {
                                    return Some(ctx.ir.get_string(sv.variant_name).to_string());
                                }
                            }
                        }
                    }
                }
                // O(1) global lookup (first-seen wins; non-Span uniqueness guaranteed).
                ctx.global_sub_variants.get(ty).cloned()
            })
            .collect()
    } else {
        Vec::new()
    };

    if let Some(disp) = dispatch {
        emit_mono_dispatch(
            branches,
            disp,
            &branch_tys,
            needs_coercion,
            &local_sub_variants,
            ctx,
            mctx,
            effective_elide_box,
        )
    } else if let Some(ts) = try_emit_key_dispatch(
        branches,
        &branch_tys,
        needs_coercion,
        &local_sub_variants,
        ctx,
        mctx,
        effective_elide_box,
    ) {
        ts
    } else {
        emit_mono_flat_alt(
            branches,
            &branch_tys,
            needs_coercion,
            &local_sub_variants,
            ctx,
            mctx,
            effective_elide_box,
        )
    }
}

/// Emit a monolithic dispatch-table Alt -- direct `match` on first byte.
#[allow(clippy::too_many_arguments)]
fn emit_mono_dispatch(
    branches: &[AltBranch],
    disp: &bbnf_ir::AltDispatch,
    branch_tys: &[TypeDesc],
    needs_coercion: bool,
    local_sub_variants: &[Option<String>],
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    // Generate the fallback expression once if this dispatch has a fallback branch.
    let fallback_expr = disp.fallback_idx.map(|fb_idx| {
        let fb = &branches[fb_idx as usize];
        let fb_expr = emit_mono_expr(&fb.node, ctx, mctx, elide_box);
        if needs_coercion {
            let sv_name = local_sub_variants
                .get(fb_idx as usize)
                .and_then(|s| s.as_deref());
            if elide_box {
                coerce_mono_branch_by_value(fb_expr, &branch_tys[fb_idx as usize], sv_name, ctx)
            } else {
                coerce_mono_branch(fb_expr, &branch_tys[fb_idx as usize], sv_name, ctx)
            }
        } else {
            fb_expr
        }
    });

    let mut match_arms: Vec<TokenStream> = Vec::new();
    let mut used = vec![false; branches.len()];

    // Mark fallback branch as used so it's not emitted as a normal arm.
    if let Some(fb_idx) = disp.fallback_idx {
        used[fb_idx as usize] = true;
    }

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
        // (e.g., dispatch matches `{` -> object body re-checks `{`).
        mctx.dispatch_guaranteed_byte = if bytes.len() == 1 {
            Some(bytes[0])
        } else {
            None
        };

        let branch_expr = emit_mono_expr(&branch.node, ctx, mctx, elide_box);
        mctx.dispatch_guaranteed_byte = None;

        // Apply coercion if branches are heterogeneous.
        let coerced = if needs_coercion {
            let sv_name = local_sub_variants.get(idx).and_then(|s| s.as_deref());
            if elide_box {
                coerce_mono_branch_by_value(branch_expr, &branch_tys[idx], sv_name, ctx)
            } else {
                coerce_mono_branch(branch_expr, &branch_tys[idx], sv_name, ctx)
            }
        } else {
            branch_expr
        };

        // If there's a fallback, wrap dispatched branches so failures fall through.
        if let Some(ref fb) = fallback_expr {
            match_arms.push(quote! {
                #(#byte_patterns)|* => {
                    let __fb_cp = state.offset;
                    if let Some(__v) = (|| { #coerced })() {
                        Some(__v)
                    } else {
                        state.offset = __fb_cp;
                        #fb
                    }
                },
            });
        } else {
            match_arms.push(quote! { #(#byte_patterns)|* => { #coerced }, });
        }
    }

    // Default arm: fallback branch or None.
    if let Some(ref fb) = fallback_expr {
        match_arms.push(quote! { _ => { #fb }, });
    } else {
        match_arms.push(quote! { _ => None, });
    }

    quote! {
        {
            let __byte = *state.src_bytes.get(state.offset)?;
            match __byte {
                #(#match_arms)*
            }
        }
    }
}

/// Emit a monolithic flat Alt -- checkpoint/restore chain.
fn emit_mono_flat_alt(
    branches: &[AltBranch],
    branch_tys: &[TypeDesc],
    needs_coercion: bool,
    local_sub_variants: &[Option<String>],
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    let cp_var = mctx.fresh("alt_cp");
    // Return type must match what branches actually produce.
    // - Homogeneous BoxedEnum -> &'a Enum (boxed_enum_type)
    // - Homogeneous Enum -> Enum (enum_type)
    // - Heterogeneous + elide_box -> Enum (enum_type, by value)
    // - Heterogeneous + !elide_box -> &'a Enum (boxed_enum_type, slab ref)
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
            let sv_name = local_sub_variants.get(i).and_then(|s| s.as_deref());
            if elide_box {
                coerce_mono_branch_by_value(branch_expr, &branch_tys[i], sv_name, ctx)
            } else {
                coerce_mono_branch(branch_expr, &branch_tys[i], sv_name, ctx)
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

fn coerce_mono_branch_by_value(
    expr: TokenStream,
    branch_ty: &TypeDesc,
    variant_name: Option<&str>,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    if *branch_ty == TypeDesc::Enum {
        return expr;
    }

    let enum_ident = &ctx.enum_ident;

    if let Some(name) = variant_name {
        let variant_ident = format_ident!("{}", name);
        quote! { #expr.map(|__x| #enum_ident::#variant_ident(__x)) }
    } else {
        expr
    }
}

/// All branches must produce the same type. BoxedEnum branches pass through.
/// Enum branches get boxed/alloc'd. Sub-variant branches get wrapped + boxed/alloc'd.
fn coerce_mono_branch(
    expr: TokenStream,
    branch_ty: &TypeDesc,
    variant_name: Option<&str>,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    // Already BoxedEnum (&Enum / Box<Enum>) -- no coercion needed.
    if *branch_ty == TypeDesc::BoxedEnum {
        return expr;
    }

    let enum_ident = &ctx.enum_ident;

    if let Some(variant_name) = variant_name {
        let variant_ident = format_ident!("{}", variant_name);
        let alloc_expr = quote! { #enum_ident::#variant_ident(__x) };
        let alloc_code = ctx.emit_alloc_let(&alloc_expr);
        quote! {
            #expr.map(|__x| {
                #alloc_code
            })
        }
    } else if *branch_ty == TypeDesc::Enum {
        // Enum (Enum) -> boxed/alloc'd -> BoxedEnum.
        let inner = quote! { __x };
        let alloc_code = ctx.emit_alloc_let(&inner);
        quote! {
            #expr.map(|__x| {
                #alloc_code
            })
        }
    } else if *branch_ty == TypeDesc::Span {
        // Span without sub-variant -- boxed/alloc'd directly.
        // (Shouldn't happen in practice; IR always generates sub-variants
        // for heterogeneous Alt branches.)
        let inner = quote! { __x };
        let alloc_code = ctx.emit_alloc_let(&inner);
        quote! {
            #expr.map(|__x| {
                #alloc_code
            })
        }
    } else {
        expr
    }
}
