//! Monolithic Alt emission: dispatch-table match and flat checkpoint chain.

use bbnf_ir::{AltBranch, FnDescriptor, IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::ir_types::{self, IrCodegenCtx};
use super::super::infer::{infer_node_type, infer_node_type_elide_box};
use super::super::unescape_literal;
use super::{emit_mono_expr, MonoCtx};

/// Result of extracting a literal through a Map wrapper.
/// `lit_sid` is the StringId of the literal; `constant_value` is `Some(StringId)`
/// when the node is `Map(Literal, Constant { value, .. })`.
#[derive(Clone, Copy)]
struct LitThroughMap {
    lit_sid: bbnf_ir::StringId,
    constant_value: Option<bbnf_ir::StringId>,
}

/// Extract a literal StringId from a node, looking through Map(_, Constant)
/// wrappers. Returns the literal's StringId and the constant value StringId
/// (if wrapped), allowing the caller to emit the constant value directly
/// instead of constructing a Span.
fn extract_literal_through_map(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
) -> Option<LitThroughMap> {
    match node {
        IrNode::Literal(sid) => Some(LitThroughMap {
            lit_sid: *sid,
            constant_value: None,
        }),
        IrNode::Map { inner, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::Constant { value, .. } => {
                    // Recurse through nested Maps, but attach the outermost constant value.
                    let inner_info = extract_literal_through_map(inner, ctx)?;
                    Some(LitThroughMap {
                        lit_sid: inner_info.lit_sid,
                        constant_value: Some(*value),
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

    // Check for all-literal (or all Map(Literal, Constant)) → direct byte matching.
    // Extract (literal_string, optional_constant_value_sid) for each branch.
    let lit_infos: Vec<Option<LitThroughMap>> = branches
        .iter()
        .map(|b| extract_literal_through_map(&b.node, ctx))
        .collect();

    let all_literal_like = lit_infos.iter().all(|x| x.is_some());
    if all_literal_like {
        let entries: Vec<LitThroughMap> = lit_infos.into_iter().map(|x| x.unwrap()).collect();

        let any_mapped = entries.iter().any(|e| e.constant_value.is_some());
        let all_bare = !any_mapped;

        // All-bare-literal path: Span return (any_span for large sets, sequential for small).
        // All-mapped-literal path: constant return values via sequential byte comparison.
        // Mixed (some bare, some mapped) also uses sequential when ≤ threshold.

        // Sequential byte comparison threshold: for bare literals use 8 (above that,
        // any_span with Aho-Corasick is faster). For mapped literals, sequential is the
        // only option since each branch has a unique return value.
        let use_sequential = if any_mapped {
            true // No any_span alternative for mapped branches.
        } else {
            entries.len() <= 8
        };

        if use_sequential {
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
                    // Bare literal — return Span.
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

            return quote! {
                (|| -> Option<#return_type> {
                    let #cp_var = state.offset;
                    #(#arms)*
                })()
            };
        }

        // Large all-bare sets: inline sequential byte matching (no combinator).
        // Even for large sets (>8), sequential inline byte comparison avoids
        // SpanParser/any_span combinator overhead in the monolithic path.
        debug_assert!(all_bare, "mapped literals should always use sequential path");
        {
            let cp_var = mctx.fresh("lit_cp");
            let mut arms: Vec<TokenStream> = Vec::new();
            for (i, info) in entries.iter().enumerate() {
                let raw = ctx.ir.get_string(info.lit_sid);
                let s = unescape_literal(raw);
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

                if i < entries.len() - 1 {
                    arms.push(check);
                } else {
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
    }

    // Check branch homogeneity for elide_box propagation.
    // Only propagate elide_box when branches are homogeneous in the elide_box
    // context. Heterogeneous branches require sub-variant coercion to BoxedEnum
    // (arena ref), which must match the enum variant type from `infer_types`.
    let effective_elide_box = if elide_box {
        let elide_tys: Vec<TypeDesc> = branches
            .iter()
            .map(|b| infer_node_type_elide_box(&b.node, ctx))
            .collect();
        elide_tys.windows(2).all(|w| w[0] == w[1])
    } else {
        false
    };

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

    // Build a local sub-variant map for THIS specific Alt's branches.
    // This avoids the global lookup ambiguity when two rules produce
    // structurally identical sub-variant types.
    let local_sub_variants: Vec<Option<String>> = if needs_coercion {
        branch_tys
            .iter()
            .map(|ty| {
                if *ty == TypeDesc::BoxedEnum || *ty == TypeDesc::Enum {
                    return None;
                }
                // Search the current rule first (handles non-fused bodies).
                if let Some(ref name) = mctx.current_rule_name {
                    if let Some(rule) = ctx.ir.find_rule(name) {
                        for sv in &rule.meta.sub_variants {
                            if sv.ty == *ty {
                                return Some(ctx.ir.get_string(sv.variant_name).to_string());
                            }
                        }
                    }
                }
                // Fallback: search all rules (handles fused/inlined bodies).
                for r in &ctx.ir.rules {
                    for sv in &r.meta.sub_variants {
                        if sv.ty == *ty {
                            return Some(ctx.ir.get_string(sv.variant_name).to_string());
                        }
                    }
                }
                None
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

/// Emit a monolithic dispatch-table Alt — direct `match` on first byte.
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
            let sv_name = local_sub_variants.get(idx).and_then(|s| s.as_deref());
            if elide_box {
                coerce_mono_branch_by_value(branch_expr, &branch_tys[idx], sv_name, ctx)
            } else {
                coerce_mono_branch(branch_expr, &branch_tys[idx], sv_name, ctx)
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
    local_sub_variants: &[Option<String>],
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

/// Coerce a branch expression to Enum (`ArenaEnum`) by value for heterogeneous Alt
/// in elide_box context. No arena allocation — Span branches get wrapped in a
/// sub-variant enum constructor, Enum branches pass through.
/// Public wrapper for token_dispatch module.
pub(super) fn coerce_mono_branch_by_value_pub(
    expr: TokenStream,
    branch_ty: &TypeDesc,
    variant_name: Option<&str>,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    coerce_mono_branch_by_value(expr, branch_ty, variant_name, ctx)
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

/// Coerce a branch expression to BoxedEnum (`&'a ArenaEnum`) for heterogeneous Alt.
///
/// Public wrapper for token_dispatch module.
pub(super) fn coerce_mono_branch_pub(
    expr: TokenStream,
    branch_ty: &TypeDesc,
    variant_name: Option<&str>,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    coerce_mono_branch(expr, branch_ty, variant_name, ctx)
}

/// All branches must produce the same type. BoxedEnum branches pass through.
/// Enum branches get arena.alloc'd. Sub-variant branches get wrapped + alloc'd.
fn coerce_mono_branch(
    expr: TokenStream,
    branch_ty: &TypeDesc,
    variant_name: Option<&str>,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    // Already BoxedEnum (&ArenaEnum) — no coercion needed.
    if *branch_ty == TypeDesc::BoxedEnum {
        return expr;
    }

    let enum_ident = &ctx.enum_ident;
    let helper = ctx.arena_helper_ident();

    if let Some(variant_name) = variant_name {
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
