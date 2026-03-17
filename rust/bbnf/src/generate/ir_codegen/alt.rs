//! Alt (alternation) emission.
//!
//! Dispatch tables, sub-variant coercion, SpanParser fast-paths, flat alternation.

use bbnf_ir::{AltBranch, FnDescriptor, IrNode, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::ir_types::{type_is_span, IrCodegenCtx};
use super::infer::{infer_node_type, infer_node_type_in_vec};
use super::unescape_literal;
use super::{ir_node_to_tokens, ir_node_to_tokens_vec};

/// Emit an Alt (alternation) expression.
///
/// Uses dispatch table when available, sub-variant coercion for heterogeneous branches,
/// any_span for all-literal branches, and inline flat alternation otherwise.
pub fn emit_alt(
    branches: &[AltBranch],
    dispatch: Option<&bbnf_ir::AltDispatch>,
    ctx: &IrCodegenCtx<'_>,
    in_vec: bool,
) -> TokenStream {
    if branches.is_empty() {
        return quote! { ::parse_that::epsilon().map(|_| unreachable!()) };
    }
    if branches.len() == 1 {
        // Single branch: can propagate in_vec since no coercion needed.
        return ir_node_to_tokens_vec(&branches[0].node, ctx, in_vec);
    }

    // Check for all-literal → any_span fast path.
    let all_literal = branches
        .iter()
        .all(|b| matches!(&b.node, IrNode::Literal(_)));
    if all_literal {
        let lits: Vec<proc_macro2::Literal> = branches
            .iter()
            .map(|b| {
                let IrNode::Literal(sid) = &b.node else {
                    unreachable!()
                };
                let raw = ctx.ir.get_string(*sid);
                let unescaped = unescape_literal(raw);
                proc_macro2::Literal::string(&unescaped)
            })
            .collect();
        return quote! { ::parse_that::any_span(&[#(#lits),*]) };
    }

    // Compute branch types with in_vec context to check homogeneity.
    let branch_tys_vec: Vec<TypeDesc> = branches
        .iter()
        .map(|b| infer_node_type_in_vec(&b.node, ctx))
        .collect();
    let all_same_vec = branch_tys_vec.windows(2).all(|w| w[0] == w[1]);

    // Only propagate in_vec if branches are homogeneous WITH in_vec.
    // Heterogeneous alts need coercion to BoxedEnum, which defeats in_vec.
    let effective_in_vec = in_vec && all_same_vec;

    let branch_tys: Vec<TypeDesc> = if effective_in_vec {
        branch_tys_vec
    } else {
        branches
            .iter()
            .map(|b| infer_node_type(&b.node, ctx))
            .collect()
    };
    let all_same = branch_tys.windows(2).all(|w| w[0] == w[1]);
    let overall_is_boxed_enum = !all_same;

    let parsers: Vec<TokenStream> = branches
        .iter()
        .map(|b| ir_node_to_tokens_vec(&b.node, ctx, effective_in_vec))
        .collect();

    // Coerce branches if heterogeneous.
    let coerced = if overall_is_boxed_enum {
        coerce_branches(&parsers, &branch_tys, ctx)
    } else {
        parsers.clone()
    };

    // Dispatch table (O(1) byte dispatch).
    if let Some(disp) = dispatch {
        return emit_dispatch(&coerced, disp, overall_is_boxed_enum, branches, ctx);
    }

    // Flat alternation: for ≤8 branches, emit inline closure.
    if coerced.len() <= 8 {
        let bindings: Vec<TokenStream> = coerced
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let ident = format_ident!("_alt_{}", i);
                quote! { let #ident = #p; }
            })
            .collect();
        let arms: Vec<TokenStream> = (0..coerced.len())
            .map(|i| {
                let ident = format_ident!("_alt_{}", i);
                if i < coerced.len() - 1 {
                    quote! {
                        if let Some(v) = #ident.call(state) { return Some(v); }
                        state.furthest_offset = state.furthest_offset.max(state.offset);
                        state.offset = cp;
                    }
                } else {
                    quote! { #ident.call(state) }
                }
            })
            .collect();
        return quote! {
            {
                #(#bindings)*
                ::parse_that::Parser::new(move |state: &mut ::parse_that::ParserState<'a>| {
                    let cp = state.offset;
                    #(#arms)*
                })
            }
        };
    }

    // >8 branches: one_of(vec![...]).
    quote! { ::parse_that::one_of(vec![#(#coerced),*]) }
}

/// Emit dispatch table match expression.
fn emit_dispatch(
    coerced: &[TokenStream],
    disp: &bbnf_ir::AltDispatch,
    overall_is_boxed_enum: bool,
    branches: &[AltBranch],
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    let mut branch_bindings: Vec<TokenStream> = Vec::new();
    let mut match_arms: Vec<TokenStream> = Vec::new();
    let mut used = vec![false; coerced.len()];

    for (idx, parser) in coerced.iter().enumerate() {
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

        // SpanParser fast-path for dispatch branches.
        if overall_is_boxed_enum {
            if let Some(sp_info) = try_sp_dispatch_branch(&branches[idx].node, ctx) {
                let (sp_constructor, map_fn) = sp_info;
                let sp_binding = format_ident!("_sp_{}", idx);
                branch_bindings.push(quote! { let #sp_binding = #sp_constructor; });
                let call = quote! { #sp_binding.call(state).map(#map_fn).map(Box::new) };
                match_arms.push(quote! { #(#byte_patterns)|* => { #call }, });
                continue;
            }
        }

        let branch_ident = format_ident!("_branch_{}", idx);
        branch_bindings.push(quote! { let #branch_ident = #parser; });
        match_arms.push(quote! { #(#byte_patterns)|* => #branch_ident.call(state), });
    }

    match_arms.push(quote! { _ => None, });

    quote! {
        {
            #(#branch_bindings)*
            ::parse_that::Parser::new(move |state: &mut ::parse_that::ParserState<'a>| {
                let byte = *state.src_bytes.get(state.offset)?;
                match byte {
                    #(#match_arms)*
                }
            })
        }
    }
}

/// Check if a branch node can use a SpanParser fast-path in dispatch.
/// Returns (sp_constructor, map_fn) if so.
///
/// Handles three cases:
/// 1. `Map { inner: Ref(id), fn_id }` — rule has _sp method
/// 2. `Ref(id)` — rule has _sp method and body type is Span
/// 3. `Map { inner: <inlined node>, fn_id }` — after inline_acyclic, Ref was replaced
///    with the inlined body (e.g., Regex, Literal, nested Alt). Try generating a
///    SpanParser expression directly via `try_ir_span_expr`.
/// 4. Bare inlined nodes — try `try_ir_span_expr` directly (identity map).
fn try_sp_dispatch_branch(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
) -> Option<(TokenStream, TokenStream)> {
    match node {
        IrNode::Map { inner, fn_id } => {
            // Case 1: Map { inner: Ref(id) } — original (non-inlined) path.
            if let IrNode::Ref(id) = inner.as_ref() {
                let rule = &ctx.ir.rules[*id as usize];
                let name = ctx.ir.get_string(rule.name);
                if ctx.has_sp_method(name) && !rule.meta.is_transparent {
                    let sp_ident = format_ident!("{}_sp", name);
                    let fd = &ctx.ir.fns[*fn_id as usize];
                    let map_fn = match fd {
                        FnDescriptor::EnumWrap { variant } => {
                            let vname = ctx.ir.get_string(*variant);
                            let vident = format_ident!("{}", vname);
                            let enum_ident = &ctx.enum_ident;
                            quote! { |x| #enum_ident::#vident(x) }
                        }
                        _ => return None,
                    };
                    return Some((quote! { Self::#sp_ident() }, map_fn));
                }
            }

            // Case 3: Map { inner: <inlined node> } — after inline_acyclic, the
            // inner Ref was replaced with the rule body (Regex, Literal, Alt, etc.).
            // Try generating a SpanParser expression for the inlined body.
            let sp_expr = super::super::ir_span::try_ir_span_expr(inner, ctx)?;
            let fd = &ctx.ir.fns[*fn_id as usize];
            let map_fn = match fd {
                FnDescriptor::EnumWrap { variant } => {
                    let vname = ctx.ir.get_string(*variant);
                    let vident = format_ident!("{}", vname);
                    let enum_ident = &ctx.enum_ident;
                    quote! { |x| #enum_ident::#vident(x) }
                }
                _ => return None,
            };
            Some((sp_expr, map_fn))
        }
        IrNode::Ref(id) => {
            // Case 2: bare Ref — rule has _sp method and body type is Span.
            let rule = &ctx.ir.rules[*id as usize];
            let name = ctx.ir.get_string(rule.name);
            if ctx.has_sp_method(name) && !rule.meta.is_transparent {
                let body_ty = ctx.rule_types.get(&rule.id);
                let is_span_body = body_ty.is_some_and(|ty| type_is_span(ty));
                if is_span_body {
                    let sp_ident = format_ident!("{}_sp", name);
                    let enum_ident = &ctx.enum_ident;
                    let variant_ident = format_ident!("{}", name);
                    return Some((
                        quote! { Self::#sp_ident() },
                        quote! { |x| #enum_ident::#variant_ident(x) },
                    ));
                }
            }
            None
        }
        // Case 4: Bare inlined nodes without Map wrapper.
        // After inline_acyclic, if the branch node doesn't have a Map wrapper but
        // can be expressed as a SpanParser, emit it directly. The coercion wrapper
        // (Box::new / enum variant) is already handled by `coerce_branches` in the
        // caller, so we only need an identity map here. This applies when the
        // alternation context uses sub-variant coercion for this branch.
        other => {
            let sp_expr = super::super::ir_span::try_ir_span_expr(other, ctx)?;
            // Check if there's a matching sub-variant for the inferred type.
            let node_ty = infer_node_type(other, ctx);
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
                .find(|(_, vty)| **vty == node_ty)
            {
                let variant_ident = format_ident!("{}", variant_name);
                let enum_ident = &ctx.enum_ident;
                Some((sp_expr, quote! { |x| #enum_ident::#variant_ident(x) }))
            } else if node_ty == TypeDesc::Span {
                // Homogeneous Span output — identity map (Box wrapping done by caller).
                Some((sp_expr, quote! { |x| x }))
            } else {
                None
            }
        }
    }
}

/// Coerce alternation branch parsers to uniform `Box<Enum>` output type.
fn coerce_branches(
    parsers: &[TokenStream],
    branch_tys: &[TypeDesc],
    ctx: &IrCodegenCtx<'_>,
) -> Vec<TokenStream> {
    let enum_ident = &ctx.enum_ident;

    // Collect all sub-variants into a flat lookup by type.
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

    parsers
        .iter()
        .zip(branch_tys.iter())
        .map(|(parser, branch_ty)| {
            if *branch_ty == TypeDesc::BoxedEnum || *branch_ty == TypeDesc::Enum {
                // Already (Box<)Enum(>) — no sub-variant coercion needed.
                parser.clone()
            } else if let Some((variant_name, _)) = all_sub_variants
                .iter()
                .find(|(_, vty)| *vty == branch_ty)
            {
                // Found matching sub-variant.
                let variant_ident = format_ident!("{}", variant_name);
                quote! { #parser.map(|x| Box::new(#enum_ident::#variant_ident(x))) }
            } else if *branch_ty == TypeDesc::Span {
                // Span branch without sub-variant.
                quote! { #parser.map(|x| Box::new(x)) }
            } else {
                parser.clone()
            }
        })
        .collect()
}
