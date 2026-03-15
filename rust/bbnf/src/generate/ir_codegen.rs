//! Core IR → TokenStream code generation.
//!
//! Walks `IrNode` trees and emits Rust parser combinator code as `TokenStream`.
//! Replaces the AST-based `codegen.rs` + `alternation.rs` + `concatenation.rs`.

use bbnf_ir::{AltBranch, FnDescriptor, IrNode, RuleId, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::fast_paths;
use super::ir_types::{type_is_span, IrCodegenCtx};

/// Unescape a BBNF literal string (e.g. `\n` → newline, `\t` → tab).
/// BBNF literals store escape sequences as raw characters (backslash + letter)
/// since they come from source text between quotes. We need to unescape them
/// before embedding into Rust string literals via `quote!`.
pub fn unescape_literal(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('\'') => result.push('\''),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some('f') => result.push('\x0C'),
                Some('b') => result.push('\x08'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Generate a parser TokenStream from an IrNode.
pub fn ir_node_to_tokens(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let lit = proc_macro2::Literal::string(&unescaped);
            quote! { ::parse_that::string_span(#lit) }
        }

        IrNode::Regex(sid) => {
            let pattern = ctx.ir.get_string(*sid);
            fast_paths::emit_regex_parser(pattern)
        }

        IrNode::Epsilon => quote! { ::parse_that::epsilon() },

        IrNode::Ref(rule_id) => emit_ref(*rule_id, ctx),

        IrNode::Seq(children) => emit_seq(children, ctx),

        IrNode::Alt(branches, dispatch) => emit_alt(branches, dispatch.as_ref(), ctx),

        IrNode::Repeat { inner, lo, hi } => emit_repeat(inner, *lo, *hi, ctx),

        IrNode::Skip(left, right) => {
            let left_ts = ir_node_to_tokens(left, ctx);
            let right_ts = ir_node_to_tokens(right, ctx);
            quote! { #left_ts.skip(#right_ts) }
        }

        IrNode::Next(left, right) => {
            let left_ts = ir_node_to_tokens(left, ctx);
            let right_ts = ir_node_to_tokens(right, ctx);
            quote! { #left_ts.next(#right_ts) }
        }

        IrNode::Minus(left, right) => {
            let left_ts = ir_node_to_tokens(left, ctx);
            let right_ts = ir_node_to_tokens(right, ctx);
            quote! { #left_ts.minus(#right_ts) }
        }

        IrNode::Negate(inner) => {
            let inner_ts = ir_node_to_tokens(inner, ctx);
            quote! { #inner_ts.negate() }
        }

        IrNode::Map { inner, fn_id } => {
            let inner_ts = ir_node_to_tokens(inner, ctx);
            emit_map(inner_ts, *fn_id, ctx)
        }

        IrNode::OptionalWhitespace(inner) => {
            let inner_ts = ir_node_to_tokens(inner, ctx);
            quote! { #inner_ts.trim_whitespace() }
        }
    }
}

/// Emit a nonterminal reference.
fn emit_ref(rule_id: RuleId, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let rule = &ctx.ir.rules[rule_id as usize];
    let resolved_name = ctx.resolve_rule_name(rule_id);
    let ident = format_ident!("{}", resolved_name);

    if rule.meta.is_transparent {
        // Transparent rules return Box<Enum> directly — no extra boxing.
        quote! { Self::#ident() }
    } else {
        // Non-transparent: wrap result in Box for recursive types.
        quote! { Self::#ident().map(|x| Box::new(x)) }
    }
}

/// Emit a Seq (concatenation) expression.
///
/// Handles:
/// - Span compression (consecutive Span children → `.then_span()`)
/// - sp_method_rules override (Ref to span-eligible → `Self::rule_sp().into_parser()`)
/// - `(T, Vec<T>)` → `Vec<T>` flattening
fn emit_seq(children: &[IrNode], ctx: &IrCodegenCtx<'_>) -> TokenStream {
    if children.is_empty() {
        return quote! { ::parse_that::epsilon() };
    }
    if children.len() == 1 {
        return ir_node_to_tokens(&children[0], ctx);
    }

    // Compute per-child types and determine sp_method_rules overrides.
    let child_info: Vec<(TokenStream, bool)> = children
        .iter()
        .map(|c| {
            // Check for sp_method_rules override.
            if let IrNode::Ref(id) = c {
                let rule = &ctx.ir.rules[*id as usize];
                let name = ctx.ir.get_string(rule.name);
                if ctx.has_sp_method(name) && !rule.meta.is_transparent {
                    let sp_ident = format_ident!("{}_sp", name);
                    return (quote! { Self::#sp_ident().into_parser() }, true);
                }
            }
            (ir_node_to_tokens(c, ctx), false)
        })
        .collect();

    // Determine types for each child.
    let child_types: Vec<TypeDesc> = children
        .iter()
        .enumerate()
        .map(|(i, c)| {
            if child_info[i].1 {
                // sp_method_rules override → Span
                TypeDesc::Span
            } else {
                infer_node_type(c, ctx)
            }
        })
        .collect();

    // All-Span guard: if all children would be Span after override, don't use override.
    let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
    let (parsers, types): (Vec<TokenStream>, Vec<TypeDesc>) = if all_span {
        children
            .iter()
            .map(|c| (ir_node_to_tokens(c, ctx), infer_node_type(c, ctx)))
            .unzip()
    } else {
        child_info
            .into_iter()
            .zip(child_types.into_iter())
            .map(|((ts, _), ty)| (ts, ty))
            .unzip()
    };

    // Build chains: group consecutive Span children for then_span().
    let mut chains: Vec<(bool, Vec<TokenStream>)> = Vec::new();
    for (parser, ty) in parsers.iter().zip(types.iter()) {
        let is_span = *ty == TypeDesc::Span;
        if let Some((last_is_span, last_chain)) = chains.last_mut() {
            if is_span && *last_is_span {
                last_chain.push(parser.clone());
                continue;
            }
        }
        chains.push((is_span, vec![parser.clone()]));
    }

    // Fold chains into a single expression.
    let mut acc: Option<TokenStream> = None;
    for (n, (_, chain)) in chains.iter().enumerate() {
        let chain_acc = chain.iter().fold(None::<TokenStream>, |acc, parser| match acc {
            None => Some(parser.clone()),
            Some(acc) => Some(quote! { #acc.then_span(#parser) }),
        });
        acc = match acc {
            None => chain_acc,
            Some(prev) => {
                if n > 1 {
                    Some(quote! { #prev.then_flat(#chain_acc) })
                } else {
                    Some(quote! { #prev.then(#chain_acc) })
                }
            }
        };
    }
    let parser = acc.unwrap();

    // Flattening: (A, Vec<A>) → Vec<A> and (Vec<A>, A) → Vec<A>.
    // Compute effective types after Span compression.
    let effective_types = {
        let mut result: Vec<TypeDesc> = Vec::new();
        let mut in_span_run = false;
        for ty in &types {
            if *ty == TypeDesc::Span {
                if !in_span_run {
                    result.push(TypeDesc::Span);
                    in_span_run = true;
                }
            } else {
                result.push(ty.clone());
                in_span_run = false;
            }
        }
        result
    };

    if effective_types.len() == 2 {
        // (A, Vec<A>) → prepend first element
        if let TypeDesc::Vec(inner) = &effective_types[1] {
            if **inner == effective_types[0] {
                return quote! {
                    #parser.map(|(first, rest)| {
                        let mut v = Vec::with_capacity(1 + rest.len());
                        v.push(first);
                        v.extend(rest);
                        v
                    })
                };
            }
        }
        // (Vec<A>, A) → append last element
        if let TypeDesc::Vec(inner) = &effective_types[0] {
            if **inner == effective_types[1] {
                return quote! {
                    #parser.map(|(mut v, last)| {
                        v.push(last);
                        v
                    })
                };
            }
        }
    }

    parser
}

/// Emit an Alt (alternation) expression.
///
/// Uses dispatch table when available, sub-variant coercion for heterogeneous branches,
/// any_span for all-literal branches, and inline flat alternation otherwise.
fn emit_alt(
    branches: &[AltBranch],
    dispatch: Option<&bbnf_ir::AltDispatch>,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    if branches.is_empty() {
        return quote! { ::parse_that::epsilon().map(|_| unreachable!()) };
    }
    if branches.len() == 1 {
        return ir_node_to_tokens(&branches[0].node, ctx);
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

    // Compute branch types and determine if coercion is needed.
    let branch_tys: Vec<TypeDesc> = branches
        .iter()
        .map(|b| infer_node_type(&b.node, ctx))
        .collect();
    let all_same = branch_tys.windows(2).all(|w| w[0] == w[1]);
    let overall_is_boxed_enum = !all_same;

    // Generate branch parsers.
    let parsers: Vec<TokenStream> = branches
        .iter()
        .map(|b| ir_node_to_tokens(&b.node, ctx))
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
                let call =
                    quote! { #sp_binding.call(state).map(#map_fn).map(Box::new) };
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

/// Check if a branch node is a Ref to a span-eligible rule with an _sp method.
/// Returns (sp_constructor, map_fn) if so.
fn try_sp_dispatch_branch(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
) -> Option<(TokenStream, TokenStream)> {
    // Check for Map { inner: Ref(id), fn_id } where the ref is span-eligible.
    match node {
        IrNode::Map { inner, fn_id } => {
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
            None
        }
        IrNode::Ref(id) => {
            let rule = &ctx.ir.rules[*id as usize];
            let name = ctx.ir.get_string(rule.name);
            if ctx.has_sp_method(name) && !rule.meta.is_transparent {
                // Only use SpanParser fast-path if the rule's body type is Span.
                // Otherwise the _sp() method produces Span but the enum variant
                // expects the full body type (e.g., a tuple).
                let body_ty = ctx.rule_types.get(&rule.id);
                let is_span_body = body_ty.is_some_and(|ty| {
                    type_is_span(ty)
                });
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
        _ => None,
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
            if *branch_ty == TypeDesc::BoxedEnum {
                // Already Box<Enum>.
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

/// Emit a Repeat expression.
fn emit_repeat(inner: &IrNode, lo: u32, hi: u32, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let inner_ts = ir_node_to_tokens(inner, ctx);
    let inner_ty = infer_node_type(inner, ctx);
    let is_span = inner_ty == TypeDesc::Span;

    if lo == 0 && hi == 1 {
        // Optional.
        if is_span {
            quote! { #inner_ts.opt_span() }
        } else {
            quote! { #inner_ts.opt() }
        }
    } else if lo == 0 {
        // Many (zero or more).
        if is_span {
            quote! { #inner_ts.many_span(..) }
        } else {
            quote! { #inner_ts.many(..) }
        }
    } else if lo == 1 {
        // Many1 (one or more).
        if is_span {
            quote! { #inner_ts.many_span(1..) }
        } else {
            quote! { #inner_ts.many(1..) }
        }
    } else {
        let lo = lo as usize;
        quote! { #inner_ts.many(#lo..) }
    }
}

/// Emit a Map expression.
fn emit_map(inner_ts: TokenStream, fn_id: u32, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let fd = &ctx.ir.fns[fn_id as usize];
    match fd {
        FnDescriptor::EnumWrap { variant } => {
            let vname = ctx.ir.get_string(*variant);
            let vident = format_ident!("{}", vname);
            let enum_ident = &ctx.enum_ident;
            quote! { #inner_ts.map(|x| #enum_ident::#vident(x)) }
        }
        FnDescriptor::BoxWrap => {
            quote! { #inner_ts.map(Box::new) }
        }
        FnDescriptor::Custom { source, .. } => {
            let closure_src = ctx.ir.get_string(*source);
            let closure: syn::ExprClosure = syn::parse_str(closure_src).unwrap_or_else(|e| {
                panic!(
                    "Invalid mapping closure `{}` in IR codegen: {}",
                    closure_src, e
                )
            });
            quote! { #inner_ts.map(#closure) }
        }
    }
}

/// Quick type inference for a single IrNode (used by codegen for Span detection).
/// This is a simplified version that doesn't need the full InferCtx.
pub fn infer_node_type(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TypeDesc {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) => TypeDesc::Span,
        IrNode::Epsilon => TypeDesc::Tuple(vec![]),
        // Always BoxedEnum: emit_ref wraps with .map(Box::new), matching the
        // AST type inference (type_inference.rs:122 returns boxed_enum_type for
        // all nonterminal references).
        IrNode::Ref(_) => TypeDesc::BoxedEnum,
        IrNode::Seq(children) => {
            // B.1: Override Ref to rules with _sp() methods with Span.
            // Matches emit_seq's sp_method_rules override and infer_types:infer_seq.
            let child_types: Vec<TypeDesc> = children
                .iter()
                .map(|c| {
                    if let IrNode::Ref(id) = c {
                        let rule = &ctx.ir.rules[*id as usize];
                        if rule.meta.has_sp_method && !rule.meta.is_transparent {
                            return TypeDesc::Span;
                        }
                    }
                    infer_node_type(c, ctx)
                })
                .collect();

            // All-Span guard: if all children would be Span after override,
            // don't apply (emit_seq also has this guard).
            let all_span = child_types.iter().all(|t| *t == TypeDesc::Span);
            let tys = if all_span {
                children.iter().map(|c| infer_node_type(c, ctx)).collect::<Vec<_>>()
            } else {
                child_types
            };

            // Span compression.
            let mut compressed: Vec<TypeDesc> = Vec::new();
            let mut in_span = false;
            for ty in &tys {
                if *ty == TypeDesc::Span {
                    if !in_span {
                        compressed.push(TypeDesc::Span);
                        in_span = true;
                    }
                } else {
                    compressed.push(ty.clone());
                    in_span = false;
                }
            }

            // (T, Vec<T>) flattening — matches emit_seq.
            if compressed.len() == 2 {
                if let TypeDesc::Vec(inner) = &compressed[1] {
                    if **inner == compressed[0] {
                        return compressed[1].clone();
                    }
                }
                if let TypeDesc::Vec(inner) = &compressed[0] {
                    if **inner == compressed[1] {
                        return compressed[0].clone();
                    }
                }
            }

            if compressed.len() == 1 {
                compressed.into_iter().next().unwrap()
            } else {
                TypeDesc::Tuple(compressed)
            }
        }
        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return TypeDesc::Tuple(vec![]);
            }
            let first = infer_node_type(&branches[0].node, ctx);
            if branches[1..]
                .iter()
                .all(|b| infer_node_type(&b.node, ctx) == first)
            {
                first
            } else {
                TypeDesc::BoxedEnum
            }
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_ty = infer_node_type(inner, ctx);
            if *lo == 0 && *hi == 1 {
                if inner_ty == TypeDesc::Span {
                    TypeDesc::Span
                } else {
                    TypeDesc::Option(Box::new(inner_ty))
                }
            } else if inner_ty == TypeDesc::Span {
                TypeDesc::Span
            } else {
                TypeDesc::Vec(Box::new(inner_ty))
            }
        }
        IrNode::Skip(left, _) => infer_node_type(left, ctx),
        IrNode::Next(_, right) => infer_node_type(right, ctx),
        IrNode::Minus(left, _) => infer_node_type(left, ctx),
        IrNode::Negate(_) => TypeDesc::Tuple(vec![]),
        IrNode::OptionalWhitespace(inner) => infer_node_type(inner, ctx),
        IrNode::Map { fn_id, .. } => match &ctx.ir.fns[*fn_id as usize] {
            FnDescriptor::EnumWrap { .. } => TypeDesc::Enum,
            FnDescriptor::BoxWrap => TypeDesc::BoxedEnum,
            FnDescriptor::Custom { return_type, source } => {
                return_type.clone().unwrap_or(TypeDesc::Named(*source))
            }
        },
    }
}
