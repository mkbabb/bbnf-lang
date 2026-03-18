//! Inline (direct-dispatch) code generation.
//!
//! Instead of building combinator chains (`.then().skip().map()`) that each
//! allocate a `Box<dyn ParserFn>`, the inline emitter wraps entire rule bodies
//! in a single `Parser::new(move |state| { ... })` closure with sequential
//! `.call(state)` invocations. Parser/SpanParser leaf constructors are hoisted
//! outside the closure so they're built once, not per-parse.
//!
//! The combinator path (`ir_node_to_tokens`) is used as a fallback for nodes
//! where the combinator approach is already efficient (dispatch Alt, Repeat)
//! or too complex to inline (Minus, Negate).

use bbnf_ir::{FnDescriptor, IrNode, RuleId, TypeDesc};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::fast_paths;
use super::super::ir_types::IrCodegenCtx;
use super::{ir_node_to_tokens, ir_node_to_tokens_vec, repeat, seq, unescape_literal};

/// Context for inline code generation — tracks hoisted parser bindings and
/// generates unique variable names within a rule body.
pub(crate) struct InlineCtx {
    /// Parser/SpanParser bindings hoisted outside the `Parser::new` closure.
    pub hoisted: Vec<TokenStream>,
    /// Monotonic counter for unique binding names.
    counter: usize,
}

impl InlineCtx {
    pub fn new() -> Self {
        Self {
            hoisted: Vec::new(),
            counter: 0,
        }
    }

    /// Generate a unique identifier with the given prefix.
    pub fn fresh_ident(&mut self, prefix: &str) -> syn::Ident {
        let id = self.counter;
        self.counter += 1;
        format_ident!("__{}_{}", prefix, id)
    }

    /// Hoist an expression into a `let` binding outside the closure.
    /// Returns the binding identifier for use inside the closure body.
    pub fn hoist(&mut self, expr: TokenStream) -> syn::Ident {
        let name = self.fresh_ident("h");
        self.hoisted.push(quote! { let #name = #expr; });
        name
    }
}

/// Check if a node would benefit from inline codegen (has inlineable sub-structure)
/// vs falling back entirely to combinator mode.
fn benefits_from_inline(node: &IrNode) -> bool {
    match node {
        // Leaf nodes: inlining saves one Box (sp_xxx vs string_span).
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        // Sequences, Skip, Next: inlining eliminates .then()/.skip()/.next() chains.
        IrNode::Seq(_) | IrNode::Skip(_, _) | IrNode::Next(_, _) => true,
        // Map wrapping a beneficial inner node: inline eliminates the .map() Box.
        IrNode::Map { inner, .. } => benefits_from_inline(inner),
        // OW wrapping a beneficial inner: inline eliminates the .trim_whitespace() Box.
        IrNode::OptionalWhitespace(inner) => benefits_from_inline(inner),
        // Ref: inlining saves the .map(Box::new) Box allocation.
        IrNode::Ref(_) => true,
        // Optional (Repeat 0..1): inline when inner benefits, avoiding .opt() combinator wrapper.
        IrNode::Repeat { inner, lo, hi } if *lo == 0 && *hi == 1 => benefits_from_inline(inner),
        // Alt, Repeat (non-optional), Minus, Negate: already efficient or complex — no benefit.
        _ => false,
    }
}

/// Check if a node would generate `?` operators when inlined.
/// These nodes are unsafe to inline inside an optional's `if let Some(...) = { ... }`
/// because the `?` would return from the outer parser closure instead of just
/// the optional scope.
fn inner_generates_try(node: &IrNode) -> bool {
    match node {
        // Sequences generate `__h.call(state)?` for each step.
        IrNode::Seq(children) => children.len() > 1,
        // Skip/Next generate `?` for sub-expressions.
        IrNode::Skip(_, _) | IrNode::Next(_, _) => true,
        // Map/OW: delegate to inner.
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            inner_generates_try(inner)
        }
        // Leaves and refs: no `?` generated.
        _ => false,
    }
}

/// Emit a rule body as inline direct-dispatch code.
///
/// If `enum_wrap` is `Some((enum_ident, variant_ident))`, the result is wrapped
/// in the enum variant, absorbing what would otherwise be an outer `.map()`.
///
/// Falls back to combinator mode when inlining would add a redundant wrapper
/// (e.g., when the body is a single dispatch Alt that's already a `Parser::new`).
pub fn emit_rule_body_inline(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    enum_wrap: Option<(&syn::Ident, &syn::Ident)>,
) -> TokenStream {
    // If the body wouldn't benefit from inlining (e.g., a single dispatch Alt),
    // use the combinator path to avoid a redundant Parser::new wrapper.
    if !benefits_from_inline(node) {
        let mut parser = ir_node_to_tokens(node, ctx);
        if let Some((enum_ident, variant_ident)) = enum_wrap {
            parser = quote! { #parser.map(|x| #enum_ident::#variant_ident(x)) };
        }
        return parser;
    }

    let mut ictx = InlineCtx::new();
    let body = ir_node_to_inline(node, ctx, &mut ictx);
    let hoisted = &ictx.hoisted;

    let result_expr = if let Some((enum_ident, variant_ident)) = enum_wrap {
        quote! { #body.map(|__x| #enum_ident::#variant_ident(__x)) }
    } else {
        body
    };

    quote! {
        {
            #(#hoisted)*
            ::parse_that::Parser::new(move |state: &mut ::parse_that::ParserState<'a>| {
                #result_expr
            })
        }
    }
}

/// Emit inline code for an IrNode.
///
/// Returns an expression of type `Option<T>` that uses a `state: &mut ParserState`
/// binding from the enclosing closure. Complex sub-expressions (Alt, Repeat) fall
/// back to the combinator path, hoisting the constructed parser and calling it inline.
///
/// The `in_vec` parameter indicates the result will be stored in a Vec, so Box
/// indirection on Ref calls is unnecessary.
pub(crate) fn ir_node_to_inline(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    ictx: &mut InlineCtx,
) -> TokenStream {
    ir_node_to_inline_vec(node, ctx, ictx, false)
}

/// Emit inline code for an IrNode with Vec context control.
pub(crate) fn ir_node_to_inline_vec(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    ictx: &mut InlineCtx,
    in_vec: bool,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let lit = proc_macro2::Literal::string(&unescaped);
            let name = ictx.hoist(quote! { ::parse_that::sp_string(#lit) });
            quote! { #name.call(state) }
        }

        IrNode::Regex(sid) => {
            let pattern = ctx.ir.get_string(*sid);
            let sp = fast_paths::emit_regex_span(pattern);
            let name = ictx.hoist(sp);
            quote! { #name.call(state) }
        }

        IrNode::Epsilon => {
            quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) }
        }

        IrNode::Ref(rule_id) => emit_ref_inline(*rule_id, ctx, ictx, in_vec),

        IrNode::Seq(children) => seq::emit_seq_inline(children, ctx, ictx, in_vec),

        IrNode::Alt(..) => {
            // Alt (especially dispatch) is already efficient — fall back to combinator.
            emit_inline_fallback(node, ctx, ictx, in_vec)
        }

        IrNode::Repeat { inner, lo, hi } if *lo == 0 && *hi == 1 => {
            // Phase 2b: inline Optional when inner benefits from inlining.
            let inner_ty = super::infer::infer_node_type(inner, ctx);
            if inner_ty != TypeDesc::Span {
                // Phase 1a: transparent ref → _unboxed() inline optional.
                if let IrNode::Ref(rule_id) = inner.as_ref() {
                    let rule = &ctx.ir.rules[*rule_id as usize];
                    if rule.meta.is_transparent {
                        let resolved_name = ctx.resolve_rule_name(*rule_id);
                        let unboxed_ident = format_ident!("{}_unboxed", resolved_name);
                        let name = ictx.hoist(quote! { Self::#unboxed_ident() });
                        let cp_var = ictx.fresh_ident("cp");
                        return quote! {
                            {
                                let #cp_var = state.offset;
                                if let Some(__opt_v) = #name.call(state) {
                                    Some(Some(__opt_v))
                                } else {
                                    state.offset = #cp_var;
                                    Some(None)
                                }
                            }
                        };
                    }
                }

                if benefits_from_inline(inner) {
                    let inner_expr = ir_node_to_inline_vec(inner, ctx, ictx, in_vec);
                    let cp_var = ictx.fresh_ident("cp");
                    // When inner generates `?` operators (sequences, skip/next),
                    // wrap in an immediately-invoked closure so `?` returns from
                    // the closure (producing None → optional miss) rather than
                    // propagating out of the outer parser closure.
                    let try_expr = if inner_generates_try(inner) {
                        quote! { (|| { #inner_expr })() }
                    } else {
                        quote! { { #inner_expr } }
                    };
                    return quote! {
                        {
                            let #cp_var = state.offset;
                            if let Some(__opt_v) = #try_expr {
                                Some(Some(__opt_v))
                            } else {
                                state.offset = #cp_var;
                                Some(None)
                            }
                        }
                    };
                }
            }
            // Span case or non-beneficial inner: fall back to combinator.
            emit_inline_fallback(node, ctx, ictx, false)
        }

        IrNode::Repeat { .. } => {
            // Non-optional Repeat needs combinator loop infrastructure — fall back.
            emit_inline_fallback(node, ctx, ictx, false)
        }

        IrNode::Skip(left, right) => {
            // Wrap detection: Skip(Next(open, middle), close) → wrap combinator.
            if let IrNode::Next(_, _) = left.as_ref() {
                return emit_inline_fallback(node, ctx, ictx, in_vec);
            }
            let left_expr = ir_node_to_inline_vec(left, ctx, ictx, in_vec);
            // Right side is discarded — use emit_discarded to strip Map/Box overhead,
            // then hoist and call inline.
            let right_parser = repeat::emit_discarded(right, false, ctx);
            let right_name = ictx.hoist(right_parser);
            let left_var = ictx.fresh_ident("skip");
            quote! {
                {
                    let #left_var = #left_expr?;
                    #right_name.call(state)?;
                    Some(#left_var)
                }
            }
        }

        IrNode::Next(left, right) => {
            // Wrap detection: Next(open, Skip(middle, close)) → wrap combinator.
            if let IrNode::Skip(_, _) = right.as_ref() {
                return emit_inline_fallback(node, ctx, ictx, in_vec);
            }
            // Left side is discarded — use emit_discarded to strip Map/Box overhead,
            // then hoist and call inline.
            let left_parser = repeat::emit_discarded(left, false, ctx);
            let left_name = ictx.hoist(left_parser);
            let right_expr = ir_node_to_inline_vec(right, ctx, ictx, in_vec);
            quote! {
                {
                    #left_name.call(state)?;
                    #right_expr
                }
            }
        }

        IrNode::Minus(_, _) | IrNode::Negate(_) => {
            // Rare — fall back to combinator.
            emit_inline_fallback(node, ctx, ictx, in_vec)
        }

        IrNode::Map { inner, fn_id } => {
            // Map fusion: detect Map { inner: Map { .. }, .. } and fuse.
            if let IrNode::Map {
                inner: inner2,
                fn_id: fn_id2,
            } = inner.as_ref()
            {
                let inner_fd = &ctx.ir.fns[*fn_id2 as usize];
                let outer_fd = &ctx.ir.fns[*fn_id as usize];
                match (inner_fd, outer_fd) {
                    // EnumWrap then BoxWrap → fused .map(|x| Box::new(Enum::Variant(x)))
                    (FnDescriptor::EnumWrap { variant }, FnDescriptor::BoxWrap) => {
                        let inner_expr = ir_node_to_inline_vec(inner2, ctx, ictx, in_vec);
                        let vname = ctx.ir.get_string(*variant);
                        let vident = format_ident!("{}", vname);
                        let enum_ident = &ctx.enum_ident;
                        return quote! { #inner_expr.map(|__x| Box::new(#enum_ident::#vident(__x))) };
                    }
                    // BoxWrap then EnumWrap → fused .map(|x| Enum::Variant(Box::new(x)))
                    (FnDescriptor::BoxWrap, FnDescriptor::EnumWrap { variant }) => {
                        let inner_expr = ir_node_to_inline_vec(inner2, ctx, ictx, in_vec);
                        let vname = ctx.ir.get_string(*variant);
                        let vident = format_ident!("{}", vname);
                        let enum_ident = &ctx.enum_ident;
                        return quote! { #inner_expr.map(|__x| #enum_ident::#vident(Box::new(__x))) };
                    }
                    _ => {}
                }
            }

            let inner_expr = ir_node_to_inline_vec(inner, ctx, ictx, in_vec);
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { variant } => {
                    let vname = ctx.ir.get_string(*variant);
                    let vident = format_ident!("{}", vname);
                    let enum_ident = &ctx.enum_ident;
                    quote! { #inner_expr.map(|__x| #enum_ident::#vident(__x)) }
                }
                FnDescriptor::BoxWrap => {
                    quote! { #inner_expr.map(Box::new) }
                }
                FnDescriptor::Custom { source, .. } => {
                    let closure_src = ctx.ir.get_string(*source);
                    let closure: syn::ExprClosure =
                        syn::parse_str(closure_src).unwrap_or_else(|e| {
                            panic!(
                                "Invalid mapping closure `{}` in IR codegen: {}",
                                closure_src, e
                            )
                        });
                    quote! { #inner_expr.map(#closure) }
                }
            }
        }

        IrNode::OptionalWhitespace(inner) => {
            // sep_by_ws detection (same as ir_node_to_tokens).
            if let IrNode::Repeat {
                inner: rep_inner,
                lo,
                hi,
            } = inner.as_ref()
            {
                if !(*lo == 0 && *hi == 1) {
                    if let Some((element, separator)) = repeat::try_sep_by(rep_inner) {
                        let parser = repeat::emit_sep_by_ws(element, separator, *lo, ctx);
                        let name = ictx.hoist(parser);
                        return quote! { #name.call(state) };
                    }
                }
            }

            // Inline whitespace trimming: trim before, call inner, trim after on success.
            let inner_expr = ir_node_to_inline_vec(inner, ctx, ictx, in_vec);
            let result_var = ictx.fresh_ident("ow");
            quote! {
                {
                    ::parse_that::trim_leading_whitespace_mut(state);
                    let #result_var = #inner_expr;
                    if #result_var.is_some() {
                        ::parse_that::trim_leading_whitespace_mut(state);
                    }
                    #result_var
                }
            }
        }
    }
}

/// Emit a nonterminal reference as inline code.
///
/// When `in_vec` is true and the rule is non-transparent, skip Box wrapping
/// since Vec provides heap indirection.
fn emit_ref_inline(rule_id: RuleId, ctx: &IrCodegenCtx<'_>, ictx: &mut InlineCtx, in_vec: bool) -> TokenStream {
    let rule = &ctx.ir.rules[rule_id as usize];
    let resolved_name = ctx.resolve_rule_name(rule_id);
    let ident = format_ident!("{}", resolved_name);

    if in_vec {
        if rule.meta.is_transparent {
            // Vec context + transparent: call _unboxed() which returns Enum directly.
            let unboxed_ident = format_ident!("{}_unboxed", resolved_name);
            let name = ictx.hoist(quote! { Self::#unboxed_ident() });
            quote! { #name.call(state) }
        } else {
            // Vec context + non-transparent: skip Box.
            let name = ictx.hoist(quote! { Self::#ident() });
            quote! { #name.call(state) }
        }
    } else if rule.meta.is_transparent {
        // Non-Vec + transparent: returns Box<Enum> directly.
        let name = ictx.hoist(quote! { Self::#ident() });
        quote! { #name.call(state) }
    } else {
        // Non-Vec + non-transparent: call and box the result inline.
        let name = ictx.hoist(quote! { Self::#ident() });
        quote! { #name.call(state).map(|__x| Box::new(__x)) }
    }
}

/// Fallback: build a combinator expression via `ir_node_to_tokens_vec`, hoist it,
/// and call it inline. Used for complex nodes (Alt, Repeat, Wrap) where the
/// combinator approach is already efficient or too complex to inline.
pub(super) fn emit_inline_fallback(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    ictx: &mut InlineCtx,
    in_vec: bool,
) -> TokenStream {
    let parser = ir_node_to_tokens_vec(node, ctx, in_vec);
    let name = ictx.hoist(parser);
    quote! { #name.call(state) }
}
