//! Core IR → TokenStream code generation.
//!
//! Walks `IrNode` trees and emits Rust parser combinator code as `TokenStream`.
//!
//! Two codegen modes:
//! - **Combinator mode** (`ir_node_to_tokens`): Builds parser combinator chains
//!   (`.then().skip().map()`). Each combinator creates a `Box<dyn ParserFn>`.
//! - **Inline mode** (`emit_rule_body_inline`): Wraps the entire rule body in a
//!   single `Parser::new(move |state| { ... })` with sequential `.call(state)`
//!   invocations. Eliminates intermediate `Box<dyn ParserFn>` allocations.
//!
//! Sub-modules:
//! - `alt`: Alternation, dispatch tables, sub-variant coercion
//! - `seq`: Concatenation, Span compression, flattening
//! - `repeat`: Repeat quantifiers, sep_by pattern detection, separator stripping
//! - `wrap`: Wrap pattern emission, regex coalescing
//! - `infer`: Quick type inference for Span detection
//! - `inline`: Direct-dispatch codegen (hoisted parsers + single closure)

mod alt;
pub mod infer;
mod inline;
pub mod monolithic;
mod repeat;
mod seq;
pub mod trace;
mod wrap;

use bbnf_ir::{FnDescriptor, IrNode, RuleId};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::fast_paths;
use super::ir_types::IrCodegenCtx;

// Re-exports used by other generate modules.
pub use infer::infer_node_type;
pub use inline::emit_rule_body_inline;

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

/// Generate a parser TokenStream from an IrNode (combinator mode).
///
/// Builds parser combinator chains — each combinator creates a `Box<dyn ParserFn>`.
/// Used internally by the inline emitter as a fallback for complex nodes, and by
/// other codegen modules (ir_span, ir_pretty) that don't need inline optimization.
///
/// The `elide_box` parameter indicates the parent provides heap indirection
/// (Vec, Option, or discarded context), so Box wrapping on Ref calls is unnecessary.
pub fn ir_node_to_tokens(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    ir_node_to_tokens_elide(node, ctx, false)
}

/// Generate a parser TokenStream from an IrNode with Box elision control.
///
/// When `elide_box` is true, Ref calls use `_unboxed()` variants that return
/// `Enum` directly instead of `Box<Enum>`, since the parent provides heap indirection.
pub fn ir_node_to_tokens_elide(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    elide_box: bool,
) -> TokenStream {
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

        IrNode::Ref(rule_id) => emit_ref(*rule_id, ctx, elide_box),

        IrNode::Seq(children) => seq::emit_seq(children, ctx, elide_box),

        IrNode::Alt(branches, dispatch) => {
            alt::emit_alt(branches, dispatch.as_ref(), ctx, elide_box)
        }

        IrNode::Repeat { inner, lo, hi } => repeat::emit_repeat(inner, *lo, *hi, ctx, elide_box),

        IrNode::Skip(left, right) => {
            // wrap detection: Skip(Next(open, middle), close) → middle.wrap(open, close)
            if let IrNode::Next(open, middle) = left.as_ref() {
                return wrap::emit_wrap(open, middle, right, ctx, elide_box);
            }
            let left_ts = ir_node_to_tokens_elide(left, ctx, elide_box);
            // Right side is discarded by .skip() — strip Map/Box overhead.
            let right_ts = repeat::emit_discarded(right, false, ctx);
            quote! { #left_ts.skip(#right_ts) }
        }

        IrNode::Next(left, right) => {
            // wrap detection: Next(open, Skip(middle, close)) → middle.wrap(open, close)
            if let IrNode::Skip(middle, close) = right.as_ref() {
                return wrap::emit_wrap(left, middle, close, ctx, elide_box);
            }
            // Left side is discarded by .next() — strip Map/Box overhead.
            let left_ts = repeat::emit_discarded(left, false, ctx);
            let right_ts = ir_node_to_tokens_elide(right, ctx, elide_box);
            quote! { #left_ts.next(#right_ts) }
        }

        IrNode::Minus(left, right) => {
            let left_ts = ir_node_to_tokens_elide(left, ctx, elide_box);
            let right_ts = ir_node_to_tokens_elide(right, ctx, false);
            quote! { #left_ts.minus(#right_ts) }
        }

        IrNode::Negate(inner) => {
            let inner_ts = ir_node_to_tokens_elide(inner, ctx, false);
            quote! { #inner_ts.negate() }
        }

        IrNode::Map { inner, fn_id } => {
            // Map fusion: detect Map { inner: Map { .. }, .. } and fuse into single .map().
            if let IrNode::Map {
                inner: inner2,
                fn_id: fn_id2,
            } = inner.as_ref()
            {
                if let Some(fused) = try_fuse_maps(inner2, *fn_id2, *fn_id, ctx, elide_box) {
                    return fused;
                }
            }
            let inner_ts = ir_node_to_tokens_elide(inner, ctx, elide_box);
            emit_map(inner_ts, *fn_id, ctx, elide_box)
        }

        IrNode::OptionalWhitespace(inner) => {
            // sep_by_ws detection: OptionalWhitespace(Repeat { inner: Skip(elem, Repeat { sep, 0, 1 }) })
            if let IrNode::Repeat {
                inner: rep_inner,
                lo,
                hi,
            } = inner.as_ref()
            {
                if !(*lo == 0 && *hi == 1) {
                    if let Some((element, separator)) = repeat::try_sep_by(rep_inner) {
                        return repeat::emit_sep_by_ws(element, separator, *lo, ctx);
                    }
                }
            }

            let inner_ts = ir_node_to_tokens_elide(inner, ctx, elide_box);
            quote! { #inner_ts.trim_whitespace() }
        }

        IrNode::TokenDispatch { fallback, .. } => {
            // Combinator path: fall back to the unfused expression.
            // TokenDispatch is an arena-path optimization; the combinator path
            // uses the fallback (which contains all original branches).
            ir_node_to_tokens_elide(fallback, ctx, elide_box)
        }
    }
}

/// Emit a nonterminal reference (combinator mode).
///
/// When `elide_box` is true, skip Box wrapping. For transparent rules, call
/// `_unboxed()` which returns Enum directly. For non-transparent rules, call
/// the normal method (which already returns Enum — Box wrapping is a call-site concern).
fn emit_ref(rule_id: RuleId, ctx: &IrCodegenCtx<'_>, elide_box: bool) -> TokenStream {
    let rule = &ctx.ir.rules[rule_id as usize];
    let ident = ctx.rule_method_ident(rule_id);

    if elide_box {
        if rule.meta.is_transparent {
            let unboxed_ident = ctx.unboxed_method_ident_for_name(ctx.resolve_rule_name(rule_id));
            quote! { Self::#unboxed_ident() }
        } else {
            quote! { Self::#ident() }
        }
    } else if rule.meta.is_transparent {
        quote! { Self::#ident() }
    } else {
        let state_ident = format_ident!("state");
        let body = ctx.wrap_recur_expr_with_state(quote! { x }, &state_ident);
        ctx.wrap_recur_map_with_state(quote! { Self::#ident() }, body, &state_ident)
    }
}

/// Try to fuse two nested Map operations into a single `.map()` call.
///
/// Patterns:
/// - `EnumWrap` + `BoxWrap` → `.map(|x| Box::new(Enum::Variant(x)))`
/// - `BoxWrap` + `EnumWrap` → `.map(|x| Enum::Variant(Box::new(x)))`
fn try_fuse_maps(
    inner: &IrNode,
    inner_fn_id: u32,
    outer_fn_id: u32,
    ctx: &IrCodegenCtx<'_>,
    elide_box: bool,
) -> Option<TokenStream> {
    let inner_fd = &ctx.ir.fns[inner_fn_id as usize];
    let outer_fd = &ctx.ir.fns[outer_fn_id as usize];

    let inner_ts = ir_node_to_tokens_elide(inner, ctx, elide_box);

    match (inner_fd, outer_fd) {
        (FnDescriptor::EnumWrap { variant }, FnDescriptor::BoxWrap) => {
            let vname = ctx.ir.get_string(*variant);
            let vident = format_ident!("{}", vname);
            let enum_ident = &ctx.enum_ident;
            if elide_box {
                Some(quote! { #inner_ts.map(|x| #enum_ident::#vident(x)) })
            } else {
                let state_ident = format_ident!("state");
                let wrapped = ctx
                    .wrap_recur_expr_with_state(quote! { #enum_ident::#vident(x) }, &state_ident);
                Some(ctx.wrap_recur_map_with_state(inner_ts.clone(), wrapped, &state_ident))
            }
        }
        (FnDescriptor::BoxWrap, FnDescriptor::EnumWrap { variant }) => {
            let vname = ctx.ir.get_string(*variant);
            let vident = format_ident!("{}", vname);
            let enum_ident = &ctx.enum_ident;
            let state_ident = format_ident!("state");
            let wrapped = ctx.wrap_recur_expr_with_state(quote! { x }, &state_ident);
            Some(quote! {
                #inner_ts.map_with_ctx(|x, #state_ident| #enum_ident::#vident(#wrapped))
            })
        }
        _ => None,
    }
}

/// Emit a Map expression (combinator mode).
///
/// When `elide_box` is true, `BoxWrap` is a no-op (the parent provides indirection).
fn emit_map(
    inner_ts: TokenStream,
    fn_id: u32,
    ctx: &IrCodegenCtx<'_>,
    elide_box: bool,
) -> TokenStream {
    let fd = &ctx.ir.fns[fn_id as usize];
    match fd {
        FnDescriptor::EnumWrap { variant } => {
            let vname = ctx.ir.get_string(*variant);
            let vident = format_ident!("{}", vname);
            let enum_ident = &ctx.enum_ident;
            quote! { #inner_ts.map(|x| #enum_ident::#vident(x)) }
        }
        FnDescriptor::BoxWrap => {
            if elide_box {
                inner_ts
            } else {
                let state_ident = format_ident!("state");
                let wrapped = ctx.wrap_recur_expr_with_state(quote! { x }, &state_ident);
                ctx.wrap_recur_map_with_state(inner_ts, wrapped, &state_ident)
            }
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
        FnDescriptor::NumberConvert => {
            quote! { scan_number_f64(state) }
        }
        FnDescriptor::HexConvert { fn_path } => {
            let fn_path_str = ctx.ir.get_string(*fn_path);
            let fn_path_tokens: syn::Path = syn::parse_str(fn_path_str).unwrap_or_else(|e| {
                panic!(
                    "Invalid HexConvert fn_path `{}`: {}",
                    fn_path_str, e
                )
            });
            quote! { #inner_ts.map(|__s| #fn_path_tokens(__s.as_str())) }
        }
        FnDescriptor::Constant { value, .. } => {
            let val_src = ctx.ir.get_string(*value);
            let val_expr: syn::Expr = syn::parse_str(val_src).unwrap_or_else(|e| {
                panic!("Invalid constant expression `{}` in IR codegen: {}", val_src, e)
            });
            quote! { #inner_ts.map(|_| #val_expr) }
        }
        FnDescriptor::SpanCapture => {
            // @{expr}: parse inner for validation, return Span of consumed input.
            quote! {
                {
                    let __start = state.offset;
                    let __result = #inner_ts;
                    if __result.is_some() {
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    }
                }
            }
        }
    }
}
