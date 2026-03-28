//! Monolithic arena code generation.
//!
//! Instead of building combinator chains (`lazy → dispatch → sep_by → map_with_ctx`)
//! that allocate ~30 `Parser` objects with ~60 heap allocations per parse, the
//! monolithic emitter generates **direct recursive functions** —
//! `fn __rule_arena(state) -> Option<ArenaEnum>` — with zero combinator overhead.
//!
//! Each public `rule_arena()` method wraps a single function pointer in
//! `Parser::new()`: one SmallBox, zero vtable dispatches, O(1) construction.
//!
//! All internal functions return `Option<ArenaEnum<'a>>` (the unboxed enum type).
//! Arena allocation (`arena.alloc`) happens at:
//! - Non-elide_box Ref call sites (producing `&'a ArenaEnum<'a>`)
//! - The public method wrapper for transparent rules
//!
//! Sub-modules:
//! - `alt`: Dispatch-table and flat alternation emission
//! - `seq`: Concatenation with span compression
//! - `repeat`: Quantifiers, sep_by, sep_by_ws loops
//! - `expr`: Leaf, Ref, Skip/Next, Wrap, Map, OptionalWhitespace

mod alt;
mod delim_scan;
mod expr;
mod generate;
mod helpers;
mod repeat;
mod seq;
pub mod span;
mod token_dispatch;

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::fast_paths;
use super::super::ir_types::{IrCodegenCtx, StorageMode};
use super::unescape_literal;

// Re-export the entry point at the original visibility.
pub use generate::generate_monolithic_arena;

// Re-export items used by sub-modules via `super::`.
pub(super) use generate::compute_single_site_inline;
pub(super) use helpers::{
    emit_literal_inline, emit_literal_inline_unchecked, emit_mono_discarded, emit_mono_fallback,
    mono_fn_ident,
};

/// Emit the whitespace-trimming call for `?w` (OptionalWhitespace).
///
/// When `@ws /regex/` is set, emits a call to the corresponding fast-path scanner
/// (or a hoisted regex SpanParser). Otherwise, emits the default `trim_leading_whitespace_mut`.
///
/// The emitted code is a statement (not an expression) — it advances `state.offset`
/// past whitespace and returns nothing.
pub(super) fn emit_ws_trim(ctx: &IrCodegenCtx<'_>, mctx: &mut MonoCtx) -> TokenStream {
    if let Some(ws_sid) = ctx.ir.ws_pattern {
        let pattern = ctx.ir.get_string(ws_sid);
        // Try direct scanner call (SIMD fast path for known patterns).
        if let Some(direct) = fast_paths::emit_regex_direct_call(pattern) {
            // Direct scanner returns Option<Span>; we just need the side effect (advance offset).
            return quote! { #direct; };
        }
        // Try HIR-based inline compilation.
        if let Some(inline) = super::super::regex_emit::try_emit_regex_inline(pattern) {
            return quote! { #inline; };
        }
        // Fall back to LazyLock<Regex> — NEVER sp_regex.
        let lazy = super::super::regex_emit::emit_regex_lazy_static(pattern);
        quote! { #lazy; }
    } else {
        quote! { ::parse_that::trim_leading_whitespace_mut(state); }
    }
}

/// Check whether an IrNode emits a simple expression that doesn't use the `?`
/// operator — meaning the IIFE closure wrapper `(|| expr)()` can be elided.
///
/// Simple nodes: direct Ref calls (not inlined), Literal, Regex, Map wrapping a simple node.
/// Inlined Refs (fusion or single-site) may contain `?` in the expanded body, so they
/// are NOT simple — the IIFE is needed to scope the `?` operator.
pub(super) fn is_simple_expr(node: &IrNode, mctx: &MonoCtx) -> bool {
    match node {
        IrNode::Ref(rule_id) => {
            // If the Ref gets inlined (fusion or single-site), the emitted code
            // may contain `?` operators that need IIFE scoping.
            // Only direct function calls (non-inlined) are guaranteed simple.
            let is_inlined =
                mctx.fusion_eligible.get(*rule_id as usize).copied() == Some(true)
                    || mctx
                        .single_site_inline
                        .get(*rule_id as usize)
                        .copied()
                        == Some(true);
            !is_inlined
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => true,
        IrNode::Map { inner, .. } => is_simple_expr(inner, mctx),
        IrNode::OptionalWhitespace(inner) => is_simple_expr(inner, mctx),
        _ => false,
    }
}

// ── Mono Context ─────────────────────────────────────────────────────────────

/// Context for monolithic code generation — tracks hoisted leaf-parser bindings,
/// generates unique variable names, and tracks fusion-eligible rules.
pub(super) struct MonoCtx {
    pub hoisted: Vec<TokenStream>,
    /// Deduplication map: expression string → hoisted binding name.
    hoist_dedup: std::collections::HashMap<String, syn::Ident>,
    counter: usize,
    /// Per-rule flag: true if the rule's body can be inlined at call sites.
    /// Indexed by RuleId. Computed once in `generate_monolithic_arena`.
    pub fusion_eligible: Vec<bool>,
    /// Phase 9: Per-rule flag for single-site inline eligibility.
    /// A cyclic rule can be inlined at its single call site when:
    /// 1. It does NOT contain a direct self-reference (Ref(self))
    /// 2. It has exactly 1 reference across all rule bodies
    /// 3. It is NOT the grammar entry point
    pub single_site_inline: Vec<bool>,
    /// When set, the byte at `state.offset` is guaranteed to equal this value
    /// (from a preceding dispatch-table match). The next single-byte literal
    /// check that matches can skip the bounds check — just advance offset.
    /// Consumed (set to None) after use.
    pub dispatch_guaranteed_byte: Option<u8>,
    /// Name of the rule currently being generated. Used by delim_scan for
    /// self-recursion (nested blocks call the enclosing wrap function).
    pub current_rule_name: Option<String>,
}

impl MonoCtx {
    pub fn new(fusion_eligible: Vec<bool>, single_site_inline: Vec<bool>) -> Self {
        Self {
            hoisted: Vec::new(),
            hoist_dedup: std::collections::HashMap::new(),
            counter: 0,
            fusion_eligible,
            single_site_inline,
            dispatch_guaranteed_byte: None,
            current_rule_name: None,
        }
    }

    pub fn fresh(&mut self, prefix: &str) -> syn::Ident {
        let id = self.counter;
        self.counter += 1;
        format_ident!("__{}{}", prefix, id)
    }

    pub fn hoist(&mut self, expr: TokenStream) -> syn::Ident {
        // Deduplicate: if an identical expression was already hoisted, reuse it.
        let expr_str = expr.to_string();
        if let Some(existing) = self.hoist_dedup.get(&expr_str) {
            return existing.clone();
        }
        let name = self.fresh("h");

        self.hoisted.push(quote! { let #name = #expr; });

        self.hoist_dedup.insert(expr_str, name.clone());
        name
    }
}

// ── Expression Dispatch ──────────────────────────────────────────────────────

/// Emit monolithic code for an IrNode.
///
/// Returns a TokenStream that evaluates to `Option<T>` where T depends on the
/// node type. Uses `state: &mut ParserState<'a>` from the enclosing function.
pub(super) fn emit_mono_expr(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let bytes = unescaped.as_bytes();
            // Dispatch guaranteed byte: skip bounds check, emit unchecked Span.
            if bytes.len() == 1 && mctx.dispatch_guaranteed_byte == Some(bytes[0]) {
                mctx.dispatch_guaranteed_byte = None;
                return quote! {
                    {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                };
            }
            emit_literal_inline(&unescaped, true)
        }

        IrNode::Regex(sid) => {
            let pattern = ctx.ir.get_string(*sid);
            // Phase 2: try direct scanner call (bypasses SpanParser dispatch stack).
            // Arena context: fuse number conversion (returns (Span, f64) for JSON numbers).
            let fuse = ctx.storage_mode == StorageMode::Arena;
            // 1. Try known fast paths (scan_ident, scan_number_f64, etc.)
            if let Some(direct) = fast_paths::emit_regex_direct_call_with_fuse(pattern, fuse) {
                direct
            }
            // 2. Try HIR-based inline compilation
            else if let Some(inline) = super::super::regex_emit::try_emit_regex_inline(pattern) {
                inline
            }
            // 3. Fall back to LazyLock<Regex> — NEVER sp_regex
            else {
                super::super::regex_emit::emit_regex_lazy_static(pattern)
            }
        }

        IrNode::Epsilon => {
            quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) }
        }

        IrNode::Ref(rule_id) => expr::emit_mono_ref(*rule_id, ctx, mctx, elide_box),

        IrNode::Seq(children) => seq::emit_mono_seq(children, ctx, mctx, elide_box),

        IrNode::Alt(branches, dispatch) => {
            alt::emit_mono_alt(branches, dispatch.as_ref(), ctx, mctx, elide_box)
        }

        IrNode::Repeat { inner, lo, hi } => {
            repeat::emit_mono_repeat(inner, *lo, *hi, ctx, mctx, elide_box)
        }

        IrNode::Skip(left, right) => expr::emit_mono_skip(left, right, ctx, mctx, elide_box),
        IrNode::Next(left, right) => expr::emit_mono_next(left, right, ctx, mctx, elide_box),

        IrNode::Minus(..) | IrNode::Negate(..) => {
            // Rare — fall back to combinator (hoisted and called inline).
            emit_mono_fallback(node, ctx, mctx, elide_box)
        }

        IrNode::Map { inner, fn_id } => expr::emit_mono_map(inner, *fn_id, ctx, mctx, elide_box),

        IrNode::OptionalWhitespace(inner) => expr::emit_mono_ow(inner, ctx, mctx, elide_box),

        IrNode::TokenDispatch { token, arms, fallback } => {
            token_dispatch::emit_token_dispatch(token, arms, fallback, ctx, mctx, elide_box)
        }
    }
}
