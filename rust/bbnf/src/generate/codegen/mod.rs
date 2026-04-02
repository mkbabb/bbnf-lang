//! Monolithic code generation.
//!
//! Instead of building combinator chains (`lazy → dispatch → sep_by → map_with_ctx`)
//! that allocate ~30 `Parser` objects with ~60 heap allocations per parse, the
//! monolithic emitter generates **direct recursive functions** —
//! `fn __rule(state) -> Option<Enum>` — with zero combinator overhead.
//!
//! Each public `rule()` method wraps a single function pointer in
//! `Parser::new()`: one SmallBox, zero vtable dispatches, O(1) construction.
//!
//! All internal functions return `Option<Enum<'a>>` (the unboxed enum type).
//! Slab allocation (`slab.alloc`) happens at:
//! - Non-elide_box Ref call sites (producing `&'a Enum<'a>`)
//! - The public method wrapper for transparent rules
//!
//! Sub-modules:
//! - `alt`: Dispatch-table and flat alternation emission
//! - `seq`: Concatenation with span compression
//! - `repeat`: Quantifiers, sep_by, sep_by_ws loops
//! - `expr`: Leaf, Ref, Skip/Next, Wrap, Map, OptionalWhitespace

mod alt;
mod alloc_emit;
mod delim_scan;
mod expr;
mod generate;
mod helpers;
pub mod ir_enums;
pub mod ir_types;
pub mod prettify;
mod repeat;
mod sep_by;
mod seq;
mod token_dispatch;
pub mod trace;

use bbnf_ir::IrNode;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use ir_types::IrCodegenCtx;

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

// Re-export the entry point.
pub use generate::generate_monolithic;

// Re-export items used by sub-modules via `super::`.
pub(super) use generate::compute_single_site_inline;
pub(super) use helpers::{
    emit_literal_inline, emit_literal_inline_unchecked, emit_mono_discarded, mono_fn_ident,
};

/// Emit the whitespace-trimming call for `?w` (OptionalWhitespace).
///
/// When `@ws /regex/` is set, emits a call to the corresponding fast-path scanner
/// (or a hoisted regex SpanParser). Otherwise, emits the default `trim_leading_whitespace_mut`.
///
/// The emitted code is a statement (not an expression) — it advances `state.offset`
/// past whitespace and returns nothing.
pub(super) fn emit_ws_trim(ctx: &IrCodegenCtx<'_>, _mctx: &mut MonoCtx) -> TokenStream {
    if let Some(ws_sid) = ctx.ir.ws_pattern {
        let pattern = ctx.ir.get_string(ws_sid);
        let opts = super::regex::EmitOpts::new(&super::regex::CostModel::DEFAULT);
        let code = super::regex::emit_regex(pattern, &opts);
        // Emit as statement — we just need the side effect (advance offset).
        quote! { #code; }
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
            let is_inlined = mctx.fusion_eligible.get(*rule_id as usize).copied() == Some(true)
                || mctx.single_site_inline.get(*rule_id as usize).copied() == Some(true);
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
    counter: usize,
    /// Per-rule flag: true if the rule's body can be inlined at call sites.
    /// Indexed by RuleId. Computed once in `generate_monolithic`.
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
    /// ID of the rule currently being generated. Used for scratch type lookup
    /// in slab mode to ensure type agreement with ir.types.
    pub current_rule_id: Option<bbnf_ir::RuleId>,
    /// @pretty hints for the current rule. Used by the prettify repeat codegen
    /// to determine the separator between items (softline, hardline, blankline,
    /// sep("str"), etc.).
    pub current_pretty_hints: Option<bbnf_ir::PrettyHints>,
}

impl MonoCtx {
    pub fn new(fusion_eligible: Vec<bool>, single_site_inline: Vec<bool>) -> Self {
        Self {
            hoisted: Vec::new(),
            counter: 0,
            fusion_eligible,
            single_site_inline,
            dispatch_guaranteed_byte: None,
            current_rule_name: None,
            current_rule_id: None,
            current_pretty_hints: None,
        }
    }

    pub fn fresh(&mut self, prefix: &str) -> syn::Ident {
        let id = self.counter;
        self.counter += 1;
        format_ident!("__{}{}", prefix, id)
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
            // Slab context: fuse number conversion (returns (Span, f64) for JSON numbers).
            // Skip fusing when prettify is enabled — formatters only need Spans.
            let fuse = !ctx.parser_attrs.prettify;
            let opts = super::regex::EmitOpts::new(&super::regex::CostModel::DEFAULT)
                .with_fuse(fuse);
            super::regex::emit_regex(pattern, &opts)
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

        IrNode::Minus(left, right) => {
            // Checkpoint/restore pattern: try right (excluded), if it matches
            // at this position, reject. Otherwise try left.
            let right_expr = emit_mono_expr(right, ctx, mctx, false);
            let left_expr = emit_mono_expr(left, ctx, mctx, elide_box);
            quote! {
                {
                    let __save_minus = state.offset;
                    let __excluded = #right_expr;
                    state.offset = __save_minus;
                    if __excluded.is_some() {
                        None
                    } else {
                        #left_expr
                    }
                }
            }
        }

        IrNode::Negate(inner) => {
            // Zero-width assertion: succeeds (returning unit) iff inner fails.
            // Never advances state.offset.
            let inner_expr = emit_mono_expr(inner, ctx, mctx, false);
            quote! {
                {
                    let __save_neg = state.offset;
                    let __inner = #inner_expr;
                    state.offset = __save_neg;
                    if __inner.is_some() {
                        None
                    } else {
                        Some(())
                    }
                }
            }
        }

        IrNode::Map { inner, fn_id } => expr::emit_mono_map(inner, *fn_id, ctx, mctx, elide_box),

        IrNode::OptionalWhitespace(inner) => expr::emit_mono_ow(inner, ctx, mctx, elide_box),

        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => token_dispatch::emit_token_dispatch(token, arms, fallback, ctx, mctx, elide_box),
    }
}
