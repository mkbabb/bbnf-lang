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
mod repeat;
mod seq;
pub mod span;
mod token_dispatch;

use bbnf_ir::{FnDescriptor, GrammarIR, IrNode};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::fast_paths;
use super::super::ir_types::{IrCodegenCtx, StorageMode};
use super::unescape_literal;

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

/// Internal function name for a rule: `__rule_arena`.
pub(super) fn mono_fn_ident(name: &str) -> syn::Ident {
    format_ident!("__{}_arena", name)
}

// ── Entry Point ──────────────────────────────────────────────────────────────

/// Generate all monolithic arena methods for all rules.
///
/// For each rule, emits:
/// 1. A private associated fn `fn __rule_arena<'a>(state) -> Option<ArenaEnum<'a>>`
/// 2. A public method `pub fn rule_arena<'a>() -> Parser<'a, ReturnType>`
/// 3. For transparent rules: `pub fn rule_arena_unboxed<'a>() -> Parser<'a, ArenaEnum<'a>>`
pub fn generate_monolithic_arena(
    ir: &GrammarIR,
    ctx: &IrCodegenCtx<'_>,
) -> TokenStream {
    assert!(ctx.storage_mode == StorageMode::Arena);

    let mut methods: Vec<TokenStream> = Vec::new();
    let enum_type = &ctx.enum_type;

    // Pre-compute fusion eligibility: non-cyclic, no @recover, no @pretty, no @no_collapse.
    // @token rules are always fusion-eligible — body inlined at call sites, but the enum
    // variant is preserved (unlike force_inline which eliminates the variant entirely).
    // This allows @token to coexist with @pretty: the parsing body is flat inline code,
    // but to_doc() match arms can still reference the variant by name.
    //
    // Code bloat guard: `estimate_expansion_cost(body) * ref_count <= 4096`.
    // A rule's fused body is copied at every call site, so total expansion is
    // proportional to cost × sites. `value` (cost ~150, 20 sites → 3000) passes
    // the per-rule cost check but exceeds the total budget, preventing 33K-line
    // functions. `declaration` (cost ~280, 1–2 sites → ≤560) stays fused.
    let mut ref_counts = vec![0u32; ir.rules.len()];
    for rule in &ir.rules {
        count_refs_vec(&rule.body, &mut ref_counts);
    }

    let fusion_eligible: Vec<bool> = ir
        .rules
        .iter()
        .enumerate()
        .map(|(i, rule)| {
            // @token rules always inline (small by definition).
            if rule.meta.is_token {
                return true;
            }
            // Don't inline cyclic, recoverable, pretty, or no_collapse rules.
            if rule.meta.is_cyclic
                || rule.meta.recover.is_some()
                || rule.meta.pretty.is_some()
                || rule.meta.no_collapse
            {
                return false;
            }
            // Inline rules with moderate expansion cost.
            // The estimate weights Ref nodes at 8 (IIFE + checkpoint + call + restore),
            // Alt branches at 5 each, Repeats at 10. This prevents:
            // - value (14 Ref branches, cost ~182): NOT inlined
            // - namedColor (147 literal branches): NOT inlined (max_alt_branches > 48)
            // - But allows dimension (4 Ref branches, cost ~52): inlined
            // - And small rules like ident/string: inlined
            max_alt_branches(&rule.body) <= 32 && estimate_expansion_cost(&rule.body) <= 80
        })
        .collect();

    // Phase 9: Pre-compute single-site inline eligibility for cyclic rules.
    // A cyclic rule qualifies when: (1) body has no self-reference, (2) ref count == 1,
    // (3) not the entry point (rule id 0).
    let single_site_inline = compute_single_site_inline(ir);

    for rule in &ir.rules {
        let name = ir.get_string(rule.name);
        let fn_ident = mono_fn_ident(name);
        let pub_ident = ctx.method_ident_for_name(name);
        let return_type = ctx.rule_return_type(rule.id);

        // Set no_collapse for @pretty / @no_collapse rules.
        ctx.no_collapse
            .set(rule.meta.no_collapse || rule.meta.pretty.is_some());

        // State-based memo is disabled for monolithic arena fns.
        // Rationale: monolithic fns have zero lazy/combinator construction overhead,
        // dispatch tables provide O(1) branch selection without ambiguity, and the
        // memo cache is dropped after each parse (no cross-iteration benefit).
        // The clone + HashMap insert per memoized call is pure overhead.
        // If a grammar truly needs packrat memo for correctness (ambiguous grammars),
        // this can be re-enabled selectively.
        let memo_id: Option<usize> = None;

        // ── Generate internal function body ──────────────────────────────

        let mut mctx = MonoCtx::new(fusion_eligible.clone(), single_site_inline.clone());

        // Fused number scan+convert: if the rule body is a JSON number regex,
        // emit number_scan_convert which returns (Span, f64) in one pass.
        // The enum variant stores (Span<'a>, f64) instead of plain Span.
        // Fused number: bare JSON number regex → (Span, f64) enum variant.
        // NumberConvert (from -> f64 map) is handled separately by emit_mono_map —
        // it produces f64 directly, NOT (Span, f64).
        let is_fused_number = match &rule.body {
            IrNode::Regex(sid) => fast_paths::is_fused_number_regex(ir.get_string(*sid)),
            _ => false,
        };

        // All internal fns return Option<ArenaEnum<'a>>.
        // Transparent rules: body emitted with elide_box=true (returns ArenaEnum directly).
        // Non-transparent rules: body emitted with elide_box=false, wrapped in enum variant.
        let body_expr = if is_fused_number && !rule.meta.is_transparent {
            let variant_ident = format_ident!("{}", name);
            let enum_ident = &ctx.enum_ident;
            quote! {
                ::parse_that::number_scan_convert(state)
                    .map(|__x| #enum_ident::#variant_ident(__x))
            }
        } else if rule.meta.is_transparent {
            emit_mono_expr(&rule.body, ctx, &mut mctx, true)
        } else {
            let variant_ident = format_ident!("{}", name);
            let enum_ident = &ctx.enum_ident;
            let inner = emit_mono_expr(&rule.body, ctx, &mut mctx, false);
            quote! { #inner.map(|__x| #enum_ident::#variant_ident(__x)) }
        };

        let hoisted = &mctx.hoisted;

        // ── Wrap body in memo check/store if memoized ────────────────────

        let fn_body = if let Some(id) = memo_id {
            let id_lit = proc_macro2::Literal::usize_unsuffixed(id);
            quote! {
                // Memo check.
                let __memo_key = state.offset;
                {
                    let __cache = state.memo.table_mut::<#enum_type>(#id_lit);
                    if let Some(__entry) = __cache.get(&__memo_key).cloned() {
                        return match __entry {
                            Some((__end, __val)) => {
                                state.offset = __end;
                                Some(__val)
                            }
                            None => None,
                        };
                    }
                }

                // Hoisted leaf parsers.
                #(#hoisted)*

                // Parse.
                let __result = (|| -> Option<#enum_type> {
                    #body_expr
                })();

                // Memo store.
                let __entry = __result.as_ref().map(|__v| (state.offset, __v.clone()));
                state.memo.table_mut::<#enum_type>(#id_lit).insert(__memo_key, __entry);
                __result
            }
        } else {
            quote! {
                #(#hoisted)*
                #body_expr
            }
        };

        // ── Emit internal function ───────────────────────────────────────

        let rule_debug = ir.debug_all || rule.meta.debug;
        let instrumented_body = if rule_debug {
            let trace_entry = super::trace::emit_trace_entry(name);
            let result_ident = syn::Ident::new("__trace_result", proc_macro2::Span::call_site());
            let trace_exit = super::trace::emit_trace_exit(name, &result_ident);
            quote! {
                #trace_entry
                let #result_ident = (|| -> Option<#enum_type> { #fn_body })();
                #trace_exit
                #result_ident
            }
        } else {
            fn_body
        };

        methods.push(quote! {
            #[allow(non_snake_case)]
            fn #fn_ident<'a>(
                state: &mut ::parse_that::ParserState<'a>,
            ) -> Option<#enum_type> {
                #instrumented_body
            }
        });

        // ── Emit public method(s) ────────────────────────────────────────

        if rule.meta.is_transparent {
            // Transparent: public method wraps result in arena.alloc.
            let helper_ident = ctx.arena_helper_ident();
            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                        let __v = Self::#fn_ident(state)?;
                        let __arena = #helper_ident(state);
                        Some(&*__arena.alloc(__v))
                    })
                }
            });

            // Unboxed variant: direct delegation.
            let unboxed_ident = ctx.unboxed_method_ident_for_name(name);
            methods.push(quote! {
                #[inline(always)]
                pub fn #unboxed_ident<'a>() -> Parser<'a, #enum_type> {
                    Parser::new(Self::#fn_ident)
                }
            });
        } else {
            // Non-transparent: direct delegation (fn already returns ArenaEnum).
            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    Parser::new(Self::#fn_ident)
                }
            });
        }
    }

    // Emit the thread-local depth counter if any rule is debug-instrumented.
    let has_debug = ir.debug_all || ir.rules.iter().any(|r| r.meta.debug);
    let depth_counter = if has_debug {
        super::trace::emit_depth_counter()
    } else {
        quote! {}
    };

    quote! {
        #depth_counter
        #(#methods)*
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
            // 1. Try known fast paths (css_ident_fast, number_scan_f64, etc.)
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

/// Emit a discarded expression (separator, open/close delimiter).
///
/// The value is thrown away, so skip enum/box wrapping. Returns `Option<_>`.
pub(super) fn emit_mono_discarded(
    node: &IrNode,
    strip_ow: bool,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
) -> TokenStream {
    match node {
        // Strip Map wrappers (value is discarded).
        IrNode::Map { inner, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. }
                | FnDescriptor::BoxWrap
                | FnDescriptor::Constant { .. } => {
                    emit_mono_discarded(inner, strip_ow, ctx, mctx)
                }
                _ => emit_mono_expr(node, ctx, mctx, false),
            }
        }
        // Strip OW in sep_by_ws context.
        IrNode::OptionalWhitespace(inner) if strip_ow => {
            emit_mono_discarded(inner, strip_ow, ctx, mctx)
        }
        // Phase 10: OW in discarded context — skip Span construction,
        // just trim ws and check inner (returns Option<()>).
        // Uses custom @ws pattern if configured.
        IrNode::OptionalWhitespace(inner) => {
            let ws_trim = emit_ws_trim(ctx, mctx);
            let inner_discarded = emit_mono_discarded(inner, false, ctx, mctx);

            // Loop invariant hoisting: skip redundant trailing trim when
            // inner already ends with OW.
            if expr::ends_with_ow(inner) {
                quote! {
                    {
                        #ws_trim
                        #inner_discarded
                    }
                }
            } else {
                let result_var = mctx.fresh("owd");
                let ws2 = ws_trim.clone();
                quote! {
                    {
                        #ws_trim
                        let #result_var = #inner_discarded;
                        if #result_var.is_some() {
                            #ws2
                        }
                        #result_var
                    }
                }
            }
        }
        // Literal: direct byte check, no Span construction.
        IrNode::Literal(sid) => {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let bytes = unescaped.as_bytes();
            // Dispatch guaranteed byte: skip bounds check if the byte is already
            // verified by the enclosing dispatch table match.
            if bytes.len() == 1 && mctx.dispatch_guaranteed_byte == Some(bytes[0]) {
                mctx.dispatch_guaranteed_byte = None;
                return quote! { { state.offset += 1; Some(()) } };
            }
            emit_literal_inline(&unescaped, false)
        }
        // For Ref: try fusion first (inline body in discarded context), then _sp path.
        IrNode::Ref(rule_id) => {
            // Fusion: inline discarded body of non-cyclic or single-site cyclic rules.
            let can_inline = mctx.fusion_eligible.get(*rule_id as usize).copied() == Some(true)
                || mctx
                    .single_site_inline
                    .get(*rule_id as usize)
                    .copied()
                    == Some(true);
            if can_inline {
                let rule = &ctx.ir.rules[*rule_id as usize];
                let saved_no_collapse = ctx.no_collapse.get();
                ctx.no_collapse.set(false);
                let result = emit_mono_discarded(&rule.body, strip_ow, ctx, mctx);
                ctx.no_collapse.set(saved_no_collapse);
                return result;
            }
            // Always use monolithic fn call — never construct SpanParser combinators.
            // The monolithic function does the same parsing work without combinator overhead.
            let fn_ident = mono_fn_ident(ctx.resolve_rule_name(*rule_id));
            quote! { Self::#fn_ident(state) }
        }
        // Regex/other — emit via standard path.
        _ => emit_mono_expr(node, ctx, mctx, false),
    }
}

/// Emit direct byte-matching code for a literal string.
///
/// When `need_span` is true, returns `Option<Span<'a>>`.
/// When `need_span` is false (discarded context), returns `Option<()>`.
///
/// Single-byte literals (`:`, `,`, `{`, etc.) compile to a single byte comparison.
/// Multi-byte literals compile to a slice comparison.
pub(super) fn emit_literal_inline(unescaped: &str, need_span: bool) -> TokenStream {
    let bytes = unescaped.as_bytes();
    if bytes.is_empty() {
        if need_span {
            return quote! { Some(::parse_that::Span::new(state.offset, state.offset, state.src)) };
        } else {
            return quote! { Some(()) };
        }
    }
    if bytes.len() == 1 {
        let b_lit = proc_macro2::Literal::byte_character(bytes[0]);
        if need_span {
            quote! {
                {
                    if state.src_bytes.get(state.offset).copied() == Some(#b_lit) {
                        let __start = state.offset;
                        state.offset += 1;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    } else {
                        None
                    }
                }
            }
        } else {
            quote! {
                if state.src_bytes.get(state.offset).copied() == Some(#b_lit) {
                    state.offset += 1;
                    Some(())
                } else {
                    None
                }
            }
        }
    } else {
        let len = bytes.len();
        let byte_lits: Vec<proc_macro2::Literal> =
            bytes.iter().map(|b| proc_macro2::Literal::byte_character(*b)).collect();
        if need_span {
            quote! {
                {
                    let __end = state.offset + #len;
                    if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*]) {
                        let __start = state.offset;
                        state.offset = __end;
                        Some(::parse_that::Span::new(__start, __end, state.src))
                    } else {
                        None
                    }
                }
            }
        } else {
            quote! {
                {
                    let __end = state.offset + #len;
                    if state.src_bytes.get(state.offset..__end) == Some(&[#(#byte_lits),*]) {
                        state.offset = __end;
                        Some(())
                    } else {
                        None
                    }
                }
            }
        }
    }
}

/// Phase 11: Emit an unchecked single-byte literal check for use in separator positions
/// where `offset < end` is guaranteed by a preceding successful parse.
///
/// Returns `Option<()>` — discarded context only.
pub(super) fn emit_literal_inline_unchecked(byte: u8) -> TokenStream {
    let b_lit = proc_macro2::Literal::byte_character(byte);
    quote! {
        if unsafe { *state.src_bytes.get_unchecked(state.offset) } == #b_lit {
            state.offset += 1;
            Some(())
        } else {
            None
        }
    }
}

/// Fallback: build a combinator expression, hoist it, and call inline.
pub(super) fn emit_mono_fallback(
    node: &IrNode,
    ctx: &IrCodegenCtx<'_>,
    mctx: &mut MonoCtx,
    elide_box: bool,
) -> TokenStream {
    let parser = super::ir_node_to_tokens_elide(node, ctx, elide_box);
    let name = mctx.hoist(parser);
    quote! { #name.call(state) }
}

// ── Phase 9: Single-Site Inline Detection ────────────────────────────────────

/// Compute per-rule single-site inline eligibility.
///
/// A cyclic rule is single-site inline-eligible when ALL of:
/// 1. Rule body does NOT contain `Ref(self)` (no direct self-recursion)
/// 2. Rule has exactly 1 call site (reference count across all rule bodies == 1)
/// 3. Rule is NOT the grammar entry point (rule id != 0)
pub(super) fn compute_single_site_inline(ir: &GrammarIR) -> Vec<bool> {
    let n = ir.rules.len();
    let mut ref_counts = vec![0u32; n];
    for rule in &ir.rules {
        count_refs_vec(&rule.body, &mut ref_counts);
    }

    ir.rules
        .iter()
        .enumerate()
        .map(|(i, rule)| {
            rule.meta.is_cyclic
                && rule.id != 0
                && ref_counts[i] == 1
                && !body_has_self_ref(&rule.body, rule.id)
                && rule.meta.recover.is_none()
                && rule.meta.pretty.is_none()
                && !rule.meta.no_collapse
        })
        .collect()
}

/// Maximum Alt branch count in a tree (for fusion eligibility gating).
/// Returns the largest number of branches in any Alt node.
fn max_alt_branches(node: &IrNode) -> usize {
    match node {
        IrNode::Alt(branches, _) => {
            let inner_max = branches
                .iter()
                .map(|b| max_alt_branches(&b.node))
                .max()
                .unwrap_or(0);
            branches.len().max(inner_max)
        }
        IrNode::Seq(children) => children.iter().map(max_alt_branches).max().unwrap_or(0),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => max_alt_branches(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            max_alt_branches(a).max(max_alt_branches(b))
        }
        _ => 0,
    }
}

/// Estimate the code expansion cost of inlining a rule body.
/// Weights nodes by their codegen output size, not just node count.
/// A Ref generates ~8 lines (IIFE + checkpoint + call + restore).
/// An Alt branch generates ~5 lines of scaffolding per alternative.
fn estimate_expansion_cost(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => 2,
        IrNode::Ref(_) => 8, // IIFE closure + checkpoint save + fn call + restore
        IrNode::Seq(children) => {
            1 + children.iter().map(estimate_expansion_cost).sum::<usize>()
        }
        IrNode::Alt(branches, _) => {
            // Each branch: checkpoint save (1) + IIFE (2) + body + checkpoint restore (1) + error check (1)
            branches
                .iter()
                .map(|b| 5 + estimate_expansion_cost(&b.node))
                .sum::<usize>()
        }
        IrNode::Repeat { inner, .. } => {
            // Loop setup (Vec::new, loop {}, break check, push)
            10 + estimate_expansion_cost(inner)
        }
        IrNode::Map { inner, .. } => 2 + estimate_expansion_cost(inner),
        IrNode::OptionalWhitespace(inner) => 2 + estimate_expansion_cost(inner),
        IrNode::Negate(inner) => 3 + estimate_expansion_cost(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            estimate_expansion_cost(a) + estimate_expansion_cost(b)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            estimate_expansion_cost(token)
                + arms
                    .iter()
                    .map(|a| 3 + estimate_expansion_cost(&a.continuation))
                    .sum::<usize>()
                + estimate_expansion_cost(fallback)
        }
    }
}

/// Count references to each rule in an IrNode tree.
fn count_refs_vec(node: &IrNode, counts: &mut [u32]) {
    match node {
        IrNode::Ref(id) => {
            if let Some(c) = counts.get_mut(*id as usize) {
                *c += 1;
            }
        }
        IrNode::Seq(children) => {
            for c in children {
                count_refs_vec(c, counts);
            }
        }
        IrNode::Alt(branches, _) => {
            for b in branches {
                count_refs_vec(&b.node, counts);
            }
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => count_refs_vec(inner, counts),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            count_refs_vec(a, counts);
            count_refs_vec(b, counts);
        }
        IrNode::TokenDispatch { token, arms, fallback } => {
            count_refs_vec(token, counts);
            for arm in arms {
                count_refs_vec(&arm.continuation, counts);
            }
            count_refs_vec(fallback, counts);
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => {}
    }
}

/// Check if a node body contains a direct self-reference (Ref(rule_id)).
fn body_has_self_ref(node: &IrNode, rule_id: bbnf_ir::RuleId) -> bool {
    match node {
        IrNode::Ref(id) => *id == rule_id,
        IrNode::Seq(children) => children.iter().any(|c| body_has_self_ref(c, rule_id)),
        IrNode::Alt(branches, _) => {
            branches.iter().any(|b| body_has_self_ref(&b.node, rule_id))
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => body_has_self_ref(inner, rule_id),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            body_has_self_ref(a, rule_id) || body_has_self_ref(b, rule_id)
        }
        IrNode::TokenDispatch { token, arms, fallback } => {
            body_has_self_ref(token, rule_id)
                || arms.iter().any(|a| body_has_self_ref(&a.continuation, rule_id))
                || body_has_self_ref(fallback, rule_id)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
    }
}

