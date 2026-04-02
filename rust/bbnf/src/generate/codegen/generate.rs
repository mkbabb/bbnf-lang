//! Monolithic entry-point generation.
//!
//! Contains `generate_monolithic`  and supporting
//! helper functions for fusion eligibility, single-site inline detection,
//! and expansion cost estimation.

use bbnf_ir::{GrammarIR, IrNode, RuleId};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::regex;
use super::helpers::mono_fn_ident;
use super::ir_types::IrCodegenCtx;
use super::{MonoCtx, emit_mono_expr};

// ── Entry Point ──────────────────────────────────────────────────────────────

/// Generate all monolithic methods for all rules.
///
/// Arena-only: `fn __rule<'a>(state) -> Option<Enum<'a>>` with arena.alloc.
///
/// For each rule, emits:
/// 1. A private associated fn (internal dispatch)
/// 2. A public method returning `Parser<'a, ReturnType>`
/// 3. For transparent rules: an unboxed variant
pub fn generate_monolithic(ir: &GrammarIR, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let mut methods: Vec<TokenStream> = Vec::new();
    let enum_type = &ctx.enum_type;

    // Pre-compute fusion eligibility: non-cyclic, no @recover, no @pretty.
    // @token rules are always fusion-eligible — body inlined at call sites, but the enum
    // variant is preserved.
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
        .map(|rule| {
            // @token rules always inline (small by definition).
            if rule.meta.directives.token {
                return true;
            }
            // Don't inline cyclic, recoverable, or pretty rules.
            if rule.meta.is_cyclic || rule.meta.directives.recover.is_some() || rule.meta.directives.pretty.is_some() {
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

        // ── Generate internal function body ──────────────────────────────

        let mut mctx = MonoCtx::new(fusion_eligible.clone(), single_site_inline.clone());
        mctx.current_rule_id = Some(rule.id);

        // Fused number scan+convert: if the rule body is a JSON number regex,
        // emit number_scan_convert which returns (Span, f64) in one pass.
        // The enum variant stores (Span<'a>, f64) instead of plain Span.
        // Fused number: bare JSON number regex → (Span, f64) enum variant.
        // NumberConvert (from -> f64 map) is handled separately by emit_mono_map —
        // it produces f64 directly, NOT (Span, f64).
        // Skip when prettify is enabled — formatters only need Spans.
        let is_fused_number = if ctx.parser_attrs.prettify {
            false
        } else {
            match &rule.body {
                IrNode::Regex(sid) => regex::is_fused_number_regex(ir.get_string(*sid)),
                _ => false,
            }
        };

        // All internal fns return Option<Enum<'a>>.
        // Transparent rules: body emitted with elide_box=true (returns Enum directly).
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

        let fn_body = quote! {
            #(#hoisted)*
            #body_expr
        };

        // ── Emit internal function ───────────────────────────────────────

        let rule_debug = ir.debug_all || rule.meta.directives.debug;
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

        // ── Emit sync function + recovery wrapping ─────────────────────

        let has_recover = rule.meta.directives.recover.is_some() && !ctx.parser_attrs.skip_recover;

        if let Some(ref sync_node) = rule.meta.directives.recover {
            if !ctx.parser_attrs.skip_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let mut sync_mctx =
                    MonoCtx::new(fusion_eligible.clone(), single_site_inline.clone());
                let sync_body = emit_mono_expr(sync_node, ctx, &mut sync_mctx, false);
                let sync_hoisted = &sync_mctx.hoisted;
                methods.push(quote! {
                    #[allow(non_snake_case)]
                    fn #sync_ident<'a>(
                        state: &mut ::parse_that::ParserState<'a>,
                    ) -> Option<()> {
                        #(#sync_hoisted)*
                        (#sync_body).map(|_| ())
                    }
                });
            }
        }

        // ── Emit public method(s) ────────────────────────────────────────

        if rule.meta.is_transparent {
            // Transparent: public method wraps result in Box (Owned) or arena.alloc (Arena).
            let alloc_code = ctx.emit_alloc(&quote! { __v });

            let mut pub_parser = quote! {
                Parser::new(|state: &mut ::parse_that::ParserState<'a>| {
                    let __v = Self::#fn_ident(state)?;
                    Some(#alloc_code)
                })
            };

            if has_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let sentinel = ctx.recover_sentinel(rule.id);
                pub_parser = quote! {
                    #pub_parser.recover(Parser::new(Self::#sync_ident), #sentinel)
                };
            }

            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    #pub_parser
                }
            });

            // Unboxed variant: direct delegation (no recovery wrapping — unboxed
            // is used internally, recovery is on the public boxed method).
            let unboxed_ident = ctx.unboxed_method_ident_for_name(name);
            methods.push(quote! {
                #[inline(always)]
                pub fn #unboxed_ident<'a>() -> Parser<'a, #enum_type> {
                    Parser::new(Self::#fn_ident)
                }
            });
        } else {
            // Non-transparent: direct delegation (fn already returns Enum).
            let mut pub_parser = quote! { Parser::new(Self::#fn_ident) };

            if has_recover {
                let sync_ident = format_ident!("__sync_{}", name);
                let sentinel = ctx.recover_sentinel(rule.id);
                pub_parser = quote! {
                    #pub_parser.recover(Parser::new(Self::#sync_ident), #sentinel)
                };
            }

            methods.push(quote! {
                pub fn #pub_ident<'a>() -> Parser<'a, #return_type> {
                    #pub_parser
                }
            });
        }
    }

    // Emit the thread-local depth counter if any rule is debug-instrumented.
    let has_debug = ir.debug_all || ir.rules.iter().any(|r| r.meta.directives.debug);
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

// ── Phase 9: Single-Site Inline Detection ────────────────────────────────────

/// Compute per-rule single-site inline eligibility.
///
/// A cyclic rule is single-site inline-eligible when ALL of:
/// 1. Rule body does NOT contain `Ref(self)` (no direct self-recursion)
/// 2. Rule has exactly 1 call site (reference count across all rule bodies == 1)
/// 3. Rule is NOT the grammar entry point (rule id != 0)
pub(crate) fn compute_single_site_inline(ir: &GrammarIR) -> Vec<bool> {
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
                && rule.meta.directives.recover.is_none()
                && rule.meta.directives.pretty.is_none()
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
        IrNode::Seq(children) => 1 + children.iter().map(estimate_expansion_cost).sum::<usize>(),
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
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
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
fn body_has_self_ref(node: &IrNode, rule_id: RuleId) -> bool {
    match node {
        IrNode::Ref(id) => *id == rule_id,
        IrNode::Seq(children) => children.iter().any(|c| body_has_self_ref(c, rule_id)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| body_has_self_ref(&b.node, rule_id)),
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => body_has_self_ref(inner, rule_id),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            body_has_self_ref(a, rule_id) || body_has_self_ref(b, rule_id)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            body_has_self_ref(token, rule_id)
                || arms
                    .iter()
                    .any(|a| body_has_self_ref(&a.continuation, rule_id))
                || body_has_self_ref(fallback, rule_id)
        }
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon => false,
    }
}
