//! AW-V.W3.2 — Per-shape emitter modules.
//!
//! # Role — AW-V.W3.2
//!
//! Consumes [`ShapeAssignments`] from W3.1 to emit per-(grammar, rule)
//! inline parse functions mirroring the hand-tuned
//! `crates/json-prototype/` shape. For each rule whose
//! [`ShapeTag`] is [`ShapeTag::is_w3_classified`] the matching
//! shape-emitter module produces one `parse_<shape>_<grammar>_<rule>`
//! function; leaf shape fns (Keyword / Number / String / HRegex /
//! literal-Scalar) carry `#[inline(always)]`, compound shape fns
//! (Object / Array / Flat / Wrap / ArgList / Pratt / Unordered /
//! AltDispatch / Ref-Scalar + the grammar's top-level dispatchers)
//! carry plain `#[inline]` so LLVM's inliner collapses cross-shape
//! recursive edges without unrolling indefinitely (AX.W0a.2.f).
//! The dispatcher emitter wires the top-level entry
//! `parse_<grammar>_<root>` that the grammar's `parse()` call site
//! invokes.
//!
//! # Module layout
//!
//! ## W3.2 — JSON shape emitters (active; consumed by
//! [`emit_shapes_for_grammar`]):
//!
//! - [`object`] — `parse_object_<grammar>_<rule>` emitter.
//! - [`array`] — `parse_array_<grammar>_<rule>` emitter.
//! - [`string`] — `parse_string_<grammar>_<rule>` emitter (quoted-
//!   string SIMD body via `simd-scan::emit::quoted_string_simd_body`
//!   / `first_quote_or_backslash`).
//! - [`number`] — `parse_number_<grammar>_<rule>` emitter (Eisel-Lemire
//!   body via `simd-scan::emit::eisel_lemire_body`).
//! - [`keyword`] — `parse_keyword_<grammar>_<rule>` emitter (literal or
//!   short Alt-of-literal dispatch).
//! - [`scalar`] — `parse_scalar_<grammar>_<rule>` emitter (single-leaf
//!   fallback for non-structural rules).
//! - [`dispatcher`] — top-level `parse_<grammar>_<root>` + the shared
//!   `ScanState` / whitespace-skip emission.
//!
//! ## W4.1 — CSS L4 + Sheets shape emitters (substrate landed; W4.2 /
//! W4.3 wire the per-grammar consumers):
//!
//! - [`pratt`] — `parse_pratt_<grammar>_<rule>` emitter for
//!   operator-chain head rules (Sheets operator tower, CSS calc /
//!   min / max).
//! - [`unordered`] — `parse_unordered_<grammar>_<rule>` emitter for
//!   `Repeat { lo: 1, .. }` over a disjoint-FIRST Alt (CSS
//!   `compoundSelector` canonical case).
//! - [`arglist`] — `parse_arglist_<grammar>_<rule>` emitter for
//!   `name(arg, arg, …)` positional calls (CSS function family,
//!   Sheets `func_call` / `let_call` / `lambda_call`).
//! - [`flat`] — `parse_flat_<grammar>_<rule>` emitter for typed
//!   `Seq(literal_head, body+)` rules (CSS `*Decl` family, BBNF
//!   directive bodies).
//! - [`wrap`] — `parse_wrap_<grammar>_<rule>` emitter for transparent
//!   `Alt(Ref, Ref, …)` dispatchers (JSON `value`, CSS `color` /
//!   `atRule`, Sheets `range_end`).
//! - [`hregex`] — `parse_hregex_<grammar>_<rule>` emitter for regex
//!   leaves with host decode (CSS `ident`, Sheets `cell_ref` /
//!   `identifier`, BBNF `identifier`).
//!
//! # Wire contract
//!
//! Each per-shape emitter takes `(grammar_ident, rule, ir)` and returns
//! a [`TokenStream`]; [`emit_shapes_for_grammar`] walks the IR once,
//! consults the per-rule [`ShapeTag`] on [`GrammarIR::shape_assignments`],
//! and composes the per-shape streams plus the dispatcher into the
//! output consumed by `emit_grammar_impl`.
//!
//! Rules absent from [`ShapeAssignments`] (`ShapeTag::None`) route
//! through `__dta_walker_inline::run` per the AX cold-path replay
//! contract — their codegen path is unchanged.

pub mod alt_dispatch;
pub mod arglist;
pub mod array;
pub mod dispatcher;
pub mod flat;
pub mod hregex;
pub(crate) mod inline;
pub mod keyword;
pub mod number;
pub mod object;
pub mod pratt;
pub mod scalar;
pub mod string;
pub mod substrate;
pub mod unordered;
pub mod wrap;

use bbnf_ir::GrammarIR;
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use proc_macro2::TokenStream;
use quote::quote;

use bbnf_ir::registry::EmitStrategy;

pub use dispatcher::dispatcher_fn_ident;

// AZ-I.W2-act.A — `registry_observer` deleted per audit/AUDIT-2 §6.C.
// The sub-module's docstring (lines 31-34 pre-delete) self-documented
// as removable at AZ-I close once the bridge mode collapses; the
// `record` write-only sink had no production reader and the
// `drain` / `clear` accessors were test-only consumers in
// `tests/emitter_registry_read.rs` (also retired).

/// Sanitise a grammar identifier into a Rust ident fragment.
///
/// Mirrors the sibling helper in `emitter::dta_walker::mod.rs` so
/// cross-emitter symbol composition agrees byte-for-byte.
pub(crate) fn sanitise_grammar(grammar: &str) -> String {
    let mut s = String::with_capacity(grammar.len());
    for ch in grammar.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            s.push(ch);
        } else {
            s.push('_');
        }
    }
    s
}

/// Emit the full per-shape + dispatcher stream for every rule on `ir`.
///
/// `grammar_ident_str` is the sanitised grammar symbol the caller
/// already computed (matching `dta_walker`'s sanitise_grammar output);
/// it is embedded into the emitted `parse_<shape>_<grammar>_<rule>`
/// symbols so cross-emitter compositions agree.
///
/// Walks [`GrammarIR::rules`], consults
/// [`GrammarIR::shape_assignments`] per rule, and dispatches to the
/// matching per-shape emitter. Unclassified rules contribute nothing
/// to the stream — they continue through the walker fallback in
/// `__dta_walker_inline::run`.
///
/// The dispatcher emitter runs once at the end, producing the
/// grammar's top-level `parse_<grammar>_<root>` entry point. When
/// the grammar has no shape-classified rules the dispatcher emits an
/// empty token stream — `emit_grammar_impl` gates on the same
/// condition to decide whether `parse()` routes through shape
/// dispatch or the existing `dta_run_<grammar>`.
pub fn emit_shapes_for_grammar(grammar_ident_str: &str, ir: &GrammarIR) -> TokenStream {
    // AX.W0b.A — the coverage gate retired alongside the walker;
    // every grammar admits shape emission post-W0a.2.h. Unclassified
    // rules (`ShapeTag::None`) contribute nothing to the stream —
    // they fall through to the dispatcher's classified-branch path
    // (JSON Alt-of-Refs) or inline per-Ref routing.
    let grammar_suffix = sanitise_grammar(grammar_ident_str);

    // AZ-I.W2.RA / AZ-II.cutover.O4 — codegen-time substrate
    // selection is fail-closed. `EmitStrategy::for_grammar` resolves
    // the grammar identity plus registry-population state to the
    // concrete StructDirect binding. The strategy is bound here once
    // and propagated to per-shape emitter call sites; the strategy
    // decision lives at codegen time, with no runtime conditional
    // inside a single emitted parse fn.
    let strategy = EmitStrategy::for_grammar(grammar_ident_str, &ir.struct_registry);
    debug_assert!(strategy.is_struct_direct());

    let mut per_rule: Vec<TokenStream> = Vec::new();

    for rule in &ir.rules {
        // AZ-II.cutover.H Phase 1 — transparent-rule passthrough
        // emission. The pre-cutover.H gate
        // `if rule.meta.is_transparent { continue; }` skipped per-rule
        // fn emission for ALL transparent rules. The Ref-call
        // resolver at `dispatcher::ref_call::emit_ref_call_*` resolves
        // calls via `parse_<shape>_<grammar>_<name>` based on the
        // target's classified shape — so cross-rule references to
        // transparent rules fail to link when the per-shape fn isn't
        // emitted.
        //
        // AZ-II.cutover.L Phase 3a — uniform transparent-rule emission
        // for Wrap and Keyword classifications under both substrate
        // strategies.
        //
        // The Ref-call resolver at `dispatcher::ref_call::emit_ref_call_*`
        // emits calls via `parse_<shape>_<grammar>_<name>` based on the
        // target's classified shape, regardless of whether the target is
        // transparent. Cross-rule references to transparent rules
        // therefore fail to link when the per-shape fn isn't emitted.
        //
        // - **Wrap** transparent rules: the StructDirect emitter uses
        //   `emit_alt_struct_dispatch_transparent`, so no outer
        //   compound is opened for alias-only rules.
        //
        // - **Keyword** transparent rules: CSS L4's `pseudoClass = isPseudo
        //   | … | classicPseudo` (transparent, Alt-of-Ref-led, classifies
        //   as Keyword via the `leading_literal` traversal). The
        //   Keyword-shape Ref-led emission template (added in this
        //   phase) carries dispatch + delegate to each named-pseudo
        //   sub-rule.
        //
        // Other transparent shapes (Object / Array / Flat / Pratt /
        // Unordered / ArgList / String / Number / Scalar) are not
        // expected to appear cyclically with Alt-of-Ref bodies in any
        // active grammar; if a future grammar lands such a rule the
        // missing per-shape fn will surface as a regen-time link error,
        // which the corresponding transparent-aware emitter will
        // address.
        if rule.meta.is_transparent {
            let tag = ir.shape_assignments.get(rule.id);
            let emit_transparent = matches!(tag, ShapeTag::Wrap | ShapeTag::Keyword);
            if !emit_transparent {
                continue;
            }
        }
        let tag = ir.shape_assignments.get(rule.id);
        // AW-V.W4-activation — emit per-shape fns for BOTH W3-active
        // shapes (Object / Array / String / Number / Keyword / Scalar)
        // AND W4-active shapes (Pratt / Unordered / ArgList / Flat /
        // Wrap / HRegex). The W4 emitters landed functional bodies
        // under AW-V.W4-fix; this activation wires them into the
        // dispatcher's call surface. Unclassified rules (`ShapeTag::None`)
        // continue routing through `__dta_walker_inline::run` per the
        // AX cold-path replay contract.
        if !tag.is_classified() {
            continue;
        }

        // AZ-I.W2-act.A — registry-read on every compound-emission
        // boundary.
        //
        // Each shape-classified non-transparent rule is a compound
        // emission boundary: the matching per-shape emitter writes one
        // `parse_<shape>_<grammar>_<rule>` function whose body opens
        // and closes a compound record. Every such boundary consults
        // [`bbnf_ir::StructRegistry`] for the rule's projected layout;
        // the read fires unconditionally so no `is_some` short-circuit
        // bypasses populated grammars (per
        // `feedback_no-orthogonal-codepaths`).
        //
        // The W2-substrate-era `registry_observer::record` sink retired
        // at AZ-I.W2-act.A close per `audit/AUDIT-2` §6.C — it was a
        // write-only diagnostic with no production reader. The layout
        // read itself stays load-bearing: W2 / W3 per-shape emitters
        // thread it as the struct-builder shape via `_registry_layout`.
        let layout = ir.struct_registry.layout(rule.id);
        // Bind the layout reference so it survives to the per-shape
        // emit calls (W2 / W3 thread it as the struct-builder shape).
        // The let-binding prevents the read from being optimised into
        // a side-effect-free no-op.
        let _registry_layout: Option<&bbnf_ir::StructLayout> = layout;

        let fragment = match tag {
            ShapeTag::Object => object::emit_parse_object(&grammar_suffix, rule, ir, &strategy),
            ShapeTag::Array => array::emit_parse_array(&grammar_suffix, rule, ir, &strategy),
            ShapeTag::String => string::emit_parse_string(&grammar_suffix, rule, ir, &strategy),
            ShapeTag::Number => {
                // AX.W0a.2.q — lenient-number routing: when the rule's
                // regex classification admits leading-dot literals
                // (CSS `.5` / `.25e3`), emit the Number-named fn via
                // the HRegex regex-scan path so the per-grammar regex
                // adapter handles admission. The default Number
                // emitter's inline scanner rejects `.5` (requires at
                // least one integer digit before the dot); routing
                // lenient dialects through regex-scan closes the gap
                // without modifying the strict scanner CSS's other
                // callers (JSON) depend on.
                if hregex::number_rule_allows_leading_dot(rule, ir) {
                    hregex::emit_parse_number_via_hregex(&strategy, &grammar_suffix, rule, ir)
                } else {
                    number::emit_parse_number(&grammar_suffix, rule, ir, &strategy)
                }
            }
            ShapeTag::Keyword => keyword::emit_parse_keyword(&grammar_suffix, rule, ir, &strategy),
            ShapeTag::Scalar => scalar::emit_parse_scalar(&grammar_suffix, rule, ir, &strategy),
            ShapeTag::Pratt => pratt::emit_parse_pratt(&strategy, &grammar_suffix, rule, ir),
            ShapeTag::Unordered => {
                unordered::emit_parse_unordered(&strategy, &grammar_suffix, rule, ir)
            }
            ShapeTag::ArgList => arglist::emit_parse_arglist(&strategy, &grammar_suffix, rule, ir),
            ShapeTag::Flat => flat::emit_parse_flat(&strategy, &grammar_suffix, rule, ir),
            ShapeTag::Wrap => wrap::emit_parse_wrap(&grammar_suffix, rule, ir, &strategy),
            ShapeTag::HRegex => hregex::emit_parse_hregex(&strategy, &grammar_suffix, rule, ir),
            ShapeTag::AltDispatch => {
                alt_dispatch::emit_parse_alt_dispatch(&grammar_suffix, rule, ir, &strategy)
            }
            ShapeTag::None => continue,
        };
        per_rule.push(fragment);
    }

    if per_rule.is_empty() {
        return quote! {};
    }

    let support = dispatcher::emit_support_module(&grammar_suffix, ir);
    let dispatcher_fn = dispatcher::emit_dispatcher(&grammar_suffix, ir, &strategy);
    quote! {
        #support
        #(#per_rule)*
        #dispatcher_fn
    }
}

/// Returns `true` when `ir` has at least one shape-classified rule.
/// Used internally by [`dispatcher`] to choose between root-delegation
/// and Alt-dispatch bodies.
pub(super) fn has_shape_dispatch(ir: &GrammarIR) -> bool {
    ir.rules
        .iter()
        .any(|rule| !rule.meta.is_transparent && ir.shape_assignments.get(rule.id).is_classified())
}

/// Resolve the grammar's root rule per [`GrammarIR::entry`]. Returns
/// the interned rule name as a `String` — the dispatcher emitter
/// embeds it into the emitted `parse_<grammar>_<root>` symbol.
pub fn root_rule_name(ir: &GrammarIR) -> Option<String> {
    ir.rules
        .iter()
        .find(|r| r.id == ir.entry)
        .map(|r| ir.get_string(r.name).to_string())
}
