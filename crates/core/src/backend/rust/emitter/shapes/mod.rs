//! AW-V.W3.2 — Per-shape emitter modules.
//!
//! # Role — AW-V.W3.2
//!
//! Consumes [`ShapeAssignments`] from W3.1 to emit per-(grammar, rule)
//! inline parse functions mirroring the hand-tuned
//! `crates/bbnf-json-prototype/` shape. For each rule whose
//! [`ShapeTag`] is [`ShapeTag::is_w3_classified`] the matching
//! shape-emitter module produces one `parse_<shape>_<grammar>_<rule>`
//! function carrying `#[inline(always)]`; the dispatcher emitter wires
//! the top-level entry `parse_<grammar>_<root>` that the grammar's
//! `parse()` call site invokes.
//!
//! # Module layout
//!
//! ## W3.2 — JSON shape emitters (active; consumed by
//! [`emit_shapes_for_grammar`]):
//!
//! - [`object`] — `parse_object_<grammar>_<rule>` emitter.
//! - [`array`] — `parse_array_<grammar>_<rule>` emitter.
//! - [`string`] — `parse_string_<grammar>_<rule>` emitter (quoted-
//!   string SIMD body via `bbnf-simd-scan::emit::quoted_string_simd_body`
//!   / `first_quote_or_backslash`).
//! - [`number`] — `parse_number_<grammar>_<rule>` emitter (Eisel-Lemire
//!   body via `bbnf-simd-scan::emit::eisel_lemire_body`).
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

pub mod arglist;
pub mod array;
pub mod dispatcher;
pub mod flat;
pub mod hregex;
pub mod keyword;
pub mod number;
pub mod object;
pub mod pratt;
pub mod scalar;
pub mod string;
pub mod unordered;
pub mod wrap;

use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

pub use dispatcher::{dispatcher_fn_ident, visitor_dispatcher_fn_ident};

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
    // Only emit the shape fn family when the grammar's dispatch
    // shape is consumable by the dispatcher — otherwise the emitted
    // fns compile but go unused, and the dispatcher could emit
    // incorrect routing. JSON's `value = object|array|...` pattern
    // satisfies the admission; grammars where the root rule is
    // itself a Wrap / Repeat / Seq currently defer to the walker.
    if !has_full_shape_coverage(ir) {
        return quote! {};
    }

    let grammar_suffix = sanitise_grammar(grammar_ident_str);
    let mut per_rule: Vec<TokenStream> = Vec::new();
    let mut per_rule_visitor: Vec<TokenStream> = Vec::new();

    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let tag = ir.shape_assignments.get(rule.id);
        // W3 shapes have complete emitter + dispatcher wiring.
        // W4 shapes (Pratt / Unordered / ArgList / Flat / Wrap /
        // HRegex) land their emitter scaffolding in W4.1 but are
        // not consumed by the dispatcher until W4.2 / W4.3 wire the
        // per-grammar consumer routes. Skip W4 tags here so the
        // scaffolding compiles without being called; they get
        // activated grammar-by-grammar in subsequent sub-waves.
        if !tag.is_w3_classified() {
            continue;
        }
        let fragment = match tag {
            ShapeTag::Object => object::emit_parse_object(&grammar_suffix, rule, ir),
            ShapeTag::Array => array::emit_parse_array(&grammar_suffix, rule, ir),
            ShapeTag::String => string::emit_parse_string(&grammar_suffix, rule, ir),
            ShapeTag::Number => number::emit_parse_number(&grammar_suffix, rule, ir),
            ShapeTag::Keyword => keyword::emit_parse_keyword(&grammar_suffix, rule, ir),
            ShapeTag::Scalar => scalar::emit_parse_scalar(&grammar_suffix, rule, ir),
            _ => continue,
        };
        per_rule.push(fragment);

        // AW-V.W3-bench-fix — visitor-path per-shape fns.
        let visitor_fragment = match tag {
            ShapeTag::Object => object::emit_parse_object_visitor(&grammar_suffix, rule, ir),
            ShapeTag::Array => array::emit_parse_array_visitor(&grammar_suffix, rule, ir),
            ShapeTag::String => string::emit_parse_string_visitor(&grammar_suffix, rule, ir),
            ShapeTag::Number => number::emit_parse_number_visitor(&grammar_suffix, rule, ir),
            ShapeTag::Keyword => keyword::emit_parse_keyword_visitor(&grammar_suffix, rule, ir),
            ShapeTag::Scalar => scalar::emit_parse_scalar_visitor(&grammar_suffix, rule, ir),
            _ => quote! {},
        };
        per_rule_visitor.push(visitor_fragment);
    }

    if per_rule.is_empty() {
        return quote! {};
    }

    let support = dispatcher::emit_support_module(&grammar_suffix);
    let dispatcher_fn = dispatcher::emit_dispatcher(&grammar_suffix, ir);
    let visitor_dispatcher_fn = dispatcher::emit_visitor_dispatcher(&grammar_suffix, ir);
    quote! {
        #support
        #(#per_rule)*
        #(#per_rule_visitor)*
        #dispatcher_fn
        #visitor_dispatcher_fn
    }
}

/// Returns `true` when `ir` has at least one shape-classified rule
/// (so `parse()` should route through the shape dispatcher).
pub fn has_shape_dispatch(ir: &GrammarIR) -> bool {
    ir.rules.iter().any(|rule| {
        !rule.meta.is_transparent
            && ir.shape_assignments.get(rule.id).is_w3_classified()
    })
}

/// Returns `true` when `ir` admits full shape dispatch — every non-
/// transparent rule classifies to a W3 shape OR the root rule is an
/// Alt whose branches all resolve to W3 shape fns.
///
/// The JSON grammar satisfies the Alt branch: `value = object | array
/// | string | number | bool | null` — six Refs, each landing on a W3
/// shape (`pair` is Seq of Ref+Ref+Ref and stays unshaped but is
/// consumed inside the Object shape's inline-key-value loop).
///
/// W4 shapes (Pratt / Unordered / ArgList / Flat / Wrap / HRegex)
/// are EXCLUDED from this admission predicate. The W4 emitter
/// scaffolding landed in W4.1 but the per-grammar consumer wiring
/// lands in W4.2 (CSS L4) and W4.3 (Sheets + BBNF); until that
/// wiring is in place, grammars carrying W4-classified rules stay
/// on the walker fallback. This preserves the substrate-with-
/// consumer landing rule declared in AW-V.md §invariants.
///
/// When this returns `false`, `parse()` routes through the existing
/// `dta_run_<grammar>` walker — the cold-path AX replay surface.
pub fn has_full_shape_coverage(ir: &GrammarIR) -> bool {
    // Find the entry rule. When it's a 6-arm Alt over shape-bearing
    // Refs (the JSON `value = object|array|string|number|bool|null`
    // pattern), we admit shape dispatch regardless of whether every
    // rule classifies — the Alt branches cover the dispatch surface.
    let Some(entry_rule) = ir.rules.iter().find(|r| r.id == ir.entry) else {
        return false;
    };
    use bbnf_ir::IrNode;
    if let IrNode::Alt(branches, _) = &entry_rule.body {
        // Every branch must be a Ref to a shape-classified rule.
        return branches.iter().all(|b| match &b.node {
            IrNode::Ref(rid) => {
                ir.shape_assignments.get(*rid).is_w3_classified()
            }
            _ => false,
        });
    }
    // Fallback: every non-transparent rule is shape-classified.
    ir.rules.iter().all(|rule| {
        rule.meta.is_transparent
            || ir.shape_assignments.get(rule.id).is_w3_classified()
    })
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
