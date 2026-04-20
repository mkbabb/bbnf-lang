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
pub mod unordered;
pub mod value_materialize;
pub mod wrap;

use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

pub use dispatcher::{
    dispatcher_fn_ident, has_w4_classified, visitor_dispatcher_fn_ident,
};

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
    let mut per_rule: Vec<TokenStream> = Vec::new();
    let mut per_rule_visitor: Vec<TokenStream> = Vec::new();

    // AW-V.W4-activation — visitor-path emission gates on W4-absence.
    // The visitor dispatcher's generic-V bound is narrow by design
    // (W3 sub-traits only); W4 visitor sub-traits (`PrattVisitor` and
    // similar) can't be added without rippling through every caller's
    // bound. Grammars carrying W4-classified rules emit the tape path
    // only; the visitor path activates for them in a follow-on wave
    // alongside the per-Ref `__value` dispatcher refactor.
    let emit_visitor_path = !dispatcher::has_w4_classified(ir);

    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
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
        let fragment = match tag {
            ShapeTag::Object => object::emit_parse_object(&grammar_suffix, rule, ir),
            ShapeTag::Array => array::emit_parse_array(&grammar_suffix, rule, ir),
            ShapeTag::String => string::emit_parse_string(&grammar_suffix, rule, ir),
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
                    hregex::emit_parse_number_via_hregex(&grammar_suffix, rule, ir)
                } else {
                    number::emit_parse_number(&grammar_suffix, rule, ir)
                }
            }
            ShapeTag::Keyword => keyword::emit_parse_keyword(&grammar_suffix, rule, ir),
            ShapeTag::Scalar => scalar::emit_parse_scalar(&grammar_suffix, rule, ir),
            ShapeTag::Pratt => pratt::emit_parse_pratt(&grammar_suffix, rule, ir),
            ShapeTag::Unordered => unordered::emit_parse_unordered(&grammar_suffix, rule, ir),
            ShapeTag::ArgList => arglist::emit_parse_arglist(&grammar_suffix, rule, ir),
            ShapeTag::Flat => flat::emit_parse_flat(&grammar_suffix, rule, ir),
            ShapeTag::Wrap => wrap::emit_parse_wrap(&grammar_suffix, rule, ir),
            ShapeTag::HRegex => hregex::emit_parse_hregex(&grammar_suffix, rule, ir),
            ShapeTag::AltDispatch => {
                alt_dispatch::emit_parse_alt_dispatch(&grammar_suffix, rule, ir)
            }
            ShapeTag::None => continue,
        };
        per_rule.push(fragment);

        if emit_visitor_path {
            // AX.W0a.1 — emit visitor-path fns for every shape whose
            // body stays within the visitor dispatcher's W3 bound set
            // (`ObjectVisitor + ArrayVisitor + StringVisitor +
            // NumberVisitor + KeywordVisitor`). Flat / Wrap / ArgList /
            // HRegex visitor emitters declare bounds that are a strict
            // subset of that union; only Pratt / Unordered carry W4-
            // specific bounds (`PrattVisitor` and similar) that would
            // require widening the dispatcher's generic `V` — and
            // grammars with those rules gate the visitor path off
            // wholesale via [`dispatcher::has_w4_classified`].
            let visitor_fragment = match tag {
                ShapeTag::Object => object::emit_parse_object_visitor(&grammar_suffix, rule, ir),
                ShapeTag::Array => array::emit_parse_array_visitor(&grammar_suffix, rule, ir),
                ShapeTag::String => string::emit_parse_string_visitor(&grammar_suffix, rule, ir),
                ShapeTag::Number => {
                    // AX.W0a.2.q — parallel to the tape-path routing
                    // above: lenient-number visitor emission routes
                    // through the HRegex-backed regex-scan path.
                    if hregex::number_rule_allows_leading_dot(rule, ir) {
                        hregex::emit_parse_number_visitor_via_hregex(&grammar_suffix, rule, ir)
                    } else {
                        number::emit_parse_number_visitor(&grammar_suffix, rule, ir)
                    }
                }
                ShapeTag::Keyword => {
                    keyword::emit_parse_keyword_visitor(&grammar_suffix, rule, ir)
                }
                ShapeTag::Scalar => scalar::emit_parse_scalar_visitor(&grammar_suffix, rule, ir),
                ShapeTag::Flat => flat::emit_parse_flat_visitor(&grammar_suffix, rule, ir),
                ShapeTag::Wrap => wrap::emit_parse_wrap_visitor(&grammar_suffix, rule, ir),
                ShapeTag::ArgList => {
                    arglist::emit_parse_arglist_visitor(&grammar_suffix, rule, ir)
                }
                ShapeTag::HRegex => hregex::emit_parse_hregex_visitor(&grammar_suffix, rule, ir),
                ShapeTag::AltDispatch => {
                    alt_dispatch::emit_parse_alt_dispatch_visitor(&grammar_suffix, rule, ir)
                }
                // Pratt / Unordered need W4-specific trait bounds outside
                // the dispatcher's W3 union; `has_w4_classified` gates
                // the entire visitor path off before we reach this arm
                // for grammars with those rules. The `_` is a defensive
                // guard, not an active code path.
                ShapeTag::Pratt | ShapeTag::Unordered | ShapeTag::None => quote! {},
            };
            per_rule_visitor.push(visitor_fragment);
        }
    }

    if per_rule.is_empty() {
        return quote! {};
    }

    let support = dispatcher::emit_support_module(&grammar_suffix, ir);
    let dispatcher_fn = dispatcher::emit_dispatcher(&grammar_suffix, ir);
    let visitor_dispatcher_fn = if emit_visitor_path {
        dispatcher::emit_visitor_dispatcher(&grammar_suffix, ir)
    } else {
        quote! {}
    };
    quote! {
        #support
        #(#per_rule)*
        #(#per_rule_visitor)*
        #dispatcher_fn
        #visitor_dispatcher_fn
    }
}


/// Returns `true` when `ir` has at least one shape-classified rule.
/// Used internally by [`dispatcher`] to choose between root-delegation
/// and Alt-dispatch bodies.
pub(super) fn has_shape_dispatch(ir: &GrammarIR) -> bool {
    ir.rules.iter().any(|rule| {
        !rule.meta.is_transparent
            && ir.shape_assignments.get(rule.id).is_classified()
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
