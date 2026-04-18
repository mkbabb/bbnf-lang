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
    // AW-V.W4-activation — emit the shape fn family when the grammar's
    // dispatch shape is consumable by the dispatcher. The admission
    // gate ([`has_full_shape_coverage`]) covers JSON's Alt-of-Refs
    // pattern plus the W4-activated forms (Wrap-rooted Alt dispatcher
    // OR a W3/W4 classified root whose shape fn handles recursion
    // through the `__value` dispatcher). Grammars that don't meet the
    // gate continue through the walker fallback per the AX cold-path
    // replay contract.
    if !has_full_shape_coverage(ir) {
        return quote! {};
    }

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
            ShapeTag::Number => number::emit_parse_number(&grammar_suffix, rule, ir),
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
                ShapeTag::Number => number::emit_parse_number_visitor(&grammar_suffix, rule, ir),
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

    let support = dispatcher::emit_support_module(&grammar_suffix);
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

/// Returns `true` when `ir` has at least one shape-classified rule
/// (so `parse()` should route through the shape dispatcher).
pub fn has_shape_dispatch(ir: &GrammarIR) -> bool {
    ir.rules.iter().any(|rule| {
        !rule.meta.is_transparent
            && ir.shape_assignments.get(rule.id).is_classified()
    })
}

/// Returns `true` when `ir` admits shape dispatch — the grammar has
/// W3- or W4-classified rules whose per-shape emitters land as
/// substrate for the runtime consumer.
///
/// Admission criteria (AW-V.W4-activation):
///
/// 1. **Alt-of-Refs entry.** The entry rule's body is `Alt(Ref, Ref, ...)`
///    and every branch Ref resolves to a shape-classified rule
///    (W3 or W4). Covers JSON's `value = object | array | string |
///    number | bool | null`.
///
/// 2. **Classified entry rule.** The entry rule itself carries a W3 or
///    W4 shape tag. Covers CSS L4 (`stylesheet` → Array-tagged via
///    list-rule detection), Sheets (`formula` → Flat), BBNF (`grammar`
///    → Array).
///
/// Admission drives substrate emission via [`emit_shapes_for_grammar`]:
/// every classified rule gets its per-shape `parse_<shape>_<grammar>_
/// <rule>` function compiled. Rules absent from [`ShapeAssignments`]
/// (`ShapeTag::None`) continue routing through `__dta_walker_inline::run`
/// per the AX cold-path replay contract.
///
/// See [`has_shape_dispatcher_entrypoint`] for the companion gate that
/// decides whether `parse()` invokes the shape dispatcher or falls
/// through to the walker. Admission is broader than entrypoint
/// routing: a grammar may admit substrate emission (shape fns compile
/// per-rule) without the top-level `parse()` routing through the
/// dispatcher yet — this is the architectural split the W4-activation
/// sub-wave uses to land W4 emitters as substrate before the
/// per-Ref-routed `__value` dispatcher refactor lands in a follow-on.
pub fn has_full_shape_coverage(ir: &GrammarIR) -> bool {
    let Some(entry_rule) = ir.rules.iter().find(|r| r.id == ir.entry) else {
        return false;
    };
    use bbnf_ir::IrNode;

    // Criterion 1 — Alt-of-Refs entry with every branch classified.
    if let IrNode::Alt(branches, _) = &entry_rule.body {
        return branches.iter().all(|b| match &b.node {
            IrNode::Ref(rid) => ir.shape_assignments.get(*rid).is_classified(),
            _ => false,
        });
    }

    // Criterion 2 — classified entry rule. The per-shape emitter for
    // the entry fires; internal Ref recursion currently routes through
    // `__value` (the Alt-dispatch body on Alt-rooted grammars, or the
    // root shape fn on non-Alt-rooted — see
    // [`has_shape_dispatcher_entrypoint`]).
    let entry_tag = ir.shape_assignments.get(entry_rule.id);
    entry_tag.is_classified()
}

/// Returns `true` when `parse()` should route through the shape
/// dispatcher as its top-level entrypoint.
///
/// # AW-V.W5.2 — per-Ref routing
///
/// With the per-Ref routing refactor (W5.2), admission extends beyond
/// Alt-of-Refs entries. A grammar is admissible when:
///
/// 1. **Entry is classified.** The entry rule itself has a shape tag
///    (W3 or W4 — not `None`). This means the entry's per-shape fn is
///    emitted by [`emit_shapes_for_grammar`].
///
/// 2. **Every value-position Ref transitively reachable from the
///    entry's classified shape fns resolves to a classified rule.**
///    Per-Ref routing emits direct calls to each Ref target's shape
///    fn; if any Ref target is unclassified, the emitter falls back
///    to the `__value` dispatcher — which for non-Alt roots
///    recursively invokes the entry's shape fn, triggering runtime
///    infinite recursion unless the fallback is never reached.
///
/// JSON's Alt-of-Refs entry is a special case of the general rule —
/// the entry rule itself doesn't need a shape tag (it's transparent
/// and emits NO compound); the Alt branches must all be classified.
/// The dispatcher emits a byte-dispatch over the Alt branches (see
/// [`dispatcher::emit_alt_dispatch_body`]).
///
/// Non-Alt roots (CSS `stylesheet`, Sheets `formula`, BBNF `grammar`)
/// emit a direct delegation from the dispatcher to the root's shape
/// fn — the root's per-shape body then recurses via per-Ref routing
/// (emitted inline via [`dispatcher::emit_ref_call_tape`]).
///
/// # AX.W0a.2 — entry-reachable narrowing
///
/// The reachability walk starts at the entry rule and traverses every
/// value-position Ref through classified rule bodies, accumulating a
/// visited set. Unclassified rules that are structurally present in
/// the IR but unreachable from the entry cannot trip the `__value`
/// fallback at parse time, so they do not block admission. Prior
/// implementation iterated every classified rule (reachable or not),
/// rejecting grammars whose genuine entry-reachable Ref graph was
/// already closed over classified targets — a false-negative the
/// docstring had already described the narrow shape of.
pub fn has_shape_dispatcher_entrypoint(ir: &GrammarIR) -> bool {
    let Some(entry_rule) = ir.rules.iter().find(|r| r.id == ir.entry) else {
        return false;
    };
    use bbnf_ir::IrNode;

    // Criterion 1 — Alt-of-Refs entry (JSON). Every branch must
    // resolve to a classified rule.
    if let IrNode::Alt(branches, _) = &entry_rule.body {
        return branches.iter().all(|b| match &b.node {
            IrNode::Ref(rid) => ir.shape_assignments.get(*rid).is_classified(),
            _ => false,
        });
    }

    // Criterion 2 — classified entry with entry-reachable per-Ref
    // routability. Entry is classified AND every value-position Ref
    // transitively reachable through classified rule bodies starting
    // from the entry resolves to a classified rule. Unclassified rules
    // unreachable from the entry are irrelevant — their shape fns are
    // never compiled, so no `__value` fallback call site exists that
    // could run at parse time.
    let entry_tag = ir.shape_assignments.get(entry_rule.id);
    if !entry_tag.is_classified() {
        return false;
    }

    // BFS from entry, following Refs through classified rule bodies.
    // A Ref to an unclassified rule is a dispositive rejection: the
    // shape emitter at that Ref's call site emits a `__value`
    // fallback, which re-enters the entry's shape fn and loops.
    let mut visited: std::collections::HashSet<bbnf_ir::RuleId> = Default::default();
    let mut stack: Vec<bbnf_ir::RuleId> = vec![entry_rule.id];
    visited.insert(entry_rule.id);
    while let Some(rid) = stack.pop() {
        let Some(rule) = ir.rules.iter().find(|r| r.id == rid) else {
            // Stale RuleId — treat as structural corruption. Reject
            // to preserve the walker path; a downstream wave surfaces
            // the IR-build bug.
            return false;
        };
        // AX.W0a.2.b — per-wave hard gate 7 ("No `__value` fallback
        // emission"). Several shape emitters fall back to
        // `#dispatcher_ident` on inline Alt / Regex / Negate / Minus
        // / TokenDispatch positions. For non-Alt-rooted grammars the
        // dispatcher IS the root's shape fn, so the fallback loops.
        // Reject admission when any classified entry-reachable rule's
        // body contains one of these fallback-triggering positions
        // outside of a Ref (per-Ref routing already handles Refs
        // directly via `emit_ref_call_tape`).
        if body_has_dispatcher_fallback_position(&rule.body) {
            return false;
        }
        // Skip transparent rules' bodies — transparent rules collapse
        // into their alias targets during lowering, but the interned
        // IR still carries the body. Walk their Refs so Alt-of-Refs
        // dispatchers (e.g. CSS `ruleItem`) compose transitively.
        let refs = dispatcher::collect_value_refs(&rule.body);
        for target_rid in refs {
            let target_tag = ir.shape_assignments.get(target_rid);
            if !target_tag.is_classified() {
                // The target is an unclassified rule reachable from
                // the entry — admitting this grammar would compile a
                // `__value` fallback call site that infinite-loops at
                // parse time. Reject.
                return false;
            }
            if visited.insert(target_rid) {
                stack.push(target_rid);
            }
        }
    }
    true
}

/// Whether a rule's body contains a structural position that would
/// emit `#dispatcher_ident` under the current shape emitters'
/// fallback chains (Flat, Wrap, and friends).
///
/// Positions that trigger the fallback:
///
/// - `IrNode::Alt(_, _)` — inline Alts at a Seq position have no
///   dedicated inline-dispatch emission; Flat delegates to the
///   grammar's `__value`.
/// - `IrNode::Regex(_)` — inline regex scans lack pattern-specific
///   emission in Flat / Scalar; Flat delegates to `__value`.
/// - `IrNode::Negate(_)` / `IrNode::Minus(_, _)` /
///   `IrNode::TokenDispatch { .. }` — structural guards Flat cannot
///   inline and delegates instead.
///
/// Positions that are safe: Ref (per-Ref routing via
/// `emit_ref_call_tape`), Literal (inline byte match), Repeat
/// (recursive walk that bottoms out at Refs/Literals), Seq / Next /
/// Skip / Map / OptionalWhitespace (structural wrappers the walkers
/// descend through).
fn body_has_dispatcher_fallback_position(node: &bbnf_ir::IrNode) -> bool {
    use bbnf_ir::IrNode;
    match node {
        // Dispatcher-fallback positions.
        IrNode::Regex(_) | IrNode::Alt(_, _) | IrNode::Negate(_)
        | IrNode::Minus(_, _) | IrNode::TokenDispatch { .. } => true,
        // Structural wrappers — descend.
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            body_has_dispatcher_fallback_position(inner)
        }
        IrNode::Seq(children) => children
            .iter()
            .any(body_has_dispatcher_fallback_position),
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            body_has_dispatcher_fallback_position(lhs)
                || body_has_dispatcher_fallback_position(rhs)
        }
        IrNode::Repeat { inner, .. } => {
            body_has_dispatcher_fallback_position(inner)
        }
        // Safe positions.
        IrNode::Ref(_) | IrNode::Literal(_) | IrNode::Epsilon => false,
    }
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
