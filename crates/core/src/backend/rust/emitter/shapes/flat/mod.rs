//! Flat-shape emitter — `parse_flat_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar Flat-shape parse functions for typed
//! `Seq(head, body+)` rules. Canonical:
//!
//! - CSS 28 `*Decl` rules — e.g. `displayDecl = "display" , ":" ?w ,
//!   (value ?w) * , importantSuffix , ";"?` per
//!   `grammar/css/l4/properties.bbnf`.
//! - BBNF 7 `*_directive` rules — e.g. `import_directive = "@import"
//!   ?w , ( … ) , ( ";" | "." ) ?` per `grammar/bbnf/bbnf.bbnf`.
//! - CSS typed dimensions — `length`, `angle`, `time`, etc.
//! - CSS rule scaffolding — `qualifiedRule`, `mediaQuery`, etc.
//!
//! # Emission shape
//!
//! The emitted function:
//!
//! 1. Captures `span_lo` + `outer_child = mark_children()` for the
//!    outer Seq compound.
//! 2. Walks each flattened position of the rule body, emitting per
//!    position:
//!    - `Literal(sid)` → byte-match at `*p`, push `TapeKind::Literal`
//!      leaf with `variant_idx` inherited from the rule.
//!    - `Regex(sid)` / `Ref(rid)` / `Alt(…)` → delegate to the
//!      dispatcher's value-position routine (the walker's own state
//!      path). The dispatcher resolves each to its shape fn or falls
//!      back to the walker for unclassified rules.
//!    - `Repeat(inner, 0, 1)` → one optional iteration wrapped in a
//!      `Rule` compound (mirroring the walker's Repeat tape shape).
//!    - `OptionalWhitespace(inner)` → leading + trailing ws-skip.
//! 3. Closes the outer Seq compound with `push_compound(..Seq, ..)`.
//!
//! # Wire contract
//!
//! Per the walker-tape parity contract (W3 Object / Array pattern),
//! every structural IR production corresponds to one tape record.
//! The Flat emitter walks the Seq structure once and emits a matching
//! record stream. Positions the emitter cannot inline (complex
//! Repeats, recursive Refs) dispatch through the grammar's value-
//! position dispatcher — the walker's authoritative path. When the
//! dispatcher rejects (no shape match), the top-level grammar's
//! `parse()` falls back to `__dta_walker_inline::run`.
//!
//! The emitter is gated behind `has_full_shape_coverage` in
//! [`super::emit_shapes_for_grammar`] — it compiles standalone for
//! shape-dispatch substrate tests but is not consumed on the hot
//! path until W4.2 / W4.3 wire per-grammar consumers.
//!
//! Module layout (B5.W3):
//! - [`tape`]            — tape-path per-position emission
//! - [`visitor`]         — visitor-path per-position emission
//! - [`typed_payload`]   — typed-payload Alt-branch byte-dispatch
//!   (Sheets `error_literal` factored-Alt shape)
//! - [`map_regex_host`]  — `Map { Regex, host-fn }` position emission
//!   (CSS `hex` host-fn pattern + `NumberConvert` f64 scan)

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, shape_fn_ident, visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

mod map_regex_host;
mod tape;
mod typed_payload;
mod visitor;

/// Emit `pub fn parse_flat_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_flat(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("flat", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Resolve dispatcher ident for Ref-recursion. Missing root means
    // the emitter can't route recursion; emit nothing so the grammar
    // stays on the walker fallback.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // Flatten the rule body into positional IR nodes.
    let positions = collect_positions(&rule.body);

    // AX.W0a.2.p — the owning rule's id drives leaf-kind selection
    // for `Map { Regex, host-fn }` positions (KvPair when the rule
    // type is `Tuple([Span, scalar])`; Span otherwise). Thread it
    // through emission.
    let rule_id = rule.id;

    let body_emission = tape::emit_tape_positions(
        &positions,
        variant_idx,
        rule_id,
        &support_mod,
        &dispatcher_ident,
        ir,
    );

    quote! {
        /// AW-V.W4-fix — per-grammar Flat-shape parse function,
        /// walker-tape-identical.
        ///
        /// Emits one outer Seq compound plus per-position inner
        /// records. Ref / Regex / Alt positions recurse through the
        /// grammar's value-position dispatcher (the walker's
        /// authoritative state path).
        ///
        /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`): this fn
        /// sits on a cross-shape recursive edge
        /// (`parse_flat_<grammar>_<rule>` → `emit_ref_call_tape` →
        /// peer shape fn → back here through the grammar's `__value`
        /// discriminant). LLVM's inliner collapses plain `#[inline]`
        /// candidates only when profitable and bails cleanly on
        /// detected recursion; `#[inline(always)]` would recurse the
        /// inliner until stack exhaustion (observed SIGBUS in
        /// BbnfBootstrap's `grammar_item` triangle during W0a.2.e).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            let span_lo = *p as u32;
            // AY-II.W0.b — walker-parity POST-ORDER outer Seq compound.
            // B5.W6 — bracket the post-order children scope so child
            // records stamp `frame_depth` at the correct (parent + 1)
            // depth at push time. The matching `end_compound_post_order`
            // absorbs the bracket bump; `begin_compound_post` stamps the
            // outer row at the outer-frame depth without bumping.
            //
            // B5.W6b — IIFE-wrap the body so `?`-propagation from inner
            // shape calls (refs, regex scans, dispatcher routes) cannot
            // leak past `end_compound_post_order` and leave
            // `current_depth` permanently bumped. The Err-arm rolls
            // back partial pushes FIRST (which may clobber
            // `current_depth` to the bracket-bumped depth via
            // `frame_depth[__save_cols]`), then `exit_post_order_children`
            // decrements once to the outer frame. The order is critical:
            // rollback before exit, so the rolled-back row's
            // bracket-bumped depth is corrected to the outer depth.
            let __save_cols = builder.position();
            let outer_child = builder.enter_post_order_children();
            let __post_body: ::core::result::Result<
                (),
                crate::runtime::tape::DtaError,
            > = (|| {
                #body_emission
                Ok(())
            })();
            if let ::core::result::Result::Err(__err) = __post_body {
                builder.rollback_to(__save_cols);
                builder.exit_post_order_children();
                return ::core::result::Result::Err(__err);
            }

            let span_hi = *p as u32;
            let outer_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Seq,
                span_lo,
                #variant_idx,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                outer_off,
                span_hi,
                crate::runtime::tape::TapeOffset(outer_child),
            );
            Ok(crate::runtime::tape::TapeOffset(outer_off))
        }
    }
}

/// Emit `pub fn parse_flat_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_flat_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("flat", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let positions = collect_positions(&rule.body);
    let body_emission = visitor::emit_visitor_positions(
        &positions,
        &support_mod,
        &dispatcher_ident,
        ir,
    );

    quote! {
        /// AW-V.W4-fix — visitor-path Flat-shape parse function.
        ///
        /// Mirrors the tape-path emitter structure. Literal positions
        /// byte-match without emitting a visitor event; Ref / Regex /
        /// Alt positions recurse through the visitor dispatcher.
        ///
        /// AX.W0a.2.f — compound; see tape-path comment for the
        /// `#[inline]` downgrade rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
        {
            #body_emission
            Ok(())
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Position collection
// ─────────────────────────────────────────────────────────────────────

/// A single flattened position in the rule body with leading/trailing
/// ws-trim markers inherited from enclosing `OptionalWhitespace`s.
#[derive(Clone)]
pub(super) struct PositionedNode<'a> {
    pub(super) node: &'a IrNode,
    pub(super) leading_ws: bool,
    pub(super) trailing_ws: bool,
}

/// Flatten a rule body into a list of positional nodes.
fn collect_positions<'a>(node: &'a IrNode) -> Vec<PositionedNode<'a>> {
    let mut out = Vec::new();
    walk_positions(node, false, false, &mut out);
    out
}

fn walk_positions<'a>(
    node: &'a IrNode,
    leading: bool,
    trailing: bool,
    out: &mut Vec<PositionedNode<'a>>,
) {
    match node {
        // AX.W0a.2.p — preserve `Map { Regex, host-fn }` so the
        // typed-leaf position emitter sees the annotation + emits the
        // host-fn call + arena payload (CSS `hex` host-fn pattern and
        // `NumberConvert` f64 scan). The Map arm in
        // `emit_tape_position_core` falls back to transparent unwrap
        // for structural Map / non-regex inners so other arms retain
        // their existing behaviour.
        IrNode::Map { inner, .. } if matches!(inner.as_ref(), IrNode::Regex(_)) => {
            out.push(PositionedNode {
                node,
                leading_ws: leading,
                trailing_ws: trailing,
            });
        }
        IrNode::Map { inner, .. } => walk_positions(inner, leading, trailing, out),
        IrNode::OptionalWhitespace(inner) => {
            walk_positions(inner, true, true, out)
        }
        IrNode::Seq(children) => {
            for child in children {
                walk_positions(child, leading, trailing, out);
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            walk_positions(lhs, leading, trailing, out);
            walk_positions(rhs, leading, trailing, out);
        }
        IrNode::Epsilon => {}
        _ => out.push(PositionedNode {
            node,
            leading_ws: leading,
            trailing_ws: trailing,
        }),
    }
}
