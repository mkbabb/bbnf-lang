//! AZ-I.W2.RF — struct-direct Flat-shape body.
//!
//! W2.RE asserted "JSON does not exercise Pratt / Unordered / ArgList /
//! Flat / HRegex shapes"; under contact JSON's `pair = string, colon >>
//! value` rule classifies as Flat-shape (the ws-bearing literal-led
//! `colon` rule disqualifies the Object detector for `pair` itself).
//! W2.RE's panic-guard for Flat fires when `cargo xtask regen --grammar
//! json` runs against the new `EmitStrategy::StructDirect`. This module
//! restores activation by emitting a real struct-direct body for Flat
//! that mirrors the tape body's per-position emission but writes
//! through [`crate::runtime::JsonStructBuilder`].
//!
//! # Emission shape
//!
//! For a Flat rule whose body is `Seq(p0, p1, p2, …)` (canonical
//! example: JSON `pair = string, colon >> value` post-flattening):
//!
//! ```text
//! let __pair_layout: ::bbnf_ir::registry::StructLayout = { … };
//! let __pair_handle = builder.begin_compound(&__pair_layout);
//! // p0 — Ref(string) → parse_string_<grammar>_string(input, p, state, builder)
//! // p1 — Literal(":") with surrounding ws skip → byte match (no push)
//! // p2 — Ref(value) → parse_wrap_<grammar>_value(input, p, state, builder)
//! builder.end_compound(__pair_handle);
//! ```
//!
//! `JsonStructBuilder::begin_compound` consults `(layout.kind,
//! layout.rule_name)` and routes the `(LayoutKind::Struct, "pair")` case
//! to `OpenFrame::Pair { key, value }`; the in-frame string-key push
//! lands as `Pair.key`, the recursive value call's deposit lands as
//! `Pair.value`, and `end_compound` finalises the pair onto the parent
//! `Object` frame's `pairs` Vec — exactly the wire contract documented
//! at `crates/core/src/runtime/json/builder.rs:80`.
//!
//! # Per-position emission
//!
//! The struct-direct body composes against the same per-position
//! primitives the tape body uses for byte matching (literal head + ws
//! skip), but routes Ref calls and structural pushes through the
//! builder. Sub-rule references emit `parse_<grammar>_<sub_rule>` calls
//! that themselves dispatch via strategy at codegen time — under
//! StructDirect every Ref-call target's sibling shape emitter has
//! already produced its struct-direct body (Object/Array/String/Number/
//! Keyword/Scalar/Wrap from B/C/D), and the calls thread the same
//! `&mut JsonStructBuilder` argument transparently.
//!
//! # Layout literal
//!
//! Per W2.RD's Wrap pattern, the layout literal is built inline from
//! the registry's recorded `LayoutKind` + `rule_name` (the only fields
//! `JsonStructBuilder::begin_compound` consults). For an unregistered
//! Flat rule the structural fallback is `LayoutKind::Struct` keyed by
//! the rule's grammar name — `JsonStructBuilder` falls through to the
//! `OpenFrame::Wrap` route, which is a no-op for the JSON grammar's
//! Flat rule (`pair`) but keeps the emission total per
//! `feedback_no-workarounds`.

use bbnf_ir::registry::LayoutKind;
use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{emit_ref_call_tape, shape_fn_ident};
use super::super::root_rule_name;
use super::super::substrate::{builder_ty_elided, builder_ty_with_lifetime};
use bbnf_ir::registry::EmitStrategy;
use super::collect_positions;
use super::PositionedNode;

/// Codegen-time inspection of the rule's registered layout. Returns the
/// registry entry's `(kind, rule_name)` when present; otherwise the
/// structural fallback `(LayoutKind::Struct, rule_name)` keyed by the
/// grammar-registered identifier.
///
/// `JsonStructBuilder::begin_compound` consults `(kind, rule_name)`
/// only; the literal's `fields` / `rule_type` slots default at the
/// emission site.
fn resolve_layout_meta<'a>(
    rule: &IrRule,
    ir: &'a GrammarIR,
) -> (LayoutKind, &'a str) {
    if let Some(layout) = ir.struct_registry.layout(rule.id) {
        return (layout.kind, layout.rule_name.as_str());
    }
    (LayoutKind::Struct, ir.get_string(rule.name))
}

/// Splice a [`LayoutKind`] discriminator into emitted code as
/// `::bbnf_ir::registry::LayoutKind::<variant>`.
fn quote_layout_kind(kind: LayoutKind) -> TokenStream {
    match kind {
        LayoutKind::Struct => {
            quote! { ::bbnf_ir::registry::LayoutKind::Struct }
        }
        LayoutKind::TaggedEnum => {
            quote! { ::bbnf_ir::registry::LayoutKind::TaggedEnum }
        }
        LayoutKind::UntaggedEnum => {
            quote! { ::bbnf_ir::registry::LayoutKind::UntaggedEnum }
        }
        LayoutKind::NewtypeWrapper => {
            quote! { ::bbnf_ir::registry::LayoutKind::NewtypeWrapper }
        }
    }
}

/// Construct a runtime `bbnf_ir::StructLayout` literal carrying the
/// rule's registered metadata. The `fields` / `rule_type` slots are
/// defaulted because `JsonStructBuilder::begin_compound`'s dispatch
/// only consults `kind` + `rule_name`.
fn quote_layout_literal(rule: &IrRule, ir: &GrammarIR) -> TokenStream {
    let (kind, rule_name) = resolve_layout_meta(rule, ir);
    let kind_tokens = quote_layout_kind(kind);
    let rule_id_lit = rule.id;
    let rule_name_lit = rule_name;
    quote! {
        ::bbnf_ir::registry::StructLayout {
            rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from(#rule_name_lit),
            kind: #kind_tokens,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        }
    }
}

/// Emit the struct-direct body for a Flat rule.
///
/// Mirrors the tape-path entry's outer structure (capture span + walk
/// positions + close) but replaces `tape.push_compound_*` with
/// `builder.begin_compound(&__layout)` / `builder.end_compound(handle)`
/// and routes per-position Ref calls through the same per-shape direct
/// dispatch the tape path uses (which under StructDirect-emitted bodies
/// thread `&mut JsonStructBuilder` transparently).
pub(super) fn emit_parse_flat_struct_direct(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("flat", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // The dispatcher ident is needed only for unclassified Refs. Under
    // StructDirect every Ref-target the JSON grammar exercises is shape-
    // classified (string → String, value → Wrap), so the dispatcher
    // fallback is structurally unreachable — but we resolve the ident to
    // keep the emitted body self-consistent for grammars Sheets/CSS L4
    // light up later under the same per-grammar strategy.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = super::super::dispatcher::dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let layout_literal = quote_layout_literal(rule, ir);
    let layout_var = format_ident!("__{}_layout", rule_name);
    let handle_var = format_ident!("__{}_handle", rule_name);

    // AZ-I.W2-act.B3 — substrate-binding splice: convert the
    // strategy's `builder_path` into a TokenStream so the function
    // signature carries the grammar's concrete builder type rather
    // than the JSON-specific path. Per `feedback_no-orthogonal-
    // codepaths`, the resolution happens once here; the resulting
    // tokens are spliced unchanged at every call site.
    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);
    let builder_ty_elided = builder_ty_elided(strategy);

    // Flatten the rule body into positional IR nodes (same walker the
    // tape path uses; ws-trim metadata threaded per position).
    let positions = collect_positions(&rule.body);
    let mut emissions = Vec::with_capacity(positions.len());
    for pos in &positions {
        emissions.push(emit_position_struct_direct(
            pos,
            &support_mod,
            &dispatcher_ident,
            grammar_suffix,
            ir,
        ));
    }

    quote! {
        /// AZ-I.W2.RF — per-grammar Flat-shape parse function,
        /// **struct-direct body**. Targets the grammar's concrete
        /// `StructBuilder` (JSON / CSS L4 / Sheets per the
        /// resolver's `SubstrateBinding`).
        ///
        /// Walker-tape compound emission is replaced by typed
        /// `begin_compound` / `end_compound` calls against the in-flight
        /// frame stack. Per-position pushes (string keys, recursive
        /// value calls, byte literals) land directly on the topmost
        /// open frame.
        ///
        /// Returns `TapeOffset::NONE` for compositional uniformity
        /// with sibling shape fns under struct-direct mode; the
        /// offset is unused by struct-direct callers.
        ///
        /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
        /// cross-shape recursive edge (Flat → Wrap → Flat through
        /// the grammar's `__value` discriminant). LLVM's inliner
        /// collapses plain `#[inline]` candidates only when
        /// profitable and bails cleanly on detected recursion.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            // AZ-I.W2.RF — open the Flat compound. The layout literal is
            // built inline from the rule's registered metadata; the
            // grammar's StructBuilder routes (kind, rule_name) to its
            // own concrete OpenFrame variant.
            let #layout_var: ::bbnf_ir::registry::StructLayout = #layout_literal;
            let #handle_var = <
                #builder_ty_elided as crate::runtime::StructBuilder
            >::begin_compound(builder, &#layout_var);

            // Per-position emission.
            #(#emissions)*

            <
                #builder_ty_elided as crate::runtime::StructBuilder
            >::end_compound(builder, #handle_var);

            ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
        }
    }
}

/// Emit one position's struct-direct body. Threads ws-skip around the
/// position's record-producing core.
fn emit_position_struct_direct(
    pos: &PositionedNode,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let leading = if pos.leading_ws {
        quote! { let _ = #support_mod::skip_space(input, p, state); }
    } else {
        quote! {}
    };
    let trailing = if pos.trailing_ws {
        quote! { let _ = #support_mod::skip_space(input, p, state); }
    } else {
        quote! {}
    };
    let core = emit_position_core_struct_direct(
        pos.node,
        support_mod,
        dispatcher_ident,
        grammar_suffix,
        ir,
    );
    quote! {
        {
            #leading
            #core
            #trailing
        }
    }
}

/// Emit the record-producing core for one position under StructDirect.
///
/// The position's IR-node variant determines the emission:
///
/// - `Literal(sid)` — byte-match at `*p`, no builder push (the
///   surrounding compound already opens its own frame; the literal is
///   structural punctuation that the JsonStructBuilder doesn't store).
/// - `Ref(rid)` — direct call to the target's per-shape struct-direct
///   fn (resolved at codegen time via `emit_ref_call_tape`'s shape
///   classification; the call signature is identical between tape and
///   struct-direct because `&mut builder` is the only argument that
///   differs and is threaded transparently).
/// - `OptionalWhitespace(inner)` — leading + trailing ws-skip around
///   the inner's core emission.
/// - `Next(lhs, rhs)` / `Skip(lhs, rhs)` — recurse into both children.
/// - `Seq(children)` — recurse into each child.
/// - `Epsilon` — empty.
///
/// Other variants (Alt / Regex / Repeat / Map / Negate / Minus /
/// TokenDispatch) are not exercised by JSON's `pair` rule and remain
/// orchestrator-deferred to the grammar that lights them up under
/// StructDirect (Sheets/CSS L4). Per `feedback_no-workarounds` the
/// emitter still routes them through the closest semantic equivalent
/// rather than panicking inside the codegen — Alt branches and inline
/// regex positions delegate to the dispatcher unchanged, which under
/// StructDirect must already accept the builder argument from the
/// per-grammar admission contract.
fn emit_position_core_struct_direct(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => emit_literal_match_struct_direct(*sid, ir),
        IrNode::Ref(rid) => {
            // emit_ref_call_tape resolves the target's shape and emits
            // a direct call to the matching `parse_<shape>_<grammar>_<rule>`
            // fn. Under StructDirect every shape emitter produces a
            // body whose builder argument is &mut JsonStructBuilder<'_>;
            // the call signature differs only in that argument's type.
            if let Some(call) = emit_ref_call_tape(grammar_suffix, *rid, ir) {
                quote! { let _ = (#call)?; }
            } else {
                // Unclassified target — fall back to the dispatcher.
                // Under StructDirect the dispatcher takes the same
                // builder argument; the call is signature-compatible.
                quote! {
                    let _ = #dispatcher_ident(input, p, state, builder)?;
                }
            }
        }
        IrNode::OptionalWhitespace(inner) => {
            let inner_emit = emit_position_core_struct_direct(
                inner,
                support_mod,
                dispatcher_ident,
                grammar_suffix,
                ir,
            );
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #inner_emit
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_position_core_struct_direct(
                lhs,
                support_mod,
                dispatcher_ident,
                grammar_suffix,
                ir,
            );
            let r = emit_position_core_struct_direct(
                rhs,
                support_mod,
                dispatcher_ident,
                grammar_suffix,
                ir,
            );
            quote! { #l #r }
        }
        IrNode::Seq(children) => {
            let mut out = Vec::with_capacity(children.len());
            for child in children {
                out.push(emit_position_core_struct_direct(
                    child,
                    support_mod,
                    dispatcher_ident,
                    grammar_suffix,
                    ir,
                ));
            }
            quote! { #(#out)* }
        }
        IrNode::Map { inner, .. } => emit_position_core_struct_direct(
            inner,
            support_mod,
            dispatcher_ident,
            grammar_suffix,
            ir,
        ),
        IrNode::Epsilon => quote! {},
        // Variants below are not exercised by JSON's `pair` rule. The
        // dispatcher delegation keeps the emission total — under
        // StructDirect the dispatcher's own body is struct-direct (per
        // for_grammar's contract for the admitted grammar), so the
        // call is signature-compatible.
        IrNode::Alt(_, _)
        | IrNode::Regex(_)
        | IrNode::Negate(_)
        | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. }
        | IrNode::Repeat { .. } => {
            quote! {
                let _ = #dispatcher_ident(input, p, state, builder)?;
            }
        }
    }
}

/// Emit a byte-sequence literal match. Unlike the tape path, the
/// struct-direct body emits no builder push for the literal — the
/// surrounding compound's `begin_compound` already opens the
/// structural frame, and the literal byte itself is structural
/// punctuation (`:` between key and value in JSON's `pair`) that the
/// JsonStructBuilder doesn't store.
///
/// Per the JsonStructBuilder wire contract documented at
/// `crates/core/src/runtime/json/builder.rs:32-46`, only typed
/// `->`-annotated leaves push values to the builder; structural
/// literals are byte-checked but do not contribute to the projected
/// document.
fn emit_literal_match_struct_direct(sid: u32, ir: &GrammarIR) -> TokenStream {
    let bytes = ir.get_string(sid).as_bytes();
    let len = bytes.len();
    let byte_lits: Vec<TokenStream> =
        bytes.iter().map(|b| quote! { #b }).collect();
    quote! {
        let at = *p;
        let end = at + #len;
        if input.len() < end || input[at..end] != [#(#byte_lits),*] {
            return ::core::result::Result::Err(
                crate::runtime::tape::DtaError::Syntax {
                    offset: at as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                },
            );
        }
        *p = end;
    }
}
