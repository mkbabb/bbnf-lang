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

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident,
    visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

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

    let body_emission = emit_tape_positions(
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
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let span_lo = *p as u32;
            // AY-II.W0.b — walker-parity POST-ORDER outer Seq compound.
            // Capture first-child index pre-emission; allocate the
            // compound row post-children via begin_compound; close
            // immediately; override child_off to point at first-child.
            let outer_child = builder.columns_mut().len() as u32;

            #body_emission

            let span_hi = *p as u32;
            let outer_off = builder.begin_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                span_lo,
                #variant_idx,
                0u16,
            );
            builder.end_compound(outer_off, span_hi);
            builder.columns_mut().set_child_off_at(
                outer_off,
                ::bbnf::runtime::tape::TapeOffset(outer_child),
            );
            Ok(::bbnf::runtime::tape::TapeOffset(outer_off))
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Position collection
// ─────────────────────────────────────────────────────────────────────

/// A single flattened position in the rule body with leading/trailing
/// ws-trim markers inherited from enclosing `OptionalWhitespace`s.
#[derive(Clone)]
struct PositionedNode<'a> {
    node: &'a IrNode,
    leading_ws: bool,
    trailing_ws: bool,
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

// ─────────────────────────────────────────────────────────────────────
// Tape-path per-position emission
// ─────────────────────────────────────────────────────────────────────

/// Emit the full tape-path body for all positions.
fn emit_tape_positions(
    positions: &[PositionedNode],
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let mut emissions = Vec::with_capacity(positions.len());
    for pos in positions {
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
        let core = emit_tape_position_core(
            pos.node,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        );
        emissions.push(quote! {
            {
                #leading
                #core
                #trailing
            }
        });
    }
    quote! { #(#emissions)* }
}

/// Emit the record-producing core for one position (ws-handling lives
/// on the caller).
fn emit_tape_position_core(
    node: &IrNode,
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    // AW-V.W5.2 — per-Ref routing. Extract grammar_suffix from the
    // support_mod ident ("__shape_support_<grammar>").
    let grammar_suffix = support_mod
        .to_string()
        .strip_prefix("__shape_support_")
        .unwrap_or("")
        .to_string();
    match node {
        IrNode::Literal(sid) => emit_literal_match(*sid, variant_idx, ir),
        IrNode::Ref(rid) => {
            // AW-V.W5.2 — direct per-Ref routing. Resolve the target's
            // shape at codegen time and emit a direct call.
            if let Some(call) = emit_ref_call_tape(&grammar_suffix, *rid, ir) {
                quote! { let _ = (#call)?; }
            } else {
                // Unclassified target — fall back to the dispatcher (which
                // will resolve to __value's Alt dispatch if Alt-rooted, or
                // trip the admission gate otherwise).
                quote! {
                    let _ = #dispatcher_ident(input, p, state, builder)?;
                }
            }
        }
        IrNode::Alt(branches, _)
            if alt_branches_carry_typed_payloads(branches, ir) =>
        {
            // AX.W0a.2.p — Alt position whose branches each carry a
            // `Map { inner: Literal|Seq-literal-chain, IntLit|BoolLit }`
            // annotation. The default inline emission (inline.rs
            // `emit_alt_branch_body_tape`) strips Map wrappers and
            // pushes Literal leaves with `push_leaf(Literal, ..., 0, 0)`
            // — payload LOST. Emit here instead with
            // `push_leaf_with_arena_payload` so the per-branch byte
            // discriminant reaches the tape.
            //
            // Canonical source: Sheets `error_literal` post-factoring
            // (`Seq(Literal("#"), Alt(Map{"N/A",0u8}, Map{"VALUE!",1u8},
            // …))`), and structurally-analogous CSS `keyframeStop`.
            let _ = dispatcher_ident;
            emit_alt_typed_payload_tape(branches, support_mod, &grammar_suffix, ir)
        }
        IrNode::Alt(_, _) | IrNode::Regex(_)
        | IrNode::Negate(_) | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => {
            // AX.W0a.2.e — inline-position emission. Walker-parity
            // byte-dispatch for inline Alt, regex-scan adapter for
            // inline Regex, guard-only for Negate / Minus, dedicated
            // compound for TokenDispatch. No recursion into
            // `#dispatcher_ident` — for non-Alt-rooted grammars the
            // dispatcher IS the root shape fn and would loop.
            let _ = dispatcher_ident;
            super::inline::emit_inline_position_tape(
                node, variant_idx, support_mod, &grammar_suffix, ir,
            )
        }
        IrNode::Repeat { inner, lo, hi } => emit_tape_repeat(
            inner,
            *lo,
            *hi,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ),
        IrNode::Seq(children) => {
            let inner = emit_tape_seq_children(
                children,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! {
                let seq_lo = *p as u32;
                let seq_child = builder.columns_mut().len() as u32;
                #inner
                let seq_hi = *p as u32;
                let __seq_off = builder.begin_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    seq_lo,
                    0,
                    0u16,
                );
                builder.end_compound(__seq_off, seq_hi);
                builder.columns_mut().set_child_off_at(
                    __seq_off,
                    ::bbnf::runtime::tape::TapeOffset(seq_child),
                );
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_tape_position_core(
                lhs,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            let r = emit_tape_position_core(
                rhs,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! { #l #r }
        }
        IrNode::Map { inner, fn_id } => {
            // AX.W0a.2.p — detect `Map { Regex, Expr { expr: FnCall,
            // return_type: U32 } }` (CSS `hex` host-fn pattern) and
            // emit a regex-scan + host fn call + arena-payload push
            // so the u32 value reaches the tape via
            // `push_leaf_with_arena_payload`. Other Map shapes retain
            // the transparent unwrap for structural emission.
            if let Some(emission) = emit_map_regex_host_fn(
                inner,
                *fn_id,
                variant_idx,
                rule_id,
                &grammar_suffix,
                ir,
            ) {
                emission
            } else {
                emit_tape_position_core(
                    inner,
                    variant_idx,
                    rule_id,
                    support_mod,
                    dispatcher_ident,
                    ir,
                )
            }
        }
        IrNode::OptionalWhitespace(inner) => {
            let inner_emit = emit_tape_position_core(
                inner,
                variant_idx,
                rule_id,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #inner_emit
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Epsilon => quote! {},
    }
}

/// Emit a Seq's children inline (no outer compound).
fn emit_tape_seq_children(
    children: &[IrNode],
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let mut out = Vec::with_capacity(children.len());
    for child in children {
        out.push(emit_tape_position_core(
            child,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ));
    }
    quote! { #(#out)* }
}

/// Emit a Repeat position. Two canonical sub-shapes per H1 audit:
///
/// - `Repeat { lo = 0, hi = 1, inner }` — optional. Route through the
///   dispatcher; rollback handled at the dispatcher's boundary by the
///   walker fallback.
/// - Any other repeat — iterate by dispatching the inner in a loop;
///   bail when the dispatcher returns an error.
///
/// Walker-parity outer compound: a `Rule` compound bracketing the
/// Repeat span.
fn emit_tape_repeat(
    inner: &IrNode,
    lo: u32,
    hi: u32,
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    // AX.W0a.2.g — walker-parity iter-Seq flattening. Walker's
    // `IrState::Repeat { inner: Seq }` lowering transitions directly
    // to the Seq's inner states after pushing the Repeat frame (Rule),
    // and the Seq state's compound push IS the iter Seq — no extra
    // wrapper. When the Repeat's inner is itself a Seq, emit the
    // children directly inside the iter-Seq push, not a nested Seq
    // wrapping another Seq. For non-Seq inners (Ref, Alt, Literal,
    // Regex, etc.) the iter-Seq is the only walker-pushed Seq, so
    // emission is unchanged.
    let inner_emit = match inner {
        IrNode::Seq(children) => emit_tape_seq_children(
            children,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ),
        _ => emit_tape_position_core(
            inner,
            variant_idx,
            rule_id,
            support_mod,
            dispatcher_ident,
            ir,
        ),
    };
    let lo_lit = lo as usize;

    if hi == 1 && lo == 0 {
        // Optional. AX.W0a.2.h — wrap the inner in an attempt so
        // failure silently rolls back `*p` + tape columns instead
        // of propagating to the enclosing shape fn. Matches walker's
        // `lo==0` optional semantics.
        //
        // AY-II.W0.b — truncate→rollback_to on iter failure; per-iter
        // Seq and outer Repeat compounds use begin_compound/end_compound.
        let _ = lo_lit;
        quote! {
            let repeat_lo = *p as u32;
            let repeat_child = builder.columns_mut().len() as u32;
            let iter_save_p = *p;
            let iter_save_cols = builder.columns_mut().len() as u32;
            let iter_lo = *p as u32;
            let iter_child = builder.columns_mut().len() as u32;
            let opt_attempt: ::core::result::Result<(), ::bbnf::runtime::tape::DtaError> =
                (|| {
                    #inner_emit
                    Ok(())
                })();
            let matched = opt_attempt.is_ok();
            if !matched {
                *p = iter_save_p;
                builder.columns_mut().rollback_to(iter_save_cols);
            } else {
                let iter_hi = *p as u32;
                let __iter_off = builder.begin_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_lo,
                    0,
                    0u16,
                );
                builder.end_compound(__iter_off, iter_hi);
                builder.columns_mut().set_child_off_at(
                    __iter_off,
                    ::bbnf::runtime::tape::TapeOffset(iter_child),
                );
            }
            let repeat_hi = *p as u32;
            // AX.W0a.2.j — `TapeKind::Repeat` (not `Rule`) so
            // downstream IR-lowering's `iter_rep_children` peels it.
            let __repeat_off = builder.begin_compound(
                ::bbnf::runtime::tape::TapeKind::Repeat,
                repeat_lo,
                0,
                0u16,
            );
            builder.end_compound(__repeat_off, repeat_hi);
            builder.columns_mut().set_child_off_at(
                __repeat_off,
                ::bbnf::runtime::tape::TapeOffset(repeat_child),
            );
        }
    } else {
        // Generic repeat. Iterate greedily.
        //
        // AX.W0a.2.g — column rollback on iter failure + zero-width
        // break. Walker's `handle_repeat_failure_bounded` rolls back
        // `columns` to the iter's savepoint on Err or zero-width
        // success.
        //
        // AY-II.W0.b — truncate→rollback_to across the 2 rollback sites
        // + per-iter Seq + outer Repeat compounds on begin/end_compound.
        quote! {
            let repeat_lo = *p as u32;
            let repeat_child = builder.columns_mut().len() as u32;
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.columns_mut().len() as u32;
                let iter_lo = *p as u32;
                let iter_child = builder.columns_mut().len() as u32;
                let attempt = (|| -> ::core::result::Result<(), ::bbnf::runtime::tape::DtaError> {
                    #inner_emit
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.columns_mut().rollback_to(save_cols);
                    break;
                }
                // Protect against non-progressing iterations.
                if *p == save_p {
                    builder.columns_mut().rollback_to(save_cols);
                    break;
                }
                let iter_hi = *p as u32;
                let __iter_off = builder.begin_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_lo,
                    0,
                    0u16,
                );
                builder.end_compound(__iter_off, iter_hi);
                builder.columns_mut().set_child_off_at(
                    __iter_off,
                    ::bbnf::runtime::tape::TapeOffset(iter_child),
                );
                iter_count = iter_count.saturating_add(1);
            }
            if iter_count < (#lo_lit as u32) {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let repeat_hi = *p as u32;
            let __repeat_off = builder.begin_compound(
                ::bbnf::runtime::tape::TapeKind::Repeat,
                repeat_lo,
                0,
                0u16,
            );
            builder.end_compound(__repeat_off, repeat_hi);
            builder.columns_mut().set_child_off_at(
                __repeat_off,
                ::bbnf::runtime::tape::TapeOffset(repeat_child),
            );
        }
    }
}

/// Emit a byte-sequence literal match + Literal leaf push.
fn emit_literal_match(
    sid: u32,
    variant_idx: u8,
    ir: &GrammarIR,
) -> TokenStream {
    let bytes = ir.get_string(sid).as_bytes();
    let len = bytes.len();
    let byte_lits: Vec<TokenStream> =
        bytes.iter().map(|b| quote! { #b }).collect();
    quote! {
        let at = *p;
        let end = at + #len;
        if input.len() < end || input[at..end] != [#(#byte_lits),*] {
            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                offset: at as u32,
                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
            });
        }
        *p = end;
        let _ = builder.push_leaf_with(
            ::bbnf::runtime::tape::TapeKind::Literal,
            at as u32,
            end as u32,
            #variant_idx,
            0,
            ::bbnf::runtime::tape::PayloadData::None,
        );
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4-fix — visitor-path Flat emitter.
// ─────────────────────────────────────────────────────────────────────

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
    let body_emission = emit_visitor_positions(
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
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ObjectVisitor
                + ::bbnf::runtime::tape::ArrayVisitor
                + ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::NumberVisitor
                + ::bbnf::runtime::tape::KeywordVisitor,
        {
            #body_emission
            Ok(())
        }
    }
}

/// Emit the visitor-path body-position sequence.
fn emit_visitor_positions(
    positions: &[PositionedNode],
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let mut emissions = Vec::with_capacity(positions.len());
    for pos in positions {
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
        let core = emit_visitor_position_core(
            pos.node,
            support_mod,
            dispatcher_ident,
            ir,
        );
        emissions.push(quote! {
            {
                #leading
                #core
                #trailing
            }
        });
    }
    quote! { #(#emissions)* }
}

fn emit_visitor_position_core(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    // AW-V.W5.2 — per-Ref routing. Extract grammar_suffix from support_mod.
    let grammar_suffix = support_mod
        .to_string()
        .strip_prefix("__shape_support_")
        .unwrap_or("")
        .to_string();
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> =
                bytes.iter().map(|b| quote! { #b }).collect();
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return Err(::bbnf::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    });
                }
                *p = end;
            }
        }
        IrNode::Ref(rid) => {
            // AW-V.W5.2 — direct per-Ref routing for visitor path.
            if let Some(call) = emit_ref_call_visitor(&grammar_suffix, *rid, ir) {
                quote! { (#call)?; }
            } else {
                quote! {
                    #dispatcher_ident(input, p, state, visitor)?;
                }
            }
        }
        IrNode::Regex(_) | IrNode::Alt(_, _)
        | IrNode::Negate(_) | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => {
            // AX.W0a.2.e — inline-position emission (visitor path).
            // See tape-path note above for rationale.
            let _ = dispatcher_ident;
            super::inline::emit_inline_position_visitor(
                node, support_mod, &grammar_suffix, ir,
            )
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_emit = emit_visitor_position_core(
                inner,
                support_mod,
                dispatcher_ident,
                ir,
            );
            let lo_lit = *lo as usize;
            if *hi == 1 && *lo == 0 {
                quote! {
                    let save_p = *p;
                    let res = (|| -> ::core::result::Result<(), ::bbnf::runtime::ParseErr> {
                        #inner_emit
                        Ok(())
                    })();
                    if res.is_err() {
                        *p = save_p;
                    }
                }
            } else {
                quote! {
                    let mut iter_count: u32 = 0;
                    loop {
                        let save_p = *p;
                        let res = (|| -> ::core::result::Result<(), ::bbnf::runtime::ParseErr> {
                            #inner_emit
                            Ok(())
                        })();
                        if res.is_err() {
                            *p = save_p;
                            break;
                        }
                        if *p == save_p { break; }
                        iter_count = iter_count.saturating_add(1);
                    }
                    if iter_count < (#lo_lit as u32) {
                        return Err(::bbnf::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        });
                    }
                }
            }
        }
        IrNode::Seq(children) => {
            let mut out = Vec::with_capacity(children.len());
            for c in children {
                out.push(emit_visitor_position_core(
                    c,
                    support_mod,
                    dispatcher_ident,
                    ir,
                ));
            }
            quote! { #(#out)* }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_visitor_position_core(
                lhs,
                support_mod,
                dispatcher_ident,
                ir,
            );
            let r = emit_visitor_position_core(
                rhs,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! { #l #r }
        }
        IrNode::Map { inner, .. } => emit_visitor_position_core(
            inner,
            support_mod,
            dispatcher_ident,
            ir,
        ),
        IrNode::OptionalWhitespace(inner) => {
            let inner_emit = emit_visitor_position_core(
                inner,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! {
                let _ = #support_mod::skip_space(input, p, state);
                #inner_emit
                let _ = #support_mod::skip_space(input, p, state);
            }
        }
        IrNode::Epsilon => quote! {},
    }
}

// ─────────────────────────────────────────────────────────────────────
// AX.W0a.2.p — Map { Regex, host-fn } position emission (Class 2).
// ─────────────────────────────────────────────────────────────────────

/// Emit a position whose structure is `Map { Regex(s), FnDescriptor
/// that returns u32 or f64 }` — the host-function-backed typed-leaf
/// pattern (CSS `hex = "#" , /regex/ -> parse_hex_color(input) : u32`
/// and analogous).
///
/// Returns `None` when the Map doesn't match one of the recognized
/// host-fn shapes; the caller falls back to the transparent Map
/// unwrap.
///
/// Supported FnDescriptor arms:
///
/// - `HexConvert { fn_path }` — scan the regex, call `fn_path(input)`,
///   push a Span leaf carrying the u32 return value as a 4-byte arena
///   aggregate (little-endian). `TapeKind::KvPair` when the owning
///   rule's type is `Tuple([Span, U32])` (hex's inferred type post-
///   layout-planning); Span otherwise.
/// - `NumberConvert { allow_leading_dot }` — scan number, convert to
///   f64, push Span/KvPair with 8-byte arena payload.
fn emit_map_regex_host_fn(
    inner: &bbnf_ir::IrNode,
    fn_id: u32,
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, IrNode, TypeDesc};
    let IrNode::Regex(sid) = inner else {
        return None;
    };
    let pattern = ir.get_string(*sid).to_string();
    let fd = ir.fns.get(fn_id as usize)?;
    let regex_scan_ident =
        super::super::dfa_codegen::regex_scan_adapter_ident(&super::sanitise_grammar(grammar_suffix));
    // Owner-rule type decides KvPair-vs-Span on the pushed leaf. A
    // rule whose inferred type is `Tuple([Span, scalar])` is KvPair-
    // shaped per `is_kv_pair_shape`; the walker rewrites such Seq
    // compounds to KvPair at frame-pop time. Matching that on the
    // leaf side at emit time lets `css_l4_parity::hex_color_*` find
    // a KvPair record with the 4-byte hex payload without a runtime
    // compound-rewrite.
    let kind_is_kv = matches!(
        ir.types.iter().find_map(|(rid, t)| {
            if *rid == rule_id {
                Some(t)
            } else {
                None
            }
        }),
        Some(TypeDesc::Tuple(fields)) if matches!(
            fields.as_slice(),
            [TypeDesc::Span, value] if value.is_scalar_payload()
        )
    );
    let leaf_kind = if kind_is_kv {
        quote! { ::bbnf::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { ::bbnf::runtime::tape::TapeKind::Span }
    };
    match fd {
        FnDescriptor::HexConvert { fn_path } => {
            let path_str = ir.get_string(*fn_path);
            let path: syn::Path = syn::parse_str(path_str).ok()?;
            Some(quote! {
                {
                    let span_lo = *p as u32;
                    let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                        return ::core::result::Result::Err(
                            ::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state:
                                    ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        );
                    };
                    *p += match_len as usize;
                    let span_hi = *p as u32;
                    // Host fn sees the matched substring as &str; the
                    // Map's `Expr { FnCall(path, [Input]) }` / the
                    // HexConvert specialisation declares the return
                    // type is u32, which the walker-parity emitter
                    // packs as 4-byte LE into the arena.
                    let __decoded_u32: u32 = #path(
                        core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        ).unwrap_or(""),
                    );
                    let __arena_off: u32 =
                        builder.arena_mut().len() as u32;
                    builder
                        .arena_mut()
                        .extend_from_slice(&__decoded_u32.to_le_bytes());
                    let _ = builder.push_leaf_with_arena_payload(
                        #leaf_kind,
                        span_lo,
                        span_hi,
                        #variant_idx,
                        0u8,
                        __arena_off,
                        4u32,
                    );
                }
            })
        }
        FnDescriptor::NumberConvert { allow_leading_dot } => {
            let _ = allow_leading_dot;
            Some(quote! {
                {
                    let span_lo = *p as u32;
                    let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                        return ::core::result::Result::Err(
                            ::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state:
                                    ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        );
                    };
                    *p += match_len as usize;
                    let span_hi = *p as u32;
                    let __f64: f64 = core::str::from_utf8(
                        &input[span_lo as usize..span_hi as usize]
                    )
                    .ok()
                    .and_then(|s| s.parse::<f64>().ok())
                    .unwrap_or(0.0);
                    let __arena_off: u32 =
                        builder.arena_mut().len() as u32;
                    builder
                        .arena_mut()
                        .extend_from_slice(&__f64.to_le_bytes());
                    let _ = builder.push_leaf_with_arena_payload(
                        #leaf_kind,
                        span_lo,
                        span_hi,
                        #variant_idx,
                        0u8,
                        __arena_off,
                        8u32,
                    );
                }
            })
        }
        _ => None,
    }
}

// ─────────────────────────────────────────────────────────────────────
// AX.W0a.2.p — typed-payload Alt-branch emission (Class 2).
// ─────────────────────────────────────────────────────────────────────

/// True iff every branch in `branches` carries a `Map { inner: literal
/// or literal-chain, IntLit | BoolLit }` annotation — the factored-Alt
/// shape Sheets `error_literal` and analogous CSS typed-discriminant
/// rules produce post-factoring.
///
/// Returns false when any branch is Ref-led, Regex-led, or structurally
/// incompatible with per-literal byte dispatch (the inline module's
/// linear-try fallback retains ownership of those cases).
fn alt_branches_carry_typed_payloads(
    branches: &[bbnf_ir::AltBranch],
    ir: &GrammarIR,
) -> bool {
    if branches.is_empty() {
        return false;
    }
    branches
        .iter()
        .all(|b| !extract_branch_typed_payload(&b.node, ir).is_empty())
}

/// Extract every `(literal_bytes, payload_u32)` pair a Map-carrying
/// Alt-branch declares, including nested factored branches
/// (`Seq([Literal(prefix), Alt([Map{…}, Map{…}])])` expands to the
/// Cartesian product of `prefix` with each inner Map's suffix +
/// payload).
///
/// Recognized shapes (recursing through `Map` wrappers and descending
/// `Seq` / `Next` / `Skip` / nested `Alt` prefix-factored bodies):
///
/// - `Map { Literal(s), IntLit(n) }` — single-literal branch.
/// - `Map { Literal(s), BoolLit(b) }` — single-literal with bool.
/// - `Map { Seq([Literal(a), Literal(b), …]), IntLit(n) }` —
///   literal-chain branch.
/// - `Seq([Literal(prefix), Alt([Map{Literal(s1), IntLit(n1)}, …])])`
///   — factored branch emitted post-prefix-factoring (Sheets
///   `error_literal`'s `Seq("N", Alt("/A"→0, "ULL!"→4, "UM!"→6,
///   "AME?"→5))` pattern). Expands into four flat
///   `(literal_bytes, payload)` pairs.
///
/// Returns an empty vector when the branch is not structurally
/// eligible for the typed-payload byte-dispatch.
fn extract_branch_typed_payload(
    node: &bbnf_ir::IrNode,
    ir: &GrammarIR,
) -> Vec<(Vec<u8>, u32)> {
    use bbnf_ir::{FnDescriptor, IrNode, MapExpr};
    match node {
        IrNode::Map { inner, fn_id } => {
            let Some(fd) = ir.fns.get(*fn_id as usize) else {
                return Vec::new();
            };
            let FnDescriptor::Expr { expr, .. } = fd else {
                return Vec::new();
            };
            let payload = match expr {
                MapExpr::IntLit(n) => *n as u32,
                MapExpr::BoolLit(b) => {
                    if *b {
                        1u32
                    } else {
                        0u32
                    }
                }
                _ => return Vec::new(),
            };
            let Some(bytes) = branch_literal_bytes(inner, ir) else {
                return Vec::new();
            };
            if bytes.is_empty() {
                return Vec::new();
            }
            vec![(bytes, payload)]
        }
        IrNode::OptionalWhitespace(inner) => {
            extract_branch_typed_payload(inner, ir)
        }
        IrNode::Seq(children) => {
            // Factored prefix + Alt-of-Map branch. The canonical
            // shape is `Seq([Literal(prefix), Alt([Map{...}, ...])])`,
            // possibly interleaved with trivial `Epsilon` /
            // `OptionalWhitespace` positions the lifter admits at
            // prefix boundaries.
            let substantive: Vec<&IrNode> = children
                .iter()
                .filter(|c| {
                    !matches!(
                        c,
                        IrNode::Epsilon | IrNode::OptionalWhitespace(_)
                    )
                })
                .collect();
            if substantive.len() != 2 {
                return Vec::new();
            }
            // First position is a literal (or literal-chain) prefix.
            let Some(prefix) = branch_literal_bytes(substantive[0], ir) else {
                return Vec::new();
            };
            // Second position is an Alt whose branches each are
            // Map-annotated literals.
            let suffix_alt = substantive[1];
            let IrNode::Alt(suffix_branches, _) = suffix_alt else {
                return Vec::new();
            };
            let mut out = Vec::new();
            for suffix_branch in suffix_branches {
                let suffix_pairs =
                    extract_branch_typed_payload(&suffix_branch.node, ir);
                if suffix_pairs.is_empty() {
                    return Vec::new();
                }
                for (suffix_bytes, payload) in suffix_pairs {
                    let mut combined = prefix.clone();
                    combined.extend(suffix_bytes);
                    out.push((combined, payload));
                }
            }
            out
        }
        _ => Vec::new(),
    }
}

/// Collect a literal-chain's byte sequence from a flat Map-free
/// structural body. Descends `Seq` / `Next` / `Skip` /
/// `OptionalWhitespace` / `Map` (the inner Map the lifter leaves in
/// place after factoring); rejects when any position is non-literal
/// (Ref, Regex, Alt, Repeat, Negate, Minus, TokenDispatch).
fn branch_literal_bytes(
    node: &bbnf_ir::IrNode,
    ir: &GrammarIR,
) -> Option<Vec<u8>> {
    use bbnf_ir::IrNode;
    match node {
        IrNode::Literal(sid) => Some(ir.get_string(*sid).as_bytes().to_vec()),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            branch_literal_bytes(inner, ir)
        }
        IrNode::Seq(children) => {
            let mut acc = Vec::new();
            for c in children {
                acc.extend(branch_literal_bytes(c, ir)?);
            }
            Some(acc)
        }
        IrNode::Next(l, r) | IrNode::Skip(l, r) => {
            let mut acc = branch_literal_bytes(l, ir)?;
            acc.extend(branch_literal_bytes(r, ir)?);
            Some(acc)
        }
        _ => None,
    }
}

/// Emit a byte-dispatch Alt where each branch commits to its literal
/// prefix + writes the typed-byte payload via
/// `push_leaf_with_arena_payload`. The emitted block reads one outer
/// Alt compound around the chosen branch's leaf — matching the
/// walker's Alt+Literal layering so downstream view accessors see the
/// same record stream.
///
/// Branches whose first byte collides collapse into one match arm that
/// tries each longer prefix first, then shorter (mirrors the Keyword
/// emitter's prefix-length-descending admission).
fn emit_alt_typed_payload_tape(
    branches: &[bbnf_ir::AltBranch],
    support_mod: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    use std::collections::BTreeMap;
    let _ = grammar_suffix;
    let mut by_first: BTreeMap<u8, Vec<(Vec<u8>, u32)>> = BTreeMap::new();
    for b in branches {
        let pairs = extract_branch_typed_payload(&b.node, ir);
        if pairs.is_empty() {
            return quote! {
                // Defensive fallback — the admission predicate
                // `alt_branches_carry_typed_payloads` screened this,
                // but emit nothing to keep codegen valid if a later
                // mutation violates it.
            };
        }
        for (bytes, payload) in pairs {
            let first = *bytes.first().unwrap_or(&0);
            by_first.entry(first).or_default().push((bytes, payload));
        }
    }
    let byte_arms: Vec<TokenStream> = by_first
        .iter()
        .map(|(first, group)| {
            // Longer prefixes first so the exact-match branch commits
            // before any prefix-only neighbour. Stable order preserves
            // grammar-declaration order within equal-length groups.
            let mut ordered: Vec<_> = group.clone();
            ordered.sort_by(|a, b| b.0.len().cmp(&a.0.len()));
            let tries: Vec<TokenStream> = ordered
                .iter()
                .map(|(bytes, payload)| {
                    let len = bytes.len();
                    let byte_lits: Vec<TokenStream> =
                        bytes.iter().map(|b| quote! { #b }).collect();
                    let payload_u32 = *payload;
                    quote! {
                        if input.len() >= *p + #len
                            && input[*p..*p + #len] == [#(#byte_lits),*]
                        {
                            let at = *p;
                            let end = at + #len;
                            *p = end;
                            let __arena_off: u32 =
                                builder.arena_mut().len() as u32;
                            builder.arena_mut().push((#payload_u32) as u8);
                            let _ = builder.push_leaf_with_arena_payload(
                                ::bbnf::runtime::tape::TapeKind::Span,
                                at as u32,
                                end as u32,
                                0u8,
                                0u8,
                                __arena_off,
                                1u32,
                            );
                            break 'try_branches;
                        }
                    }
                })
                .collect();
            let first_lit = *first;
            quote! {
                #first_lit => {
                    #(#tries)*
                }
            }
        })
        .collect();
    quote! {
        {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            let alt_lo = *p as u32;
            // AY-II.W0.b — walker-parity post-order Alt compound.
            let alt_child = builder.columns_mut().len() as u32;
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                return ::core::result::Result::Err(
                    ::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    },
                );
            }
            let alt_hi = *p as u32;
            let __alt_off = builder.begin_compound(
                ::bbnf::runtime::tape::TapeKind::Alt,
                alt_lo,
                0u8,
                0u16,
            );
            builder.end_compound(__alt_off, alt_hi);
            builder.columns_mut().set_child_off_at(
                __alt_off,
                ::bbnf::runtime::tape::TapeOffset(alt_child),
            );
        }
    }
}
