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

    let body_emission = emit_tape_positions(
        &positions,
        variant_idx,
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
            let outer_child = builder.mark_children();

            #body_emission

            let span_hi = *p as u32;
            let outer_off = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
                outer_child,
                span_lo,
                span_hi,
                #variant_idx,
                0,
            );
            Ok(outer_off)
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
            support_mod,
            dispatcher_ident,
            ir,
        ),
        IrNode::Seq(children) => {
            let inner = emit_tape_seq_children(
                children,
                variant_idx,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! {
                let seq_lo = *p as u32;
                let seq_child = builder.mark_children();
                #inner
                let seq_hi = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    seq_child,
                    seq_lo,
                    seq_hi,
                    0,
                    0,
                );
            }
        }
        IrNode::Next(lhs, rhs) | IrNode::Skip(lhs, rhs) => {
            let l = emit_tape_position_core(
                lhs,
                variant_idx,
                support_mod,
                dispatcher_ident,
                ir,
            );
            let r = emit_tape_position_core(
                rhs,
                variant_idx,
                support_mod,
                dispatcher_ident,
                ir,
            );
            quote! { #l #r }
        }
        IrNode::Map { inner, .. } => emit_tape_position_core(
            inner,
            variant_idx,
            support_mod,
            dispatcher_ident,
            ir,
        ),
        IrNode::OptionalWhitespace(inner) => {
            let inner_emit = emit_tape_position_core(
                inner,
                variant_idx,
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
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    let mut out = Vec::with_capacity(children.len());
    for child in children {
        out.push(emit_tape_position_core(
            child,
            variant_idx,
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
            support_mod,
            dispatcher_ident,
            ir,
        ),
        _ => emit_tape_position_core(
            inner,
            variant_idx,
            support_mod,
            dispatcher_ident,
            ir,
        ),
    };
    let lo_lit = lo as usize;

    if hi == 1 && lo == 0 {
        // Optional. AX.W0a.2.h — wrap the inner in an attempt so
        // failure silently rolls back `*p` + tape columns instead
        // of propagating to the enclosing shape fn. Prior emission
        // bubbled any inner `Err` through, which breaks `?`-gated
        // positions like `"|" ?` in BBNF's `alternation` iter
        // (optional separator) — the failing `|` check aborted the
        // whole rule. Matches walker's `lo==0` optional semantics.
        let _ = lo_lit;
        quote! {
            let repeat_lo = *p as u32;
            let repeat_child = builder.mark_children();
            let iter_save_p = *p;
            let iter_save_cols = builder.columns_mut().len();
            let iter_lo = *p as u32;
            let iter_child = builder.mark_children();
            let opt_attempt: ::core::result::Result<(), ::bbnf::runtime::tape::DtaError> =
                (|| {
                    #inner_emit
                    Ok(())
                })();
            let matched = opt_attempt.is_ok();
            if !matched {
                *p = iter_save_p;
                builder.columns_mut().truncate(iter_save_cols);
            } else {
                let iter_hi = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_child,
                    iter_lo,
                    iter_hi,
                    0,
                    0,
                );
            }
            let repeat_hi = *p as u32;
            let _ = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                repeat_child,
                repeat_lo,
                repeat_hi,
                0,
                0,
            );
        }
    } else {
        // Generic repeat. Iterate greedily.
        //
        // AX.W0a.2.g — column truncation on iter failure + zero-width
        // break. Walker's `handle_repeat_failure_bounded` rolls back
        // `columns` to the iter's savepoint on Err or zero-width
        // success so orphan leaves pushed inside the inner_emit don't
        // leak into the surrounding tape. Without truncation, a
        // zero-width regex match (e.g. `/[ \t]*/` at EOF) emits its
        // Span leaf, the iter `*p == save_p` break fires, and the leaf
        // remains on the tape outside any iter-Seq compound.
        quote! {
            let repeat_lo = *p as u32;
            let repeat_child = builder.mark_children();
            let mut iter_count: u32 = 0;
            loop {
                let save_p = *p;
                let save_cols = builder.columns_mut().len();
                let iter_lo = *p as u32;
                let iter_child = builder.mark_children();
                let attempt = (|| -> ::core::result::Result<(), ::bbnf::runtime::tape::DtaError> {
                    #inner_emit
                    Ok(())
                })();
                if attempt.is_err() {
                    *p = save_p;
                    builder.columns_mut().truncate(save_cols);
                    break;
                }
                // Protect against non-progressing iterations.
                if *p == save_p {
                    builder.columns_mut().truncate(save_cols);
                    break;
                }
                let iter_hi = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_child,
                    iter_lo,
                    iter_hi,
                    0,
                    0,
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
            let _ = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                repeat_child,
                repeat_lo,
                repeat_hi,
                0,
                0,
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
