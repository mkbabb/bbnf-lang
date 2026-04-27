//! ArgList-shape emitter — `parse_arglist_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar ArgList-shape parse functions for
//! `name(arg, arg, …)` positional function calls. The emitted body
//! matches the function-name head, consumes the `(`, runs the
//! positional-arg body via the grammar's value-position dispatcher,
//! matches the `)`, and emits a `Rule` outer compound covering the
//! whole call site.
//!
//! # Head variants
//!
//! The detector admits three head variants; the emitter produces a
//! matching branch per variant:
//!
//! 1. **Literal head** (`"calc" , "(" >> body << ")"`) — direct
//!    byte-sequence match for the literal, followed by a separate `"("`
//!    position.
//! 2. **Regex head with inline `(`** (`/[lL][eE][tT]\(/, body, ")"`)
//!    — the regex scan consumes the `(` as part of the lexeme; no
//!    separate `(` consume is emitted.
//! 3. **Ref head** — two flavours:
//!    - Ref to an identifier-like rule whose body ends with `"("`
//!      (Sheets `func_open = identifier , "("`). The ref call is
//!      responsible for consuming the `(`.
//!    - Ref to a plain identifier path followed by a separate `"("`
//!      body position (BBNF `value_fn_call = value_path , "(" , args
//!      , ")"`).
//!
//! # Emission shape
//!
//! ```text
//! Rule compound {
//!   span_lo = *p
//!   <head match — Literal byte-seq / dispatcher call for Ref / Regex>
//!   [ "(" Literal leaf — when head doesn't consume the paren ]
//!   <arg body — each inter-paren position via dispatcher>
//!   ")" Literal leaf
//!   span_hi = *p
//! }
//! ```
//!
//! # Wire contract
//!
//! Walker-tape parity: every structural IR production → one tape
//! record. The emitter is gated behind `has_full_shape_coverage` in
//! [`super::emit_shapes_for_grammar`].

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident,
    visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

/// Emit `pub fn parse_arglist_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_arglist(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("arglist", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // Flatten the rule body into positional IR nodes.
    let positions = collect_positions(&rule.body);

    let body_emission = emit_tape_body(
        &positions,
        variant_idx,
        &support_mod,
        &dispatcher_ident,
        ir,
    );

    quote! {
        /// AW-V.W4-fix — per-grammar ArgList-shape parse function.
        ///
        /// Emits one outer Rule compound over the whole call site.
        /// Head (Literal / Regex / Ref) + optional `(` + body arg
        /// positions (dispatched through the grammar's value-
        /// dispatcher) + `)` literal.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale (see `flat.rs`).
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
            // AY-II.W0.b — walker-parity post-order outer Rule compound.
            // B5.W6 — bracket the post-order children scope.
            let outer_child = builder.enter_post_order_children();

            #body_emission

            let span_hi = *p as u32;
            let outer_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Rule,
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

// ─────────────────────────────────────────────────────────────────────
// Position collection + tape emission
// ─────────────────────────────────────────────────────────────────────

/// A flattened position in the rule body carrying leading / trailing
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

/// Emit the tape-path body for all positions.
fn emit_tape_body(
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

/// Emit the record-producing core for one position. ArgList shares
/// the same structure as Flat; the difference is only the outer
/// compound kind (Rule vs Seq) which the caller emits.
fn emit_tape_position_core(
    node: &IrNode,
    variant_idx: u8,
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
                    return Err(crate::runtime::tape::DtaError::Syntax {
                        offset: at as u32,
                        failing_state: crate::runtime::tape::DtaStateId::NONE,
                        failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                *p = end;
                let _ = builder.push_leaf_with(
                    crate::runtime::tape::TapeKind::Literal,
                    at as u32,
                    end as u32,
                    #variant_idx,
                    0,
                    crate::runtime::tape::PayloadData::None,
                );
            }
        }
        IrNode::Ref(rid) => {
            // AW-V.W5.2 — direct per-Ref routing.
            if let Some(call) = emit_ref_call_tape(&grammar_suffix, *rid, ir) {
                quote! { let _ = (#call)?; }
            } else {
                quote! {
                    let _ = #dispatcher_ident(input, p, state, builder)?;
                }
            }
        }
        IrNode::Regex(_) | IrNode::Alt(_, _)
        | IrNode::Negate(_) | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => {
            // AX.W0a.2.e — inline-position emission (tape path).
            let _ = dispatcher_ident;
            super::inline::emit_inline_position_tape(
                node, variant_idx, support_mod, &grammar_suffix, ir,
            )
        }
        IrNode::Repeat { inner, lo, hi } => {
            let inner_emit = emit_tape_position_core(
                inner,
                variant_idx,
                support_mod,
                dispatcher_ident,
                ir,
            );
            let lo_lit = *lo as usize;
            if *hi == 1 && *lo == 0 {
                // Optional — attempt once, restore on failure.
                // AY-II.W0.b — post-order iter Seq compound via
                // begin_compound_post / end_compound_post_order; retry
                // uses rollback_to (iter_save_cols).
                //
                // B5.W6 — bracket the iter Seq's post-order children
                // scope; on failure exit the bracket alongside the
                // rollback so `current_depth` mirrors the structural
                // rewind.
                quote! {
                    let save_p = *p;
                    let iter_save_cols = builder.position();
                    let iter_lo = *p as u32;
                    let iter_child = builder.enter_post_order_children();
                    let attempt = (|| -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
                        #inner_emit
                        Ok(())
                    })();
                    if attempt.is_err() {
                        *p = save_p;
                        builder.rollback_to(iter_save_cols);
                        builder.exit_post_order_children();
                    } else {
                        let iter_hi = *p as u32;
                        let __iter_off = builder.begin_compound_post(
                            crate::runtime::tape::TapeKind::Seq,
                            iter_lo,
                            0,
                            0u8,
                            0u16,
                        );
                        builder.end_compound_post_order(
                            __iter_off,
                            iter_hi,
                            crate::runtime::tape::TapeOffset(iter_child),
                        );
                    }
                }
            } else {
                // Generic repeat — iterate greedily, count iters.
                // B5.W6 — bracket the outer Repeat scope and each
                // per-iter Seq scope; failure paths close the inner
                // bracket; the outer bracket closes either via the
                // `end_compound_post_order` success path or via
                // `exit_post_order_children` on the underflow error.
                quote! {
                    let repeat_lo = *p as u32;
                    let repeat_child = builder.enter_post_order_children();
                    let mut iter_count: u32 = 0;
                    loop {
                        let save_p = *p;
                        let save_cols = builder.position();
                        let iter_lo = *p as u32;
                        let iter_child = builder.enter_post_order_children();
                        let attempt = (|| -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
                            #inner_emit
                            Ok(())
                        })();
                        if attempt.is_err() {
                            *p = save_p;
                            builder.rollback_to(save_cols);
                            builder.exit_post_order_children();
                            break;
                        }
                        if *p == save_p {
                            builder.rollback_to(save_cols);
                            builder.exit_post_order_children();
                            break;
                        }
                        let iter_hi = *p as u32;
                        let __iter_off = builder.begin_compound_post(
                            crate::runtime::tape::TapeKind::Seq,
                            iter_lo,
                            0,
                            0u8,
                            0u16,
                        );
                        builder.end_compound_post_order(
                            __iter_off,
                            iter_hi,
                            crate::runtime::tape::TapeOffset(iter_child),
                        );
                        iter_count = iter_count.saturating_add(1);
                    }
                    if iter_count < (#lo_lit as u32) {
                        builder.exit_post_order_children();
                        return Err(crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                    let repeat_hi = *p as u32;
                    let __repeat_off = builder.begin_compound_post(
                        crate::runtime::tape::TapeKind::Rule,
                        repeat_lo,
                        0,
                        0u8,
                        0u16,
                    );
                    builder.end_compound_post_order(
                        __repeat_off,
                        repeat_hi,
                        crate::runtime::tape::TapeOffset(repeat_child),
                    );
                }
            }
        }
        IrNode::Seq(children) => {
            let mut out = Vec::with_capacity(children.len());
            for c in children {
                out.push(emit_tape_position_core(
                    c,
                    variant_idx,
                    support_mod,
                    dispatcher_ident,
                    ir,
                ));
            }
            quote! {
                let seq_lo = *p as u32;
                // B5.W6 — bracket the post-order Seq's children.
                let seq_child = builder.enter_post_order_children();
                #(#out)*
                let seq_hi = *p as u32;
                let __seq_off = builder.begin_compound_post(
                    crate::runtime::tape::TapeKind::Seq,
                    seq_lo,
                    0,
                    0u8,
                    0u16,
                );
                builder.end_compound_post_order(
                    __seq_off,
                    seq_hi,
                    crate::runtime::tape::TapeOffset(seq_child),
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

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4-fix — visitor-path ArgList emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_arglist_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_arglist_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("arglist", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let positions = collect_positions(&rule.body);
    let body_emission = emit_visitor_body(
        &positions,
        &support_mod,
        &dispatcher_ident,
        ir,
    );

    quote! {
        /// AW-V.W4-fix — visitor-path ArgList-shape parse function.
        ///
        /// Visitor method dispatch replaces tape record writes.
        /// Literal positions byte-match only; Ref / Regex / Alt
        /// positions recurse through the visitor dispatcher.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
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

fn emit_visitor_body(
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
    // AW-V.W5.2 — per-Ref routing.
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
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: at as u32, rule: None,
                    });
                }
                *p = end;
            }
        }
        IrNode::Ref(rid) => {
            // AW-V.W5.2 — direct per-Ref routing.
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
                    let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
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
                        let res = (|| -> ::core::result::Result<(), crate::runtime::ParseErr> {
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
                        return Err(crate::runtime::ParseErr::Syntax {
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
