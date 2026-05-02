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
//! StructDirect parity: every structural IR production routes through
//! the grammar's concrete [`StructBuilder`](crate::runtime::StructBuilder).

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{dispatcher_fn_ident, emit_ref_call_shape, shape_fn_ident};
use super::root_rule_name;
use super::substrate::builder_ty_with_lifetime;
use bbnf_ir::registry::EmitStrategy;

/// Emit `pub fn parse_arglist_<grammar>_<rule>(input, p, state,
/// builder) -> Result<(), DtaError>`.
///
/// # AZ-I.W2.RE — strategy gate
///
/// `strategy` is the codegen-time substrate selector resolved by
/// [`EmitStrategy::for_grammar`] in `shapes/mod.rs`. AZ-II O5 leaves
/// only the StructDirect substrate.
pub fn emit_parse_arglist(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    emit_parse_arglist_struct_direct(strategy, grammar_suffix, rule, ir)
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
        IrNode::OptionalWhitespace(inner) => walk_positions(inner, true, true, out),
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
// AZ-I.W2-act.B3 — ArgList struct-direct body.
// ─────────────────────────────────────────────────────────────────────

/// Emit the struct-direct ArgList body. Opens a compound on the
/// builder via `begin_compound(&__layout)`; the grammar's
/// StructBuilder routes the (LayoutKind, rule_name) to its concrete
/// frame variant (CSS L4: Function frame; Sheets: similar). Walks the
/// body via `emit_struct_body`, each Ref / Regex / dispatcher call
/// passing the `&mut <builder_ty>` argument transparently. Closes
/// with `end_compound(handle)`.
fn emit_parse_arglist_struct_direct(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("arglist", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_id_lit = rule.id;
    let rule_name_lit = rule_name.to_string();

    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let positions = collect_positions(&rule.body);
    let body_emission = emit_struct_direct_body(
        &positions,
        &support_mod,
        &dispatcher_ident,
        grammar_suffix,
        ir,
    );

    quote! {
        /// AZ-I.W2-act.B3 — per-grammar ArgList-shape parse function,
        /// **struct-direct body**.
        ///
        /// Opens a compound on the grammar's StructBuilder
        /// (`begin_compound(&__layout)`), walks the head + parens +
        /// arg positions, and closes via `end_compound(handle)`. The
        /// builder routes the (LayoutKind, rule_name) to its concrete
        /// Function frame variant (CSS L4 — calc / min / max / clamp
        /// / var / env / url / gradient / transform / etc.).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
            cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            __P: for<'__c> crate::path::schema::PathSchema<'__c>,
        {
            let _ = cursor;
            let __layout: ::bbnf_ir::registry::StructLayout =
                ::bbnf_ir::registry::StructLayout {
                    rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
                    rule_name: ::std::string::String::from(#rule_name_lit),
                    kind: ::bbnf_ir::registry::LayoutKind::Struct,
                    rule_type: ::bbnf_ir::TypeDesc::Span,
                    fields: ::std::vec::Vec::new(),
                };
            let __arglist_checkpoint = <
                #builder_ty as crate::runtime::StructBuilder
            >::checkpoint(builder);
            let __handle = <
                #builder_ty as crate::runtime::StructBuilder
            >::begin_compound(builder, &__layout);
            // AZ-II.cutover.K Phase 2 — IIFE wrapping the body so any
            // inner `?`-propagation closes the matching frame.
            let __body_result: ::core::result::Result<
                (),
                crate::runtime::DtaError,
            > = (|| {
                #body_emission
                ::core::result::Result::Ok(())
            })();
            match __body_result {
                ::core::result::Result::Ok(()) => {
                    <
                        #builder_ty as crate::runtime::StructBuilder
                    >::end_compound(builder, __handle);
                    Ok(())
                }
                ::core::result::Result::Err(__err) => {
                    <
                        #builder_ty as crate::runtime::StructBuilder
                    >::rollback(builder, __arglist_checkpoint);
                    ::core::result::Result::Err(__err)
                }
            }
        }
    }
}

/// Walk each position and emit per-position struct-direct body.
fn emit_struct_direct_body(
    positions: &[PositionedNode],
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    grammar_suffix: &str,
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
        let core = emit_struct_direct_position_core(
            pos.node,
            support_mod,
            dispatcher_ident,
            grammar_suffix,
            ir,
        );
        emissions.push(quote! { #leading #core #trailing });
    }
    quote! { #(#emissions)* }
}

/// Emit one position's struct-direct core.
fn emit_struct_direct_position_core(
    node: &IrNode,
    support_mod: &proc_macro2::Ident,
    dispatcher_ident: &proc_macro2::Ident,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            let len = bytes.len();
            let byte_lits: Vec<TokenStream> = bytes.iter().map(|b| quote! { #b }).collect();
            quote! {
                let at = *p;
                let end = at + #len;
                if input.len() < end || input[at..end] != [#(#byte_lits),*] {
                    return ::core::result::Result::Err(
                        crate::runtime::DtaError::Syntax {
                            offset: at as u32,
                        },
                    );
                }
                *p = end;
            }
        }
        IrNode::Ref(rid) => {
            if let Some(call) = emit_ref_call_shape(grammar_suffix, *rid, ir) {
                quote! { let _ = (#call)?; }
            } else {
                quote! {
                    let _ = #dispatcher_ident(input, p, state, builder, cursor)?;
                }
            }
        }
        IrNode::OptionalWhitespace(inner) => {
            let inner_emit = emit_struct_direct_position_core(
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
            let l = emit_struct_direct_position_core(
                lhs,
                support_mod,
                dispatcher_ident,
                grammar_suffix,
                ir,
            );
            let r = emit_struct_direct_position_core(
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
                out.push(emit_struct_direct_position_core(
                    child,
                    support_mod,
                    dispatcher_ident,
                    grammar_suffix,
                    ir,
                ));
            }
            quote! { #(#out)* }
        }
        IrNode::Map { inner, .. } => emit_struct_direct_position_core(
            inner,
            support_mod,
            dispatcher_ident,
            grammar_suffix,
            ir,
        ),
        IrNode::Epsilon => quote! {},
        IrNode::Repeat { inner, .. } => {
            // Best-effort inline: dispatch the inner repeatedly until
            // it errs, then catch and proceed. Mirrors the value-list
            // pattern in the tape body.
            let inner_emit = emit_struct_direct_position_core(
                inner,
                support_mod,
                dispatcher_ident,
                grammar_suffix,
                ir,
            );
            quote! {
                loop {
                    let __save = *p;
                    let __res: ::core::result::Result<
                        (),
                        crate::runtime::DtaError,
                    > = (|| {
                        #inner_emit
                        Ok(())
                    })();
                    if __res.is_err() {
                        *p = __save;
                        break;
                    }
                }
            }
        }
        // Alt / Regex / Negate / Minus / TokenDispatch — fall through
        // to the dispatcher; the dispatcher's body under StructDirect
        // takes the same builder argument.
        IrNode::Alt(_, _)
        | IrNode::Regex(_)
        | IrNode::Negate(_)
        | IrNode::Minus(_, _)
        | IrNode::TokenDispatch { .. } => {
            quote! {
                let _ = #dispatcher_ident(input, p, state, builder, cursor)?;
            }
        }
    }
}
