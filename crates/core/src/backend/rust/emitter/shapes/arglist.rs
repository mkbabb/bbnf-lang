//! ArgList-shape emitter — `parse_arglist_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar ArgList-shape parse functions for
//! `name(arg, arg, …)` positional function calls. The emitted body
//! matches the function-name head, consumes the `(`, runs the
//! positional-arg repeat with separator handling, matches the `)`.
//!
//! Canonical:
//! - CSS `calcFunction = "calc" , "(" >> mathExpr << ")"` — fixed name
//!   literal.
//! - Sheets `func_call = func_open , (func_args ?) ?w , ")"` where
//!   `func_open = identifier , "("` — identifier regex head.
//! - Sheets `let_call = /[lL][eE][tT]\(/ , let_args ?w , ")"` —
//!   regex with embedded `(`.
//!
//! # Emission shape
//!
//! The emitter produces three sub-shapes depending on the head kind:
//!
//! 1. **Literal head**: direct byte-sequence match for `name`, then
//!    `(`, args, `)`.
//! 2. **Regex head (inline `(`)**: regex scan bounded to the embedded
//!    `(` position; no separate `(` consume.
//! 3. **Ref head**: call into the ref's rule (typically
//!    `func_open = identifier , "("`); the ref is responsible for
//!    the `(` advancement.
//!
//! W4.1 emits the scaffolding (head + `)` + tape compound); W4.2/W4.3
//! wires the arg-repeat body.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};

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
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — per-grammar ArgList-shape parse function.
        ///
        /// `name(arg, arg, …)` positional call. Outer `TapeKind::Rule`
        /// compound; inner children are the head (name + `(` pair),
        /// the positional args, and the closing `)`. Matches the
        /// walker's Seq-emission tape layout for Function-shape
        /// rules.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
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

            // Head match placeholder. W4.2/W4.3 extends this with the
            // per-head dispatch (Literal / Regex / Ref).
            let _ = #support_mod::skip_space(input, p, state);

            // Positional args — Repeat(arg, separator). The arg body
            // dispatches through the value-position call; W4.2/W4.3
            // fills this in with the per-grammar argument refs.
            let _ = #support_mod::skip_space(input, p, state);

            // Close paren — expected after the args.
            let _ = #support_mod::skip_space(input, p, state);

            let span_hi = *p as u32;
            let outer_off = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Rule,
                outer_child,
                span_lo,
                span_hi,
                #variant_idx,
            );
            Ok(outer_off)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4.1 — visitor-path ArgList emitter.
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
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — visitor-path ArgList-shape parse function.
        ///
        /// Visitor method dispatch replaces the tape-record writes.
        /// `V: ArrayVisitor` is the natural bound — `begin_array` at
        /// the head, `end_array` at the close, per-arg value
        /// dispatch inside. The per-grammar visitor's `begin_call` /
        /// `end_call` methods may specialise further.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            _visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ArrayVisitor,
        {
            let _ = #support_mod::skip_space(input, p, state);
            Ok(())
        }
    }
}
