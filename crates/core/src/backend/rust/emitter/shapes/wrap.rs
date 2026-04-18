//! Wrap-shape emitter — `parse_wrap_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar Wrap-shape parse functions for transparent
//! `Alt(Ref, Ref, …)` dispatchers. The Wrap-shape rule emits NO
//! compound of its own — it's a pass-through that dispatches to the
//! chosen branch rule's shape fn.
//!
//! Canonical:
//! - JSON `value = object | array | string | number | bool | null` —
//!   byte-dispatch onto the 6 branch shape fns. The grammar's
//!   dispatcher emitter at [`super::dispatcher`] already handles
//!   this case; Wrap-shape is the explicit tag that identifies the
//!   pattern for non-root occurrences.
//! - CSS `color = colorMix | colorFn | hex | colorFunction |
//!   namedColor` — each branch is a Ref.
//! - Sheets `range_end = cell_ref | /\$?[A-Za-z]{1,3}/ | /\$?\d+/` —
//!   mixed Ref + Regex branches.
//!
//! # Emission shape
//!
//! The emitted function performs byte-dispatch on the first
//! non-whitespace byte and directly delegates to the chosen branch's
//! shape fn. No outer compound is pushed — the branch's own
//! compound carries the final record.
//!
//! Like the other W4 emitters, W4.1 emits the scaffolding; the
//! per-grammar branch resolution lives in the W4.2/W4.3 consumer
//! waves that wire the Ref → shape-fn-ident mapping.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};

/// Emit `pub fn parse_wrap_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_wrap(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — per-grammar Wrap-shape parse function.
        ///
        /// Transparent dispatcher — byte-match the first non-ws
        /// byte → call the matched branch's shape fn. No compound
        /// emission here; the branch's own shape fn owns the
        /// tape record.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            _builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            // Byte-dispatch substrate; W4.2/W4.3 fills per-branch
            // arms with the matched branch shape fn call.
            let _first = #support_mod::skip_space(input, p, state)
                .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            Ok(::bbnf::runtime::tape::TapeOffset::NONE)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4.1 — visitor-path Wrap emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_wrap_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_wrap_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — visitor-path Wrap-shape parse function.
        ///
        /// Transparent dispatcher — no visitor method fires; the
        /// chosen branch's visitor-path shape fn handles the visit.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            _visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ObjectVisitor
                + ::bbnf::runtime::tape::ArrayVisitor
                + ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::NumberVisitor
                + ::bbnf::runtime::tape::KeywordVisitor,
        {
            let _first = #support_mod::skip_space(input, p, state)
                .ok_or(::bbnf::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })?;
            Ok(())
        }
    }
}
