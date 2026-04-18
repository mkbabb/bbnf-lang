//! HRegex-shape emitter — `parse_hregex_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar HRegex-shape parse functions for regex leaves
//! whose classification is NOT QuotedString / Numeric (those are
//! String / Number shapes). Canonical:
//!
//! - CSS `hex = "#" , /[0-9a-fA-F]{3,8}/ -> parse_hex_color(input) :
//!   u32` — the Seq is ArgList-like; the pure-regex case is CSS
//!   `ident = /-?[a-zA-Z_][\w-]*/`.
//! - Sheets `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/ -> input : Span`.
//! - Sheets `identifier = /[A-Za-z_][A-Za-z0-9_.]*/ -> input : Span`.
//! - BBNF `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span`.
//!
//! # Emission shape
//!
//! The emitted function runs a regex scan over the input via the
//! rule's cached pattern alphabet (the
//! [`bbnf_ir::passes::recognizers::pattern_alphabet::PatternAlphabet`]
//! miner's output), then either:
//!
//! - Emits a `TapeKind::Regex` leaf with a `PayloadData::Span` when
//!   the rule's `-> input : Span` annotation is span-typed.
//! - Emits a `TapeKind::Regex` leaf with the host-fn-decoded payload
//!   when the rule declares `-> host_fn(input) : type`.
//!
//! W4.1 emits the scaffolding (span capture + leaf compound); W4.2/
//! W4.3 wires the regex scanner + host fn per grammar.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};

/// Emit `pub fn parse_hregex_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_hregex(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("hregex", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — per-grammar HRegex-shape parse function.
        ///
        /// Regex scan + optional host-fn decode → `TapeKind::Regex`
        /// leaf. Payload routing (Span / InlineScalar / Aggregate)
        /// matches the walker's emission shape for `Map { Regex, … }`
        /// bodies.
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
            let _ = #support_mod::skip_space(input, p, state);
            // Regex scan placeholder — W4.2/W4.3 wires the per-rule
            // pattern-alphabet scanner + host-fn decode.
            let span_hi = *p as u32;
            let leaf_off = builder.push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                #variant_idx,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
            );
            Ok(leaf_off)
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4.1 — visitor-path HRegex emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_hregex_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_hregex_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("hregex", grammar_suffix, rule_name);
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — visitor-path HRegex-shape parse function.
        ///
        /// Regex scan + visitor method call (visitor's string /
        /// scalar / aggregate arm per the rule's annotation). The
        /// `V: StringVisitor` bound is the minimal compose — span-
        /// typed HRegex rules always emit a span; typed-host-fn
        /// rules specialise at the visitor impl level.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            _visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::StringVisitor,
        {
            let _ = #support_mod::skip_space(input, p, state);
            Ok(())
        }
    }
}
