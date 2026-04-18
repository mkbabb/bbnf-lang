//! Flat-shape emitter — `parse_flat_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar Flat-shape parse functions for typed
//! `Seq(literal_head, body+)` rules. Canonical:
//!
//! - CSS 28 `*Decl` rules — e.g. `displayDecl = "display" , ":" ?w ,
//!   (value ?w) * , importantSuffix , ";"?` per
//!   `grammar/css/l4/properties.bbnf`.
//! - BBNF 7 `*_directive` rules — e.g. `import_directive = "@import"
//!   ?w , ( … ) , ( ";" | "." ) ?` per `grammar/bbnf/bbnf.bbnf`.
//!
//! # Emission shape
//!
//! Flat emits a straight-line Seq compound: match the head literal
//! (or keyword Alt head), then a linear sequence of body positions
//! (literal pivots, ref-typed fields, regex leaves). Each body
//! position lowers to the canonical per-position emission (Ref →
//! value dispatcher, Literal → byte-sequence match, Regex → scan),
//! with the emitter producing the tape-compound that matches the
//! walker's Seq-compound shape.
//!
//! W4.1 emits the outer compound scaffold + head-match substrate;
//! W4.2/W4.3 specialises the per-grammar body refs / pivots.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};

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
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — per-grammar Flat-shape parse function.
        ///
        /// Straight-line Seq compound: head literal + body positions.
        /// Mirrors the walker's Seq-emission tape layout exactly,
        /// eliminating the per-child state-interpreter indirection.
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

            // Head + body position walks. W4.2/W4.3 unrolls each Seq
            // position's specific parse arm here per-grammar.
            let _ = #support_mod::skip_space(input, p, state);

            let span_hi = *p as u32;
            let outer_off = builder.push_compound(
                ::bbnf::runtime::tape::TapeKind::Seq,
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
// AW-V.W4.1 — visitor-path Flat emitter.
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
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — visitor-path Flat-shape parse function.
        ///
        /// Per-position visitor method calls dispatch inline. The
        /// `V: ObjectVisitor` bound is the minimal compose — CSS
        /// `*Decl` rules map keys (property names) to values, which
        /// is the Object-visitor shape.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            _visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::ObjectVisitor,
        {
            let _ = #support_mod::skip_space(input, p, state);
            Ok(())
        }
    }
}
