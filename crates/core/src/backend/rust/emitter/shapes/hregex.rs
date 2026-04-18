//! HRegex-shape emitter — `parse_hregex_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar HRegex-shape parse functions for regex leaves
//! whose classification is NOT QuotedString / Numeric (those are
//! String / Number shapes). Canonical:
//!
//! - CSS `ident = /[a-zA-Z_][\w-]*/` — a bare Identifier-class regex
//!   leaf.
//! - Sheets `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/ -> input : Span`.
//! - Sheets `identifier = /[A-Za-z_][A-Za-z0-9_.]*/ -> input : Span`.
//! - BBNF `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span`.
//!
//! # Emission shape
//!
//! The emitted function:
//!
//! 1. Captures `span_lo` at `*p`.
//! 2. Calls the per-grammar regex-scan adapter
//!    (`__regex_scan_<grammar>`) with the rule's pattern string; the
//!    adapter returns `Some(match_len)` on success, `None` on no-match.
//! 3. Advances `*p` by `match_len`.
//! 4. Pushes a `TapeKind::Regex` leaf carrying a `PayloadData::Span`
//!    (default `-> input : Span`) or the rule's decoded scalar payload
//!    when the rule declares `-> host_fn(input) : type`.
//!
//! # Host decode
//!
//! Rules with `-> <fn>(input) : <type>` annotations (e.g. CSS `hex ->
//! parse_hex_color(input) : u32`) emit a call to the declared host
//! function post-scan and package the return as a `PayloadData`
//! variant matching the type. The AW-V emitter honours the same
//! contract as the walker's `DtaState::Regex` arm: when a decoder
//! selector is attached, the arena slot is pre-allocated up front.
//!
//! W4.1 emitted scaffolding; W4-fix ships the walker-parity body
//! that invokes the regex scanner and pushes a Span leaf.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dta_walker::regex_scan_adapter_ident;
use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};
use super::sanitise_grammar;

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
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Extract the regex pattern from the rule body. HRegex admits a
    // single Regex leaf (after Map / OW stripping); when the body
    // is a different shape, emit a minimal scaffold that returns an
    // error — the detector gate is upstream.
    let pattern_sid = extract_regex_pattern(&rule.body);
    let Some(sid) = pattern_sid else {
        return emit_unsupported_stub(&fn_ident, &support_mod, variant_idx);
    };
    let pattern = ir.get_string(sid);
    let pattern_lit = pattern.to_string();

    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    quote! {
        /// AW-V.W4-fix — per-grammar HRegex-shape parse function.
        ///
        /// Regex scan via the per-grammar adapter; emits a
        /// `TapeKind::Regex` leaf carrying the matched span. Decoder
        /// hooks (host_fn payloads) are wired at the dispatcher level
        /// post-scan; the raw Span-leaf path is the default.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
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
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: span_lo,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            };
            *p += match_len as usize;
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

/// Emit a defensive stub when the rule body is not a single Regex
/// leaf. The HRegex detector should gate admission upstream; this
/// branch exists only to keep the emitter output compilable when the
/// emitter is invoked over a misclassified rule.
fn emit_unsupported_stub(
    fn_ident: &proc_macro2::Ident,
    support_mod: &proc_macro2::Ident,
    variant_idx: u8,
) -> TokenStream {
    quote! {
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let _ = state;
            let span_lo = *p as u32;
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

/// Extract the regex pattern's `StringId` from the rule body, walking
/// through Map / OptionalWhitespace trivia wrappers.
fn extract_regex_pattern(node: &IrNode) -> Option<u32> {
    match node {
        IrNode::Regex(sid) => Some(*sid),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_regex_pattern(inner)
        }
        _ => None,
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4-fix — visitor-path HRegex emitter.
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
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let pattern_sid = extract_regex_pattern(&rule.body);
    let Some(sid) = pattern_sid else {
        return quote! {
            #[inline(always)]
            #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
            pub fn #fn_ident<V>(
                input: &[u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                visitor: &mut V,
            ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
            where
                V: ::bbnf::runtime::tape::StringVisitor,
            {
                let _ = (input, p, state, visitor);
                Ok(())
            }
        };
    };
    let pattern = ir.get_string(sid);
    let pattern_lit = pattern.to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    quote! {
        /// AW-V.W4-fix — visitor-path HRegex-shape parse function.
        ///
        /// Regex scan via the per-grammar adapter; fires the
        /// visitor's `string()` event with the matched span when
        /// visitor is a StringVisitor. Non-string decoders (host_fn
        /// payloads) dispatch at the per-grammar consumer wave.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::StringVisitor,
        {
            let span_lo = *p;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                });
            };
            let span_hi = *p + match_len as usize;
            *p = span_hi;
            visitor.string(&input[span_lo..span_hi]).map_err(|_| {
                ::bbnf::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                }
            })
        }
    }
}
