//! Inline Regex emission — tape and visitor paths.
//!
//! Each Regex position emits a single `TapeKind::Span` leaf covering
//! the scan match, dispatched through the per-grammar
//! `__regex_scan_<grammar>` adapter shared with the HRegex emitter.
//! Mirrors the walker's `emit_regex_arm` (sans PSI payload scheduling
//! — inline positions don't carry host decoders; when they do, the
//! rule carrying the Regex is classified as HRegex and doesn't hit
//! this path).

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

use super::super::super::dfa_codegen::regex_scan_adapter_ident;
use super::super::sanitise_grammar;

/// Emit an inline Regex scan producing a `TapeKind::Span` leaf.
/// Uses the per-grammar regex adapter shared with the HRegex emitter.
pub(super) fn emit_regex_tape(
    pattern_sid: u32,
    variant_idx: u8,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let pattern = ir.get_string(pattern_sid);
    let pattern_lit = pattern.to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
    let variant_lit = variant_idx;
    quote! {
        {
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: span_lo,
                    },
                );
            };
            *p += match_len as usize;
            let span_hi = *p as u32;
            let _ = builder.push_leaf_with(
                ::tape::TapeKind::Span,
                span_lo,
                span_hi,
                #variant_lit,
                0,
                ::tape::PayloadData::None,
            );
        }
    }
}

pub(super) fn emit_regex_visitor(
    pattern_sid: u32,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> TokenStream {
    let pattern = ir.get_string(pattern_sid);
    let pattern_lit = pattern.to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));
    quote! {
        {
            let span_lo = *p;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return ::core::result::Result::Err(
                    crate::runtime::ParseErr::Syntax {
                        offset: span_lo as u32, rule: None,
                    },
                );
            };
            *p = span_lo + match_len as usize;
        }
    }
}
