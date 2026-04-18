//! Array-shape emitter — `parse_array_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Array-shape parse function mirroring
//! `bbnf_json_prototype::parse_array` (crates/bbnf-json-prototype/
//! src/lib.rs:308). The emitted function:
//!
//! 1. Marks children via [`TapeBuilder::mark_children`].
//! 2. Fast-empty check: `]` immediately after `[` closes the compound
//!    without visiting elements.
//! 3. Loop: value (recurse into dispatcher) → `,` or `]`.
//! 4. Pushes a compound record with `TapeKind::Rule` on close.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{dispatcher_fn_ident, shape_fn_ident};
use super::root_rule_name;

/// Emit `pub fn parse_array_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_array(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;

    // Non-root dispatcher — skips the outer Rule wrap. See object.rs
    // for the same rationale.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    quote! {
        /// AW-V.W3.2 — per-grammar Array-shape parse function.
        ///
        /// Mirrors `bbnf_json_prototype::parse_array`. `[` must NOT be
        /// consumed by the caller — this function reads and verifies
        /// it, advances past it, and parses the array body.
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
            if input.get(*p).copied() != Some(b'[') {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p += 1;
            let child_off = builder.mark_children();
            let mut first = #support_mod::skip_space(input, p, state)
                .ok_or(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;

            if first == b']' {
                *p += 1;
                let span_hi = *p as u32;
                let off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    child_off,
                    span_lo,
                    span_hi,
                    #variant_idx,
                    0,
                );
                return Ok(off);
            }

            loop {
                let _value_off = #dispatcher_ident(input, p, state, builder)?;
                match #support_mod::skip_space(input, p, state) {
                    Some(b']') => {
                        *p += 1;
                        let span_hi = *p as u32;
                        let off = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Rule,
                            child_off,
                            span_lo,
                            span_hi,
                            #variant_idx,
                            0,
                        );
                        return Ok(off);
                    }
                    Some(b',') => {
                        *p += 1;
                        let _ = #support_mod::skip_space(input, p, state);
                        let _ = first;
                    }
                    _ => {
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                }
            }
        }
    }
}
