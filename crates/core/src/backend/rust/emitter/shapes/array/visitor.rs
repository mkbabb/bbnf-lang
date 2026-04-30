//! AW-V.W3-bench-fix — visitor-path Array emitter.
//!
//! Mirrors the prototype's `json_prototype::parse_array::<V>`
//! (`crates/json-prototype/src/lib.rs:308`). Bypasses the tape;
//! `visitor.begin_array()` / `visitor.end_array()` replace the compound
//! + leaf record pushes the tape-path emits.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{
    emit_ref_call_visitor, visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::super::root_rule_name;
use super::element::extract_array_value_ref;

/// Emit `pub fn parse_array_visitor_<grammar>_<rule><V: JsonVisitor>(...)
/// -> Result<(), ParseErr>`.
pub fn emit_parse_array_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — resolve value-position Ref for visitor path.
    let value_ref = extract_array_value_ref(&rule.body, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_visitor(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, visitor)?;
            }
        });

    quote! {
        /// AW-V.W3-bench-fix — visitor-path Array-shape parse function.
        ///
        /// Mirrors `json_prototype::parse_array::<V>`. Bypasses
        /// the tape entirely.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: ::tape::ObjectVisitor
                + ::tape::ArrayVisitor
                + ::tape::StringVisitor
                + ::tape::NumberVisitor
                + ::tape::KeywordVisitor,
        {
            let begin_at = *p;
            if input.get(*p).copied() != Some(b'[') {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: begin_at as u32, rule: None,
                });
            }
            *p += 1;
            visitor.begin_array().map_err(|_| crate::runtime::ParseErr::Syntax {
                offset: begin_at as u32, rule: None,
            })?;
            // Fast-empty check: `]` immediately closes.
            if let Some(b) = #support_mod::skip_space(input, p, state) {
                if b == b']' {
                    *p += 1;
                    return visitor.end_array().map_err(|_| crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    });
                }
            } else {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                });
            }
            loop {
                // AW-V.W5.2 — per-Ref direct call when classified.
                #value_call
                match #support_mod::skip_space(input, p, state) {
                    Some(b']') => {
                        *p += 1;
                        return visitor.end_array().map_err(|_| crate::runtime::ParseErr::Syntax {
                            offset: *p as u32, rule: None,
                        });
                    }
                    Some(b',') => {
                        *p += 1;
                        let _ = #support_mod::skip_space(input, p, state);
                    }
                    _ => return Err(crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    }),
                }
            }
        }
    }
}
