//! Unordered-shape emitter — `parse_unordered_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar Unordered-shape parse functions for
//! `Repeat { lo: 1, .. }` over disjoint-FIRST Alts. The emitted body
//! runs a byte-dispatch sub-loop — first non-ws byte → branch index
//! via the existing mined
//! [`DisjointFirstTable`](
//! bbnf_ir::passes::recognizers::disjoint_first::DisjointFirstTable)
//! → per-branch inline parse → repeat.
//!
//! Canonical: CSS `compoundSelector = (classSelector | idSelector |
//! attrSelector | colonSelector | typeSelector) +`. Five branches,
//! each with a distinct first byte.
//!
//! # Emission shape
//!
//! ```text
//! loop {
//!     let b = input.get(*p).copied().ok_or(DtaError::UnexpectedEnd)?;
//!     let idx = DISJOINT_FIRST_TABLE_<rule>[b as usize];
//!     match idx {
//!         0 => /* branch 0 parse */,
//!         1 => /* branch 1 parse */,
//!         ...
//!         0xFF => break,  // no branch admits — exit the Kleene
//!     }
//! }
//! ```
//!
//! Like Pratt, the actual per-branch body dispatches to the
//! grammar's ref-resolution substrate; W4.2/W4.3 fills those refs.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};

/// Emit `pub fn parse_unordered_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_unordered(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("unordered", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — per-grammar Unordered-shape parse function.
        ///
        /// Byte-dispatch over the mined disjoint-FIRST Alt branches
        /// inside a Kleene-plus sub-loop. Emits one `TapeKind::Rule`
        /// outer compound for the full compound-selector list; each
        /// admitted branch contributes its own sub-compound through
        /// the per-branch dispatch.
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
            let _ = #support_mod::skip_space(input, p, state);

            // Kleene-plus sub-loop. The per-grammar branch dispatch
            // is wired by W4.2/W4.3; W4.1 emits the scaffolding that
            // the consumer wave extends with branch-specific arms
            // keyed off the mined disjoint-FIRST table.
            let iters_lo: u32 = 1;
            let mut iters: u32 = 0;
            loop {
                match input.get(*p).copied() {
                    Some(_) => {
                        iters += 1;
                        // Branch dispatch placeholder — W4.2/W4.3
                        // extends with per-branch inline parse.
                        break;
                    }
                    None => break,
                }
            }
            if iters < iters_lo {
                return Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                });
            }

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
// AW-V.W4.1 — visitor-path Unordered emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_unordered_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_unordered_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("unordered", grammar_suffix, rule_name);
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — visitor-path Unordered-shape parse function.
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
            let mut iters: u32 = 0;
            loop {
                match input.get(*p).copied() {
                    Some(_) => {
                        iters += 1;
                        break;
                    }
                    None => break,
                }
            }
            if iters < 1 {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                });
            }
            Ok(())
        }
    }
}
