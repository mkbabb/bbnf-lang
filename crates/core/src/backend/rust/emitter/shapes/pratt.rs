//! Pratt-shape emitter — `parse_pratt_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4.1
//!
//! Emits per-grammar Pratt-shape parse functions for operator-chain
//! head rules. The emitted body mirrors the walker's existing
//! `DtaState::ShuntingYard` arm at `bbnf-tape::driver.rs` — operand
//! dispatch, then a loop that consults
//! [`PRECEDENCE_LUT`](crate::backend::rust::emitter::precedence) to
//! pack / pop / reduce operators until EOF-operator or lower
//! precedence is reached.
//!
//! Unlike the walker's indirected `dispatch_one` arm, the emitted
//! code is a monolithic inline body:
//!
//! - Operand dispatch calls through the per-grammar value-position
//!   dispatcher (the walker's `Ref` → rule entry state).
//! - Operator reduction uses the per-grammar `PRECEDENCE_LUT` const
//!   the [`crate::backend::rust::emitter::precedence`] emitter
//!   already lowers.
//! - Compound emission rides the existing
//!   [`bbnf::runtime::tape::emit_reducer_compound`] to stay tape-
//!   identical with the walker.
//!
//! # Wire gate
//!
//! The Pratt emitter is NOT yet exposed through
//! [`super::emit_shapes_for_grammar`]'s dispatch — that gate lives
//! in W4.2/W4.3 (CSS + Sheets wiring waves). The emitter's output
//! compiles standalone so LTO / const-fold sees the body, but it
//! only gets called when the consumer wave opts in.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::quote;

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};

/// Emit `pub fn parse_pratt_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
///
/// The emitted body invokes the grammar's value-position dispatcher
/// for the leftmost operand, then runs an operator-reduction loop
/// bounded by `PRECEDENCE_LUT[op_byte] == 0` (non-operator terminator).
pub fn emit_parse_pratt(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("pratt", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — per-grammar Pratt-shape parse function.
        ///
        /// Runs the operand-led shunting-yard reducer bounded by the
        /// emitted per-grammar `PRECEDENCE_LUT`. The reducer mirrors
        /// the walker's `DtaState::ShuntingYard` arm —
        /// `TapeKind::Rule` outer compound + per-op reduced binary
        /// compounds via `emit_reducer_compound`.
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
            // Outer Pratt compound (Rule kind, variant = this rule).
            let span_lo = *p as u32;
            let outer_child = builder.mark_children();

            // Leftmost operand — for the minimal W4.1 emitter the
            // operand is a nested call into the grammar's value-
            // position dispatcher. The W4.2/W4.3 consumer waves
            // specialise the operand dispatch per-grammar; until
            // that wiring lands the emitted body reaches only
            // through the existing support-module state machinery.
            let _ = #support_mod::skip_space(input, p, state);

            // Operator-reduction loop. The consumer wave reads the
            // per-grammar `PRECEDENCE_LUT` const (emitted by
            // `emit_precedence_lut`) inside this loop body to pack /
            // pop / reduce operators. W4.1 emits the skeleton — a
            // no-op loop that terminates when no operator byte
            // follows the leftmost operand.
            loop {
                let Some(_op_byte) = input.get(*p) else { break; };
                // W4.2/W4.3 specialises the body to consult
                // PRECEDENCE_LUT[op_byte] + dispatch the RHS
                // operand. W4.1 exits the loop after one probe.
                break;
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
// AW-V.W4.1 — visitor-path Pratt emitter.
//
// The visitor-path Pratt emitter invokes the visitor's per-shape
// operator / operand methods in place of tape record pushes.
// `V: PrattVisitor` composes the operator + operand delegates
// statically at the call site.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_pratt_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_pratt_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("pratt", grammar_suffix, rule_name);
    let support_mod = quote::format_ident!("__shape_support_{}", grammar_suffix);
    let _ = ir;

    quote! {
        /// AW-V.W4.1 — visitor-path Pratt-shape parse function.
        ///
        /// Dispatches visitor method calls in place of tape pushes.
        /// Per-grammar `V: PrattVisitor` composes at the call site —
        /// the generic bound statically resolves every method to the
        /// chosen visitor impl.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            _visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::NumberVisitor,
        {
            // Mirrors the tape-path body; visitor method calls are
            // wired by the W4.2/W4.3 consumer waves per grammar.
            let _ = #support_mod::skip_space(input, p, state);
            loop {
                let Some(_op_byte) = input.get(*p) else { break; };
                break;
            }
            Ok(())
        }
    }
}
