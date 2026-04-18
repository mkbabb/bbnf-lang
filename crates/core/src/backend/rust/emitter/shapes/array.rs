//! Array-shape emitter — `parse_array_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Array-shape parse function with **walker-
//! identical tape emission**. The walker lowers the canonical JSON
//! array rule
//!
//! ```text
//! array = "[" >> ((value << comma?)*)?w << "]"
//! ```
//!
//! to a nested Seq/Seq/Repeat/Seq compound tree; each structural IR
//! production becomes a `push_compound` record. Downstream view
//! derives (`arrayView`, `valueView`, typed-field projections) and the
//! `tape_parity` golden fixtures navigate that exact record sequence,
//! so the shape emitter must reproduce it byte-for-byte — only the
//! **dispatch** is inlined (no `dispatch_one` / `try_branch` / cross-
//! crate helper chain), not the **records**.
//!
//! # Emitted tape shape (for `[v1, v2]`)
//!
//! ```text
//! [ 0] Seq     variant=<array_id>  span=0..N   child=1  has_children=true
//! [ 1] Seq     variant=0           span=0..N-1 child=2  has_children=true   <- Next("[", rest)
//! [ 2] Literal variant=0           span=0..1                                 <- "["
//! [ 3] Seq     variant=0           span=1..N-1 child=4  has_children=true   <- OptionalWhitespace
//! [ 4] Rule    variant=0           span=1..N-1 child=5  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- Skip(value, Repeat(,?))
//!         ...value records...
//!         Rule  variant=0          has_children=true                        <- Repeat(,?)
//!           Seq variant=0                                                    <- OptionalWhitespace(",")
//!             Literal variant=0                                              <- ","
//! [ N] Literal variant=0           span=N-1..N                               <- "]"
//! ```

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

    // Non-root dispatcher — the shape fn the value position recurses
    // into. Both root and non-root call sites share the same
    // dispatcher in the walker-parity emission (the walker's value
    // ByteDispatch is identical at root and nested positions).
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    quote! {
        /// AW-V.W3.2 — per-grammar Array-shape parse function,
        /// **walker-tape-identical**.
        ///
        /// Emits the same nested Seq/Seq/Repeat/Seq compound tree the
        /// walker produces for the canonical JSON array rule. The
        /// record tree is navigated by every downstream view derive
        /// and the `tape_parity` golden fixtures; only dispatch is
        /// inlined relative to the walker, not the record stream.
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

            // Outer compound: Skip(Next("[", rest), "]") — the array rule itself.
            // Walker variant_idx comes from the Ref's pending stamp = rule.id & 0xFF.
            let outer_child = builder.mark_children();

            // Inner Next compound: Next("[", OptionalWhitespace(Repeat(...)))
            let lbracket_open = *p as u32;
            let next_child = builder.mark_children();

            // Leaf: "[" Literal (payload-less; TapeKind::Literal per
            // walker). Walker stamps variant_idx from the enclosing
            // rule's `pending_variant_idx` (set by the Ref landing in
            // this rule). Downstream `Leaf::Other` tests assert that
            // every structural literal inside a rule inherits the
            // owning rule's variant.
            *p += 1;
            let bracket_close = *p as u32;
            let _ = builder.push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Literal,
                lbracket_open,
                bracket_close,
                #variant_idx,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
            );

            // OptionalWhitespace Seq compound — contains the Repeat.
            let opt_ws_open = *p as u32;
            let opt_ws_child = builder.mark_children();

            // Leading whitespace trim.
            let _ = #support_mod::skip_space(input, p, state);
            let repeat_open = *p as u32;

            // Repeat compound — one per rule invocation, regardless of iterations.
            let repeat_child = builder.mark_children();

            // Fast-empty check: `]` immediately closes everything.
            let maybe_close = input.get(*p).copied();
            if maybe_close == Some(b']') {
                // Close Repeat (no iterations), OptionalWhitespace, Next, outer.
                let repeat_close = *p as u32;
                let _repeat_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    repeat_child,
                    repeat_open,
                    repeat_close,
                    0,
                    0,
                );
                let opt_ws_close = *p as u32;
                let _opt_ws_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    opt_ws_child,
                    opt_ws_open,
                    opt_ws_close,
                    0,
                    0,
                );
                let next_close = *p as u32;
                let _next_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    next_child,
                    lbracket_open,
                    next_close,
                    0,
                    0,
                );
                // Consume the ].
                *p += 1;
                let rbracket_lo = next_close;
                let rbracket_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    rbracket_lo,
                    rbracket_hi,
                    #variant_idx,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
                let outer_close = *p as u32;
                let outer_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    outer_child,
                    span_lo,
                    outer_close,
                    #variant_idx,
                    0,
                );
                return Ok(outer_off);
            }

            // Non-empty loop: parse iterations. Each iteration emits:
            //   Seq (iter) { value_records; Rule (comma_repeat) { maybe: Seq { Literal "," } } }
            loop {
                let iter_open = *p as u32;
                let iter_child = builder.mark_children();

                // Value position — recurse through the shape dispatcher.
                let _value_off = #dispatcher_ident(input, p, state, builder)?;

                // Comma-optional Repeat compound — always one per iteration.
                let _ = #support_mod::skip_space(input, p, state);
                let comma_repeat_open = *p as u32;
                let comma_repeat_child = builder.mark_children();
                let has_comma = input.get(*p).copied() == Some(b',');
                if has_comma {
                    // OptionalWhitespace(",") — Seq with Literal ","
                    let opt_comma_open = *p as u32;
                    let opt_comma_child = builder.mark_children();
                    let comma_lo = *p as u32;
                    *p += 1;
                    let comma_hi = *p as u32;
                    let _ = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Literal,
                        comma_lo,
                        comma_hi,
                        #variant_idx,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
                    let opt_comma_close = *p as u32;
                    let _opt_comma_off = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        opt_comma_child,
                        opt_comma_open,
                        opt_comma_close,
                        0,
                        0,
                    );
                }
                let comma_repeat_close = *p as u32;
                let _comma_repeat_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    comma_repeat_child,
                    comma_repeat_open,
                    comma_repeat_close,
                    0,
                    0,
                );

                let iter_close = *p as u32;
                let _iter_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_child,
                    iter_open,
                    iter_close,
                    0,
                    0,
                );

                // Peek: continue loop or close all.
                let _ = #support_mod::skip_space(input, p, state);
                match input.get(*p).copied() {
                    Some(b']') => {
                        // Close Repeat, OptionalWhitespace, Next, outer, emit "]".
                        let repeat_close = *p as u32;
                        let _repeat_off = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Rule,
                            repeat_child,
                            repeat_open,
                            repeat_close,
                            0,
                            0,
                        );
                        let opt_ws_close = *p as u32;
                        let _opt_ws_off = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Seq,
                            opt_ws_child,
                            opt_ws_open,
                            opt_ws_close,
                            0,
                            0,
                        );
                        let next_close = *p as u32;
                        let _next_off = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Seq,
                            next_child,
                            lbracket_open,
                            next_close,
                            0,
                            0,
                        );
                        // Consume "]".
                        *p += 1;
                        let rbracket_hi = *p as u32;
                        let _ = builder.push_leaf_with(
                            ::bbnf::runtime::tape::TapeKind::Literal,
                            next_close,
                            rbracket_hi,
                            #variant_idx,
                            0,
                            ::bbnf::runtime::tape::PayloadData::None,
                        );
                        let outer_close = *p as u32;
                        let outer_off = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Seq,
                            outer_child,
                            span_lo,
                            outer_close,
                            #variant_idx,
                            0,
                        );
                        return Ok(outer_off);
                    }
                    Some(_) => {
                        // Continue: caller emitted a trailing comma already via the iter.
                    }
                    None => {
                        return Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                            offset: *p as u32,
                        });
                    }
                }
            }
        }
    }
}
