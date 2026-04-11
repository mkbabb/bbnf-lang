//! Tape-first rule body scaffolding for the Rust backend.
//!
//! Tranche AB.2 substrate. These helpers emit the `mark_children` /
//! `push_leaf` / `push_compound` boilerplate that wraps every
//! tape-emitting rule body under the three-class materialization
//! scheme. They are intended for consumption by the forthcoming
//! rule-function emitter rewrite that changes the parser ABI from
//! `fn __rule(state) -> Option<Enum>` to `fn __rule(state, tape) ->
//! Option<TapeOffset>`.
//!
//! The full rewrite lands as a series of AB.2 sub-phases — each
//! emitter path (leaves, seq, alt, repeat, map, ...) is migrated
//! individually, with the tape parity gate covering the whole
//! suite. This file is the common codegen shim that every path
//! shares.
//!
//! # Rule prelude / epilogue shapes
//!
//! **`MustTape`** — full compound record with children. Prelude
//! captures `__span_lo` + calls `mark_children`; epilogue calls
//! `push_compound` at the return site.
//!
//! ```ignore
//! fn __pair<'a>(
//!     state: &mut ParserState<'a>,
//!     tape: &mut TapeBuilder,
//! ) -> Option<TapeOffset> {
//!     'rule_blk: {
//!         let __span_lo = state.offset as u32;       // prelude
//!         let __children = tape.mark_children();     // prelude
//!
//!         // ... body: sub-parses return Option<TapeOffset>,
//!         //            match ... break 'rule_blk None on failure
//!
//!         Some(tape.push_compound(                  // epilogue
//!             TapeKind::Rule,
//!             __children,
//!             __span_lo,
//!             state.offset as u32,
//!             PAIR_VIDX,
//!         ))
//!     }
//! }
//! ```
//!
//! **`TapeSpanOnly`** — single leaf span record. No children, no
//! compound header. Prelude captures `__span_lo` only; epilogue
//! calls `push_leaf`.
//!
//! ```ignore
//! fn __comma<'a>(
//!     state: &mut ParserState<'a>,
//!     tape: &mut TapeBuilder,
//! ) -> Option<TapeOffset> {
//!     let __span_lo = state.offset as u32;
//!     state.eat_byte(b',')?;
//!     Some(tape.push_leaf(
//!         TapeKind::Span,
//!         __span_lo,
//!         state.offset as u32,
//!         0,
//!     ))
//! }
//! ```
//!
//! **`TransparentElide`** — no function emitted at all. Handled in
//! `compile_ref`: when a call site references a transparent-elide
//! rule, the body is inlined at the call site instead of emitting
//! a `Self::__rule(state, tape)` call. The inlined body carries
//! its own prelude / epilogue (whichever class the inlined form
//! was classified as).
//!
//! # Why these helpers exist
//!
//! Keeping the `TokenStream` templates for the three shapes in one
//! file means every per-kind emitter (seq, alt, repeat, ...) calls
//! the same helper when it needs to wrap a body in the rule
//! prelude / epilogue. Drift between the leaf path and the
//! compound path would be a correctness bug — the view layer
//! would read mismatched spans. A single shim ensures that can't
//! happen.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit the prelude for a `MustTape` rule body.
///
/// Captures the starting byte offset and reserves the children run.
/// The `__span_lo` and `__children` locals are in scope for the
/// rule body and for the matching [`emit_must_tape_epilogue`].
pub fn emit_must_tape_prelude() -> TokenStream {
    quote! {
        let __span_lo = state.offset as u32;
        let __children = ::bbnf_tape::TapeBuilder::mark_children(tape);
    }
}

/// Emit the epilogue for a `MustTape` rule body.
///
/// `body_expr` is the final expression the body evaluates to (the
/// last sub-parse's offset result, ignored for its offset value —
/// the compound record's children run is what the view layer
/// walks). `variant_idx` is the rule's codegen-assigned variant
/// discriminator (u8).
pub fn emit_must_tape_epilogue(variant_idx: u8) -> TokenStream {
    let variant_lit = variant_idx;
    quote! {
        Some(::bbnf_tape::TapeBuilder::push_compound(
            tape,
            ::bbnf_tape::TapeKind::Rule,
            __children,
            __span_lo,
            state.offset as u32,
            #variant_lit,
        ))
    }
}

/// Emit the prelude for a `TapeSpanOnly` rule body.
///
/// Captures only the starting byte offset — the leaf record has no
/// children to reserve.
pub fn emit_tape_span_only_prelude() -> TokenStream {
    quote! {
        let __span_lo = state.offset as u32;
    }
}

/// Emit the epilogue for a `TapeSpanOnly` rule body.
///
/// `variant_idx` is the rule's codegen-assigned variant
/// discriminator (u8). The leaf record carries the span and the
/// variant discriminator; no child run.
pub fn emit_tape_span_only_epilogue(variant_idx: u8) -> TokenStream {
    let variant_lit = variant_idx;
    quote! {
        Some(::bbnf_tape::TapeBuilder::push_leaf(
            tape,
            ::bbnf_tape::TapeKind::Span,
            __span_lo,
            state.offset as u32,
            #variant_lit,
        ))
    }
}

/// Emit the rule function signature for a tape-first rule.
///
/// Always `(state, tape) -> Option<TapeOffset>` — the single ABI
/// commitment of Tranche AB.2. `TransparentElide` rules skip
/// `emit_rule_signature` entirely; their bodies are inlined at
/// every call site.
pub fn emit_rule_signature(fn_name: &str) -> TokenStream {
    let fn_ident = format_ident!("__{}", fn_name);
    quote! {
        #[allow(non_snake_case)]
        fn #fn_ident<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            tape: &mut ::bbnf_tape::TapeBuilder,
        ) -> Option<::bbnf_tape::TapeOffset>
    }
}
