//! Tape-first rule body scaffolding for the Rust backend.
//!
//! Tranche AC.2. These helpers emit the `mark_children` /
//! `push_leaf` / `push_compound` boilerplate that wraps every
//! tape-emitting rule body under the three-class materialization
//! scheme. Every rule function has the ABI
//! `fn __rule(state: &mut ParserState, tape: &mut TapeBuilder)
//! -> Option<TapeOffset>`. This is the single emitter contract the
//! rest of the backend is built on.
//!
//! # Rule prelude / epilogue shapes
//!
//! **`MustTape`** — full compound record with children. Prelude
//! captures `__span_lo` + calls `mark_children`; epilogue calls
//! `push_compound` at the return site.
//!
//! ```ignore
//! fn __pair<'a>(
//!     state: &mut ::parse_that::ParserState<'a>,
//!     tape: &mut ::bbnf::runtime::tape::TapeBuilder,
//! ) -> Option<::bbnf::runtime::tape::TapeOffset> {
//!     'rule_blk: {
//!         let __span_lo = state.offset as u32;
//!         let __children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
//!
//!         // ... body (Option<()> or Option<TapeOffset>), each
//!         //    sub-parse is matched with `match ... Some(_) => (),
//!         //    None => break 'rule_blk None`.
//!
//!         Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
//!             tape,
//!             ::bbnf::runtime::tape::TapeKind::Rule,
//!             __children,
//!             __span_lo,
//!             state.offset as u32,
//!             PAIR_VIDX,
//!         ))
//!     }
//! }
//! ```
//!
//! **`TapeSpanOnly`** — single leaf span record. No children run,
//! no compound header. Prelude captures `__span_lo`; epilogue
//! calls `push_leaf`.
//!
//! **`TransparentElide`** — no function emitted at all. Handled by
//! the driver at call sites: when a `Ref` targets a transparent
//! rule, the body is inlined rather than emitting a
//! `Self::__rule(state, tape)` call.
//!
//! # Why these helpers exist
//!
//! Every per-kind emitter (seq, alt, repeat, ...) that needs to
//! wrap a body in the rule prelude / epilogue calls into one of
//! these helpers. A single shim guarantees no drift between leaf
//! and compound emission paths.

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
        let __children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
    }
}

/// Emit the epilogue for a `MustTape` rule body.
///
/// `variant_idx` is the rule's codegen-assigned variant
/// discriminator (u8) — the rule's index in `ir.rules`.
pub fn emit_must_tape_epilogue(variant_idx: u8) -> TokenStream {
    let variant_lit = variant_idx;
    quote! {
        Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
            tape,
            ::bbnf::runtime::tape::TapeKind::Rule,
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
        Some(::bbnf::runtime::tape::TapeBuilder::push_leaf(
            tape,
            ::bbnf::runtime::tape::TapeKind::Span,
            __span_lo,
            state.offset as u32,
            #variant_lit,
        ))
    }
}

/// Emit the rule function signature for a tape-first rule.
///
/// Always `(state, tape) -> Option<TapeOffset>` — the single ABI
/// commitment of Tranche AC.2. `TransparentElide` rules skip
/// `emit_rule_signature` entirely; their bodies are inlined at
/// every call site.
pub fn emit_rule_signature(fn_name: &str) -> TokenStream {
    let fn_ident = format_ident!("__{}", fn_name);
    quote! {
        #[allow(non_snake_case)]
        fn #fn_ident<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            tape: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::option::Option<::bbnf::runtime::tape::TapeOffset>
    }
}

// ── Tranche AI.1 — Direct-tier signatures ──────────────────────────────────

/// Emit the signature for a Direct-tier rule's inner parse function.
///
/// `__<name>_inner(state) -> Option<()>` — the shared parse body
/// for both the tape wrapper and the direct shim. Takes only
/// `state`; no tape parameter. The inner function is the single
/// source of parse logic for Direct-tier rules.
pub fn emit_direct_inner_signature(fn_name: &str) -> TokenStream {
    let fn_ident = format_ident!("__{}_inner", fn_name);
    quote! {
        #[allow(non_snake_case)]
        fn #fn_ident<'a>(
            state: &mut ::parse_that::ParserState<'a>,
        ) -> ::core::option::Option<()>
    }
}

/// Emit the signature for a Direct-tier rule's direct shim.
///
/// `__<name>_direct(state) -> Option<()>` — skips the tape
/// entirely. Called when a Direct-tier caller invokes a Direct-tier
/// callee, avoiding the tape push/pop overhead. The shim delegates
/// to `__<name>_inner`.
pub fn emit_direct_shim_signature(fn_name: &str) -> TokenStream {
    let fn_ident = format_ident!("__{}_direct", fn_name);
    quote! {
        #[allow(non_snake_case)]
        fn #fn_ident<'a>(
            state: &mut ::parse_that::ParserState<'a>,
        ) -> ::core::option::Option<()>
    }
}
