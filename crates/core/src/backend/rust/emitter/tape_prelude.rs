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
//!             0u8,  // meta_idx — 0 for non-Alt rules
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

use bbnf_ir::passes::PayloadLayout;
use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

const META_IDX_ZERO: u8 = 0;

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
    let meta_lit = META_IDX_ZERO;
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
                tape,
                ::bbnf::runtime::tape::TapeKind::Rule,
                __children,
                __span_lo,
                state.offset as u32,
                __vi,
                __mi,
            ))
        }
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
    let meta_lit = META_IDX_ZERO;
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            Some(::bbnf::runtime::tape::TapeBuilder::push_leaf(
                tape,
                ::bbnf::runtime::tape::TapeKind::Span,
                __span_lo,
                state.offset as u32,
                __vi,
                __mi,
            ))
        }
    }
}

// ── AQ.6.A: Generalized scalar-payload prelude / epilogue ────────
//
// Replaces the F64-/Bool-/U8-specific helpers with a single pair
// keyed on `TypeDesc`. The Rust scalar identifier
// (`td.rust_ident()`) drives both the local variable name
// (`__payload_<rust_ident>`) and the tape API selection
// (`push_leaf_with_<rust_ident>`).

/// Emit the prelude for a `TapeSpanOnly` rule with a scalar payload
/// of type `td`.
///
/// Declares `__span_lo`, `__payload_<rust_ident(td)>`, and
/// `__has_payload`. Panics if `td` is not a scalar payload type —
/// callers must gate on [`TypeDesc::is_scalar_payload`] first.
pub fn emit_tape_span_only_scalar_prelude(td: &TypeDesc) -> TokenStream {
    let rust_ident = td
        .rust_ident()
        .expect("emit_tape_span_only_scalar_prelude: non-scalar TypeDesc");
    let payload_ident = format_ident!("__payload_{}", rust_ident);
    let ty_ident = format_ident!("{}", rust_ident);
    let init = scalar_zero_init(td);
    quote! {
        let __span_lo = state.offset as u32;
        let mut #payload_ident: #ty_ident = #init;
        let mut __has_payload = false;
    }
}

/// Emit the epilogue for a `TapeSpanOnly` rule with a scalar payload
/// of type `td`.
///
/// Stores the captured `__payload_<rust_ident(td)>` value into the
/// tape's payload buffer via `push_leaf_with_<rust_ident(td)>`.
/// View-layer accessors read it back in O(1) — zero span re-parsing.
pub fn emit_tape_span_only_scalar_epilogue(td: &TypeDesc, variant_idx: u8) -> TokenStream {
    let rust_ident = td
        .rust_ident()
        .expect("emit_tape_span_only_scalar_epilogue: non-scalar TypeDesc");
    let payload_ident = format_ident!("__payload_{}", rust_ident);
    let push_ident = format_ident!("push_leaf_with_{}", rust_ident);
    let variant_lit = variant_idx;
    let meta_lit = META_IDX_ZERO;
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            Some(::bbnf::runtime::tape::TapeBuilder::#push_ident(
                tape,
                ::bbnf::runtime::tape::TapeKind::Span,
                __span_lo,
                state.offset as u32,
                __vi,
                __mi,
                #payload_ident,
            ))
        }
    }
}

/// Emit the zero-initializer expression for a scalar `TypeDesc`.
/// `f64` gets `0.0`, `bool` gets `false`, integer types get `0`.
fn scalar_zero_init(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::F64 => quote! { 0.0 },
        TypeDesc::Bool => quote! { false },
        _ => quote! { 0 },
    }
}

// ── AQ.6.B: Aggregate-payload prelude / epilogue ─────────────────
//
// Activated when `ir.payload_layouts` carries a layout for the
// rule. The prelude reserves a 16-byte stack buffer plus the
// per-field payload cursor; per-field scalars write into the
// buffer at their layout-recorded offsets; the epilogue commits
// the leading `total_bytes` via `push_leaf_with_aggregate`.

/// Emit the prelude for a `TapeSpanOnly` rule with an aggregate
/// payload layout.
///
/// Reserves a 16-byte stack buffer (zero-initialized so unwritten
/// bytes are deterministic) plus a `__has_payload` flag the
/// epilogue checks at runtime — when no field of the body wrote a
/// scalar (e.g. an Alt branch that was not taken), the rule falls
/// back to plain `push_leaf` so the tape never carries garbage
/// payload bytes.
pub fn emit_tape_span_only_aggregate_prelude(_layout: &PayloadLayout) -> TokenStream {
    quote! {
        let __span_lo = state.offset as u32;
        let mut __aggregate_buf: [u8; 16] = [0u8; 16];
        let mut __has_payload = false;
    }
}

/// Emit the epilogue for a `TapeSpanOnly` rule with an aggregate
/// payload layout.
///
/// On the success path, calls `push_leaf_with_aggregate` over the
/// leading `total_bytes` of `__aggregate_buf`. When `__has_payload`
/// is false (the body finished without writing any scalar
/// captures), falls back to bare `push_leaf` so the tape carries
/// the span without any payload reference.
///
/// AR.9: when `kv_pair` is true, the aggregate leaf uses
/// `TapeKind::KvPair` instead of `TapeKind::Span`, signalling
/// the view layer that the record is a flattened key-value pair.
pub fn emit_tape_span_only_aggregate_epilogue(
    layout: &PayloadLayout,
    variant_idx: u8,
    kv_pair: bool,
) -> TokenStream {
    let variant_lit = variant_idx;
    let meta_lit = META_IDX_ZERO;
    let total_bytes = layout.total_bytes as usize;
    let tape_kind = if kv_pair {
        quote! { ::bbnf::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { ::bbnf::runtime::tape::TapeKind::Span }
    };
    let tape_kind_fallback = quote! { ::bbnf::runtime::tape::TapeKind::Span };
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            if __has_payload {
                Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with_aggregate(
                    tape,
                    #tape_kind,
                    __span_lo,
                    state.offset as u32,
                    __vi,
                    __mi,
                    &__aggregate_buf[..#total_bytes],
                ))
            } else {
                Some(::bbnf::runtime::tape::TapeBuilder::push_leaf(
                    tape,
                    #tape_kind_fallback,
                    __span_lo,
                    state.offset as u32,
                    __vi,
                    __mi,
                ))
            }
        }
    }
}

/// Emit the prelude for a `MustTape` rule with an aggregate payload
/// layout.
///
/// Like the `TapeSpanOnly` aggregate prelude, but additionally
/// reserves the children run so the epilogue can decide between
/// `push_leaf_with_aggregate` (any field wrote a scalar capture) or
/// the standard compound-children pathway.
pub fn emit_must_tape_aggregate_prelude(_layout: &PayloadLayout) -> TokenStream {
    quote! {
        let __span_lo = state.offset as u32;
        let __children = ::bbnf::runtime::tape::TapeBuilder::mark_children(tape);
        let mut __aggregate_buf: [u8; 16] = [0u8; 16];
        let mut __has_payload = false;
    }
}

/// Emit the epilogue for a `MustTape` rule with an aggregate
/// payload layout.
///
/// Prefers `push_leaf_with_aggregate` when any field wrote a scalar
/// capture; otherwise falls through to the compound-children push.
///
/// AR.9: when `kv_pair` is true, the aggregate leaf uses
/// `TapeKind::KvPair` instead of `TapeKind::Span`.
pub fn emit_must_tape_aggregate_epilogue(
    layout: &PayloadLayout,
    variant_idx: u8,
    kv_pair: bool,
) -> TokenStream {
    let variant_lit = variant_idx;
    let meta_lit = META_IDX_ZERO;
    let total_bytes = layout.total_bytes as usize;
    let tape_kind = if kv_pair {
        quote! { ::bbnf::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { ::bbnf::runtime::tape::TapeKind::Span }
    };
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            if __has_payload {
                Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with_aggregate(
                    tape,
                    #tape_kind,
                    __span_lo,
                    state.offset as u32,
                    __vi,
                    __mi,
                    &__aggregate_buf[..#total_bytes],
                ))
            } else {
                Some(::bbnf::runtime::tape::TapeBuilder::push_compound(
                    tape,
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    __children,
                    __span_lo,
                    state.offset as u32,
                    __vi,
                    __mi,
                ))
            }
        }
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

