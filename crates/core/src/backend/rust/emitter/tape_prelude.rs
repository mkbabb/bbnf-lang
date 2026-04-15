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
// (`__payload_<rust_ident>`) and the AU.6.7 `PayloadData` constructor
// (`InlineScalar` / `WideScalar`) used by `push_leaf_with`.

/// Emit the prelude for a `TapeSpanOnly` rule with a scalar payload
/// of type `td`.
///
/// Declares `__span_lo`, `__payload_<rust_ident(td)>`, and
/// `__has_payload`. Panics if `td` is not a scalar payload type —
/// callers must gate on [`TypeDesc::is_scalar_payload`] first.
///
/// AS.2: `TypeDesc::Span` declares two `u32` locals (`__payload_lo`,
/// `__payload_hi`) instead of a single typed variable — the two
/// offsets pack into a single `u64` arena slot via
/// `PayloadData::WideScalar` at epilogue time.
pub fn emit_tape_span_only_scalar_prelude(td: &TypeDesc) -> TokenStream {
    if matches!(td, TypeDesc::Span) {
        return quote! {
            let __span_lo = state.offset as u32;
            let mut __payload_lo: u32 = 0;
            let mut __payload_hi: u32 = 0;
            let mut __has_payload = false;
        };
    }
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
/// tape via `push_leaf_with` + `PayloadData`. View-layer accessors
/// read it back in O(1) — zero span re-parsing.
///
/// AU.6.7: all scalar pushes route through the unified
/// `push_leaf_with` entry point with `PayloadData::InlineScalar`
/// (<= 4 B types) or `PayloadData::WideScalar` (8 B types including
/// `TypeDesc::Span`, whose two `u32` locals pack into a `u64`).
pub fn emit_tape_span_only_scalar_epilogue(td: &TypeDesc, variant_idx: u8) -> TokenStream {
    let payload_expr = scalar_payload_data_token(td);
    let variant_lit = variant_idx;
    let meta_lit = META_IDX_ZERO;
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with(
                tape,
                ::bbnf::runtime::tape::TapeKind::Span,
                __span_lo,
                state.offset as u32,
                __vi,
                __mi,
                #payload_expr,
            ))
        }
    }
}

/// Emit the `PayloadData` expression for a scalar `TypeDesc`, given
/// locals `__payload_<rust_ident>` (for scalars other than Span) or
/// `__payload_lo`/`__payload_hi` (for Span). Mirrors
/// `emit_scalar_payload_data` in `grammar.rs` with different local
/// names (no `__variant_idx`/`__branch_idx` wrappers).
fn scalar_payload_data_token(td: &TypeDesc) -> TokenStream {
    if matches!(td, TypeDesc::Span) {
        return quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(
                (__payload_lo as u64) | ((__payload_hi as u64) << 32),
            )
        };
    }
    let rust_ident = td
        .rust_ident()
        .expect("scalar_payload_data_token: non-scalar TypeDesc");
    let payload_ident = format_ident!("__payload_{}", rust_ident);
    match td {
        TypeDesc::F64 => quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(#payload_ident.to_bits())
        },
        TypeDesc::U64 => quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(#payload_ident)
        },
        TypeDesc::I64 => quote! {
            ::bbnf::runtime::tape::PayloadData::WideScalar(#payload_ident as u64)
        },
        TypeDesc::Bool => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_ident as u32)
        },
        TypeDesc::I8 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(
                u32::from_le_bytes([#payload_ident as u8, 0, 0, 0]),
            )
        },
        TypeDesc::U8 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_ident as u32)
        },
        TypeDesc::I16 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar({
                let __b = (#payload_ident as i16).to_le_bytes();
                u32::from_le_bytes([__b[0], __b[1], 0, 0])
            })
        },
        TypeDesc::U16 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_ident as u32)
        },
        TypeDesc::I32 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_ident as u32)
        },
        TypeDesc::U32 => quote! {
            ::bbnf::runtime::tape::PayloadData::InlineScalar(#payload_ident)
        },
        _ => unreachable!(
            "scalar_payload_data_token: unhandled TypeDesc {:?}",
            td
        ),
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
// the leading `total_bytes` via `push_leaf_with` +
// `PayloadData::Aggregate`.

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
/// On the success path, calls `push_leaf_with` with a
/// `PayloadData::Aggregate(&__aggregate_buf[..total_bytes])` or
/// `PayloadData::LargeAggregate(&__aggregate_buf[..total_bytes])`
/// depending on total_bytes (AV.0.5). When `__has_payload` is false
/// (the body finished without writing any scalar captures), falls
/// back to bare `push_leaf` so the tape carries the span without any
/// payload reference.
///
/// AR.9: when `kv_pair` is true, the aggregate leaf uses
/// `TapeKind::KvPair` instead of `TapeKind::Span`, signalling
/// the view layer that the record is a flattened key-value pair.
///
/// AV.0.2: bare-`Span` layouts — those with exactly one field of
/// `TypeDesc::Span` at offset 0 — get an unconditional epilogue
/// pack of `(__span_lo, state.offset)` into the first 8 bytes of
/// `__aggregate_buf` and set `__has_payload = true`. Multi-leaf
/// rule bodies (e.g. BBNF `literal` whose Alt branches emit three
/// leaves each) would otherwise write the first leaf's partial
/// `hi` into the buffer; the epilogue rewrite ensures the on-tape
/// Span always carries the rule-final `state.offset`.
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
    let span_fixup = bare_span_epilogue_fixup(layout);
    let payload_ctor = aggregate_payload_ctor(total_bytes);
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            #span_fixup
            if __has_payload {
                Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with(
                    tape,
                    #tape_kind,
                    __span_lo,
                    state.offset as u32,
                    __vi,
                    __mi,
                    #payload_ctor,
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
/// `push_leaf_with` + `PayloadData::Aggregate` (any field wrote a
/// scalar capture) or the standard compound-children pathway.
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
/// Prefers `push_leaf_with` + `PayloadData::Aggregate` (total_bytes
/// ≤ 16) or `PayloadData::LargeAggregate` (total_bytes > 16) when
/// any field wrote a scalar capture; otherwise falls through to the
/// compound-children push.
///
/// AR.9: when `kv_pair` is true, the aggregate leaf uses
/// `TapeKind::KvPair` instead of `TapeKind::Span`.
///
/// AV.0.2: bare-`Span` layouts — those with exactly one field of
/// `TypeDesc::Span` at offset 0 — get an unconditional epilogue
/// pack of `(__span_lo, state.offset)` into `__aggregate_buf` and
/// set `__has_payload = true`, guaranteeing the on-tape Span
/// carries the rule-final cursor regardless of how many leaves the
/// body stepped through.
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
    let span_fixup = bare_span_epilogue_fixup(layout);
    let payload_ctor = aggregate_payload_ctor(total_bytes);
    quote! {
        {
            let __vi: u8 = #variant_lit;
            let __mi: u8 = #meta_lit;
            #span_fixup
            if __has_payload {
                Some(::bbnf::runtime::tape::TapeBuilder::push_leaf_with(
                    tape,
                    #tape_kind,
                    __span_lo,
                    state.offset as u32,
                    __vi,
                    __mi,
                    #payload_ctor,
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

/// AV.0.2 + AV.0.5: select the `PayloadData` constructor for an
/// aggregate leaf push based on `total_bytes`. Layouts up to
/// [`crate::backend::rust::emitter::tape_prelude::MAX_INLINE_AGGREGATE_BYTES`]
/// (`16` — matching `bbnf_tape::MAX_INLINE_AGGREGATE_BYTES`) keep
/// the `PayloadData::Aggregate` inline-aggregate path; larger
/// layouts (CSS colour-function `colorFunction` / `colorFn` /
/// `colorMix` land at 33-40 B) route through
/// `PayloadData::LargeAggregate`, arena-backed without a length
/// prefix. Readers recover the width from the grammar's payload-
/// layout table keyed by `(kind, variant_idx)`.
fn aggregate_payload_ctor(total_bytes: usize) -> TokenStream {
    if total_bytes > MAX_INLINE_AGGREGATE_BYTES {
        quote! {
            ::bbnf::runtime::tape::PayloadData::LargeAggregate(
                &__aggregate_buf[..#total_bytes],
            )
        }
    } else {
        quote! {
            ::bbnf::runtime::tape::PayloadData::Aggregate(
                &__aggregate_buf[..#total_bytes],
            )
        }
    }
}

/// AV.0.2: produce the prologue stamp for the aggregate epilogue
/// when the rule's layout is exactly one `TypeDesc::Span` field at
/// offset 0 (total_bytes == 8). Writes `__span_lo` + `state.offset`
/// into the buffer and sets `__has_payload = true` before the
/// epilogue's `push_leaf_with` dispatch. For non-bare-Span layouts
/// returns an empty stream, leaving the epilogue untouched.
///
/// The stamp is idempotent with the leaf-level Span pack in
/// `leaves.rs::probe_span_aggregate_pack`: single-leaf bodies
/// (`identifier = /regex/ -> Span`) write the pack once at leaf
/// match success and the epilogue rewrites with the same values.
/// Multi-leaf bodies (`literal = ( "\"" , /…/ , "\"" ) -> Span`)
/// rely exclusively on the epilogue rewrite because the leaf-level
/// pack runs with a premature `hi = state.offset` (the cursor is
/// mid-body at that point).
fn bare_span_epilogue_fixup(layout: &PayloadLayout) -> TokenStream {
    if layout.total_bytes == 8
        && layout.fields.len() == 1
        && matches!(layout.fields[0].ty, bbnf_ir::TypeDesc::Span)
        && layout.fields[0].offset == 0
    {
        quote! {
            __aggregate_buf[0..4]
                .copy_from_slice(&(__span_lo as u32).to_le_bytes());
            __aggregate_buf[4..8]
                .copy_from_slice(&(state.offset as u32).to_le_bytes());
            __has_payload = true;
        }
    } else {
        quote! {}
    }
}

/// AV.0.5: aggregates > `MAX_INLINE_AGGREGATE_BYTES` route through
/// `PayloadData::LargeAggregate`. Kept in sync with
/// `bbnf_tape::MAX_INLINE_AGGREGATE_BYTES` (same constant, same
/// value).
const MAX_INLINE_AGGREGATE_BYTES: usize = 16;

/// Emit the rule function signature for a tape-first rule.
///
/// Always `(state, tape) -> Option<TapeOffset>` — the single ABI
/// commitment of Tranche AC.2. `TransparentElide` rules skip
/// `emit_rule_signature` entirely; their bodies are inlined at
/// every call site.
pub fn emit_rule_signature(fn_name: &str) -> TokenStream {
    let fn_ident = format_ident!("__{}", fn_name);
    quote! {
        fn #fn_ident<'a>(
            state: &mut ::parse_that::ParserState<'a>,
            tape: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::option::Option<::bbnf::runtime::tape::TapeOffset>
    }
}

