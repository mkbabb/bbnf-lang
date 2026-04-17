//! AW-IV.W2.1 — Inline helper-body emission for the specialised walker.
//!
//! # Architectural role
//!
//! AW-III.W4.d routed every non-`ByteDispatch` arm through inline
//! `dispatch_one` semantics, but the per-arm bodies still crossed the
//! `bbnf-tape` crate boundary on every hot-path helper call —
//! `emit_leaf(...)`, `emit_leaf_with_payload(...)`, `close_compound(...)`,
//! `advance_seq_fast(...)`, `psi.push(...)`, `handle_repeat_failure(...)`.
//! Those calls compiled to cross-crate function boundaries whose
//! inlining depended on the callee crate's MIR availability and the
//! inliner's heuristics at each call site. Every surviving symbol in
//! `nm target/release/deps/json_monolithic-*` for a helper that SHOULD
//! be gone is a function-call boundary the hot path pays at runtime.
//!
//! AW-IV.W2.1 resolves the residual dispatch by emitting the helper
//! bodies *directly into each walker arm* at codegen time. The
//! `bbnf-tape` helpers survive ONLY as the cold-path `dispatch_one`
//! replay surface (the AX correctness ground truth); no per-grammar
//! walker arm reaches them. Each fragment emitter below produces the
//! same operations the corresponding runtime helper performs —
//! unchecked column stores, frame-stack push/peek, variant resolution
//! — as a splice inside the calling arm's body so LLVM sees the
//! operations without a function-call boundary, independent of
//! cross-crate inliner heuristics.
//!
//! # Splice scope
//!
//! The fragments here cover the multi-line wrapper helpers whose
//! call boundaries persisted in the post-W1 bench symbols:
//!
//! - [`emit_emit_leaf_inline`] — variant resolution + column push +
//!   `frame_depth` bump, replacing `bbnf_tape::emit_leaf`.
//! - [`emit_emit_leaf_with_payload_inline`] — same, payload-bearing
//!   form, replacing `bbnf_tape::emit_leaf_with_payload`.
//! - [`emit_close_compound_inline`] — frame peek + `span_hi` /
//!   `child_off` / `variant_idx` patching + KvPair promotion,
//!   replacing `bbnf_tape::close_compound`.
//! - [`emit_advance_or_pop_inline`] — Seq fast-path probe inlined,
//!   fall-through to `advance_or_pop_with` (the large SY reducer
//!   helper) as a single cold-call boundary for the ≤20% minority
//!   non-Seq path. The fast path — which covers ~80% of JSON
//!   twitter leaf emits per the post-AW-III samply — is splice-free.
//!
//! The single-call forms for `Columns::push_compound_fused` /
//! `Columns::push_leaf_fused` stay as method calls because the
//! methods are `#[inline(always)]` on `Columns` itself AND their
//! internals (`structural_min_cap` / `grow_all` / `ensure_one`) are
//! `pub(crate)` within `bbnf-tape` — exposing them cross-crate to
//! splice-duplicate the capacity guard would leak internals. The
//! `#[inline(always)]` annotation on the methods is load-bearing:
//! LLVM has the MIR in metadata and inlines the body at the splice
//! site during codegen. If the post-W2.1 bench symbols show
//! `push_compound_fused` / `push_leaf_fused` present, W2.2's LTO
//! cover catches it; otherwise the methods inline at callsite as
//! expected.
//!
//! # Why inline emission, not cross-crate inlining?
//!
//! Per `docs/tranches/AW/AW-IV.md` §Architectural-thesis: the function-
//! call boundary IS a dispatcher when the callee does not inline.
//! Workspace LTO + `#[inline(always)]` is one answer (W2.2's verification
//! cover); per-grammar inline emission is the other, and AW-IV chooses
//! the inline-emission path as its primary mechanism. The strategy
//! makes the hot path structurally flat at the source level; LLVM's
//! job is downstream scalar optimisation, not proving inlinability
//! across opaque cross-crate indirection.
//!
//! Per the §6 invariant the fragments are grammar-agnostic — every
//! emitter here reads only IR-structural inputs (variant kind, frame
//! layout, payload presence) and renders the same tokens for every
//! grammar.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the helper block placeholder.
///
/// AW-III.W4.d removed the `__StepOutcome` enum + `__dispatch_via_cold`
/// bridge; AW-IV.W2.1 removed the residual cross-crate helper calls
/// from the arm bodies. The walker module's helper block is now an
/// empty surface — every per-arm splice supplies its own helper logic.
pub(super) fn emit_inline_helpers() -> TokenStream {
    quote! {
        // AW-IV.W2.1 — per-state arms inline their helper bodies
        // directly: emit_leaf / emit_leaf_with_payload /
        // close_compound / advance_seq_fast / psi.push all splice
        // their body tokens at the arm. The runtime helpers at
        // `bbnf_tape::driver::*` survive solely as the cold-path
        // `dispatch_one` replay surface for the AX correctness
        // ground truth; no per-grammar walker arm references them.
    }
}

/// AW-IV.W2.1 — inline body of
/// `emit_leaf_with_payload(columns, frame_depth, stack, kind, lo, hi, child_off) -> u32`.
///
/// Expands the runtime helper's body directly at the call site:
/// resolves the variant from `stack.pending_variant_idx` (or walks the
/// stack for the nearest owner), chooses the `extra` bit from
/// `child_off.is_none()`, calls into `Columns::push_leaf_fused`
/// (the `#[inline(always)]` method inlines its 7-unchecked-store body
/// at the splice site), then pushes `stack.depth()` onto `frame_depth`.
///
/// `out_ident` receives the pushed row's index. Arms that don't need
/// the index (pure emit-leaves) pass a throwaway ident like `_rec` —
/// LLVM elides the unused binding.
pub(super) fn emit_emit_leaf_with_payload_inline(
    out_ident: &proc_macro2::Ident,
    kind_expr: TokenStream,
    span_lo_expr: TokenStream,
    span_hi_expr: TokenStream,
    child_off_expr: TokenStream,
) -> TokenStream {
    quote! {
        let #out_ident: u32 = {
            // AW-I.W4ζ / AW-III.W1.A — resolve variant_idx:
            //   1. pending rule-entry stamp (rule.0 as u8)
            //   2. nearest enclosing frame whose variant_idx is set
            //   3. default 0 for fully-anonymous context
            let __variant: u8 = if stack.pending_variant_idx != u8::MAX {
                stack.pending_variant_idx
            } else if let ::core::option::Option::Some(__owner) = stack.nearest_variant_frame() {
                __owner.variant_idx
            } else {
                0u8
            };
            let __child_off: ::bbnf::runtime::tape::TapeOffset = #child_off_expr;
            let __extra: u16 = if __child_off.is_none() {
                0
            } else {
                ::bbnf::runtime::tape::TapeRec::PAYLOAD_IN_ARENA_BIT
            };
            // AW-III.W5.c — fused SoA write; `push_leaf_fused` is
            // `#[inline(always)]` on `Columns`, so the capacity guard
            // + 7 unchecked stores fold directly into the surrounding
            // arm body at codegen time.
            let __idx = columns.push_leaf_fused(
                #kind_expr, __variant, __extra,
                #span_lo_expr, #span_hi_expr, __child_off,
            );
            frame_depth.push(stack.depth());
            __idx
        };
    }
}

/// AW-IV.W2.1 — inline body of
/// `emit_leaf(columns, frame_depth, stack, kind, lo, hi) -> u32`.
///
/// Trivially forwards to the payload-bearing variant with
/// `TapeOffset::NONE` as the child offset. Emitted as a direct splice
/// of the `emit_leaf_with_payload` body so LLVM sees the
/// constant-folded `__extra = 0` path without a helper hop.
pub(super) fn emit_emit_leaf_inline(
    out_ident: &proc_macro2::Ident,
    kind_expr: TokenStream,
    span_lo_expr: TokenStream,
    span_hi_expr: TokenStream,
) -> TokenStream {
    emit_emit_leaf_with_payload_inline(
        out_ident,
        kind_expr,
        span_lo_expr,
        span_hi_expr,
        quote! { ::bbnf::runtime::tape::TapeOffset::NONE },
    )
}

/// AW-IV.W2.1 — inline body of `close_compound(columns, frame_depth, stack, pos)`.
///
/// Expands the runtime helper's body directly: peek top frame (inlined
/// `stack_top` logic), patch `span_hi` / `child_off` / `has_children`,
/// apply KvPair promotion when flagged, stamp `variant_idx`, and
/// — under KvPair promotion — truncate the child rows. The body is
/// substantial (~70 lines at source) but covers every compound close
/// path in the hot walker; splicing it inline avoids the crate-boundary
/// dispatch that would otherwise sit at every Seq/Alt/Repeat/SY close.
pub(super) fn emit_close_compound_inline(span_hi_expr: TokenStream) -> TokenStream {
    quote! {
        {
            // Peek top frame without taking a mutable borrow —
            // mirror of `bbnf_tape::driver::stack_top`.
            let __top_frame: ::core::option::Option<&::bbnf::runtime::tape::Frame> = {
                if let ::core::option::Option::Some(f) = stack.overflow.last() {
                    ::core::option::Option::Some(f)
                } else if stack.inline_len == 0 {
                    ::core::option::Option::None
                } else {
                    ::core::option::Option::Some(&stack.inline[(stack.inline_len - 1) as usize])
                }
            };
            if let ::core::option::Option::Some(__frame) = __top_frame {
                let __parent = __frame.parent_rec as usize;
                let __span_hi_val: u32 = #span_hi_expr;
                let __has_children = (columns.len() as u32) > __frame.child_mark;
                columns.span_hi[__parent] = __span_hi_val;
                // AW-III.W1.6 — KvPair promotion path. Mine the most
                // recent child whose `PAYLOAD_IN_ARENA_BIT` is set,
                // flatten into a KvPair leaf, truncate children.
                if matches!(__frame.promote, ::bbnf::runtime::tape::SeqPromote::KvPair)
                    && __has_children
                {
                    let mut __scalar_arena_off: ::core::option::Option<
                        ::bbnf::runtime::tape::TapeOffset,
                    > = ::core::option::Option::None;
                    let __parent_extra: u16 =
                        ::bbnf::runtime::tape::TapeRec::PAYLOAD_IN_ARENA_BIT;
                    for __i in (__frame.child_mark as usize)..columns.len() {
                        if (columns.extra[__i]
                            & ::bbnf::runtime::tape::TapeRec::PAYLOAD_IN_ARENA_BIT)
                            != 0
                        {
                            __scalar_arena_off =
                                ::core::option::Option::Some(columns.child_off[__i]);
                        }
                    }
                    if let ::core::option::Option::Some(__off) = __scalar_arena_off {
                        let __meta_hi = columns.kinds[__parent] & 0xF0;
                        columns.kinds[__parent] = __meta_hi
                            | (::bbnf::runtime::tape::TapeKind::KvPair as u8 & 0x0F);
                        columns.child_off[__parent] = __off;
                        let __preserved = columns.extra[__parent]
                            & ::bbnf::runtime::tape::TapeRec::META_IDX_HI_BIT;
                        columns.extra[__parent] = __preserved | __parent_extra;
                        if __frame.variant_idx != u8::MAX {
                            columns.flags[__parent] = __frame.variant_idx;
                        } else if matches!(
                            __frame.kind,
                            ::bbnf::runtime::tape::DtaFrameKind::Alt,
                        ) {
                            columns.flags[__parent] = __frame.cursor as u8;
                        }
                        columns.truncate(__frame.child_mark as usize);
                        frame_depth.truncate(__frame.child_mark as usize);
                    } else {
                        if __has_children {
                            columns.child_off[__parent] =
                                ::bbnf::runtime::tape::TapeOffset(__frame.child_mark);
                            columns.extra[__parent] |=
                                ::bbnf::runtime::tape::TapeRec::HAS_CHILDREN_BIT;
                        }
                        if __frame.variant_idx != u8::MAX {
                            columns.flags[__parent] = __frame.variant_idx;
                        } else if matches!(
                            __frame.kind,
                            ::bbnf::runtime::tape::DtaFrameKind::Alt,
                        ) {
                            columns.flags[__parent] = __frame.cursor as u8;
                        }
                    }
                } else {
                    if __has_children {
                        columns.child_off[__parent] =
                            ::bbnf::runtime::tape::TapeOffset(__frame.child_mark);
                        columns.extra[__parent] |=
                            ::bbnf::runtime::tape::TapeRec::HAS_CHILDREN_BIT;
                    }
                    if __frame.variant_idx != u8::MAX {
                        columns.flags[__parent] = __frame.variant_idx;
                    } else if matches!(
                        __frame.kind,
                        ::bbnf::runtime::tape::DtaFrameKind::Alt,
                    ) {
                        columns.flags[__parent] = __frame.cursor as u8;
                    }
                }
            }
        }
    }
}

/// AW-IV.W2.1 — inline advance-or-pop dispatcher for the Seq fast path.
///
/// Emits the `advance_seq_fast` logic inline — peek top frame,
/// increment cursor if Seq has more children, return `Next(children[cursor])`
/// — with a fall-through to `advance_or_pop_with` for the cold non-Seq
/// path (Alt close, Repeat re-entry, SY reducer). `advance_or_pop_with`'s
/// body is ~250 lines (including SY reducer); splicing it into every
/// leaf-emit arm would explode code size without benefit — the Seq fast
/// path covers ~80% of JSON twitter leaf emits per the post-AW-III
/// samply. The fall-through keeps the cold helper as a single call
/// boundary reached from the ≤20% minority cases (Alt close / Repeat
/// re-entry / SY chain). `advance_seq_fast` itself is eliminated — its
/// body splices inline.
///
/// Returns a `Result<StepResult, DtaError>` expression the caller
/// assigns to the arm's `'step` break value.
pub(super) fn emit_advance_or_pop_inline() -> TokenStream {
    quote! {
        {
            // AW-IV.W2.1 — Seq fast path inlined directly, replacing
            // the cross-crate `advance_seq_fast` call. The helper
            // itself is `#[inline(always)]` on bbnf-tape; inlining
            // the body here guarantees the per-arm dispatch avoids
            // the function-call boundary regardless of inliner
            // heuristics.
            let __fast: ::core::option::Option<
                ::bbnf::runtime::tape::StepResult,
            > = 'fast: {
                let __top = match stack.top_mut() {
                    ::core::option::Option::Some(__t) => __t,
                    ::core::option::Option::None => {
                        break 'fast ::core::option::Option::None;
                    }
                };
                if matches!(__top.kind, ::bbnf::runtime::tape::DtaFrameKind::Seq) {
                    let __next_cursor = __top.cursor + 1;
                    if (__next_cursor as usize) < __top.children.len() {
                        __top.cursor = __next_cursor;
                        break 'fast ::core::option::Option::Some(
                            ::bbnf::runtime::tape::StepResult::Next(
                                __top.children[__next_cursor as usize],
                            ),
                        );
                    }
                }
                ::core::option::Option::None
            };
            if let ::core::option::Option::Some(__next) = __fast {
                ::core::result::Result::Ok(__next)
            } else {
                // Fall through to the full-body helper for Seq close,
                // Alt close, Repeat re-entry, and SY reducer. These
                // paths are cold + code-large; keeping them behind a
                // single call boundary avoids per-arm code explosion.
                ::bbnf::runtime::tape::advance_or_pop_with(
                    ::core::option::Option::Some(table),
                    ::core::option::Option::Some(input),
                    columns, frame_depth, psi, stack, pos, slot,
                )
            }
        }
    }
}

/// AW-IV.W2.1 — inline `PayloadStream::push(PayloadJob::new(...))`.
///
/// `PayloadStream::push` is a thin `#[inline] pub fn` wrapping
/// `Vec::push`; `PayloadJob::new` is `#[inline] const fn` constructing
/// a 20-byte struct with 3 padding bytes. Splicing the construction +
/// call inline (rather than letting the cross-crate call sit there)
/// ensures the walker arm's lowered body contains the store sequence
/// directly. The `push` method's internal `self.jobs.push(job)` is
/// the only operation — inlining relies on the method's
/// `#[inline]` annotation (W2.1 upgrades to `#[inline(always)]` for
/// cross-crate safety).
pub(super) fn emit_psi_push_inline(
    rec_idx_expr: TokenStream,
    input_lo_expr: TokenStream,
    input_hi_expr: TokenStream,
    kind_expr: TokenStream,
    arena_offset_expr: TokenStream,
) -> TokenStream {
    quote! {
        psi.push(::bbnf::runtime::tape::PayloadJob::new(
            #rec_idx_expr, #input_lo_expr, #input_hi_expr,
            #kind_expr, #arena_offset_expr,
        ));
    }
}
