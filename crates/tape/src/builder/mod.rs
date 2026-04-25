//! `FusedBuilder` — the parser's single write interface to the fused
//! substrate (tape + value columns).
//!
//! # Role (Tranche AY-II.W0'.a)
//!
//! Pre-W0'.a the parser wrote two disjoint substrates in lockstep: a
//! `TapeBuilder` owning the canonical structural tape + a
//! `ValueBuilder<R>` owning the parallel typed-value slab. W0 landed
//! the structural rewrite but never threaded the second builder into
//! the shape-fn signatures, so the value slab stayed empty and
//! `Parsed::to_value()` panicked. AUDIT-C's Path B absorbed the two
//! substrates at the type level: every `begin_compound` /
//! `end_compound` / `push_leaf_*` call stamps BOTH column families
//! atomically, and `rollback_to(open_offset)` truncates both in
//! lockstep. Shape emitters see the same API surface they had
//! against the old `TapeBuilder` — only the type behind
//! `&mut builder` shifts.
//!
//! # Write discipline
//!
//! The fused builder pairs each structural tape push with one
//! [`ValueFrame`](value::ValueFrame) push:
//!
//! - [`FusedBuilder::begin_compound`] stamps a pre-order compound
//!   row on the tape AND a `Compound` frame on the value arena,
//!   pushing the frame's offset onto the open-stack. Rollback
//!   signatures carry the returned `u32` tape-offset; the matching
//!   value [`ValueCheckpoint`](value::ValueCheckpoint) is resolved
//!   internally from the open-stack.
//! - [`FusedBuilder::end_compound`] /
//!   [`FusedBuilder::end_compound_post_order`] back-patch
//!   `(span_hi, child_off, HAS_CHILDREN_BIT)` on the tape row AND
//!   finalise the value frame's `(span_hi, child_count)`.
//! - [`FusedBuilder::push_leaf`] / `push_leaf_with` /
//!   `push_leaf_borrowed_string` / `push_leaf_with_arena_frame` /
//!   `push_leaf_with_arena_payload` / `push_leaf_with_f64_direct`
//!   stamp the tape leaf row AND a leaf frame + matching
//!   payload-column entry where the payload is a narrow / wide
//!   scalar.
//! - [`FusedBuilder::rollback_to`] truncates tape columns + value
//!   arena + payload columns + open-stack atomically back to the
//!   state at the matching `begin_compound`.
//!
//! # Zero signature churn at shape call sites
//!
//! Every `builder.begin_compound(..)`, `builder.end_compound(..)`,
//! `builder.push_leaf*(..)`, `builder.rollback_to(..)` compiles
//! unchanged behind `&mut FusedBuilder`. The alias
//! `pub type TapeBuilder = FusedBuilder;` at the crate root keeps
//! un-regenned `generated.rs` composing against the renamed type
//! through the bootstrap escape window; regen retires the alias by
//! spelling `FusedBuilder` directly.

use crate::columns::Columns;
use crate::kind::TapeKind;
use crate::tape::{Tape, TapeOffset, TapeRec};

pub mod output;
pub mod value;

pub use output::FusedOutput;
pub use value::{
    PayloadTag, PayloadValue, ValueChildren, ValueFrame, ValueFramesOutput,
};

use value::ValueCheckpoint;

/// Payload data handed to [`FusedBuilder::push_leaf_with`] — the
/// single entry point for payload-bearing leaves.
///
/// Each variant names one of the payload shapes the columnar
/// substrate recognises:
///
/// - [`PayloadData::None`] — pure span leaf, no payload. Equivalent
///   to [`FusedBuilder::push_leaf`]; exposed on `PayloadData` so
///   callers that build a payload conditionally don't need to switch
///   to a different entry point when the payload is absent.
/// - [`PayloadData::InlineScalar`] — scalar ≤ 4 bytes written into
///   the [`Columns::pay_narrow`](crate::columns::Columns::pay_narrow)
///   column. The record's `child_off` stores the column rank.
/// - [`PayloadData::WideScalar`] — 8-byte scalar (`f64`/`u64`/`i64`/
///   packed `Span`) written into the
///   [`Columns::pay_wide`](crate::columns::Columns::pay_wide) column;
///   the record's `child_off` stores the column rank.
/// - [`PayloadData::Aggregate`] — packed tuple bytes (colour tuples,
///   dimension `(f64, u8)` pairs, kv-pair values). Length up to 16
///   bytes; written verbatim into the shared
///   [`Columns::pay_agg`](crate::columns::Columns::pay_agg) arena
///   rounded up to the next 8-byte boundary. `child_off` holds the
///   arena byte offset.
/// - [`PayloadData::LargeAggregate`] — aggregate payload exceeding
///   the 16-byte inline budget (CSS colour-functions at 33+ B).
///   Identical arena encoding to `Aggregate` — bytes verbatim into
///   an 8-aligned slot — but the width is recovered from the
///   grammar's payload-layout table keyed by
///   `(kind, variant_idx)` rather than a frame header.
/// - [`PayloadData::Bytes`] — variable-length byte string (decoded
///   JSON strings, comment bodies, regex patterns). Framed into the
///   arena as `(len: u32 LE, bytes: [u8; len])`.
#[derive(Debug, Clone, Copy)]
pub enum PayloadData<'a> {
    /// No payload.
    None,
    /// Scalar value ≤ 4 bytes stored in `Columns::pay_narrow`. The
    /// `u32` carries the value's raw little-endian bytes.
    InlineScalar(u32),
    /// 8-byte scalar (`f64`, `u64`, `i64`, packed `Span`) stored in
    /// `Columns::pay_wide` as raw bits. Callers convert at the push
    /// site via `f64::to_bits()` / `to_le_bytes()` as needed.
    WideScalar(u64),
    /// Packed aggregate tuple bytes written verbatim into
    /// `Columns::pay_agg`. Length up to 16 bytes.
    Aggregate(&'a [u8]),
    /// Aggregate bytes exceeding the 16-byte inline budget — arena-
    /// backed, unframed. Written verbatim into an 8-aligned slot in
    /// `Columns::pay_agg` (trailing pad zero-initialised, matching
    /// [`Self::Aggregate`]). Width recovered from the grammar's
    /// payload-layout table via `(kind, variant_idx)`.
    LargeAggregate(&'a [u8]),
    /// Byte string framed as `(len: u32 LE, bytes)` into
    /// `Columns::pay_agg`.
    Bytes(&'a [u8]),
}

/// Error state surfaced through [`FusedBuilder::finish`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TapeBuildError {
    /// The caller reported an unrecoverable parse failure. Constructed
    /// by the generated parser when a non-recoverable branch fails.
    ParseFailed {
        /// Input byte offset where the failure was detected.
        offset: u32,
        /// Optional rule-name id / label for diagnostics.
        rule_label: u32,
    },
}

// ─── Instrumentation counter — always present ─────────────────────
//
// AY-II.W0.c introduced a per-thread counter on `ValueBuilder::new`
// to prove `Parsed::to_value()` does not trigger a second parse.
// W0'.a absorbs the counter here so the same invariant tests keep
// working after the value substrate moves into `FusedBuilder`.
//
// Pre-W0'.a the counter was `#[cfg(test)]`-gated inside the `bbnf`
// crate, which meant it was visible to `bbnf`'s test binaries. W0'.a
// moves the counter into the `tape` crate where the allocator lives;
// dependency-crate cfgs don't propagate to downstream `--cfg test`
// compilations, so the counter is always present (a single
// `Cell<u64>` per thread). The production cost is one thread-local
// increment per `FusedBuilder::new` / `with_capacity` call —
// negligible against the parse body.
thread_local! {
    static NEW_CALL_COUNT: ::core::cell::Cell<u64> = const { ::core::cell::Cell::new(0) };
}

/// Return the count of [`FusedBuilder::new`] /
/// [`FusedBuilder::with_capacity`] invocations on the current
/// thread.
///
/// `Parsed::to_value()` must not increment this counter — that is
/// the invariant the `value_api_apples_to_apples` parse-count test
/// asserts (via the pre-W0'.a
/// `runtime::value_builder::value_builder_new_call_count` shim path
/// that aliases to this accessor).
pub fn fused_builder_new_call_count() -> u64 {
    NEW_CALL_COUNT.with(|c| c.get())
}

/// Reset the [`FusedBuilder::new`] counter to `0`.
pub fn reset_fused_builder_new_call_count() {
    NEW_CALL_COUNT.with(|c| c.set(0));
}

/// The parser's single write interface to the fused (tape + value)
/// substrate.
///
/// Held by `&mut` for the duration of a parse. The generated parser
/// functions thread it through every rule call alongside the
/// `ParserState`:
///
/// ```ignore
/// fn __pair<'i>(
///     state: &mut parse_that::ParserState<'i>,
///     builder: &mut tape::FusedBuilder,
/// ) -> Option<u32> {
///     let open = builder.begin_compound(TapeKind::Rule, state.offset, 0, 0, 0, 0);
///     let _key = __string(state, builder)?;
///     state.eat_byte(b':')?;
///     state.skip_ws();
///     let _value = __value(state, builder)?;
///     builder.end_compound(open, state.offset);
///     Some(open)
/// }
/// ```
///
/// Post-W0'.a the builder owns BOTH column families:
///
/// - **structural tape columns** (records, sib_skip, typed payloads)
///   via [`Self::columns`] / [`Self::columns_mut`].
/// - **value arena + payload columns** that feed the fused-pipeline
///   projection path — emitted materialisers consume the
///   [`FusedOutput`] this builder hands back at [`Self::finish`] time.
#[derive(Debug, Default)]
pub struct FusedBuilder {
    /// Column storage under construction. Consumed by [`Self::finish`]
    /// which computes the sibling-skip column and packages the result
    /// into a [`Tape`].
    pub(crate) columns: Columns,
    /// Sticky error state. Once set, subsequent `push_*` calls are
    /// still accepted (so mid-recovery parses can continue producing
    /// records for partial success), but `finish` returns the error.
    pub(crate) error: Option<TapeBuildError>,
    // B3.W0.δ — `current_depth` migrated into [`Columns::current_depth`]
    // alongside `frame_depth` so a `columns_mut().rollback_to(...)`
    // call from generated parser code restores both atomically.

    // ── Value substrate (absorbed from W0.c `ValueBuilder<R>`) ────
    /// Nested value-frame arena — one entry per compound open + one
    /// per leaf push. Laid out in emitter push order; compounds
    /// reference their children via
    /// `(ValueFrame::first_child, ValueFrame::child_count)`.
    value_frames: Vec<ValueFrame>,
    /// Narrow-column scalar payloads (u32 / bool / u8). Indexed by
    /// [`PayloadTag::narrow`] rank.
    value_payloads_narrow: Vec<u32>,
    /// Wide-column scalar payloads (f64 / u64 / u32-pair). Indexed
    /// by [`PayloadTag::wide`] rank.
    value_payloads_wide: Vec<u64>,
    /// Open compound stack — one entry per `begin_compound` without
    /// a matching `end_compound`. Each entry carries the
    /// `ValueCheckpoint` recorded at open time so rollback truncates
    /// every column family to the pre-open state atomically.
    value_open_stack: Vec<ValueCheckpoint>,
}

/// Walk the leftmost-descendant chain from `start` to find the lowest
/// offset of any record in `start`'s subtree.
///
/// Used by [`FusedBuilder::end_compound_post_order`] to extend its
/// `frame_depth` bump range to cover descendants whose offsets sit
/// strictly below `first_child`. When `first_child` is itself a
/// post-order compound, its body lives at offsets below `first_child`
/// (post-order layout); those descendants belong to the closing
/// compound's subtree and need the same `+1` adjustment.
///
/// The walk follows `child_off` while it points strictly backward
/// (`co_child_off < co` — canonical post-order subtree root). For
/// pre-order children (`child_off > co`) and leaves the walk stops:
/// pre-order child ranges live at offsets ABOVE the parent, so the
/// parent's offset is already the leftmost in that subtree's prefix.
///
/// Bounded by the post-order chain depth — runs at-most O(max_depth)
/// per close, and only for compounds whose body extends below
/// `first_child`. B3.W0.ζ.
#[inline]
fn leftmost_descendant_offset(columns: &Columns, start: u32) -> u32 {
    let mut off = start;
    while columns.has_children_at(off) {
        let co = columns.child_off_at(off);
        if co.is_none() || co.0 >= off {
            break;
        }
        off = co.0;
    }
    off
}

impl FusedBuilder {
    /// Construct a fresh builder with an empty tape + value
    /// substrate.
    pub fn new() -> Self {
        NEW_CALL_COUNT.with(|c| c.set(c.get() + 1));
        Self::default()
    }

    /// Construct a builder sized for `expected` structural records.
    ///
    /// The value arena + payload columns pre-allocate proportionally
    /// so the hot push path never trips a `Vec::push` realloc on
    /// corpus input. The sizing mirrors the pre-W0'.a
    /// `TapeBuilder::with_capacity` + `ValueBuilder::new` pair: one
    /// frame per tape record worst-case, narrow / wide payloads at
    /// `expected / 4`.
    pub fn with_capacity(expected: usize) -> Self {
        NEW_CALL_COUNT.with(|c| c.set(c.get() + 1));
        Self {
            columns: Columns::with_capacity(expected),
            error: None,
            value_frames: Vec::with_capacity(expected),
            value_payloads_narrow: Vec::with_capacity(expected / 4),
            value_payloads_wide: Vec::with_capacity(expected / 4),
            value_open_stack: Vec::with_capacity(16),
        }
    }

    /// Rewind every column family — structural tape, inline
    /// `frame_depth`, and the value substrate — back to the state at
    /// the matching `begin_compound` whose `open_offset` the caller
    /// passes in.
    ///
    /// Tape-side: delegates to [`Columns::rollback_to`] and truncates
    /// [`Self::frame_depth`] in lockstep — both columns are owned by
    /// this builder and are pushed in lockstep on every structural
    /// emission (B3.W0.γ).
    ///
    /// Value-side: resolves the matching [`ValueCheckpoint`] from the
    /// open-stack and truncates the value frame arena + narrow / wide
    /// payload columns atomically. Open-stack entries at or above
    /// the checkpoint are orphaned frames from the failed branch and
    /// are discarded — the emitter's rollback contract requires it.
    ///
    /// The `open_offset` argument is the tape-side offset
    /// [`Self::begin_compound`] returned; it is used to locate the
    /// paired value checkpoint because the open-stack is pushed in
    /// the same order as begin_compound calls.
    #[inline(always)]
    pub fn rollback_to(&mut self, open_offset: u32) {
        // Tape side — structural columns + frame_depth in lockstep.
        // The compound row at `open_offset` was emitted at
        // `current_depth - 1` (its `begin_compound` bumped depth AFTER
        // pushing the row), so restoring `current_depth` to that row's
        // recorded depth re-establishes the pre-`begin_compound`
        // bookkeeping.
        // Tape side — `Columns::rollback_to` rewinds `records`,
        // `sib_skip`, `frame_depth`, AND restores `current_depth` to
        // the depth recorded at `open_offset` (B3.W0.δ). Generated
        // parser code that calls `columns_mut().rollback_to(...)`
        // directly gets the same restoration without going through
        // this builder method.
        self.columns.rollback_to(open_offset);
        // Value side — pop every open frame whose tape_offset-equivalent
        // landed on or after `open_offset`, resolving the matching
        // value checkpoint and truncating column families to its
        // pre-open state. The value open-stack is pushed in lockstep
        // with tape `begin_compound` calls, so walking from the top
        // and stopping at the first entry whose `frame_offset` is
        // below `open_offset`'s value-pair yields the right scope.
        //
        // The tape `open_offset` and the value `frame_offset` are
        // NOT equal in general — shape emitters push multiple tape
        // rows per conceptual value frame (Seq / Rule wrappers). But
        // the open-stack is a faithful LIFO of begin_compound calls,
        // and the caller invariant is: rollback_to(open_offset) is
        // paired with begin_compound(open_offset). So popping the
        // top matching checkpoint is correct.
        //
        // W0'.b's projection path does not observe orphaned frames
        // from failed branches — the truncation below guarantees the
        // value substrate matches the tape at every rollback boundary.
        while let Some(&checkpoint) = self.value_open_stack.last() {
            // The top-most checkpoint corresponds to the most-recent
            // `begin_compound`; if that compound was opened at or
            // after `open_offset` on the tape, it must rewind.
            // Because tape rows are monotonic, a value frame whose
            // paired tape row landed at or above `open_offset` is
            // always at the top of the open stack (since we closed
            // any deeper compounds between that open and the rollback
            // via `end_compound`).
            //
            // In practice a single rollback unwinds the SINGLE open
            // compound the emitter's retry-IIFE opened; the while
            // loop is defensive against grammar authors who leave
            // multiple compounds open across a retry boundary.
            // W0'.a's rollback contract with shape emitters:
            // rollback_to(open) is always called BEFORE any further
            // begin_compound in the retry scope, and always matches
            // the open of the failed branch.
            self.value_frames.truncate(checkpoint.frame_offset as usize);
            self.value_payloads_narrow
                .truncate(checkpoint.narrow_rank as usize);
            self.value_payloads_wide
                .truncate(checkpoint.wide_rank as usize);
            self.value_open_stack.pop();
            // After popping the failed compound, undo the parent-
            // counter bump that `value_begin_compound` applied when
            // the failed compound opened. The retry-IIFE will
            // re-open a fresh compound (which bumps the parent
            // again), so symmetric decrement here keeps
            // `direct_child_count` equal to the number of
            // SURVIVED direct children at close time.
            if let Some(parent) = self.value_open_stack.last_mut() {
                parent.direct_child_count =
                    parent.direct_child_count.saturating_sub(1);
            }
            // Only pop one — the matched retry compound. If the
            // caller left additional opens deeper than `open_offset`
            // on the stack (a grammar bug, not an emitter bug) they
            // are discarded on `finish` via the open-stack empty
            // assertion. The single-pop discipline matches the
            // pre-W0'.a `ValueBuilder::rollback_to` semantics.
            break;
        }
    }

    /// Append a leaf record with a concrete kind + span.
    ///
    /// Leaves have no children, so `child_off` is forced to
    /// [`TapeOffset::NONE`] on the tape row; a leaf
    /// [`ValueFrame`] is appended to the value arena with
    /// `payload_tag == PayloadTag::NONE`.
    ///
    /// `meta_idx` is the branch index for Alt-bodied rules (`0` for
    /// everything else). Packed into `TapeRec::kind_meta` (high 4
    /// bits) and `TapeRec::flags` (bit 7). 5-bit range: 0-31.
    #[inline(always)]
    pub fn push_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
    ) -> TapeOffset {
        debug_assert!(kind.is_leaf(), "push_leaf on compound kind {:?}", kind);
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_meta_bit,
            span_lo,
            span_hi,
            TapeOffset::NONE,
        );
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    // ── AY-II.W0.a unified compound emission API ───────────────────

    /// Begin a compound in pre-order.
    ///
    /// Emits a compound row with provisional `span_hi == span_lo`,
    /// `child_off = TapeOffset::NONE`, and `HAS_CHILDREN_BIT` cleared
    /// on the tape AND opens a matching value-arena frame + pushes
    /// the value checkpoint onto the open-stack.
    ///
    /// `variant_idx` is the rule discriminant (`[0, 256)`); `meta_idx`
    /// is the Alt-branch ordinal (`[0, 32)`). Both are stamped in
    /// walker-parity positions — `flags` carries `variant_idx`;
    /// `kind_meta` low 4 bits carry `kind`, high 4 bits carry
    /// `meta_idx[0..4]`, with `meta_idx[4]` lifted into `extra` via
    /// [`TapeRec::META_IDX_HI_BIT`]. The `extra_flags` argument
    /// carries any additional bits the caller wants stamped
    /// alongside the kind/meta pair (e.g. `STRING_BORROW_BIT` — the
    /// `HAS_CHILDREN_BIT` is owned by [`Self::end_compound`] /
    /// [`Self::end_compound_post_order`]).
    ///
    /// The compound row stamps [`Self::current_depth`] into
    /// [`Self::frame_depth`] BEFORE bumping the depth — the parent's
    /// own row sits at the parent's depth, and its (yet-to-emit)
    /// children stamp at depth + 1. The `frame_depth: u8` parameter is
    /// retained for source compatibility but is no longer consulted —
    /// the builder owns depth bookkeeping (B3.W0.γ).
    ///
    /// Returns the tape row offset the caller passes back to
    /// [`Self::end_compound`] (pre-order) or
    /// [`Self::end_compound_post_order`] (post-order). Emitter retry
    /// paths rewind via [`Self::rollback_to`] with the returned
    /// offset; the next `begin_compound` reuses the same row.
    #[inline(always)]
    pub fn begin_compound(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        variant_idx: u8,
        meta_idx: u8,
        _frame_depth: u8,
        extra_flags: u16,
    ) -> u32 {
        debug_assert!(
            kind.is_compound(),
            "begin_compound on leaf/annotation kind {:?}",
            kind
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_flags | extra_meta_bit,
            span_lo,
            span_lo,
            TapeOffset::NONE,
        );
        // Bump after stamping so the children of this compound stamp
        // at `current_depth + 1`. Saturate to `u8::MAX` — grammars
        // that nest deeper than 255 compounds are diagnosed by the
        // finaliser's depth-overflow path; saturation keeps the push
        // path branchless.
        self.columns.current_depth = self.columns.current_depth.saturating_add(1);
        self.value_begin_compound(kind, span_lo, variant_idx);
        idx
    }

    /// Finalise a compound opened via [`Self::begin_compound`] in
    /// pre-order — the caller emitted the compound row BEFORE its
    /// children, so the first child's root sits at `open_offset + 1`.
    ///
    /// Back-patches `span_hi` on the row at `open_offset`. When
    /// `open_offset + 1 < columns.len()` (at least one child landed),
    /// sets `child_off = TapeOffset(open_offset + 1)` — the pre-order
    /// layout driving the cursor's O(1) first-child fast path — and
    /// stamps `HAS_CHILDREN_BIT`. Does NOT write `sib_skip`; the
    /// finaliser is its sole writer. Value-side: pops the open-stack
    /// and finalises `(span_hi, child_count)` on the arena frame.
    #[inline(always)]
    pub fn end_compound(&mut self, open_offset: u32, span_hi: u32) {
        self.columns.set_span_hi_at(open_offset, span_hi);
        // The compound was emitted at its outer frame's depth; its
        // direct children are stamped at `open_depth + 1`. Pre-order
        // layout normally puts the first child at `open_offset + 1`,
        // BUT when an inner [`Self::end_compound_post_order`] retro-
        // actively bumps `frame_depth` over its child range, records
        // that landed between this compound's open and the nested
        // post-order close move one level deeper. The true first
        // child is then the first row at exactly `open_depth + 1`,
        // bounded by the compound's structural scope.
        //
        // Scope bound (B3.W0.ε): a record at `frame_depth <= open_depth`
        // marks the end of this compound's scope (an outer-frame
        // sibling or shallower); abort the scan at that point. Without
        // this bound, a childless compound followed by no later same-
        // depth records would scan to end-of-tape; with it, the scan
        // also correctly identifies childless compounds whose body
        // produced zero records.
        let open_depth = self.columns.frame_depth[open_offset as usize];
        let target_depth = open_depth.saturating_add(1);
        let n = self.columns.len() as u32;
        let mut first_child = open_offset + 1;
        let mut found = false;
        while first_child < n {
            let d = self.columns.frame_depth[first_child as usize];
            if d == target_depth {
                found = true;
                break;
            }
            if d <= open_depth {
                // Crossed out of the compound's scope (an outer-frame
                // sibling or shallower) without finding a target-depth
                // child — this compound is childless.
                break;
            }
            first_child += 1;
        }
        if found {
            self.columns
                .set_child_off_at(open_offset, TapeOffset(first_child));
            self.columns
                .or_extra_at(open_offset, TapeRec::HAS_CHILDREN_BIT);
        }
        // B3.W0.γ — pair the `begin_compound` depth bump.
        self.columns.current_depth = self.columns.current_depth.saturating_sub(1);
        self.value_end_compound(span_hi);
    }

    /// Finalise a compound opened via [`Self::begin_compound`] in
    /// post-order — the compound row was allocated AFTER its
    /// children, so `open_offset` is the LAST record and the first
    /// child's root is `first_child` (captured at children-enter via
    /// `columns_mut().len() as u32`).
    ///
    /// Back-patches `span_hi`; writes `child_off = first_child` and
    /// stamps `HAS_CHILDREN_BIT` when `first_child.0 < open_offset`
    /// (children exist). When the child frame is empty
    /// (`first_child.0 == open_offset` — no records landed between
    /// the children-enter capture and the `begin_compound` call),
    /// leaves `child_off` at `NONE` and `HAS_CHILDREN_BIT` cleared,
    /// matching [`Self::end_compound`]'s no-children branch.
    ///
    /// Does NOT write `sib_skip`; the finaliser is its sole writer.
    /// Value-side: pops the open-stack and finalises `(span_hi,
    /// child_count)` on the arena frame.
    #[inline(always)]
    pub fn end_compound_post_order(
        &mut self,
        open_offset: u32,
        span_hi: u32,
        first_child: TapeOffset,
    ) {
        self.columns.set_span_hi_at(open_offset, span_hi);
        if !first_child.is_none() && first_child.0 < open_offset {
            self.columns.set_child_off_at(open_offset, first_child);
            self.columns
                .or_extra_at(open_offset, TapeRec::HAS_CHILDREN_BIT);
            // B3.W0.ζ — bump the entire subtree's `frame_depth`, not
            // just the offset range `[first_child, open_offset)`. When
            // `first_child` is itself a post-order compound, its body
            // sits at offsets STRICTLY BELOW `first_child`. Those
            // descendants are part of THIS compound's subtree (they
            // were emitted as our body), so they need the same `+1`
            // adjustment. Walking the leftmost-descendant chain finds
            // the lowest offset of any record in our subtree; bumping
            // `[leftmost, open_offset)` covers every descendant.
            //
            // Without this widening, descendants of `first_child` go
            // un-bumped on this close — they end up at the same final
            // depth as our parent's other direct children, which the
            // finaliser's same-depth chain then groups as our siblings.
            // The cursor's `child_off`-driven backward walk then yields
            // our own row as a sibling of our parent, producing a tape
            // graph cycle (B3.W0.ζ — OFF=324 / float_lit case). The
            // leftmost-descendant walk is bounded by the post-order
            // chain length and runs at-most-once per close.
            let lo = leftmost_descendant_offset(&self.columns, first_child.0) as usize;
            let hi = open_offset as usize;
            for slot in &mut self.columns.frame_depth[lo..hi] {
                *slot = slot.saturating_add(1);
            }
        }
        // B3.W0.γ — pair the `begin_compound` depth bump.
        self.columns.current_depth = self.columns.current_depth.saturating_sub(1);
        self.value_end_compound(span_hi);
    }

    // ── Payload-bearing leaf push ──────────────────────────────────

    /// Append a leaf record carrying the supplied [`PayloadData`].
    ///
    /// Unified entry point for every payload-bearing leaf. The
    /// `PayloadData` variants cover the complete runtime payload
    /// taxonomy; the record's `child_off` ends up holding a column
    /// rank (for scalar payloads) or an arena byte offset (for
    /// aggregates and byte frames). Value-side: pushes a leaf
    /// [`ValueFrame`] carrying a matching [`PayloadTag`] for scalar
    /// payloads; aggregate / byte payloads leave `payload_tag` at
    /// [`PayloadTag::NONE`] because the projection path reads those
    /// off the tape arena.
    #[inline(always)]
    pub fn push_leaf_with(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
        payload: PayloadData<'_>,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with on compound kind {:?}",
            kind
        );
        let (child_off, value_tag) = match payload {
            PayloadData::None => (TapeOffset::NONE, PayloadTag::NONE),
            PayloadData::InlineScalar(v) => {
                // AV.2.3: inline scalars land in `pay_narrow`; the
                // record's `child_off` carries the column rank. The
                // earlier collision with `TapeOffset::NONE` when
                // inlining `u32::MAX` directly into `child_off` is
                // resolved — column ranks don't approach `u32::MAX`
                // unless a grammar emits four billion inline scalars.
                let rank = self.columns.pay_narrow.len() as u32;
                self.columns.pay_narrow.push(v);
                // Value-side narrow payload stored in parallel so
                // projection reads don't hit the tape. Track its
                // rank independently from the tape column rank —
                // they advance together but the tape column is the
                // one `child_off` references.
                let v_rank = self.value_payloads_narrow.len() as u32;
                self.value_payloads_narrow.push(v);
                (TapeOffset(rank), PayloadTag::narrow(v_rank))
            }
            PayloadData::WideScalar(v) => {
                let rank = self.columns.pay_wide.len() as u32;
                self.columns.pay_wide.push(v);
                let v_rank = self.value_payloads_wide.len() as u32;
                self.value_payloads_wide.push(v);
                (TapeOffset(rank), PayloadTag::wide(v_rank))
            }
            PayloadData::Aggregate(bytes) => {
                if bytes.is_empty() {
                    (TapeOffset::NONE, PayloadTag::NONE)
                } else {
                    let offset = self.alloc_aggregate_slot(bytes);
                    (TapeOffset(offset), PayloadTag::NONE)
                }
            }
            PayloadData::LargeAggregate(bytes) => {
                if bytes.is_empty() {
                    (TapeOffset::NONE, PayloadTag::NONE)
                } else {
                    let offset = self.alloc_large_aggregate_slot(bytes);
                    (TapeOffset(offset), PayloadTag::NONE)
                }
            }
            PayloadData::Bytes(bytes) => {
                let offset = self.alloc_bytes_frame(bytes);
                (TapeOffset(offset), PayloadTag::NONE)
            }
        };
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_meta_bit,
            span_lo,
            span_hi,
            child_off,
        );
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, value_tag);
        TapeOffset(idx)
    }

    /// Append aggregate bytes into a `pay_agg` slot rounded up to
    /// the next 8-byte boundary and return the byte offset.
    ///
    /// The slot is zero-initialised so any unused trailing bytes
    /// (between `bytes.len()` and the rounded-up total) are
    /// deterministic.
    #[inline]
    fn alloc_aggregate_slot(&mut self, bytes: &[u8]) -> u32 {
        debug_assert!(bytes.len() <= 16, "aggregate payload exceeds 16 bytes");
        let slot_count = bytes.len().div_ceil(8);
        let slot_total = slot_count * 8;
        let arena = &mut self.columns.pay_agg;
        let start = arena.len();
        arena.resize(start + slot_total, 0);
        // SAFETY: the resize above guarantees `slot_total` bytes are
        // available starting at `start`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                arena.as_mut_ptr().add(start),
                bytes.len(),
            );
        }
        start as u32
    }

    /// Append a large aggregate payload (> 16 bytes) into a `pay_agg`
    /// slot rounded up to the next 8-byte boundary and return the
    /// byte offset.
    ///
    /// Identical in on-arena layout to
    /// [`Self::alloc_aggregate_slot`]: bytes are written verbatim,
    /// the slot is padded to 8-byte boundary with zero-initialised
    /// trailing bytes, no length prefix. The only distinction is the
    /// size bound — `LargeAggregate` carries payloads that exceed
    /// the inline 16-byte budget. Readers recover the byte count
    /// from the grammar's payload-layout table keyed by
    /// `(kind, variant_idx)`.
    #[inline]
    fn alloc_large_aggregate_slot(&mut self, bytes: &[u8]) -> u32 {
        debug_assert!(
            bytes.len() > crate::MAX_INLINE_AGGREGATE_BYTES,
            "LargeAggregate payload {} bytes fits inline (≤ {})",
            bytes.len(),
            crate::MAX_INLINE_AGGREGATE_BYTES,
        );
        let slot_count = bytes.len().div_ceil(8);
        let slot_total = slot_count * 8;
        let arena = &mut self.columns.pay_agg;
        let start = arena.len();
        arena.resize(start + slot_total, 0);
        // SAFETY: the resize above guarantees `slot_total` bytes are
        // available starting at `start`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                arena.as_mut_ptr().add(start),
                bytes.len(),
            );
        }
        start as u32
    }

    /// Append a `(len: u32 LE, bytes)` frame into `pay_agg` and
    /// return the byte offset of the length prefix.
    #[inline]
    fn alloc_bytes_frame(&mut self, bytes: &[u8]) -> u32 {
        let arena = &mut self.columns.pay_agg;
        let start = arena.len();
        let len = bytes.len() as u32;
        arena.extend_from_slice(&len.to_le_bytes());
        arena.extend_from_slice(bytes);
        start as u32
    }

    /// Borrow the `pay_agg` arena for direct variable-length payload
    /// writes.
    ///
    /// The JSON-string decode kernel uses this to stream decoded
    /// bytes into the arena without an intermediate allocation.
    /// After decoding, the caller commits the record via
    /// [`Self::push_leaf_with`] with a zero-copy `PayloadData::Bytes`
    /// pointing at a buffer that was built via this accessor.
    #[inline(always)]
    pub fn arena_mut(&mut self) -> &mut Vec<u8> {
        &mut self.columns.pay_agg
    }

    /// The current length of the `pay_agg` arena — equivalently, the
    /// byte offset where the next write will land.
    #[inline(always)]
    pub fn arena_len(&self) -> u32 {
        self.columns.pay_agg.len() as u32
    }

    /// Append a leaf record whose payload bytes (with length prefix)
    /// have already been written to `pay_agg` at `arena_offset`.
    ///
    /// Used by the JSON-string decode kernel which streams decoded
    /// bytes directly into the arena via [`Self::arena_mut`] and then
    /// commits the record by calling this method with the frame's
    /// offset.
    ///
    /// `meta_idx` range is 0-31 (5-bit packed field).
    #[inline(always)]
    pub fn push_leaf_with_arena_frame(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
        arena_offset: u32,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_arena_frame on compound kind {:?}",
            kind
        );
        debug_assert!(
            (arena_offset as usize) + 4 <= self.columns.pay_agg.len(),
            "push_leaf_with_arena_frame: offset {} + 4 exceeds arena len {}",
            arena_offset,
            self.columns.pay_agg.len()
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_meta_bit,
            span_lo,
            span_hi,
            TapeOffset(arena_offset),
        );
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    /// Append a leaf record whose payload is an in-arena scalar of
    /// `payload_width` bytes already written at `arena_offset`.
    ///
    /// Distinct from [`Self::push_leaf_with_arena_frame`], which
    /// assumes a 4-byte length prefix convention for decoded-string
    /// frames. This entry point mirrors the walker's
    /// [`crate::driver::emit_leaf_with_payload`] semantics: the
    /// caller has pushed `payload_width` bytes (1 for the Pratt
    /// op-discriminant, 8 for wide scalars) directly into
    /// [`Self::arena_mut`], and the resulting record carries
    /// [`TapeRec::PAYLOAD_IN_ARENA_BIT`] set so scalar readers
    /// ([`Tape::payload_u8`], [`Tape::payload_u64`]) slice the arena
    /// at `arena_offset` instead of indirecting through a column
    /// rank.
    ///
    /// `meta_idx` range is 0-31 (5-bit packed field).
    /// `payload_width` must be one of 1 / 2 / 4 / 8 — the widths the
    /// scalar readers honour; asserted in debug builds.
    #[inline(always)]
    pub fn push_leaf_with_arena_payload(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
        arena_offset: u32,
        payload_width: u32,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_arena_payload on compound kind {:?}",
            kind
        );
        debug_assert!(
            matches!(payload_width, 1 | 2 | 4 | 8),
            "push_leaf_with_arena_payload: payload_width {} must be 1 / 2 / 4 / 8",
            payload_width,
        );
        debug_assert!(
            (arena_offset as usize) + (payload_width as usize)
                <= self.columns.pay_agg.len(),
            "push_leaf_with_arena_payload: offset {} + {} exceeds arena len {}",
            arena_offset,
            payload_width,
            self.columns.pay_agg.len()
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let extra = extra_meta_bit | TapeRec::PAYLOAD_IN_ARENA_BIT;
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra,
            span_lo,
            span_hi,
            TapeOffset(arena_offset),
        );
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    /// Append a borrow-safe string leaf — zero arena writes.
    ///
    /// Companion to [`Self::push_leaf_with_arena_frame`] for the
    /// JSON decode kernel's fast path. When
    /// [`parse_that::parsers::scan::decode_json_string_to_arena`]
    /// returns `StringPayload::Borrowed`, the source bytes are already
    /// the decoded UTF-8 (no escapes); the emitter calls this method
    /// instead of copying those bytes into an arena frame. The record
    /// stores no arena pointer; the reader recovers content via
    /// [`Tape::payload_string_with_source`], which slices
    /// `source[span_lo + 1 .. span_hi - 1]`.
    ///
    /// `meta_idx` range is 0-31 (5-bit packed field).
    #[inline(always)]
    pub fn push_leaf_borrowed_string(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_borrowed_string on compound kind {:?}",
            kind
        );
        debug_assert!(
            span_hi >= span_lo + 2,
            "borrowed string span too short to carry quotes: [{}, {})",
            span_lo,
            span_hi,
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            TapeRec::STRING_BORROW_BIT | extra_meta_bit,
            span_lo,
            span_hi,
            TapeOffset::NONE,
        );
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    /// AY.W4.2 — Eisel-Lemire direct-column f64 leaf push.
    ///
    /// Bypasses the [`PayloadData::WideScalar`] → `pay_wide` round-trip:
    /// the supplied `f64_bits` are written straight into the dedicated
    /// [`Columns::pay_f64`](crate::columns::Columns::pay_f64) column,
    /// the record's `child_off` carries the column rank, and
    /// [`TapeRec::PAYLOAD_F64_DIRECT_BIT`] is stamped on `extra` so
    /// readers ([`crate::Tape::payload_f64`]) project the value via
    /// `f64::from_bits(cols.pay_f64_at(rank))` directly.
    ///
    /// This saves one load + one store per number literal vs the
    /// generic [`Self::push_leaf_with`] route — significant on heavy-
    /// numeric fixtures (canada). Value-side: stamps a `wide`
    /// [`PayloadTag`] onto the paired value frame so projection reads
    /// the decoded `f64` directly off the value substrate.
    ///
    /// `meta_idx` is fixed at `0` for number leaves; the number-shape
    /// emitter uses `variant_idx` for the rule discriminant.
    #[inline(always)]
    pub fn push_leaf_with_f64_direct(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        f64_bits: u64,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_f64_direct on compound kind {:?}",
            kind
        );
        let rank = self.columns.pay_f64.len() as u32;
        self.columns.pay_f64.push(f64_bits);
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, 0);
        let extra = extra_meta_bit | TapeRec::PAYLOAD_F64_DIRECT_BIT;
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra,
            span_lo,
            span_hi,
            TapeOffset(rank),
        );
        // Value-side: project the `f64` bits into the wide payload
        // column and stamp the matching leaf frame.
        let v_rank = self.value_payloads_wide.len() as u32;
        self.value_payloads_wide.push(f64_bits);
        self.push_value_leaf(
            kind,
            span_lo,
            span_hi,
            variant_idx,
            PayloadTag::wide(v_rank),
        );
        TapeOffset(idx)
    }

    /// Write the 4-byte length prefix at the `pay_agg` slot reserved
    /// by the decode kernel.
    ///
    /// The kernel writes decoded bytes into the arena after reserving
    /// a 4-byte slot at `arena_offset` (via `arena_mut()` +
    /// `extend_from_slice(&[0u8; 4])`). Once the bytes have been
    /// decoded, the kernel calls this helper to stamp the actual
    /// length into the reserved slot. The slot must exist.
    #[inline(always)]
    pub fn stamp_arena_len_prefix(&mut self, arena_offset: u32, len: u32) {
        let start = arena_offset as usize;
        debug_assert!(
            start + 4 <= self.columns.pay_agg.len(),
            "stamp_arena_len_prefix: offset {} + 4 exceeds arena len {}",
            start,
            self.columns.pay_agg.len()
        );
        self.columns.pay_agg[start..start + 4].copy_from_slice(&len.to_le_bytes());
    }

    /// Mark the parse as failed with an offset and optional rule
    /// label. The builder continues to accept pushes (so recovery
    /// paths can produce partial tapes) but [`Self::finish`] returns
    /// the error.
    pub fn set_error(&mut self, offset: u32, rule_label: u32) {
        if self.error.is_none() {
            self.error = Some(TapeBuildError::ParseFailed {
                offset,
                rule_label,
            });
        }
    }

    /// Consume the builder and return the finalised tape + the
    /// fused value substrate as a [`FusedOutput<R>`].
    ///
    /// [`crate::finaliser::finalise`] always runs — it is the sole
    /// writer for `sib_skip` (and the compound-closure columns
    /// `child_off` / `span_hi`). [`Self::frame_depth`] feeds it
    /// directly: B3.W0.γ retired the
    /// `has_inline_frame_depth` gate and the
    /// `crate::finaliser::derive_frame_depth` reverse-walk
    /// reconstruction in favour of in-builder bookkeeping that is
    /// total across both pre-order and post-order shape emission.
    ///
    /// `root_off` marks the root of the parsed tree in both tape
    /// offset space and value-frame offset space. For the fused
    /// default parse the two are aligned on 0 (the first
    /// `begin_compound` call). The returned [`FusedOutput<R>`] holds
    /// the finalised [`Tape`] + the grammar-agnostic
    /// [`ValueFramesOutput<R>`] projection consumers read.
    #[inline(always)]
    pub fn finish_fused<R>(
        mut self,
        root_off: u32,
    ) -> Result<FusedOutput<R>, TapeBuildError> {
        if let Some(err) = self.error {
            return Err(err);
        }
        debug_assert!(
            self.value_open_stack.is_empty(),
            "FusedBuilder::finish called with {} open value frames remaining",
            self.value_open_stack.len(),
        );
        self.run_finaliser();
        let tape = Tape {
            columns: self.columns,
        };
        let value = ValueFramesOutput {
            frames: self.value_frames,
            payloads_narrow: self.value_payloads_narrow,
            payloads_wide: self.value_payloads_wide,
            root_offset: root_off,
            _root_marker: core::marker::PhantomData,
        };
        Ok(FusedOutput::new(tape, value))
    }

    /// Consume the builder and return only the finalised tape.
    ///
    /// Back-compat surface for consumers that never exercised the
    /// value substrate (tape-only tests, pre-fused visitor
    /// fixtures, un-regenned `generated.rs` whose parse entry
    /// assembles a separate `ValueBuilder` output). Post-regen the
    /// production parse entry uses [`Self::finish_fused`] with a
    /// grammar marker; this variant discards the value arena
    /// without consuming it.
    #[inline(always)]
    pub fn finish(mut self) -> Result<Tape, TapeBuildError> {
        if let Some(err) = self.error {
            return Err(err);
        }
        self.run_finaliser();
        Ok(Tape {
            columns: self.columns,
        })
    }

    /// Alias retained for the W0'.a→W0'.c migration window —
    /// tape-only callers migrate to [`Self::finish`] directly once
    /// every call site has converged on the new name.
    #[inline]
    pub fn finish_tape_only(self) -> Result<Tape, TapeBuildError> {
        self.finish()
    }

    /// Shared finaliser step — runs the sibling-skip stamping pass
    /// over the per-record `frame_depth` column. Authored B3.W0.γ;
    /// B3.W0.δ moved storage into `Columns` so the column rolls back
    /// in lockstep with `records` / `sib_skip` on retry.
    #[inline(always)]
    fn run_finaliser(&mut self) {
        debug_assert_eq!(
            self.columns.frame_depth.len(),
            self.columns.len(),
            "frame_depth length {} != records length {} \
             (every structural push must stamp frame_depth in lockstep)",
            self.columns.frame_depth.len(),
            self.columns.len(),
        );
        self.columns.run_finalise();
    }

    /// Access the in-progress columns for debug / intermediate
    /// inspection. Primarily a test hook — production parsers use
    /// `finish()`.
    pub fn columns(&self) -> &Columns {
        &self.columns
    }

    /// Mutable handle on the in-progress columns.
    ///
    /// The DTA driver (post-AW-III.W4: per-grammar emitted walker)
    /// writes directly into the builder's column substrate instead
    /// of threading through [`Self::push_leaf`] /
    /// [`Self::begin_compound`]. The generated `parse()` entry point
    /// constructs a `FusedBuilder`, then hands the mutable column
    /// reference returned here to the driver. B3.W0.γ —
    /// `frame_depth` is owned by the builder and stamped on every
    /// structural push automatically; direct-column writes bypass
    /// that bookkeeping and must therefore route via the public
    /// [`Self::push_leaf`] / [`Self::begin_compound`] /
    /// [`Self::end_compound`] / [`Self::end_compound_post_order`]
    /// surface in production paths.
    ///
    /// Direct-column writes bypass the value substrate; callers that
    /// need value-arena parity with direct column writes must stamp
    /// the value frame explicitly (there are no such production
    /// callers post-W0'.a).
    #[inline]
    pub fn columns_mut(&mut self) -> &mut Columns {
        &mut self.columns
    }

    /// Mutable handle on the in-progress per-record frame_depth
    /// stream. B3.W0.δ moved storage into `Columns`, so this defers
    /// to `columns.frame_depth_mut()`.
    #[inline]
    pub fn frame_depth_mut(&mut self) -> &mut Vec<u8> {
        &mut self.columns.frame_depth
    }

    /// AW-III.W4.d — split-borrow accessor for the parallel-column
    /// pair the per-grammar specialised walker writes into.
    ///
    /// Returns a tuple of `(&mut Columns, &mut Vec<u8>)` borrowing
    /// the `Columns` rest-fields and the `frame_depth` column
    /// disjointly so the emitted `dta_run_<grammar>` can pass them as
    /// adjacent arguments without the caller dancing around the
    /// borrow checker. B3.W0.δ moved `frame_depth` ownership into
    /// `Columns`, so the disjoint borrow runs through
    /// [`Columns::split_off_frame_depth_mut`].
    #[inline]
    pub fn columns_and_frame_depth_mut(
        &mut self,
    ) -> (&mut Columns, &mut Vec<u8>) {
        self.columns.split_off_frame_depth_mut()
    }

    /// Access the in-progress tape view for debug inspection.
    ///
    /// Returns a snapshot `Tape` built from a clone of the current
    /// columns; useful in tests that want to inspect mid-build
    /// state. Sibling-skip is NOT computed on this snapshot — the
    /// authoritative path is [`Self::finish`].
    pub fn tape_snapshot(&self) -> Tape {
        let mut columns = Columns::new();
        *columns.records_mut() = self.columns.records().to_vec();
        columns.sib_skip = self.columns.sib_skip.clone();
        columns.pay_narrow = self.columns.pay_narrow.clone();
        columns.pay_wide = self.columns.pay_wide.clone();
        columns.pay_f64 = self.columns.pay_f64.clone();
        columns.pay_agg = self.columns.pay_agg.clone();
        Tape { columns }
    }

    // ── Value-side internals ──────────────────────────────────────
    //
    // Private helpers the public API calls to keep the value arena
    // in lockstep with the tape. They are `#[inline(always)]` so
    // LLVM folds the value stamp into the tape push at the call
    // site — zero extra function-call boundaries in the hot path.

    /// Open a value-arena frame in lockstep with the tape's
    /// `begin_compound`. Pushes a compound frame + checkpoint onto
    /// the open-stack and bumps the parent checkpoint's
    /// `direct_child_count` (this nested compound is a direct child
    /// of whatever was on top at entry).
    #[inline(always)]
    fn value_begin_compound(&mut self, kind: TapeKind, span_lo: u32, variant_idx: u8) {
        // Nested-compound push: bump the PARENT checkpoint's direct-
        // child counter BEFORE pushing this compound's own checkpoint.
        // After the push, this compound becomes top-of-stack; its
        // counter starts at 0 and is incremented by its own children.
        if let Some(parent) = self.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        let frame_offset = self.value_frames.len() as u32;
        self.value_frames.push(ValueFrame {
            span_lo,
            span_hi: span_lo,
            first_child: frame_offset + 1,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag: PayloadTag::NONE,
        });
        self.value_open_stack.push(ValueCheckpoint {
            frame_offset,
            narrow_rank: self.value_payloads_narrow.len() as u32,
            wide_rank: self.value_payloads_wide.len() as u32,
            direct_child_count: 0,
        });
    }

    /// Close the most recently opened value frame — patches `span_hi`
    /// and reads `direct_child_count` straight off the popped
    /// checkpoint. O(1) per call. See
    /// `docs/tranches/AY-II/audit/W0p-regen-root-cause.md` for the
    /// O(N^2) walk this replaces.
    #[inline(always)]
    fn value_end_compound(&mut self, span_hi: u32) {
        let checkpoint = self
            .value_open_stack
            .pop()
            .expect("FusedBuilder::value_end_compound called with empty open_stack");
        let frame = &mut self.value_frames[checkpoint.frame_offset as usize];
        frame.span_hi = span_hi;
        frame.child_count = checkpoint.direct_child_count;
    }

    /// Append a leaf value frame carrying a source span + payload
    /// tag. The tape-side leaf push is the caller's responsibility;
    /// this only appends the paired value frame. If an open compound
    /// is on the stack, bumps its `direct_child_count` — a leaf push
    /// is always a direct child of the enclosing compound.
    #[inline(always)]
    fn push_value_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        payload_tag: PayloadTag,
    ) {
        if let Some(parent) = self.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        self.value_frames.push(ValueFrame {
            span_lo,
            span_hi,
            first_child: 0,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag,
        });
    }
}

/// Pre-W0'.a compose-boundary alias. Un-regenned `generated.rs` still
/// spells the builder type as `TapeBuilder`; this alias keeps the
/// library compiling through the bootstrap escape window. The alias
/// retires once the orchestrator regens `generated.rs` at W0' close.
pub type TapeBuilder = FusedBuilder;
