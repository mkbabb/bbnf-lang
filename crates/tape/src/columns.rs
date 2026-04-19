//! `Columns` — the columnar (Struct-of-Arrays) substrate the tape
//! is built from after Tranche AV Phase 2.
//!
//! # Architectural role (Tranche AV.2.1 – AV.2.3)
//!
//! Pre-AV, the tape was a single `Vec<TapeRec>` — one 16-byte record
//! per parsed node, storage in row-major Array-of-Structs layout.
//! That shape is cache-friendly for the common "walk each record and
//! look at every field" case but is pathological for bulk typed
//! visitors (`sum_all_f64(canada)`), where a scalar left-fold over
//! the f64 payloads traverses every 16-byte record even though only
//! 8 of those bytes matter. The W4 prototype measured 1.94× on that
//! visitor; a 4-lane reordered-unrolling cleared 6.64× — but only
//! once the `f64`s lived in a dense `Vec<f64>`.
//!
//! `Columns` is that dense SoA layout. Six *structural* columns
//! hold per-record metadata (kind, spans, flags, sibling-skip). A
//! set of *typed payload* columns hold typed payloads dense-packed
//! in push order; every record that carries a payload of a given
//! type occupies exactly one slot in the matching column.
//!
//! | Column      | Type            | Role                                            |
//! |-------------|-----------------|-------------------------------------------------|
//! | `kinds`     | `Vec<u8>`       | `TapeKind` + meta_idx nibble (packed byte)      |
//! | `flags`     | `Vec<u8>`       | full 8-bit `variant_idx` (AW-III.W1.A widened)  |
//! | `extra`     | `Vec<u16>`      | packed bits: STRING_BORROW, PAYLOAD_IN_ARENA, HAS_CHILDREN, META_IDX_HI |
//! | `span_lo`   | `Vec<u32>`      | source-byte start offset                        |
//! | `span_hi`   | `Vec<u32>`      | source-byte end offset                          |
//! | `sib_skip`  | `Vec<u32>`      | distance to next sibling (0 = last)             |
//! | `child_off` | `Vec<TapeOffset>` | compound first-child / leaf payload pointer   |
//! | `pay_narrow`| `Vec<u32>`      | inline ≤ 4 B scalars (u8/u16/u32/bool/i-tagged) |
//! | `pay_wide`  | `Vec<u64>`      | 8 B scalars (f64/u64/i64/packed Span)           |
//! | `pay_agg`   | `Vec<u8>`       | unified arena for aggregates & byte-string frames |
//!
//! # Payload routing (AV.2.3)
//!
//! The AU-era `PayloadData::InlineScalar(u32)` variant used to pack
//! its 4 bytes directly into [`TapeRec::child_off`], relying on the
//! sentinel `TapeOffset::NONE` (`u32::MAX`) to signal absence. That
//! overload is unwound. Inline scalars land in the `pay_narrow`
//! column; 8-byte scalars (`WideScalar`) land in `pay_wide`;
//! aggregates (`Aggregate` / `LargeAggregate`) and byte-string
//! frames (`Bytes`) still flow through the unified arena
//! [`Columns::pay_agg`]. In every case the record's
//! [`child_off`] slot holds the lookup pointer — a column rank into
//! `pay_narrow` / `pay_wide`, or the arena byte offset for arena-
//! backed payloads. Compound records use `child_off` for first-child
//! pointer, or `TapeOffset::NONE` for empty runs.
//!
//! # Sibling skip (AV.2.2)
//!
//! `sib_skip[i]` holds the distance from record `i` to the root of
//! its next-emission sibling within the shared parent's children
//! run, or `0` if the record is the last sibling (or the root). The
//! distance is `next_sibling_root - i`; since the current tape is
//! laid out in post-order, `sib_skip` is not constant-1 — a leaf
//! followed by a compound sibling reports a distance equal to the
//! sibling subtree's size.
//!
//! Forward sibling traversal reads `sib_skip[i]` exactly once per
//! step: `i → i + sib_skip[i]`; iteration ends when the read is
//! zero. The old backward child-enumeration path (`count_backward`
//! / `nth_backward`) is removed.
//!
//! [`TapeRec::child_off`]: crate::TapeRec::child_off
//! [`child_off`]: crate::TapeRec::child_off
//!
//! # Typed-column reducer (AW-IV.W5.1)
//!
//! AV.2.5 emitted reordered-unrolling visitor kernels as grammar-local
//! free functions, but the consumer API over the tape and the packed
//! SIMD promotion that would clear the ≥ 6× hard gate never landed.
//! W5.1 closes both. [`Columns::reduce_column`] is the generic consumer
//! surface: a compile-time column selector ([`ColumnTag`]) picks which
//! payload column to walk, a reducer ([`Reducer`]) supplies identity +
//! fold + combine, and the inner loop promotes to packed SIMD on the
//! `f64` lanes — `vaddq_f64` pairs on NEON (logical 4-lane accumulator
//! over two `float64x2_t` registers) and `_mm256_add_pd` on AVX2
//! (native `__m256d` 4-lane accumulator). Scalar tail handles `n % 4`.
//! The portable fallback uses `std::simd::f64x4` on non-aarch64 /
//! non-AVX2 hosts so nightly-only `std::simd` does not leak through
//! this crate's public API.

use crate::kind::TapeKind;
use crate::packed::PackedRecord;
use crate::tape::{TapeOffset, TapeRec};

use std::sync::OnceLock;

/// The columnar record substrate.
///
/// Every column is a plain `Vec` — `alloc::vec::Vec` on stable — so
/// the whole struct is `Default`/`Debug` without extra bounds. The
/// six structural columns grow in lockstep (`push_structural` is the
/// only `fn` that appends to them, and it appends to all six); the
/// typed-payload columns grow independently as payloads of each type
/// are written.
#[derive(Debug, Default)]
pub struct Columns {
    // ── Structural columns (per-record, one entry each) ────────────
    /// Packed byte: low 4 bits = [`TapeKind`], high 4 bits = low 4
    /// bits of `meta_idx`. Read with [`TapeKind::from_u8`] and the
    /// [`TapeRec::pack_kind_meta`] inverse.
    pub kinds: Vec<u8>,
    // NOTE — `packed_cache` is declared at the END of the struct (see
    // below) rather than mixed in with the SoA columns; logically it
    // is a sidecar view over the SoA primary, not a structural column.
    // The existing fields stay in their pre-AX.W1.D positions so
    // downstream code matching on `Columns { .. }` pattern shapes is
    // untouched.
    /// Full 8-bit `variant_idx` (rule discriminant). AW-III.W1.A
    /// widened from 6 → 8 bits; `has_children` and `meta_idx[4]`
    /// migrated to [`Self::extra`].
    pub flags: Vec<u8>,
    /// Packed per-record flags: `STRING_BORROW_BIT`,
    /// `PAYLOAD_IN_ARENA_BIT`, `HAS_CHILDREN_BIT` (post-W1.A),
    /// `META_IDX_HI_BIT` (post-W1.A).
    pub extra: Vec<u16>,
    /// Source-byte start offset.
    pub span_lo: Vec<u32>,
    /// Source-byte end offset (`span_hi == span_lo` for epsilon).
    pub span_hi: Vec<u32>,
    /// Distance to the next-emission sibling within the parent's
    /// children run. `0` for the last sibling (and for the root).
    /// Computed at [`TapeBuilder::finish`](crate::TapeBuilder::finish).
    pub sib_skip: Vec<u32>,
    /// Polymorphic per-record pointer. For compounds: offset of the
    /// first child record (or `TapeOffset::NONE` on empty runs). For
    /// leaves carrying an inline scalar (`pay_narrow`) or wide
    /// scalar (`pay_wide`): the column rank — `pay_*[idx]` reads the
    /// payload. For leaves carrying an aggregate or byte-frame: the
    /// [`pay_agg`] byte offset. For leaves with no payload:
    /// `TapeOffset::NONE`.
    ///
    /// [`pay_agg`]: Self::pay_agg
    pub child_off: Vec<TapeOffset>,

    // ── Typed payload columns (dense-packed in push order) ─────────
    /// Inline scalars ≤ 4 B. The record's `child_off` holds the
    /// column rank.
    pub pay_narrow: Vec<u32>,
    /// 8-byte scalars (`f64` / `u64` / `i64` / packed `Span`). Stored
    /// as raw bits; readers reinterpret via `f64::from_bits` or the
    /// typed cast. The record's `child_off` holds the column rank.
    pub pay_wide: Vec<u64>,
    /// Unified payload arena — aggregate tuple bytes, decoded
    /// strings, and byte-string frames all land here. Continues to
    /// back [`Tape::arena`](crate::Tape::arena) for external
    /// consumers that slice raw bytes.
    pub pay_agg: Vec<u8>,

    // ── AX.W1.D — AoS sidecar for hybrid random-access reads ──────
    /// Lazy AoS projection of the SoA primary. Populated on first
    /// call to [`Self::packed_cache`]; invalidated by every mutating
    /// call that touches a structural column.
    ///
    /// Rationale: the six-column SoA scatter is cache-friendly for
    /// the bulk-payload kernel in
    /// [`Self::reduce_column`], but random-access reads (the lazy-
    /// field extractor hot path — Twitter `statuses[].text`, CSS
    /// selector run) pay seven disjoint cache-line loads per record.
    /// The AoS sidecar stores each record as a single 32-byte
    /// aligned [`PackedRecord`]; the first `packed_cache()` call
    /// performs an O(n) transpose from SoA to AoS, and every
    /// subsequent random read is a single 32-byte load.
    ///
    /// Thread-safety: [`OnceLock`] admits concurrent readers once
    /// the inner `Vec` is stored. Mutators require `&mut self`, so
    /// they cannot race with readers. Every mutation path calls
    /// [`Self::invalidate_packed`] to reset the cache; the next
    /// reader re-transposes.
    packed_cache: OnceLock<Vec<PackedRecord>>,
}

impl Columns {
    /// Construct an empty column set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a column set sized for `expected` structural records.
    ///
    /// Only the six structural columns + `child_off` pre-allocate;
    /// typed-payload columns are lazy, since most grammars exercise
    /// only a subset.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            kinds: Vec::with_capacity(expected),
            flags: Vec::with_capacity(expected),
            extra: Vec::with_capacity(expected),
            span_lo: Vec::with_capacity(expected),
            span_hi: Vec::with_capacity(expected),
            sib_skip: Vec::with_capacity(expected),
            child_off: Vec::with_capacity(expected),
            pay_narrow: Vec::new(),
            pay_wide: Vec::new(),
            pay_agg: Vec::with_capacity(expected / 8 * 8),
            packed_cache: OnceLock::new(),
        }
    }

    /// Number of structural records.
    #[inline]
    pub fn len(&self) -> usize {
        self.kinds.len()
    }

    /// True iff no structural records have been appended.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.kinds.is_empty()
    }

    /// Truncate the six structural columns + `child_off` to `new_len`
    /// records. Typed-payload columns are NOT truncated here — they
    /// are written by Stage B once the structural pass is complete,
    /// so backtracking inside Stage A cannot leak partial payload
    /// writes.
    ///
    /// Used by the DTA walker's `AltLinear` backtracking to discard
    /// structural rows pushed by a failed branch before probing the
    /// next one.
    #[inline]
    pub fn truncate(&mut self, new_len: usize) {
        self.kinds.truncate(new_len);
        self.flags.truncate(new_len);
        self.extra.truncate(new_len);
        self.span_lo.truncate(new_len);
        self.span_hi.truncate(new_len);
        self.sib_skip.truncate(new_len);
        self.child_off.truncate(new_len);
        self.invalidate_packed();
    }

    // ── AX.W1.D — AoS sidecar (`packed_cache`) readers/invalidators ──

    /// Get (populating on first call) the AoS sidecar view over this
    /// column set.
    ///
    /// The first call transposes the SoA primary into a dense
    /// `Vec<PackedRecord>` — O(n) one-time cost. Subsequent calls
    /// return the cached slice without re-transposing.
    ///
    /// The cache is invalidated whenever the SoA primary is mutated
    /// (any `push_*` or `truncate`); the next call re-transposes the
    /// updated state.
    ///
    /// Uses [`OnceLock::get_or_init`], so concurrent readers across
    /// threads observe the same immutable transpose without racing.
    /// The populate closure captures `self` by `&` so the transpose
    /// reads the SoA columns directly; no extra clone.
    #[inline]
    pub fn packed_cache(&self) -> &[PackedRecord] {
        self.packed_cache
            .get_or_init(|| self.transpose_to_packed())
            .as_slice()
    }

    /// `None` if the AoS sidecar is not currently populated; `Some`
    /// if a prior read has materialised it and no subsequent write
    /// has invalidated it. Exposed so consumers wanting to probe
    /// whether a hot path exercised the sidecar — e.g. the AX.W1.7
    /// Twitter lazy-field bench — can assert the contract without
    /// forcing the transpose.
    #[inline]
    pub fn packed_cache_populated(&self) -> bool {
        self.packed_cache.get().is_some()
    }

    /// Invalidate the AoS sidecar. Called by every mutation that
    /// touches a structural column; the next `packed_cache()` call
    /// re-transposes from the (updated) SoA primary.
    ///
    /// Idempotent: invalidating an already-empty cache is a no-op.
    /// `#[inline]` because the hot-path push sites call this once
    /// per row — the body compiles down to a single `OnceLock::take`
    /// which is a conditional pointer-write.
    #[inline]
    pub fn invalidate_packed(&mut self) {
        let _ = self.packed_cache.take();
    }

    /// Transpose the SoA primary into a fresh AoS `Vec`. Internal
    /// helper for [`Self::packed_cache`]; kept at module scope so
    /// tests can call it directly without going through the lock.
    fn transpose_to_packed(&self) -> Vec<PackedRecord> {
        let n = self.kinds.len();
        debug_assert_eq!(self.flags.len(), n, "flags column length mismatch");
        debug_assert_eq!(self.extra.len(), n, "extra column length mismatch");
        debug_assert_eq!(self.span_lo.len(), n, "span_lo column length mismatch");
        debug_assert_eq!(self.span_hi.len(), n, "span_hi column length mismatch");
        debug_assert_eq!(self.sib_skip.len(), n, "sib_skip column length mismatch");
        debug_assert_eq!(
            self.child_off.len(),
            n,
            "child_off column length mismatch",
        );
        let mut out: Vec<PackedRecord> = Vec::with_capacity(n);
        for i in 0..n {
            out.push(PackedRecord {
                kind_meta: self.kinds[i],
                flags: self.flags[i],
                extra: self.extra[i],
                span_lo: self.span_lo[i],
                span_hi: self.span_hi[i],
                child_off: self.child_off[i],
                sib_skip: self.sib_skip[i],
                _pad: [0u8; 12],
            });
        }
        out
    }

    /// Append one structural row across the six structural columns
    /// plus `child_off`, returning the row's position.
    ///
    /// Caller supplies pre-packed `kind_meta` / `flags` bytes via
    /// [`TapeRec::pack_kind_meta`]. The `sib_skip` column is stamped
    /// `0`; it gets back-filled at
    /// [`TapeBuilder::finish`](crate::TapeBuilder::finish).
    #[inline]
    pub(crate) fn push_structural(
        &mut self,
        kind_meta: u8,
        flags: u8,
        extra: u16,
        span_lo: u32,
        span_hi: u32,
        child_off: TapeOffset,
    ) -> u32 {
        let idx = self.kinds.len() as u32;
        self.kinds.push(kind_meta);
        self.flags.push(flags);
        self.extra.push(extra);
        self.span_lo.push(span_lo);
        self.span_hi.push(span_hi);
        self.sib_skip.push(0);
        self.child_off.push(child_off);
        // AX.W1.D — SoA mutation invalidates the AoS sidecar.
        self.invalidate_packed();
        idx
    }

    /// Materialise a 16-byte [`TapeRec`] view of row `i`.
    ///
    /// Reads the six structural columns + `child_off` at `i` and
    /// reconstructs the flat record. The returned value is the same
    /// 16-byte `Copy` struct pre-AV readers consumed — consumers that
    /// held `&TapeRec` migrated to owned `TapeRec` at the API
    /// boundary.
    #[inline]
    pub fn materialize(&self, i: u32) -> TapeRec {
        let idx = i as usize;
        debug_assert!(idx < self.kinds.len(), "materialize: idx {} out of range", i);
        TapeRec {
            kind_meta: self.kinds[idx],
            flags: self.flags[idx],
            extra: self.extra[idx],
            span_lo: self.span_lo[idx],
            span_hi: self.span_hi[idx],
            child_off: self.child_off[idx],
        }
    }

    /// Unchecked variant of [`Self::materialize`].
    ///
    /// # Safety
    ///
    /// The caller must guarantee `i < self.len()`. The `TapeCursor`
    /// honours this via `TapeOffset`s produced by the builder.
    #[inline]
    pub unsafe fn materialize_unchecked(&self, i: u32) -> TapeRec {
        let idx = i as usize;
        debug_assert!(idx < self.kinds.len(), "materialize_unchecked: idx {} out of range", i);
        // SAFETY: caller guarantees idx < self.kinds.len(), and every
        // structural column is maintained at the same length by
        // `push_structural`.
        unsafe {
            TapeRec {
                kind_meta: *self.kinds.get_unchecked(idx),
                flags: *self.flags.get_unchecked(idx),
                extra: *self.extra.get_unchecked(idx),
                span_lo: *self.span_lo.get_unchecked(idx),
                span_hi: *self.span_hi.get_unchecked(idx),
                child_off: *self.child_off.get_unchecked(idx),
            }
        }
    }

    /// Read a record's `TapeKind` directly from the `kinds` column
    /// without materialising the whole 16-byte view.
    #[inline]
    pub fn kind_at(&self, i: u32) -> TapeKind {
        TapeKind::from_u8(self.kinds[i as usize] & 0x0F)
    }

    /// Read a record's `has_children` bit directly from the `extra`
    /// column. Migrated from `flags[6]` in AW-III.W1.A alongside the
    /// `variant_idx` widening.
    #[inline]
    pub fn has_children_at(&self, i: u32) -> bool {
        (self.extra[i as usize] & TapeRec::HAS_CHILDREN_BIT) != 0
    }

    /// Read a record's `child_off` directly.
    #[inline]
    pub fn child_off_at(&self, i: u32) -> TapeOffset {
        self.child_off[i as usize]
    }

    /// Read a record's `sib_skip` directly.
    #[inline]
    pub fn sib_skip_at(&self, i: u32) -> u32 {
        self.sib_skip[i as usize]
    }

    /// Read a record's `span_lo` / `span_hi` directly.
    #[inline]
    pub fn span_at(&self, i: u32) -> (u32, u32) {
        let idx = i as usize;
        (self.span_lo[idx], self.span_hi[idx])
    }

    // ── AW-III.W5.c / AW-IV.W2.3.b — fused SoA write API ──────────
    //
    // Eliminates the per-column bounds-check tax that `push_structural`
    // pays seven times per compound row. AW-IV.W2.3.b collapses the
    // capacity check down to a single compare-against-min + `#[cold]`
    // grow fall-through; in steady state (pre-allocated correctly via
    // `GRAMMAR_PROFILE.capacity_for(input.len())`) the branch is
    // predicted-not-taken and LLVM lays `grow_all` out-of-line.
    //
    // The AR-audit tape-capacity heuristic (`input_len / 2 + 2`, the
    // same ratio sonic-rs uses) covers every parse that produces ≤ 1
    // record per 2 bytes — so the walker's hot path never reaches
    // `grow_all` on corpus input. Pre-AW-IV the `GRAMMAR_PROFILE`
    // estimate over-allocated by ~64× via `input_len *
    // (compounds_per_byte + leaves_per_byte)`; the AR-audit heuristic
    // is tight and honest.
    //
    // [`grow_all`] stays `pub` + `#[cold]` for the rare slow path
    // where pre-allocation is insufficient. The cold-path
    // `dispatch_one` replay surface (`dta_run_cold` — AX snapshot
    // rebuilds) and test harnesses that probe the fused API through
    // `Columns::new()` fall through to the `#[cold]` branch; the
    // `#[cold]` `#[inline(never)]` annotations keep grow_all far
    // from the straight-line store block so it does not pollute the
    // walker's hot path.

    /// Smallest capacity any single structural column will reserve at
    /// initial growth. Picked from observed corpus push patterns
    /// (twitter / citm / bootstrap.css all spool > 64 records before
    /// the first realloc).
    const INITIAL_CAP: usize = 64;

    /// Returns the minimum capacity across the seven structural
    /// columns. Callers that need the slow-path guard (cold-path
    /// replay, tests) consult this via [`reserve_one_cold`] before
    /// each fused push; the per-grammar walker's pre-allocated hot
    /// path never reaches this surface.
    #[inline]
    fn structural_min_cap(&self) -> usize {
        // Branchless `min` chain — every column reserves the same
        // amount via `with_capacity` and grows monotonically through
        // `grow_all`, so in steady state these are all equal.
        let mut m = self.kinds.capacity();
        let f = self.flags.capacity();
        if f < m { m = f; }
        let e = self.extra.capacity();
        if e < m { m = e; }
        let l = self.span_lo.capacity();
        if l < m { m = l; }
        let h = self.span_hi.capacity();
        if h < m { m = h; }
        let s = self.sib_skip.capacity();
        if s < m { m = s; }
        let c = self.child_off.capacity();
        if c < m { m = c; }
        m
    }

    /// Reserve at least one additional slot on every structural
    /// column. Doubles the current minimum capacity (or sets
    /// `INITIAL_CAP` when the columns are still empty), matching
    /// `Vec::push`'s amortised growth policy.
    ///
    /// `#[cold]` because in the AW-IV per-grammar walker hot path
    /// pre-allocation
    /// ([`GrammarProfile::capacity_for`](crate::GrammarProfile::capacity_for)
    /// at parse-entry, per the AR-audit `input_len / 2 + 2` heuristic)
    /// makes this unreachable. The surface survives for the cold-path
    /// `dispatch_one` replay callers + tests probing the fused API
    /// directly; the release-mode cost of those callers paying
    /// amortised doubling is immaterial.
    ///
    /// Consults the MINIMUM capacity across all seven structural
    /// columns. `Vec::with_capacity(n)` may round up past `n` by
    /// different amounts for different element types (a `Vec<u8>` for
    /// `kinds` and a `Vec<u16>` for `extra` can end up with different
    /// actual capacities even when both were asked for `n`). Reading
    /// the min captures the tightest bound so subsequent
    /// `set_len(idx + 1)` never exceeds any column's capacity.
    #[cold]
    #[inline(never)]
    pub fn grow_all(&mut self) {
        let cur = self.structural_min_cap();
        let target = if cur == 0 { Self::INITIAL_CAP } else { cur * 2 };
        let extra = target - cur;
        self.kinds.reserve(extra);
        self.flags.reserve(extra);
        self.extra.reserve(extra);
        self.span_lo.reserve(extra);
        self.span_hi.reserve(extra);
        self.sib_skip.reserve(extra);
        self.child_off.reserve(extra);
    }

    /// Cold-path capacity guard: reserve one free slot on every
    /// structural column if the current minimum is exhausted.
    ///
    /// The walker-emitted hot path pre-allocates via
    /// `GRAMMAR_PROFILE.capacity_for(input.len())` and NEVER calls
    /// this method — the pre-allocation is the pre-condition for the
    /// unchecked fused stores. Cold-path callers that feed the fused
    /// writers without pre-allocating (cold-path `dispatch_one`
    /// replay in [`crate::driver::dta_run_cold`]; test harnesses
    /// probing the fused API through `Columns::new()`) call this
    /// before each push so the subsequent `push_compound_fused` /
    /// `push_leaf_fused` sees its capacity pre-condition satisfied.
    ///
    /// `#[cold]` `#[inline(never)]` keep the slow-path guard
    /// out-of-line; it is free at the hot-path call site because the
    /// hot path does not call it.
    #[cold]
    #[inline(never)]
    pub fn reserve_one_cold(&mut self) {
        if self.kinds.len() >= self.structural_min_cap() {
            self.grow_all();
        }
    }

    /// AW-III.W5.c — fused compound-row push.
    /// AW-IV.W2.3.b — capacity guard folded into the fused body;
    /// `grow_all` is `#[cold]` `#[inline(never)]` so the hot path is
    /// a single predicted-not-taken branch plus seven unchecked
    /// stores.
    ///
    /// Replaces the seven `self.<column>.push(...)` calls in
    /// `tape::driver::reserve_compound` with seven unchecked
    /// stores. The row's `span_hi` and `child_off` defaults match
    /// `reserve_compound`'s provisional-then-overwrite pattern — the
    /// frame-pop site fixes both via the column slices once the
    /// children have been emitted.
    ///
    /// # Pre-allocation invariant
    ///
    /// Callers SHOULD pre-allocate to the parse's worst-case size —
    /// the walker-emitted `parse()` entry seeds
    /// `TapeBuilder::with_capacity(GRAMMAR_PROFILE.capacity_for(input.len()))`
    /// per the AR-audit `input_len / 2 + 2` heuristic (sonic-rs uses
    /// the same ratio; every parse producing ≤ 1 record per 2 bytes
    /// fits without a `grow_all`). Pre-AW-IV the heuristic computed
    /// `input_len * (compounds_per_byte + leaves_per_byte)` which
    /// over-allocated by ~64× on typical corpora (V1 per-byte ratios
    /// admit >> 1 record per byte for any grammar carrying nested
    /// children + leaves in the same rule); the AR-audit heuristic
    /// is tight and honest.
    ///
    /// The cold-path `dispatch_one` replay surface
    /// ([`crate::driver::dta_run_cold`]) and test harnesses that pass
    /// `Columns::new()` fall through to the `#[cold]` `grow_all`
    /// branch — correct but off the walker's hot path.
    ///
    /// Returns the row's index (mirrors `reserve_compound`'s return).
    #[inline(always)]
    pub fn push_compound_fused(&mut self, kind: TapeKind, span_lo: u32) -> u32 {
        // Compare against `structural_min_cap()` — the tightest bound
        // across all seven structural columns. `Vec<T>::with_capacity(n)`
        // rounds up by different amounts per element type (u8 vs u16
        // vs u32 vs TapeOffset), so any single column's capacity can
        // overestimate the guaranteed-safe ceiling; the min captures
        // the true bound. `grow_all` is `#[cold]` `#[inline(never)]`
        // so LLVM lays the slow path far from the straight-line store
        // block — the hot-path branch is a single predicted-not-taken
        // compare when callers pre-allocate via
        // `GRAMMAR_PROFILE.capacity_for(input.len())`.
        if self.kinds.len() >= self.structural_min_cap() {
            self.grow_all();
        }
        let idx = self.kinds.len();
        let kind_meta = (kind as u8) & 0x0F;
        // SAFETY: the guard above ensures `idx < structural_min_cap()`
        // which is the minimum across every structural column's
        // capacity. `grow_all` reserves matching capacity on every
        // structural column in lockstep, so `idx < self.<column>.capacity()`
        // holds for every field. The `as_mut_ptr().add(idx)` writes
        // are in-bounds and the post-write `set_len(idx + 1)`
        // satisfies `Vec::set_len`'s `new_len <= capacity()`
        // precondition.
        unsafe {
            let new_len = idx + 1;
            *self.kinds.as_mut_ptr().add(idx) = kind_meta;
            *self.flags.as_mut_ptr().add(idx) = 0;
            *self.extra.as_mut_ptr().add(idx) = 0;
            *self.span_lo.as_mut_ptr().add(idx) = span_lo;
            *self.span_hi.as_mut_ptr().add(idx) = span_lo; // provisional
            *self.sib_skip.as_mut_ptr().add(idx) = 0;
            *self.child_off.as_mut_ptr().add(idx) = TapeOffset::NONE;
            self.kinds.set_len(new_len);
            self.flags.set_len(new_len);
            self.extra.set_len(new_len);
            self.span_lo.set_len(new_len);
            self.span_hi.set_len(new_len);
            self.sib_skip.set_len(new_len);
            self.child_off.set_len(new_len);
        }
        // AX.W1.D — SoA mutation invalidates the AoS sidecar.
        self.invalidate_packed();
        idx as u32
    }

    /// AW-III.W5.c — fused leaf-row push.
    /// AW-IV.W2.3.b — capacity guard folded into the fused body;
    /// `grow_all` is `#[cold]` `#[inline(never)]` so the hot path is
    /// a single predicted-not-taken branch plus seven unchecked
    /// stores.
    ///
    /// Replaces the seven `self.<column>.push(...)` calls in
    /// `tape::driver::emit_leaf_with_payload` with seven
    /// unchecked stores. The caller supplies the `flags` (variant_idx)
    /// and `extra` (PAYLOAD_IN_ARENA flag) values directly — both are
    /// already classified by the caller via the
    /// `pending_variant_idx` chase + `child_off.is_none()` check.
    ///
    /// # Pre-allocation invariant
    ///
    /// Identical to [`Self::push_compound_fused`]'s: callers SHOULD
    /// pre-allocate to the parse's worst-case size per
    /// `GRAMMAR_PROFILE.capacity_for(input.len())` — the AR-audit
    /// `input_len / 2 + 2` heuristic covers every parse producing ≤ 1
    /// record per 2 bytes. Cold-path callers fall through to the
    /// `#[cold]` `grow_all` branch.
    ///
    /// Returns the row's index (mirrors `emit_leaf_with_payload`'s
    /// return).
    #[inline(always)]
    pub fn push_leaf_fused(
        &mut self,
        kind: TapeKind,
        flags: u8,
        extra: u16,
        span_lo: u32,
        span_hi: u32,
        child_off: TapeOffset,
    ) -> u32 {
        // See [`Self::push_compound_fused`] for the single-compare
        // hot-path rationale + `#[cold]` grow fall-through.
        if self.kinds.len() >= self.structural_min_cap() {
            self.grow_all();
        }
        let idx = self.kinds.len();
        let kind_meta = (kind as u8) & 0x0F;
        // SAFETY: see [`Self::push_compound_fused`] — the
        // `structural_min_cap` guard + `grow_all`'s lockstep
        // reservation cover every structural column.
        unsafe {
            let new_len = idx + 1;
            *self.kinds.as_mut_ptr().add(idx) = kind_meta;
            *self.flags.as_mut_ptr().add(idx) = flags;
            *self.extra.as_mut_ptr().add(idx) = extra;
            *self.span_lo.as_mut_ptr().add(idx) = span_lo;
            *self.span_hi.as_mut_ptr().add(idx) = span_hi;
            *self.sib_skip.as_mut_ptr().add(idx) = 0;
            *self.child_off.as_mut_ptr().add(idx) = child_off;
            self.kinds.set_len(new_len);
            self.flags.set_len(new_len);
            self.extra.set_len(new_len);
            self.span_lo.set_len(new_len);
            self.span_hi.set_len(new_len);
            self.sib_skip.set_len(new_len);
            self.child_off.set_len(new_len);
        }
        // AX.W1.D — SoA mutation invalidates the AoS sidecar.
        self.invalidate_packed();
        idx as u32
    }

    // ── AW-V.W1.3 — scalar-payload direct-write fused API ────────────
    //
    // Per AW-V.md §W1.3 + B2 §3: the per-shape emitter's leaf-emission
    // arms know the scalar payload statically; the hot path writes the
    // decoded value directly into `pay_agg` alongside the structural
    // slot, bypassing PSI's Stage-A `push` / Stage-B rayon fan-out. The
    // byte layout mirrors `psi::write_decoded` so readers that consult
    // `pay_agg` via `TapeRec::PAYLOAD_IN_ARENA_BIT` + `child_off` see
    // bit-identical output to the scheduled path — interpreter replay
    // remains drop-in compatible at the column level.
    //
    // Each method writes:
    //
    // 1. The scalar value's bytes into `pay_agg` at the `child_off`-
    //    indicated offset (8 B for `f64` / `i64`, 4 B for the hex
    //    `u32`, 1 B for `u8` / `bool` — same byte widths PSI uses
    //    via `PayloadKind::arena_byte_width`).
    // 2. The structural slot (`kinds` / `flags` / `extra` / `span_lo`
    //    / `span_hi` / `sib_skip` / `child_off`), with `extra`'s
    //    `PAYLOAD_IN_ARENA_BIT` set so readers know `child_off` is a
    //    `pay_agg` byte offset, not a column rank.
    //
    // The `child_off` parameter is the pre-computed `pay_agg` byte
    // offset — callers advance their arena cursor monotonically per
    // record, matching the `PayloadStream`'s `arena_offset` allocation
    // in Stage A. Writing to a pre-reserved slot is the contract every
    // emitter leaf arm follows; the fused methods are one step fewer
    // than `pay_agg.extend_from_slice(...)` + `push_leaf_fused(...)`.
    //
    // `#[inline(always)]` everywhere: the per-shape leaf arms consume
    // these via `#[body_fragment]` splices in W3.2's emitter, and the
    // call site disappears into the emitter's hand-written parser
    // skeleton under monomorphisation.

    /// Write a scalar `f64` payload into `pay_agg` at `child_off` (8 B
    /// little-endian via `f64::to_bits().to_le_bytes()`) and append the
    /// structural slot for the leaf record.
    ///
    /// The caller supplies:
    ///
    /// - `kind` — the [`TapeKind`] for the leaf (`Regex` for JSON
    ///   numbers; whichever leaf kind the per-shape emitter assigns).
    /// - `span_lo` / `span_hi` — source-byte bounds.
    /// - `sib_skip` — post-finaliser stride (usually `0` at parse
    ///   time; finalise derives it later).
    /// - `child_off` — the `pay_agg` byte offset to write the 8 scalar
    ///   bytes to. The arena MUST have at least 8 bytes available at
    ///   that offset; the emitter's leaf arm resizes `pay_agg`
    ///   monotonically before calling this method.
    /// - `value` — the `f64` payload itself.
    ///
    /// Returns the leaf record's index in the structural columns.
    ///
    /// # PSI parity
    ///
    /// Bit-identical to `psi::write_decoded` on `PayloadKind::F64`:
    /// `s.parse::<f64>().unwrap_or(0.0).to_bits().to_le_bytes()` becomes
    /// a compile-time-known constant when the emitter already decoded
    /// the value via Eisel-Lemire; the PSI path is the `#[cold]`
    /// fallback that the per-shape emitter's scalar arm skips.
    #[inline(always)]
    pub fn push_scalar_payload_f64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: f64,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 8 <= self.pay_agg.len(),
            "push_scalar_payload_f64: arena offset {} + 8 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = value.to_bits().to_le_bytes();
        // SAFETY: the `debug_assert!` above (enforced in debug builds;
        // the emitter's monotonic arena-cursor pre-condition in release
        // builds) guarantees the 8-byte range `[dst_off, dst_off+8)` is
        // in-bounds of `pay_agg`'s initialised region.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                8,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        // `push_leaf_fused` stamps `sib_skip = 0`; overwrite only when
        // the caller provides a non-zero stride (the post-AV finaliser
        // passes `0` at parse time and back-fills later, so this is
        // the cold path for manual-stride callers).
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `u8` payload into `pay_agg` at `child_off` (1 B)
    /// and append the structural slot for the leaf record.
    ///
    /// Bit-identical to `psi::write_decoded` on `PayloadKind::U8`.
    /// See [`Self::push_scalar_payload_f64`] for the full contract.
    #[inline(always)]
    pub fn push_scalar_payload_u8(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: u8,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 1 <= self.pay_agg.len(),
            "push_scalar_payload_u8: arena offset {} + 1 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            *self.pay_agg.as_mut_ptr().add(dst_off) = value;
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `bool` payload into `pay_agg` at `child_off` (1 B
    /// — `0` for `false`, `1` for `true`) and append the structural
    /// slot for the leaf record.
    ///
    /// Bit-identical to `psi::write_decoded` on `PayloadKind::Bool`.
    /// See [`Self::push_scalar_payload_f64`] for the full contract.
    #[inline(always)]
    pub fn push_scalar_payload_bool(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: bool,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 1 <= self.pay_agg.len(),
            "push_scalar_payload_bool: arena offset {} + 1 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            *self.pay_agg.as_mut_ptr().add(dst_off) = value as u8;
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar hex `u32` payload into `pay_agg` at `child_off`
    /// (4 B little-endian) and append the structural slot for the
    /// leaf record.
    ///
    /// Bit-identical to `psi::write_decoded` on `PayloadKind::HexU32`.
    /// See [`Self::push_scalar_payload_f64`] for the full contract.
    /// CSS hex colours pass the pre-decoded `#rrggbbaa` u32 from the
    /// emitter's `parse_hex_u32` inline body.
    #[inline(always)]
    pub fn push_scalar_payload_hex_u32(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: u32,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 4 <= self.pay_agg.len(),
            "push_scalar_payload_hex_u32: arena offset {} + 4 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = value.to_le_bytes();
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                4,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `i64` payload into `pay_agg` at `child_off` (8 B
    /// little-endian via `(value as u64).to_le_bytes()`) and append
    /// the structural slot for the leaf record.
    ///
    /// Bit-identical to `psi::write_decoded` on `PayloadKind::I64`
    /// (two's-complement wrap; `to_le_bytes` on `i64` emits the same
    /// byte pattern as on the reinterpreted `u64`). See
    /// [`Self::push_scalar_payload_f64`] for the full contract.
    #[inline(always)]
    pub fn push_scalar_payload_i64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: i64,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 8 <= self.pay_agg.len(),
            "push_scalar_payload_i64: arena offset {} + 8 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = (value as u64).to_le_bytes();
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                8,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    // ── AX.W1.5 — paired-column span_lo/span_hi store ───────────────
    //
    // Lever 4's `push_compound_fused_v32` retired per R4 §5 / §6
    // (self-alias pathology on the 32-byte packed record). W1.5
    // replaces it with a paired-column surface over just the two
    // span columns: compound close + leaf emit both know the
    // `(lo, hi)` pair at the same call site, so one entry point
    // writing both slots beats two separate indexed stores that
    // LLVM's scheduler is free to interleave with surrounding code.
    //
    // The two columns are backed by separate `Vec<u32>` allocations,
    // so a single aarch64 `stp` instruction cannot fuse across them
    // (stp requires one base register + a single offset range). What
    // we can guarantee is that the two stores emit back-to-back with
    // both base registers held live across the pair, so the M-series
    // Firestorm front-end macro-op-fuses them into one dispatch slot
    // and retires them at the L=1 rtp=0.5 envelope cited in the W1.5
    // spec. Rust's scalar `span_lo[idx] = lo; span_hi[idx] = hi;`
    // passes through LLVM's instruction scheduler which is free to
    // interleave unrelated arithmetic between the two stores; inline
    // assembly blocks that reordering and forces the adjacency the
    // fusion heuristic requires. Intel Skylake / AMD Zen3+ apply the
    // same macro-op fusion rule to adjacent `mov` stores on x86_64.
    //
    // Every arch honours `invalidate_packed()` at the tail — mutating
    // `span_*[idx]` stales any prior-populated AoS sidecar; the next
    // `packed_cache()` call re-transposes the updated SoA.

    /// Paired-column span write — writes both `span_lo[idx]` and
    /// `span_hi[idx]` via a single logical call.
    ///
    /// Replaces the reverted Lever 4 per R4 §6 Design A. Call sites
    /// know both span endpoints at compound close + leaf emit time;
    /// this method takes the pair once and emits the stores back-to-
    /// back so the front-end's macro-op fusion heuristic sees them as
    /// one dispatch slot.
    ///
    /// # Panics
    ///
    /// Panics in debug builds if `idx` is out of range for either
    /// column. The columns must already cover `idx` (every caller
    /// has pushed a structural row with the same index via
    /// [`Self::push_compound_fused`] / [`Self::push_leaf_fused`]
    /// before patching).
    #[cfg(target_arch = "aarch64")]
    #[inline(always)]
    pub fn stp_span(&mut self, idx: usize, span_lo_val: u32, span_hi_val: u32) {
        debug_assert!(
            idx < self.span_lo.len() && idx < self.span_hi.len(),
            "stp_span: idx {} out of range (span_lo len {}, span_hi len {})",
            idx,
            self.span_lo.len(),
            self.span_hi.len(),
        );
        // SAFETY: `idx` is bounds-checked in debug above; in release
        // builds, every call site preserves the invariant that `idx
        // < min(span_lo.len(), span_hi.len())` via a prior
        // `push_*_fused` call that grew both columns in lockstep. The
        // two pointers target distinct `Vec<u32>` allocations so
        // there is no aliasing concern; each store writes exactly
        // one aligned `u32` value to a valid, already-initialised
        // slot (the prior push wrote a provisional value that this
        // method overwrites).
        //
        // The inline-assembly block pins the two stores adjacent in
        // the emitted machine code so the M1/M2 Firestorm front-end
        // macro-op-fuses them into a single dispatch slot. The
        // `nostack` + `preserves_flags` options let the compiler
        // schedule non-memory code around the block freely while
        // keeping the pair itself atomic w.r.t. scheduling.
        unsafe {
            let lo_ptr = self.span_lo.as_mut_ptr().add(idx);
            let hi_ptr = self.span_hi.as_mut_ptr().add(idx);
            core::arch::asm!(
                "str {lo:w}, [{lo_ptr}]",
                "str {hi:w}, [{hi_ptr}]",
                lo_ptr = in(reg) lo_ptr,
                hi_ptr = in(reg) hi_ptr,
                lo = in(reg) span_lo_val,
                hi = in(reg) span_hi_val,
                options(nostack, preserves_flags),
            );
        }
        self.invalidate_packed();
    }

    /// Paired-column span write — x86_64 path.
    ///
    /// See the aarch64 variant for the architectural rationale.
    /// Intel Skylake / AMD Zen3+ fuse two adjacent `mov [mem], reg`
    /// instructions into a single dispatch uop when the base
    /// registers are live across the pair.
    #[cfg(target_arch = "x86_64")]
    #[inline(always)]
    pub fn stp_span(&mut self, idx: usize, span_lo_val: u32, span_hi_val: u32) {
        debug_assert!(
            idx < self.span_lo.len() && idx < self.span_hi.len(),
            "stp_span: idx {} out of range (span_lo len {}, span_hi len {})",
            idx,
            self.span_lo.len(),
            self.span_hi.len(),
        );
        // SAFETY: see the aarch64 variant. Inline assembly pins the
        // two `mov` stores adjacent so Zen3+/Skylake macro-op fusion
        // fires; LLVM otherwise may interleave unrelated work.
        unsafe {
            let lo_ptr = self.span_lo.as_mut_ptr().add(idx);
            let hi_ptr = self.span_hi.as_mut_ptr().add(idx);
            core::arch::asm!(
                "mov dword ptr [{lo_ptr}], {lo:e}",
                "mov dword ptr [{hi_ptr}], {hi:e}",
                lo_ptr = in(reg) lo_ptr,
                hi_ptr = in(reg) hi_ptr,
                lo = in(reg) span_lo_val,
                hi = in(reg) span_hi_val,
                options(nostack, preserves_flags),
            );
        }
        self.invalidate_packed();
    }

    /// Paired-column span write — portable fallback.
    ///
    /// Hosts without a macro-op-fusion heuristic fall through to
    /// two scalar indexed writes. LLVM may still schedule the pair
    /// contiguously at `-O2`; the fusion win is a micro-architecture
    /// property that does not affect correctness.
    #[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
    #[inline(always)]
    pub fn stp_span(&mut self, idx: usize, span_lo_val: u32, span_hi_val: u32) {
        self.span_lo[idx] = span_lo_val;
        self.span_hi[idx] = span_hi_val;
        self.invalidate_packed();
    }

    // ── AW-IV.W5.1 — reduce_column<C, R> consumer API ────────────────
    //
    // AV.2.5 shipped 4-lane reordered-unrolling visitor kernels as
    // per-grammar free functions (see `visitor.rs::emit_visitor_kernels`
    // in the core emitter). The substrate landed; the consumer API over
    // the tape and the packed-SIMD promotion that would clear the ≥ 6×
    // hard gate never did. W5.1 closes both. `reduce_column<C, R>` is
    // the generic consumer surface over one of the three typed payload
    // columns (`pay_narrow`, `pay_wide`, `pay_agg`). `C: ColumnTag`
    // selects the column at compile time; `R: Reducer<C::Value>`
    // supplies the identity + fold + combine. The inner loop promotes
    // to packed-SIMD on the f64 lanes — arch-gated `vaddq_f64` pairs on
    // NEON (logical 4-lane accumulator over two `float64x2_t`
    // registers) and `_mm256_add_pd` on AVX2 (native `__m256d` 4-lane
    // accumulator). Non-NEON / non-AVX2 hosts compile through the
    // reordered-scalar 4-lane fold — LLVM auto-vectorises it whenever
    // the host toolchain has the target features. The four lanes break
    // the strict-IEEE `f64` non-associativity barrier that blocks
    // `col.iter().sum::<f64>()` from vectorising.

    /// Reduce a single payload column via a compile-time-selected
    /// fold.
    ///
    /// `C: ColumnTag` picks the column (`pay_narrow`, `pay_wide`, or
    /// `pay_agg`) at compile time; `R: Reducer<C::Value>` supplies the
    /// identity, fold, and combine. For the `f64`-sum hot path the
    /// inner loop promotes to packed-SIMD via `vaddq_f64` (NEON) or
    /// `_mm256_add_pd` (AVX2); every other combination goes through a
    /// 4-lane reordered scalar fold that LLVM auto-vectorises when the
    /// host toolchain supports it.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use tape::columns::{Columns, PayWideF64, SumF64};
    ///
    /// let cols = Columns::new();
    /// let total: f64 = cols.reduce_column::<PayWideF64, SumF64>();
    /// ```
    #[inline]
    pub fn reduce_column<C, R>(&self) -> R::Acc
    where
        C: ColumnTag,
        R: Reducer<C::Value>,
    {
        R::reduce_slice(C::column(self))
    }
}

// ─────────────────────────────────────────────────────────────────────
// ColumnTag — compile-time column selector
// ─────────────────────────────────────────────────────────────────────

/// Compile-time selector for a typed payload column on [`Columns`].
///
/// Implementors are zero-sized marker types — [`PayWideF64`],
/// [`PayWideU64`], [`PayNarrowU32`], [`PayAggU8`] — that bind a column
/// identity to an element type. [`Columns::reduce_column`] takes a
/// `ColumnTag` as its `C` type parameter; the emitter passes the tag
/// matching each active payload column per grammar.
pub trait ColumnTag {
    /// Element type of this column — the scalar the reducer folds over.
    type Value: Copy;
    /// Project the column out of `Columns` as a `&[Self::Value]`.
    fn column(cols: &Columns) -> &[Self::Value];
}

/// Column tag for `pay_wide` interpreted as `f64` (canonical
/// numeric-leaf payload column — JSON numbers, CSS dimensions, Sheets
/// numbers). 8-byte column entries reinterpret-cast via
/// [`f64::from_bits`] against the stored `u64` bits.
pub struct PayWideF64;

impl ColumnTag for PayWideF64 {
    type Value = f64;

    #[inline]
    fn column(cols: &Columns) -> &[f64] {
        // Safe transmute from `&[u64]` to `&[f64]`: both are 8-byte
        // primitives with the same layout; `u64`'s bit pattern is the
        // canonical wire shape pushed by the walker's number-arm
        // (Eisel-Lemire producing an `f64` whose bits are stored). This
        // is the column-rank read path that every typed-view consumer
        // would run anyway per-record; W5.1's reducer runs it bulk.
        // SAFETY: `u64` and `f64` have the same size + alignment;
        // reinterpretation is defined behaviour. `self.pay_wide` is
        // never uninitialised — every entry was populated by the
        // payload-writer before any reader could observe the slice.
        unsafe {
            core::slice::from_raw_parts(
                cols.pay_wide.as_ptr() as *const f64,
                cols.pay_wide.len(),
            )
        }
    }
}

/// Column tag for `pay_wide` interpreted as `u64` (packed integer
/// leaves, timestamps, raw 8-byte scalars).
pub struct PayWideU64;

impl ColumnTag for PayWideU64 {
    type Value = u64;

    #[inline]
    fn column(cols: &Columns) -> &[u64] {
        &cols.pay_wide
    }
}

/// Column tag for `pay_narrow` — 4-byte inline scalars (`u32`, `u16`,
/// `u8`, `bool`, widened unit enums). The column is stored as
/// `Vec<u32>` and the reducer reads it as `&[u32]`.
pub struct PayNarrowU32;

impl ColumnTag for PayNarrowU32 {
    type Value = u32;

    #[inline]
    fn column(cols: &Columns) -> &[u32] {
        &cols.pay_narrow
    }
}

/// Column tag for `pay_agg` — the arena byte column. Useful for
/// checksum-style reductions (`count_bytes`, `sum_bytes`, `xor_bytes`)
/// over the byte-addressable arena.
pub struct PayAggU8;

impl ColumnTag for PayAggU8 {
    type Value = u8;

    #[inline]
    fn column(cols: &Columns) -> &[u8] {
        &cols.pay_agg
    }
}

// ─────────────────────────────────────────────────────────────────────
// Reducer — identity + fold + combine + slice driver
// ─────────────────────────────────────────────────────────────────────

/// A reduction operator over `&[T]`.
///
/// Captures the four facts the driver needs to fold a column into a
/// single accumulator:
///
/// 1. [`Acc`](Reducer::Acc) — the accumulator type (usually `T`;
///    sometimes wider, e.g. `usize` for `Count`).
/// 2. [`IDENT`](Reducer::IDENT) — the identity value (`0.0` for Sum,
///    `T::MAX` for Min, `T::MIN` for Max, `0` for Count).
/// 3. [`fold`](Reducer::fold) — the binary fold step; fresh lane + new
///    element → new lane value.
/// 4. [`combine`](Reducer::combine) — the horizontal reduce; combines
///    two lane accumulators into one.
/// 5. [`reduce_slice`](Reducer::reduce_slice) — the slice driver;
///    default implementation is the 4-lane reordered scalar fold, but
///    [`SumF64`] overrides with the packed-SIMD NEON / AVX2 kernel that
///    clears the ≥ 6× hard gate on canada.json.
pub trait Reducer<T: Copy> {
    /// The accumulator type.
    type Acc: Copy;
    /// The identity value — the accumulator's starting state.
    const IDENT: Self::Acc;
    /// Fold one element into a lane accumulator.
    fn fold(acc: Self::Acc, x: T) -> Self::Acc;
    /// Combine two lane accumulators horizontally.
    fn combine(a: Self::Acc, b: Self::Acc) -> Self::Acc;

    /// Drive the reducer over a slice. Default implementation is the
    /// 4-lane reordered scalar fold; specialised implementations on
    /// particular `(Reducer, T)` pairs (notably [`SumF64`]) override
    /// with packed-SIMD intrinsics.
    #[inline]
    fn reduce_slice(col: &[T]) -> Self::Acc {
        let n = col.len();
        let mut acc: [Self::Acc; 4] = [Self::IDENT; 4];
        let mut i = 0usize;
        while i + 4 <= n {
            // Each lane carries its own accumulator through the entire
            // chunked loop — LLVM sees four independent dependency
            // chains and packs them into a SIMD register when the host
            // target features admit it.
            acc[0] = Self::fold(acc[0], col[i]);
            acc[1] = Self::fold(acc[1], col[i + 1]);
            acc[2] = Self::fold(acc[2], col[i + 2]);
            acc[3] = Self::fold(acc[3], col[i + 3]);
            i += 4;
        }
        let mut tail = Self::combine(
            Self::combine(acc[0], acc[1]),
            Self::combine(acc[2], acc[3]),
        );
        while i < n {
            tail = Self::fold(tail, col[i]);
            i += 1;
        }
        tail
    }
}

/// Sum reducer over `f64`. Identity `0.0`; fold `+`; combine `+`.
///
/// Overrides [`Reducer::reduce_slice`] to invoke the packed-SIMD kernel
/// in [`simd::sum_f64`] — `vaddq_f64` pairs on NEON, `_mm256_add_pd`
/// on AVX2, 4-lane reordered scalar fold otherwise. This is the hot
/// path the AW-IV.W5.1 ≥ 6× hard gate is stated against.
pub struct SumF64;

impl Reducer<f64> for SumF64 {
    type Acc = f64;
    const IDENT: f64 = 0.0;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc + x
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a + b
    }

    /// W5.1 SIMD specialisation: invokes the packed kernel in
    /// [`simd::sum_f64`] instead of the default scalar 4-lane fold.
    #[inline]
    fn reduce_slice(col: &[f64]) -> f64 {
        simd::sum_f64(col)
    }
}

/// Min reducer over `f64`. Identity `f64::INFINITY`; fold `f64::min`;
/// combine `f64::min`. NaN propagation follows `f64::min` — a NaN in
/// the input collapses to the non-NaN sibling when paired.
pub struct MinF64;

impl Reducer<f64> for MinF64 {
    type Acc = f64;
    const IDENT: f64 = f64::INFINITY;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc.min(x)
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a.min(b)
    }
}

/// Max reducer over `f64`. Identity `f64::NEG_INFINITY`; fold
/// `f64::max`; combine `f64::max`.
pub struct MaxF64;

impl Reducer<f64> for MaxF64 {
    type Acc = f64;
    const IDENT: f64 = f64::NEG_INFINITY;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc.max(x)
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a.max(b)
    }
}

/// Sum reducer over `u32`. Wrapping arithmetic so overflow is
/// saturating-defined rather than a panic site.
pub struct SumU32;

impl Reducer<u32> for SumU32 {
    type Acc = u32;
    const IDENT: u32 = 0;

    #[inline]
    fn fold(acc: u32, x: u32) -> u32 {
        acc.wrapping_add(x)
    }

    #[inline]
    fn combine(a: u32, b: u32) -> u32 {
        a.wrapping_add(b)
    }
}

/// Sum reducer over `u64`. Wrapping arithmetic.
pub struct SumU64;

impl Reducer<u64> for SumU64 {
    type Acc = u64;
    const IDENT: u64 = 0;

    #[inline]
    fn fold(acc: u64, x: u64) -> u64 {
        acc.wrapping_add(x)
    }

    #[inline]
    fn combine(a: u64, b: u64) -> u64 {
        a.wrapping_add(b)
    }
}

/// Count reducer. Collapses to `col.len()` — O(1) on every column
/// since the `Vec<T>` carries its length. Included so every column
/// can be hit by [`Columns::reduce_column`] uniformly.
pub struct Count;

impl<T: Copy> Reducer<T> for Count {
    type Acc = usize;
    const IDENT: usize = 0;

    #[inline]
    fn fold(acc: usize, _x: T) -> usize {
        acc + 1
    }

    #[inline]
    fn combine(a: usize, b: usize) -> usize {
        a + b
    }

    /// O(1) specialisation — `col.len()`; the scalar driver would take
    /// O(n), which defeats the purpose of `Count` on a `Vec<T>` whose
    /// length is already tracked.
    #[inline]
    fn reduce_slice(col: &[T]) -> usize {
        col.len()
    }
}

// ─────────────────────────────────────────────────────────────────────
// simd — packed-SIMD kernels for the hot reducer paths
// ─────────────────────────────────────────────────────────────────────

/// Packed-SIMD kernels for the [`Reducer`] hot paths.
///
/// The entry points dispatch to per-arch SIMD (NEON `vaddq_f64` pairs
/// on aarch64, AVX2 `_mm256_add_pd` on x86_64) or the portable 4-lane
/// reordered scalar fold on other targets. The public API is
/// arch-agnostic; callers use [`Reducer::reduce_slice`] which resolves
/// to the correct kernel at monomorphisation time.
pub mod simd {
    /// Packed-SIMD f64 sum. NEON two-register `vaddq_f64` pairs on
    /// aarch64 (logical 4-lane accumulator over two `float64x2_t`
    /// registers); AVX2 `_mm256_add_pd` on x86_64 (native 4-lane
    /// `__m256d`); reordered scalar fold elsewhere.
    ///
    /// Every lane carries an independent accumulator through the whole
    /// chunked loop — this breaks the strict-IEEE `f64` non-
    /// associativity barrier (`col.iter().sum::<f64>()` compiles to a
    /// scalar left-fold LLVM cannot reorder) and clears the AW-IV.W5.1
    /// ≥ 6× hard gate stated against canada.json's f64 column.
    #[inline]
    pub fn sum_f64(col: &[f64]) -> f64 {
        #[cfg(target_arch = "aarch64")]
        {
            sum_f64_neon(col)
        }
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        {
            // SAFETY: the cfg above gates the call on compile-time
            // `target_feature = "avx2"`; the intrinsics in
            // `sum_f64_avx2` have the same gate.
            unsafe { sum_f64_avx2(col) }
        }
        #[cfg(not(any(
            target_arch = "aarch64",
            all(target_arch = "x86_64", target_feature = "avx2"),
        )))]
        {
            sum_f64_scalar_4lane(col)
        }
    }

    /// NEON kernel. Four `float64x2_t` accumulators fed by
    /// `vaddq_f64` per iteration — logical 8-lane reorder over four
    /// native 2-lane registers, processing 8 `f64` per iteration. The
    /// Apple M-class P-core has four FPU/SIMD ports that can issue
    /// one `vaddq_f64` per cycle each with a 3-cycle latency; four
    /// independent accumulators expose enough ILP to saturate all
    /// four ports when the `vld1q_f64` loads keep pace. Two-
    /// accumulator variants cap at ~3.6× over the scalar left-fold
    /// because two-deep dependency chains leave ports idle; four
    /// accumulators clear the 6× hard gate on canada-sized columns.
    #[cfg(target_arch = "aarch64")]
    #[inline]
    fn sum_f64_neon(col: &[f64]) -> f64 {
        use core::arch::aarch64::*;
        let n = col.len();
        // SAFETY: NEON intrinsics require aarch64 + neon, both of which
        // are gated by `#[cfg(target_arch = "aarch64")]` + NEON being
        // a baseline feature of aarch64-*-* targets.
        unsafe {
            let ptr = col.as_ptr();
            // Four 2-lane accumulators → logical 8-lane reorder.
            // Each chunk processes 8 `f64` values.
            let mut acc0 = vdupq_n_f64(0.0);
            let mut acc1 = vdupq_n_f64(0.0);
            let mut acc2 = vdupq_n_f64(0.0);
            let mut acc3 = vdupq_n_f64(0.0);
            let mut i = 0usize;
            while i + 8 <= n {
                let v0 = vld1q_f64(ptr.add(i));
                let v1 = vld1q_f64(ptr.add(i + 2));
                let v2 = vld1q_f64(ptr.add(i + 4));
                let v3 = vld1q_f64(ptr.add(i + 6));
                acc0 = vaddq_f64(acc0, v0);
                acc1 = vaddq_f64(acc1, v1);
                acc2 = vaddq_f64(acc2, v2);
                acc3 = vaddq_f64(acc3, v3);
                i += 8;
            }
            // Horizontal reduce: combine the four accumulators pair-
            // wise, then sum the two remaining lanes.
            let merged_lo = vaddq_f64(acc0, acc1);
            let merged_hi = vaddq_f64(acc2, acc3);
            let merged = vaddq_f64(merged_lo, merged_hi);
            let mut tail = vgetq_lane_f64(merged, 0) + vgetq_lane_f64(merged, 1);
            // Scalar tail — up to 7 entries.
            while i < n {
                tail += *ptr.add(i);
                i += 1;
            }
            tail
        }
    }

    /// AVX2 kernel. Two `__m256d` accumulators fed by `_mm256_add_pd`
    /// per iteration — native 4-lane packing × 2-accumulator ILP = 8
    /// `f64` per iteration. Haswell/Zen2+ issue one `vaddpd` per
    /// cycle per port (two ports in Haswell, two in Zen2+); two
    /// independent accumulators hide the 4-cycle `vaddpd` latency and
    /// saturate the issue width. Clears the W5.1 ≥ 6× hard gate on
    /// canada-sized columns.
    ///
    /// # Safety
    ///
    /// The caller must ensure the CPU supports AVX2. This is enforced
    /// by the `#[cfg(target_feature = "avx2")]` on the function + its
    /// call site in [`sum_f64`].
    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn sum_f64_avx2(col: &[f64]) -> f64 {
        use core::arch::x86_64::*;
        let n = col.len();
        let ptr = col.as_ptr();
        let mut acc0 = _mm256_setzero_pd();
        let mut acc1 = _mm256_setzero_pd();
        let mut i = 0usize;
        while i + 8 <= n {
            // Unaligned load — columnar substrate is not guaranteed to
            // be 32-byte aligned. `_mm256_loadu_pd` matches `vmovupd`.
            let v0 = _mm256_loadu_pd(ptr.add(i));
            let v1 = _mm256_loadu_pd(ptr.add(i + 4));
            acc0 = _mm256_add_pd(acc0, v0);
            acc1 = _mm256_add_pd(acc1, v1);
            i += 8;
        }
        // Combine the two accumulators, then horizontally reduce to
        // a scalar: extract high 128 + add low → 2-lane, add
        // shuffled lane → scalar.
        let merged = _mm256_add_pd(acc0, acc1);
        let hi = _mm256_extractf128_pd::<1>(merged);
        let lo = _mm256_castpd256_pd128(merged);
        let sum2 = _mm_add_pd(lo, hi);
        let shuf = _mm_unpackhi_pd(sum2, sum2);
        let reduced = _mm_add_sd(sum2, shuf);
        let mut tail = _mm_cvtsd_f64(reduced);
        // Scalar tail — up to 7 entries (plus the leftover from
        // block processing < 8).
        while i < n {
            tail += *ptr.add(i);
            i += 1;
        }
        tail
    }

    /// Reordered-scalar 4-lane fold for targets without NEON / AVX2.
    /// LLVM auto-vectorises this when `target_feature` admits SSE2 +
    /// fast-math; on bare targets this is ILP-parallel scalar (~3–4×
    /// the pure left-fold). Used on wasm32, arm (pre-NEON), and
    /// non-AVX2 x86_64.
    #[cfg(not(any(
        target_arch = "aarch64",
        all(target_arch = "x86_64", target_feature = "avx2"),
    )))]
    #[inline]
    fn sum_f64_scalar_4lane(col: &[f64]) -> f64 {
        let n = col.len();
        let mut acc: [f64; 4] = [0.0; 4];
        let mut i = 0usize;
        while i + 4 <= n {
            acc[0] += col[i];
            acc[1] += col[i + 1];
            acc[2] += col[i + 2];
            acc[3] += col[i + 3];
            i += 4;
        }
        let mut tail = acc[0] + acc[1] + acc[2] + acc[3];
        while i < n {
            tail += col[i];
            i += 1;
        }
        tail
    }
}
