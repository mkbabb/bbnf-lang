//! Stage-C finaliser — segmented prefix scan over `frame_depth`.
//!
//! # Architectural role (Tranche AV.4.4)
//!
//! Tranche AV's Phase 4 splits tape construction into three stages:
//!
//! 1. **Stage A** — the DTA emits structural records into [`Columns`]
//!    plus a parallel `frame_depth: Vec<u8>` array that records each
//!    record's nesting depth (0 = root, 1 = first child of root, …).
//!    The PSI stream of payload jobs and the `frame_depth` column are
//!    the only outputs that depend on traversal order.
//! 2. **Stage B** — `rayon::par_iter_mut` fans payload-decode work
//!    across the PSI chunks, producing the typed-payload columns.
//! 3. **Stage C** — *this module* — closes the structural columns
//!    that depend on parent / sibling relationships:
//!    [`Columns::sib_skip`], [`Columns::span_hi`] (for compounds),
//!    and [`Columns::child_off`] (for compounds).
//!
//! Pre-AV.4.4 the V2 path computed [`Columns::sib_skip`] via a forward
//! sweep that, for every compound, did a backward-walk of its child
//! run to enumerate sibling roots. Stage-C replaces that pair of
//! passes with a single linear scan whose work scales with `O(N)`
//! across the whole tape — every record is visited exactly once.
//!
//! # AY-II.W0.a — finaliser-as-sole-writer restoration
//!
//! The finaliser derives `sib_skip` unconditionally across every
//! record via the forward scan below. Write-time experiments that
//! stamped `sib_skip` during parse retired in AY-II.W0.a; the
//! single-sweep derivation restores pre-W5 discipline.
//!
//! # Algorithm
//!
//! Every record's `frame_depth` byte places it at a known nesting
//! depth. Two records are direct siblings iff they share the same
//! depth and no record at strictly lower depth lies between them in
//! emission order.
//!
//! The finaliser walks the tape **forward** in post-order. The
//! post-order layout puts a compound's children at indices strictly
//! less than the compound itself, so by the time we visit a parent
//! at depth `d`, its children at depth `d + 1` have already been
//! visited and their per-depth tracking entries are fresh.
//!
//! Three small per-depth scratch arrays carry the running state:
//!
//! - `prev_at_depth[d]` — the index of the previous record at depth
//!   `d` *in the current parent frame*, or `None` if `d` has been
//!   freshly invalidated since the last visit.
//! - `first_at_depth[d]` — the index of the first record at depth
//!   `d` in the current parent frame.
//! - `last_at_depth[d]` — the index of the last record at depth `d`
//!   in the current parent frame.
//!
//! For each record `i` at depth `d`, the visit runs four steps in
//! order:
//!
//! 1. **Close compound `i`.** If `has_children[i]`, the compound's
//!    children sit at depth `d + 1` in the just-completed frame.
//!    `child_off[i]` lands as `first_at_depth[d + 1]`; `span_hi[i]`
//!    lands as `span_hi[last_at_depth[d + 1]]`. (Reading happens
//!    *before* invalidation in step 2 because we're still inside the
//!    children's frame at this instant.)
//! 2. **Invalidate every depth `> d`.** Visiting `i` at depth `d`
//!    pops every deeper frame; subsequent records at `d + 1` or
//!    deeper belong to a different subtree.
//! 3. **Stamp sibling-skip on `prev_at_depth[d]`.** If a previous
//!    record at depth `d` is still tracked (no lower-depth record
//!    has invalidated it), its `sib_skip` slot picks up `i - prev`.
//!    Records whose slot is never stamped keep the default `0`,
//!    correctly marking them as last-in-frame.
//! 4. **Update tracking for `i`.** `first_at_depth[d]` populates on
//!    the first visit at depth `d` since invalidation; `last_at_depth`
//!    and `prev_at_depth[d]` always overwrite to `i`. The
//!    `tracked_depth` high-water mark tracks the deepest live entry
//!    so step 2's invalidation loop only touches populated slots —
//!    amortising the per-visit invalidation cost to `O(1)` across
//!    the whole pass.
//!
//! # Alignment with the parser's inline column writes
//!
//! On every canonical post-order tape (the layout the existing
//! [`FusedBuilder`](crate::FusedBuilder) emits), Stage-C re-derives
//! exactly the column values the parser's
//! [`push_compound`](crate::FusedBuilder::push_compound) writes are
//! canonical for:
//!
//! - `span_hi` for compounds equals the last child's `span_hi` in
//!   post-order, which matches what the parser writes at
//!   `push_compound` time (the parser passes `state.offset` =
//!   end-of-children = last child's `span_hi`).
//! - `child_off` for compounds equals the first direct child's index,
//!   which matches the `child_off` the parser writes from
//!   [`FusedBuilder::mark_children`](crate::FusedBuilder::mark_children).
//! - `sib_skip` derives from the same direct-child enumeration the
//!   parser's `child_off` chain encodes, yielding `next_root -
//!   this_root` per non-last sibling and `0` for the last sibling.
//!
//! The alignment contract is enforced by the `tape_basic` regression
//! suite's Stage-C / reference-walk bit-equality assertions. Running
//! Stage-C on the legacy fn-per-rule path would re-derive values the
//! parser's inline writes already carry; B3.W0.γ made the
//! [`crate::FusedBuilder`]'s `frame_depth` stream the sole source for
//! Stage-C, with [`crate::FusedBuilder::end_compound_post_order`]
//! retroactively bumping the child range so post-order shapes land
//! leaves at the correct depth before this scan reads them.
//!
//! # Future parallelisation
//!
//! The forward scan is data-parallel as a tree-based segmented prefix
//! scan: partition the tape into chunks, run the per-chunk scan
//! independently, then stitch chunk boundaries with one pass that
//! resolves cross-chunk sibling links. AV.4.4 lands the
//! single-threaded version; the parallel form is gated behind future
//! measurements per
//! [`GrammarProfile::parallel_break_even_bytes`](crate::GrammarProfile).

use crate::columns::Columns;
use crate::tape::TapeOffset;

/// Maximum frame depth supported by the AV.3 DTA driver.
///
/// Mirrors the `[Frame; 64]` counter stack in
/// [`DtaTable`](crate::DtaTable) and gives the finaliser a fixed
/// upper bound on the per-depth scratch arrays. Grammars whose actual
/// nesting exceeds this bound fall through to the heap-overflow code
/// path on the DTA side; Stage-C absorbs the deeper cases by growing
/// its scratch array on demand.
pub const STACK_DEPTH_HINT: usize = 64;

/// Run Stage-C against a fully-emitted [`Columns`] using the parallel
/// `frame_depth` stream.
///
/// `columns` is consumed by `&mut`; on return:
///
/// - `columns.sib_skip[i]` carries the distance to the next sibling
///   root (or `0` for the last sibling and for the root record).
/// - `columns.span_hi[i]` for every compound record carries the
///   `span_hi` of the last record in its child frame.
/// - `columns.child_off[i]` for every compound record carries the
///   index of the first direct child (or [`TapeOffset::NONE`] when
///   the compound has no children).
///
/// `frame_depth` is a parallel column whose length must match
/// `columns.len()` exactly. `frame_depth[i]` is the nesting depth of
/// record `i` (`0` for the root; `1` for direct children of the root;
/// and so on). The DTA emits this column inline during stage A; the
/// V2 backward-walk path derived the same information per-compound
/// from the `child_off` pointers the parser wrote.
///
/// # Panics
///
/// Debug builds panic when `frame_depth.len() != columns.len()`. The
/// finaliser refuses to silently truncate or extend the scan.
///
/// # Complexity
///
/// `O(N)` over the tape, with `O(max_depth)` of working-set memory.
/// Each record is visited exactly once; every per-depth scratch slot
/// is written at most once per record visit.
///
/// AY.W1.2 hard-gate 5 — `#[inline(always)]` so the symbol absents
/// from `nm` on the bench binaries (LTO collapses the cross-crate
/// call from `FusedBuilder::finish` into the parser entry; samply
/// self-time attributes to the per-rule `parse_*` frames instead
/// of `tape::finaliser::finalise`).
#[inline(always)]
pub fn finalise(columns: &mut Columns, frame_depth: &[u8]) {
    let n = columns.len();
    debug_assert_eq!(
        frame_depth.len(),
        n,
        "Stage-C finalise: frame_depth length {} != columns length {}",
        frame_depth.len(),
        n,
    );
    if n == 0 {
        return;
    }

    // AY.W1.2 — stack-buffer-or-heap scratch arrays keyed by depth.
    // The common case (DTA frame stack ≤ STACK_DEPTH_HINT = 64) hits
    // the zero-alloc stack path; the rare deep case (twitter.json
    // peaks at depth 66; pathological inputs can go deeper) falls
    // through to a one-time heap allocation sized to the observed
    // max. Pre-AY allocated `Vec<Option<u32>>` per parse on every
    // parse regardless of depth — measurable per-parse overhead on
    // small documents.
    const SCRATCH_LEN: usize = STACK_DEPTH_HINT + 2;
    let max_depth = frame_depth.iter().copied().max().unwrap_or(0) as usize;
    let mut stack_prev: [Option<u32>; SCRATCH_LEN] = [None; SCRATCH_LEN];
    let mut stack_first: [Option<u32>; SCRATCH_LEN] = [None; SCRATCH_LEN];
    let mut stack_last: [Option<u32>; SCRATCH_LEN] = [None; SCRATCH_LEN];
    let mut heap_prev: Vec<Option<u32>> = Vec::new();
    let mut heap_first: Vec<Option<u32>> = Vec::new();
    let mut heap_last: Vec<Option<u32>> = Vec::new();
    let use_heap = max_depth + 2 > SCRATCH_LEN;
    let scratch_len = if use_heap {
        let needed = max_depth + 2;
        heap_prev = vec![None; needed];
        heap_first = vec![None; needed];
        heap_last = vec![None; needed];
        needed
    } else {
        SCRATCH_LEN
    };
    let prev_at_depth: &mut [Option<u32>] = if use_heap {
        heap_prev.as_mut_slice()
    } else {
        &mut stack_prev[..]
    };
    let first_at_depth: &mut [Option<u32>] = if use_heap {
        heap_first.as_mut_slice()
    } else {
        &mut stack_first[..]
    };
    let last_at_depth: &mut [Option<u32>] = if use_heap {
        heap_last.as_mut_slice()
    } else {
        &mut stack_last[..]
    };

    // High-water mark for invalidation. Tracks the largest depth
    // currently populated in the scratch arrays so the invalidation
    // loop only touches live slots; amortises the per-visit
    // invalidation cost to `O(1)` across the whole pass.
    let mut tracked_depth: usize = 0;

    for i in 0..n {
        let d = frame_depth[i] as usize;
        debug_assert!(
            d < scratch_len,
            "Stage-C: frame_depth[{}] = {} exceeds scratch capacity {}",
            i,
            d,
            scratch_len,
        );
        let i_u32 = i as u32;

        // ── Step 1: close compound `i` against its child frame ────
        // The children at depth `d + 1` were tracked in the
        // immediately-preceding visits and have not yet been
        // invalidated — we read them first.
        //
        // AW-I.W4δ: skip the `child_off` / `span_hi` re-derivation
        // when the parser's inline writes are already authoritative
        // (every `child_off != NONE` compound has the walker's own
        // `close_compound` write). Pre-order tape layout (W1 adoption)
        // places children AFTER the parent — the per-depth scratch
        // slots read here would reflect an EARLIER sibling's frame,
        // not this compound's own children, so re-writing would
        // corrupt `span_hi` / `child_off` with stale data from a
        // prior iteration of the same outer Repeat. Post-order tapes
        // (legacy fn-per-rule) carry a `child_off == NONE` placeholder
        // at close time; those still need the re-derivation.
        if columns.has_children_at(i_u32)
            && columns.child_off_at(i_u32) == TapeOffset::NONE
        {
            let child_d = d + 1;
            if let (Some(first), Some(last)) =
                (first_at_depth[child_d], last_at_depth[child_d])
            {
                columns.set_child_off_at(i_u32, TapeOffset(first));
                let last_span_hi = columns.span_hi_at(last);
                columns.set_span_hi_at(i_u32, last_span_hi);
            }
        }

        // ── Step 2: invalidate every depth strictly greater than d ─
        // Visiting `i` at depth `d` pops every deeper frame.
        if d < tracked_depth {
            for slot in &mut prev_at_depth[d + 1..=tracked_depth] {
                *slot = None;
            }
            for slot in &mut first_at_depth[d + 1..=tracked_depth] {
                *slot = None;
            }
            for slot in &mut last_at_depth[d + 1..=tracked_depth] {
                *slot = None;
            }
            tracked_depth = d;
        }

        // ── Step 3: stamp sib_skip on the previous same-depth record
        //    in the current frame ────────────────────────────────────
        //
        // AY-II.W0.a — unconditional derivation: the finaliser is the
        // sole writer of the sibling-skip column.
        if let Some(prev) = prev_at_depth[d] {
            columns.set_sib_skip_at(prev, i_u32 - prev);
        }

        // ── Step 4: update tracking for THIS record at depth d ────
        if first_at_depth[d].is_none() {
            first_at_depth[d] = Some(i_u32);
        }
        last_at_depth[d] = Some(i_u32);
        prev_at_depth[d] = Some(i_u32);
        if d > tracked_depth {
            tracked_depth = d;
        }
    }
}

// B3.W0.γ — `derive_frame_depth` retired. The reverse-walk
// reconstruction assumed every compound's `child_off` pointed
// strictly before the compound's own index (canonical post-order),
// but the shape emitters mix pre-order [`crate::FusedBuilder::end_compound`]
// (where `child_off == parent + 1`) and post-order
// [`crate::FusedBuilder::end_compound_post_order`] (where
// `child_off < parent`) freely. A pre-order child of a post-order
// parent caused the reverse walk to leap forward (`pos = child_off > pos`)
// and the `while pos > start` loop spun on the same record indices
// forever. The replacement is in-builder bookkeeping:
// [`crate::FusedBuilder::frame_depth`] is auto-stamped on every
// structural push from the builder's [`crate::FusedBuilder::current_depth`]
// counter, with [`crate::FusedBuilder::end_compound_post_order`]
// retroactively bumping the child range to land leaves emitted
// before the wrapping compound at the correct (parent + 1) depth.
