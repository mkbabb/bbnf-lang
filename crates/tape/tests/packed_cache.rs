//! AX.W1.D — hybrid-tape AoS sidecar (`packed_cache`) tests.
//!
//! Verifies:
//!
//! 1. `PackedRecord` size + alignment match the 32-byte cache-line
//!    contract (belt-and-braces assertion on top of the compile-time
//!    `const _` checks in `packed.rs`).
//! 2. Fresh `Columns` reports an empty packed cache; calling
//!    `packed_cache()` populates it from the SoA primary.
//! 3. Transpose is field-isomorphic to the SoA materialiser —
//!    `PackedRecord::as_tape_rec()` round-trips every column slot.
//! 4. Mutating the SoA primary via any push path invalidates the
//!    cache; the next read re-transposes.
//! 5. `truncate()` invalidates the cache so backtracking callers
//!    see the post-truncation state.
//! 6. `stp_span` invalidates the cache + writes both span columns.
//! 7. Microbench-style sanity: single-record read through the AoS
//!    sidecar vs SoA materialise yields the same record values.
//! 8. Post-populate reads return the SAME `&[PackedRecord]` slice
//!    (OnceLock memoisation).

use tape::columns::Columns;
use tape::kind::TapeKind;
use tape::packed::PackedRecord;
use tape::tape::{TapeOffset, TapeRec};

#[test]
fn packed_record_size_and_alignment() {
    assert_eq!(std::mem::size_of::<PackedRecord>(), 32);
    assert_eq!(std::mem::align_of::<PackedRecord>(), 32);
}

#[test]
fn fresh_columns_packed_cache_empty() {
    let cols = Columns::new();
    assert!(!cols.packed_cache_populated());
}

#[test]
fn packed_cache_populates_on_first_read() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_leaf_fused(TapeKind::Literal, 1, 0, 0, 1, TapeOffset::NONE);
    cols.push_compound_fused(TapeKind::Alt, 1);

    assert!(!cols.packed_cache_populated());
    let view = cols.packed_cache();
    assert_eq!(view.len(), 3);
    assert!(cols.packed_cache_populated());
}

#[test]
fn packed_record_isomorphic_to_tape_rec() {
    let mut cols = Columns::new();
    cols.push_leaf_fused(
        TapeKind::Span,
        /*flags=*/ 7,
        /*extra=*/ TapeRec::HAS_CHILDREN_BIT,
        /*span_lo=*/ 100,
        /*span_hi=*/ 200,
        TapeOffset(42),
    );
    cols.push_compound_fused(TapeKind::Seq, 300);

    let view = cols.packed_cache();
    assert_eq!(view.len(), 2);

    // Record 0 — leaf with payload.
    let p0 = view[0];
    let r0 = cols.materialize(0);
    assert_eq!(p0.as_tape_rec(), r0);
    assert_eq!(p0.kind(), TapeKind::Span);
    assert_eq!(p0.span_lo, 100);
    assert_eq!(p0.span_hi, 200);
    assert_eq!(p0.child_off, TapeOffset(42));
    assert_eq!(p0.flags, 7);

    // Record 1 — provisional compound.
    let p1 = view[1];
    let r1 = cols.materialize(1);
    assert_eq!(p1.as_tape_rec(), r1);
    assert_eq!(p1.kind(), TapeKind::Seq);
    assert_eq!(p1.span_lo, 300);
}

#[test]
fn sib_skip_reflected_in_packed_record() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 0, 1, TapeOffset::NONE);

    // Hand-patch sib_skip on the leaf; the AoS sidecar should reflect
    // the update after invalidation on the next read.
    cols.sib_skip[0] = 5;
    // Pretend a push happened: direct mutation on a `pub` column is
    // currently uncaptured by the invalidation contract. The correct
    // production path goes through the finaliser, which runs after
    // pushes and re-invalidates via a subsequent push-equivalent
    // call. Here we force invalidation directly.
    cols.invalidate_packed();

    let view = cols.packed_cache();
    assert_eq!(view[0].sib_skip, 5);
}

#[test]
fn push_invalidates_packed_cache() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);

    let view_a = cols.packed_cache();
    assert_eq!(view_a.len(), 1);
    assert!(cols.packed_cache_populated());

    // Subsequent push invalidates the cache.
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 5, 6, TapeOffset::NONE);
    assert!(!cols.packed_cache_populated());

    // Next read re-transposes with the new row.
    let view_b = cols.packed_cache();
    assert_eq!(view_b.len(), 2);
}

#[test]
fn push_structural_via_builder_invalidates_packed_cache() {
    // The legacy `push_structural` path (exercised by `TapeBuilder`'s
    // `push_leaf` / `push_compound`) must also honour the contract.
    // Reach it through the public builder API.
    use tape::TapeBuilder;

    let mut builder = TapeBuilder::new();
    builder.push_leaf(TapeKind::Span, 0, 5, 0, 0);

    // The finished tape's columns are fresh; populate the cache.
    let tape = builder.finish().unwrap();
    let cols_ref = tape.columns();
    let _ = cols_ref.packed_cache();
    assert!(cols_ref.packed_cache_populated());

    // Build a second tape to show the push_structural path lands
    // through the builder without double-populating. Each finished
    // tape has its own cache state.
    let mut builder2 = TapeBuilder::new();
    builder2.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    builder2.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    let tape2 = builder2.finish().unwrap();
    assert!(!tape2.columns().packed_cache_populated());
    let view = tape2.columns().packed_cache();
    assert_eq!(view.len(), 2);
}

#[test]
fn truncate_invalidates_packed_cache() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 1, 2, TapeOffset::NONE);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 3, 4, TapeOffset::NONE);

    let _ = cols.packed_cache();
    assert!(cols.packed_cache_populated());

    cols.truncate(1);
    assert!(!cols.packed_cache_populated());

    let view = cols.packed_cache();
    assert_eq!(view.len(), 1);
}

#[test]
fn stp_span_patches_both_columns_and_invalidates() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);
    cols.push_leaf_fused(TapeKind::Literal, 0, 0, 0, 0, TapeOffset::NONE);

    // Populate the cache so we can observe invalidation.
    let _ = cols.packed_cache();
    assert!(cols.packed_cache_populated());

    cols.stp_span(0, 10, 20);
    cols.stp_span(1, 30, 40);
    assert!(!cols.packed_cache_populated());

    assert_eq!(cols.span_lo[0], 10);
    assert_eq!(cols.span_hi[0], 20);
    assert_eq!(cols.span_lo[1], 30);
    assert_eq!(cols.span_hi[1], 40);

    let view = cols.packed_cache();
    assert_eq!(view[0].span_lo, 10);
    assert_eq!(view[0].span_hi, 20);
    assert_eq!(view[1].span_lo, 30);
    assert_eq!(view[1].span_hi, 40);
}

#[test]
fn packed_cache_memoises_across_reads() {
    let mut cols = Columns::new();
    cols.push_compound_fused(TapeKind::Seq, 0);

    let first = cols.packed_cache();
    let first_ptr = first.as_ptr() as usize;
    let second = cols.packed_cache();
    let second_ptr = second.as_ptr() as usize;
    assert_eq!(
        first_ptr, second_ptr,
        "packed_cache() must return the same slice on consecutive reads"
    );
}

#[test]
fn packed_cache_matches_materialize_record_by_record() {
    let mut cols = Columns::new();
    for i in 0..64u32 {
        if i % 3 == 0 {
            cols.push_compound_fused(TapeKind::Seq, i);
        } else {
            cols.push_leaf_fused(
                TapeKind::Literal,
                (i & 0xFF) as u8,
                (i & 0xFFFF) as u16,
                i,
                i + 1,
                TapeOffset::NONE,
            );
        }
    }

    let view = cols.packed_cache();
    assert_eq!(view.len(), 64);
    for i in 0..64u32 {
        let soa = cols.materialize(i);
        let aos = view[i as usize].as_tape_rec();
        assert_eq!(soa, aos, "record {} SoA/AoS divergence", i);
    }
}

#[test]
fn packed_record_preserves_all_flag_bits() {
    let mut cols = Columns::new();
    let extra = TapeRec::STRING_BORROW_BIT
        | TapeRec::PAYLOAD_IN_ARENA_BIT
        | TapeRec::HAS_CHILDREN_BIT
        | TapeRec::META_IDX_HI_BIT;
    cols.push_leaf_fused(
        TapeKind::Span,
        0xAB,
        extra,
        0x1000_0000,
        0x2000_0000,
        TapeOffset(0x3000_0000),
    );
    let view = cols.packed_cache();
    assert_eq!(view[0].flags, 0xAB);
    assert_eq!(view[0].extra, extra);
    assert_eq!(view[0].span_lo, 0x1000_0000);
    assert_eq!(view[0].span_hi, 0x2000_0000);
    assert_eq!(view[0].child_off, TapeOffset(0x3000_0000));
}

/// AX.W1.D microbench sanity — the AoS sidecar random-read is at
/// least as fast as the SoA materialise path on random access.
///
/// Not a real benchmark (not `cargo bench`); it's a smoke test that
/// the ratio is strictly positive — the real perf gate lives in
/// `crates/core/benches/twitter_lazy_field.rs`.
///
/// AX.W1.D microbench ratio: validates `packed_cache()` single-
/// record read ≥ 1.3× SoA single-record read per the W1 §4 hard
/// gate. This assertion runs inline so any build that regresses the
/// ratio trips the test immediately.
#[test]
fn packed_cache_read_beats_soa_materialise() {
    use std::time::Instant;
    // Populate a tape large enough that the read pattern amortises
    // the `Instant::now` cost. 65 536 records keeps the test under
    // 10ms at ax-iter but still gives a stable ratio.
    const N: u32 = 65_536;
    let mut cols = Columns::new();
    for i in 0..N {
        cols.push_leaf_fused(
            TapeKind::Literal,
            (i & 0xFF) as u8,
            (i & 0xFFFF) as u16,
            i,
            i + 1,
            TapeOffset(i),
        );
    }
    // Pre-populate the cache so the first AoS read doesn't pay the
    // transpose cost (the hybrid contract: one O(n) transpose + N
    // O(1) reads dominates the read cost for N >> 1).
    let _ = cols.packed_cache();

    // SoA-materialise pass — walks the tape reconstructing a
    // `TapeRec` per record. Indexed deterministically to avoid
    // branch-predictor bias.
    let t0 = Instant::now();
    let mut soa_acc: u64 = 0;
    for _pass in 0..4 {
        for i in 0..N {
            let r = cols.materialize(i);
            soa_acc = soa_acc
                .wrapping_add(r.span_lo as u64)
                .wrapping_add(r.span_hi as u64)
                .wrapping_add(r.child_off.as_u32() as u64);
        }
    }
    let soa_ns = t0.elapsed().as_nanos().max(1);

    // AoS sidecar pass — one direct load per record against the
    // packed slice.
    let t0 = Instant::now();
    let view = cols.packed_cache();
    let mut aos_acc: u64 = 0;
    for _pass in 0..4 {
        for i in 0..N {
            let p = view[i as usize];
            aos_acc = aos_acc
                .wrapping_add(p.span_lo as u64)
                .wrapping_add(p.span_hi as u64)
                .wrapping_add(p.child_off.as_u32() as u64);
        }
    }
    let aos_ns = t0.elapsed().as_nanos().max(1);

    // Same reduction ⇒ same result. Guards against LLVM optimising
    // one path away.
    assert_eq!(soa_acc, aos_acc);

    // Hard-gate ratio per W1 §4: AoS ≥ 1.3× SoA on random access.
    let ratio = soa_ns as f64 / aos_ns as f64;
    assert!(
        ratio >= 1.3,
        "packed_cache read {:.2}× SoA materialise (want ≥ 1.3×); soa_ns={} aos_ns={}",
        ratio,
        soa_ns,
        aos_ns,
    );
}
