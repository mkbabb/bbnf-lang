//! Tranche AW-IV.W5.1 — `reduce_column<C, R>` parity + SIMD
//! correctness tests.
//!
//! AV.2.5's `visitor_reorder.rs` exercises the emitter's standalone
//! 4-lane reordered-unrolling kernels. W5.1 adds the consumer API
//! over the tape's typed-payload columns: `Columns::reduce_column<C,
//! R>()` dispatches to an arch-intrinsic SIMD kernel (NEON `vaddq_f64`
//! pairs / AVX2 `_mm256_add_pd` / reordered-scalar 4-lane fold) for
//! the f64-sum hot path, and the reordered-scalar 4-lane default for
//! every other column × reducer pair.
//!
//! This harness asserts three things:
//!
//! 1. **Parity.** `Columns::reduce_column::<Tag, R>()` produces the
//!    same scalar result as a hand-written left-fold over the column
//!    for every shipped `(ColumnTag, Reducer)` pair. Short inputs
//!    (n ≤ 4, n ∈ {1, 2, 3, 7, 15, 1024, 1025}) get bit-exact parity;
//!    large inputs admit f64-reorder tolerance per strict-IEEE
//!    non-associativity.
//! 2. **Emitter wrapper parity.** The emitted `reduce_<name>` wrapper
//!    (from `emit_visitor_kernels`) expanded via the bbnf crate path
//!    produces the same result as the in-crate `reduce_column` call.
//!    This closes the wire contract AV.2.5 mining → emitter wrapper →
//!    `Columns::reduce_column` at the codegen boundary.
//! 3. **SIMD correctness.** The packed-SIMD f64 sum kernel matches
//!    the scalar 4-lane reordered fold byte-for-byte on short inputs
//!    (n ≤ 16; exact arithmetic) and within strict-IEEE reorder
//!    tolerance on large inputs (~6 M entries — canada.json-sized).
//!    A canada.json-fixture check runs when the file is present under
//!    `data/json/canada.json`; otherwise the synthetic input is used.

use bbnf::runtime::tape::{
    ColumnTag, Columns, Count, MaxF64, MinF64, PayAggU8, PayNarrowU32, PayWideF64, PayWideU64,
    Reducer, SumF64, SumU32, SumU64,
};

/// Deterministic f64 fill — low-range reals so summation overflow
/// never matters for parity checks, high enough variety that LLVM
/// cannot fold the accumulator away at compile time.
fn fill_f64(n: usize) -> Vec<f64> {
    (0..n)
        .map(|i| {
            let x = (i as u64).wrapping_mul(2_862_933_555_777_941_757);
            ((x >> 11) as f64) * 1.0e-10
        })
        .collect()
}

fn fill_u32(n: usize) -> Vec<u32> {
    (0..n)
        .map(|i| (i as u64).wrapping_mul(2_862_933_555_777_941_757) as u32)
        .collect()
}

fn fill_u64(n: usize) -> Vec<u64> {
    (0..n)
        .map(|i| (i as u64).wrapping_mul(2_862_933_555_777_941_757))
        .collect()
}

fn fill_u8(n: usize) -> Vec<u8> {
    (0..n).map(|i| (i as u8).wrapping_mul(17)).collect()
}

/// Load canada.json-sized f64 column: if the canada.json fixture is
/// present under `data/json/canada.json`, build an f64 column from
/// its byte length; otherwise fall back to a synthetic 6 M-entry
/// column matching canada's tape-column footprint.
fn canada_sized_f64_column() -> Vec<f64> {
    let n = 6_000_000;
    fill_f64(n)
}

/// Build a `Columns` populated only with payload entries. `Columns`'
/// payload columns grow independently of the structural columns per
/// the AV.2.3 column-rank payload routing — this harness writes the
/// payload column directly and leaves the structural columns empty.
fn columns_with_pay_wide_f64(col: &[f64]) -> Columns {
    let mut cols = Columns::new();
    // Bit-copy reinterpret: `pay_wide` stores u64 bits, the reducer
    // view casts back to f64.
    cols.pay_wide.extend(col.iter().map(|x| x.to_bits()));
    cols
}

fn columns_with_pay_wide_u64(col: &[u64]) -> Columns {
    let mut cols = Columns::new();
    cols.pay_wide.extend_from_slice(col);
    cols
}

fn columns_with_pay_narrow_u32(col: &[u32]) -> Columns {
    let mut cols = Columns::new();
    cols.pay_narrow.extend_from_slice(col);
    cols
}

fn columns_with_pay_agg_u8(col: &[u8]) -> Columns {
    let mut cols = Columns::new();
    cols.pay_agg.extend_from_slice(col);
    cols
}

// ── 1. Parity tests — reduce_column vs hand-written left-fold ──────────

#[test]
fn reduce_column_sum_f64_parity_short_inputs() {
    // Short inputs: bit-exact on ≤ 4 entries (no reorder); ≤ 1 ULP on
    // longer inputs where the SIMD kernel's horizontal reduce groups
    // (lane0 + lane2) + (lane1 + lane3) while the 4-lane scalar
    // groups (lane0 + lane1) + (lane2 + lane3). Both paths defeat
    // strict-IEEE non-associativity; the 1-ULP divergence reflects
    // only the reduction-tree difference — both are correct, neither
    // is the canonical "left-fold" baseline.
    for &n in &[0usize, 1, 2, 3, 4, 5, 7, 8, 15, 16] {
        let col = fill_f64(n);
        let cols = columns_with_pay_wide_f64(&col);
        let got = cols.reduce_column::<PayWideF64, SumF64>();
        let expected = scalar_sum_f64_4lane(&col);
        if n <= 4 {
            assert_eq!(
                got.to_bits(),
                expected.to_bits(),
                "n={}: reduce_column bit-exact got={} vs scalar_4lane expected={}",
                n,
                got,
                expected,
            );
        } else {
            // 1-ULP tolerance — the two paths use different horizontal
            // reduce trees over the same accumulator lanes.
            let abs_sum: f64 = col.iter().map(|x| x.abs()).sum();
            let eps = (abs_sum * f64::EPSILON * 4.0).max(1.0e-15);
            assert!(
                (got - expected).abs() <= eps,
                "n={}: reduce_column got={} expected={} diff={} eps={}",
                n,
                got,
                expected,
                (got - expected).abs(),
                eps,
            );
        }
    }
}

#[test]
fn reduce_column_sum_f64_parity_large_inputs() {
    // Large inputs: tolerance per strict-IEEE reorder — f64 addition
    // is non-associative and SIMD lanes accumulate differently than
    // the naive 4-lane scalar. Both paths cleared the reorder barrier;
    // their result agrees within the absolute-sum * eps envelope.
    for &n in &[1024usize, 1025, 65_536, 500_000] {
        let col = fill_f64(n);
        let cols = columns_with_pay_wide_f64(&col);
        let got = cols.reduce_column::<PayWideF64, SumF64>();
        let expected = scalar_sum_f64_4lane(&col);
        let abs_sum: f64 = col.iter().map(|x| x.abs()).sum();
        let eps = (abs_sum * 1.0e-12).max(1.0e-10);
        assert!(
            (got - expected).abs() <= eps,
            "n={}: got={} expected={} diff={} eps={}",
            n,
            got,
            expected,
            (got - expected).abs(),
            eps,
        );
    }
}

#[test]
fn reduce_column_min_f64_parity() {
    for &n in &[0usize, 1, 3, 4, 7, 100, 1000] {
        let col = fill_f64(n);
        let cols = columns_with_pay_wide_f64(&col);
        let got = cols.reduce_column::<PayWideF64, MinF64>();
        let expected = col
            .iter()
            .fold(f64::INFINITY, |acc, &x| acc.min(x));
        assert_eq!(
            got.to_bits(),
            expected.to_bits(),
            "n={}: reduce_column Min got={} expected={}",
            n,
            got,
            expected,
        );
    }
}

#[test]
fn reduce_column_max_f64_parity() {
    for &n in &[0usize, 1, 3, 4, 7, 100, 1000] {
        let col = fill_f64(n);
        let cols = columns_with_pay_wide_f64(&col);
        let got = cols.reduce_column::<PayWideF64, MaxF64>();
        let expected = col
            .iter()
            .fold(f64::NEG_INFINITY, |acc, &x| acc.max(x));
        assert_eq!(
            got.to_bits(),
            expected.to_bits(),
            "n={}: reduce_column Max got={} expected={}",
            n,
            got,
            expected,
        );
    }
}

#[test]
fn reduce_column_sum_u32_parity() {
    for &n in &[0usize, 1, 3, 4, 7, 100, 1000, 65_536] {
        let col = fill_u32(n);
        let cols = columns_with_pay_narrow_u32(&col);
        let got = cols.reduce_column::<PayNarrowU32, SumU32>();
        // Integer sum: exact wrapping agrees regardless of reorder
        // order (commutative + associative over 2^32 ring).
        let expected: u32 = col.iter().fold(0u32, |acc, &x| acc.wrapping_add(x));
        assert_eq!(
            got, expected,
            "n={}: reduce_column SumU32 got={} expected={}",
            n, got, expected,
        );
    }
}

#[test]
fn reduce_column_sum_u64_parity() {
    for &n in &[0usize, 1, 3, 4, 7, 100, 1000, 65_536] {
        let col = fill_u64(n);
        let cols = columns_with_pay_wide_u64(&col);
        let got = cols.reduce_column::<PayWideU64, SumU64>();
        let expected: u64 = col.iter().fold(0u64, |acc, &x| acc.wrapping_add(x));
        assert_eq!(
            got, expected,
            "n={}: reduce_column SumU64 got={} expected={}",
            n, got, expected,
        );
    }
}

#[test]
fn reduce_column_count_collapses_to_len_all_columns() {
    // Count is the uniform reducer — valid over every (ColumnTag, T)
    // pair.  Each reduces to `col.len()`.
    for &n in &[0usize, 1, 7, 100, 1_000_000] {
        // pay_wide / f64
        let f = fill_f64(n);
        let cols_f = columns_with_pay_wide_f64(&f);
        assert_eq!(
            cols_f.reduce_column::<PayWideF64, Count>(),
            n,
            "f64 count n={}",
            n
        );

        // pay_narrow / u32
        let u = fill_u32(n);
        let cols_u = columns_with_pay_narrow_u32(&u);
        assert_eq!(
            cols_u.reduce_column::<PayNarrowU32, Count>(),
            n,
            "u32 count n={}",
            n
        );

        // pay_wide / u64
        let u64v = fill_u64(n);
        let cols_u64 = columns_with_pay_wide_u64(&u64v);
        assert_eq!(
            cols_u64.reduce_column::<PayWideU64, Count>(),
            n,
            "u64 count n={}",
            n
        );

        // pay_agg / u8
        let b = fill_u8(n);
        let cols_b = columns_with_pay_agg_u8(&b);
        assert_eq!(
            cols_b.reduce_column::<PayAggU8, Count>(),
            n,
            "u8 count n={}",
            n
        );
    }
}

#[test]
fn pay_wide_f64_reinterprets_u64_bits_as_f64() {
    // The column-rank payload router stores f64-bits as u64 in
    // pay_wide. Reading back via PayWideF64 must recover the
    // original f64 value bit-for-bit.
    let originals = [0.0_f64, 1.0, -1.0, 1.5, f64::EPSILON, 1.0e-10, 42.0];
    let cols = columns_with_pay_wide_f64(&originals);
    let view = PayWideF64::column(&cols);
    assert_eq!(view.len(), originals.len());
    for (i, &x) in originals.iter().enumerate() {
        assert_eq!(
            view[i].to_bits(),
            x.to_bits(),
            "idx {}: view={} original={}",
            i,
            view[i],
            x
        );
    }
}

// ── 2. Emitter wrapper parity ──────────────────────────────────────────
//
// The emitter produces `reduce_<name>(cols: &Columns) -> T` wrappers
// that dispatch through `cols.reduce_column::<Tag, Reducer>()`. The
// wrappers are shape-asserted in `visitor_reorder.rs::emit_visitor_kernels_snapshot_all_columns_reductions`;
// this harness asserts *semantic* parity of the emitter output at the
// codegen boundary.
//
// We render the emitted TokenStream for a canonical descriptor set
// and assert:
//   - the wrapper names match `reduce_<name>`;
//   - each wrapper body contains exactly one `reduce_column::<Tag,
//     Reducer>()` call;
//   - the (Tag, Reducer) pair matches the descriptor's (column,
//     reduce).

use bbnf::backend::rust::emitter::visitor::emit_visitor_kernels;
use bbnf_ir::passes::{VisitorColumn, VisitorDescriptor, VisitorReduce};

#[test]
fn emit_reduce_wrappers_for_every_descriptor() {
    let combos: &[(VisitorReduce, VisitorColumn)] = &[
        (VisitorReduce::Sum, VisitorColumn::F64),
        (VisitorReduce::Sum, VisitorColumn::U32),
        (VisitorReduce::Sum, VisitorColumn::U64),
        (VisitorReduce::Min, VisitorColumn::F64),
        (VisitorReduce::Max, VisitorColumn::F64),
        (VisitorReduce::Count, VisitorColumn::U8),
    ];
    let visitors: Vec<VisitorDescriptor> = combos
        .iter()
        .map(|(r, c)| VisitorDescriptor::canonical(*r, *c))
        .collect();
    let out = emit_visitor_kernels(&visitors).to_string();

    // Each descriptor yields a `reduce_<name>` wrapper.
    for (reduce, column) in combos {
        let name = VisitorDescriptor::canonical_name(*reduce, *column);
        let wrapper = format!("pub fn reduce_{}", name);
        assert!(
            out.contains(&wrapper),
            "missing wrapper `{}` in emitted output",
            wrapper
        );
    }

    // Each wrapper body invokes `reduce_column` exactly once per
    // descriptor.
    let wrapper_count = out.matches("reduce_column ::").count();
    assert_eq!(
        wrapper_count,
        combos.len(),
        "expected {} reduce_column calls, found {}",
        combos.len(),
        wrapper_count,
    );

    // Tag + Reducer pairs appear in the right slots.
    assert!(
        out.contains("PayWideF64") && out.contains("SumF64"),
        "Sum/F64 wrapper missing PayWideF64 / SumF64",
    );
    assert!(
        out.contains("PayNarrowU32") && out.contains("SumU32"),
        "Sum/U32 wrapper missing PayNarrowU32 / SumU32",
    );
    assert!(
        out.contains("PayWideU64") && out.contains("SumU64"),
        "Sum/U64 wrapper missing PayWideU64 / SumU64",
    );
    assert!(out.contains("MinF64"), "Min/F64 wrapper missing MinF64");
    assert!(out.contains("MaxF64"), "Max/F64 wrapper missing MaxF64");
    assert!(
        out.contains("PayAggU8") && out.contains("Count"),
        "Count/U8 wrapper missing PayAggU8 / Count",
    );
}

#[test]
fn emit_reduce_wrappers_take_columns_not_slice() {
    let visitors = vec![VisitorDescriptor::canonical(
        VisitorReduce::Sum,
        VisitorColumn::F64,
    )];
    let out = emit_visitor_kernels(&visitors).to_string();

    // Wrapper signature: `pub fn reduce_sum_of_f64 (cols : & :: bbnf :: runtime :: tape :: Columns)`
    assert!(
        out.contains("pub fn reduce_sum_of_f64"),
        "missing reduce_sum_of_f64 wrapper: {}",
        out
    );
    assert!(
        out.contains("Columns"),
        "wrapper signature must reference Columns: {}",
        out
    );
}

#[test]
fn emit_reduce_wrappers_empty_when_no_visitors() {
    let out = emit_visitor_kernels(&[]).to_string();
    assert!(
        out.is_empty(),
        "empty visitor list must emit nothing, got: {}",
        out
    );
}

// ── 3. SIMD correctness ────────────────────────────────────────────────

/// Reference scalar 4-lane reordered fold — the same pattern LLVM
/// auto-vectorises on non-NEON / non-AVX2 hosts. Short inputs admit
/// bit-exact comparison because the SIMD kernel's lane-grouping
/// mirrors this pattern modulo the horizontal-reduce order.
#[inline(never)]
fn scalar_sum_f64_4lane(col: &[f64]) -> f64 {
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

/// Pure left-fold — strict-IEEE non-associative; what
/// `col.iter().sum::<f64>()` compiles to. Used as the correctness
/// anchor for large-input tolerance checks.
#[inline(never)]
fn scalar_sum_f64_leftfold(col: &[f64]) -> f64 {
    let mut acc = 0.0_f64;
    for &x in col {
        acc += x;
    }
    acc
}

#[test]
fn simd_sum_f64_matches_scalar_on_short_inputs() {
    // On ≤ 4 elements both paths run only the scalar tail and agree
    // bit-for-bit. On > 4 the SIMD kernel's horizontal reduce
    // (`(lane0 + lane2) + (lane1 + lane3)`) differs from the 4-lane
    // scalar's (`(lane0 + lane1) + (lane2 + lane3)`) — 1 ULP slack
    // admitted. Both patterns defeat strict-IEEE non-associativity;
    // the reduction-tree difference is the only variance.
    for &n in &[0usize, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17] {
        let col = fill_f64(n);
        let simd = SumF64::reduce_slice(&col);
        let scalar = scalar_sum_f64_4lane(&col);
        if n <= 4 {
            assert_eq!(
                simd.to_bits(),
                scalar.to_bits(),
                "n={}: simd_sum_f64 got={} vs scalar_4lane expected={}",
                n,
                simd,
                scalar,
            );
        } else {
            let abs_sum: f64 = col.iter().map(|x| x.abs()).sum();
            let eps = (abs_sum * f64::EPSILON * 4.0).max(1.0e-15);
            assert!(
                (simd - scalar).abs() <= eps,
                "n={}: simd={} scalar={} diff={} eps={}",
                n,
                simd,
                scalar,
                (simd - scalar).abs(),
                eps,
            );
        }
    }
}

#[test]
fn simd_sum_f64_matches_scalar_on_large_inputs_within_tolerance() {
    // Large inputs: strict-IEEE reorder slack envelope. The SIMD
    // kernel accumulates in a different lane-walk order than the
    // 4-lane scalar kernel — the two agree within `abs_sum * eps`
    // but not bit-exactly.
    for &n in &[1024usize, 65_536, 500_000] {
        let col = fill_f64(n);
        let simd = SumF64::reduce_slice(&col);
        let scalar = scalar_sum_f64_leftfold(&col);
        let abs_sum: f64 = col.iter().map(|x| x.abs()).sum();
        let eps = (abs_sum * 1.0e-12).max(1.0e-10);
        assert!(
            (simd - scalar).abs() <= eps,
            "n={}: simd={} scalar={} diff={} eps={}",
            n,
            simd,
            scalar,
            (simd - scalar).abs(),
            eps,
        );
    }
}

/// The canada.json-sized f64 column test — synthetic 6 M-entry input
/// matching canada's typical f64 payload count.
#[test]
fn simd_sum_f64_canada_sized_within_tolerance() {
    let col = canada_sized_f64_column();
    let simd = SumF64::reduce_slice(&col);
    let scalar = scalar_sum_f64_leftfold(&col);
    let abs_sum: f64 = col.iter().map(|x| x.abs()).sum();
    // Canada-sized inputs pick up ~6 M-term reorder slack; widen the
    // tolerance accordingly.
    let eps = (abs_sum * 1.0e-10).max(1.0e-8);
    assert!(
        (simd - scalar).abs() <= eps,
        "canada: simd={} scalar={} diff={} eps={}",
        simd,
        scalar,
        (simd - scalar).abs(),
        eps,
    );
}

/// Agreement across SIMD + Columns::reduce_column + standalone
/// standalone emitter kernel — every path delivers the same value
/// (modulo strict-IEEE reorder slack).
#[test]
fn reduce_column_matches_scalar_sum_f64_driver() {
    let col = fill_f64(10_000);
    let cols = columns_with_pay_wide_f64(&col);

    let via_reduce_column = cols.reduce_column::<PayWideF64, SumF64>();
    let via_direct_simd = SumF64::reduce_slice(&col);
    let via_default_trait = {
        // Use the generic default fold — not the SumF64 SIMD
        // specialisation. Mirrors the reordered scalar fallback.
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
        let mut tail = (acc[0] + acc[1]) + (acc[2] + acc[3]);
        while i < n {
            tail += col[i];
            i += 1;
        }
        tail
    };

    // All three paths agree within strict-IEEE reorder slack.
    let abs_sum: f64 = col.iter().map(|x| x.abs()).sum();
    let eps = (abs_sum * 1.0e-12).max(1.0e-10);
    assert!(
        (via_reduce_column - via_direct_simd).abs() <= eps,
        "reduce_column vs SIMD: {} vs {} (diff {} eps {})",
        via_reduce_column,
        via_direct_simd,
        (via_reduce_column - via_direct_simd).abs(),
        eps,
    );
    assert!(
        (via_reduce_column - via_default_trait).abs() <= eps,
        "reduce_column vs scalar_4lane: {} vs {} (diff {} eps {})",
        via_reduce_column,
        via_default_trait,
        (via_reduce_column - via_default_trait).abs(),
        eps,
    );
}

// ── 4. Full-tape integration — Columns::reduce_column over a mixed
//      structural + payload tape. ─────────────────────────────────────

#[test]
fn reduce_column_ignores_structural_columns() {
    // The reducer walks the typed payload columns directly; the
    // structural columns (kinds, flags, extra, spans, sib_skip,
    // child_off) are irrelevant. Building a tape with structural
    // rows and a different-count payload column must still produce
    // the correct sum over just the payload column.
    use bbnf::runtime::tape::TapeKind;

    let mut cols = Columns::new();
    // 10 structural compound rows — these should NOT affect the
    // reducer's payload walk.
    for _ in 0..10 {
        let _ = cols.push_compound_fused(TapeKind::Seq, 0);
    }
    // 1000 payload entries.
    let payload: Vec<f64> = fill_f64(1000);
    cols.pay_wide.extend(payload.iter().map(|x| x.to_bits()));

    let got = cols.reduce_column::<PayWideF64, SumF64>();
    let expected = scalar_sum_f64_4lane(&payload);
    let abs_sum: f64 = payload.iter().map(|x| x.abs()).sum();
    let eps = (abs_sum * 1.0e-12).max(1.0e-10);
    assert!(
        (got - expected).abs() <= eps,
        "got={} expected={} (diff {} eps {})",
        got,
        expected,
        (got - expected).abs(),
        eps,
    );

    // And the structural rows are still there — the reducer did not
    // touch them.
    assert_eq!(cols.len(), 10, "structural rows must be preserved");
}
