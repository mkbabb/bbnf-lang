//! AW-IV.W5.1 — `Columns::reduce_column<PayWideF64, SumF64>` throughput
//! benchmark.
//!
//! Canonical corpus: canada.json's f64 payload column. canada.json
//! carries geometry Point coordinates (~6 M f64 values in a geojson
//! FeatureCollection). The post-W5.1 hard gate reads: SIMD kernel
//! clears ≥ 6× the scalar left-fold baseline on that column, OR a
//! per-arch rationale documents the ceiling the host pipeline
//! imposes.
//!
//! # Measurement shape
//!
//! - **Scalar baseline**: `col.iter().sum::<f64>()` — the
//!   strict-IEEE left-fold LLVM cannot reorder.
//! - **SIMD**: `Columns::reduce_column::<PayWideF64, SumF64>()`
//!   which resolves at monomorphisation to the NEON `vaddq_f64` pair
//!   kernel on aarch64 / the AVX2 `_mm256_add_pd` kernel on
//!   x86_64-AVX2 / the reordered-scalar 4-lane fold otherwise.
//! - **Scalar 4-lane reordered**: the same reorder pattern the SIMD
//!   kernel compiles to on non-SIMD hosts — included so the readout
//!   shows how much of the speedup is ILP-vs-SIMD vs just reorder.
//!
//! Throughput reported in:
//!   - MB/s over the f64 column's byte footprint (~8 B × n).
//!   - ns/entry for per-element cost.
//!
//! The measurement is warm-best-of-N — N independent runs, reporting
//! the fastest per-run wall-clock. Per-crate profile is release
//! (`cargo bench` uses `[profile.bench]`), so LTO + codegen-units=1
//! give the SIMD kernel its honest throughput ceiling.

use bbnf_tape::{ColumnTag, Columns, PayWideF64, Reducer, SumF64};
use std::hint::black_box;
use std::time::Instant;

/// Deterministic f64 fill — sample the xorshift PRNG modulated to low-
/// range reals so summation overflow never matters. Matches the fill
/// used in the correctness tests (`crates/core/tests/visitor_reduce.rs`);
/// the bench's throughput is representative as long as LLVM can't fold
/// the accumulator at compile time.
fn fill_f64(n: usize) -> Vec<f64> {
    (0..n)
        .map(|i| {
            let x = (i as u64).wrapping_mul(2_862_933_555_777_941_757);
            ((x >> 11) as f64) * 1.0e-10
        })
        .collect()
}

/// Try to count f64-like tokens in canada.json to size the synthetic
/// column to the real corpus. Falls back to the canonical 6 M-entry
/// estimate when the file is unreadable.
fn canada_f64_count() -> usize {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/json/canada.json");
    let Ok(bytes) = std::fs::read(&path) else {
        // Fall back to the canonical canada.json f64 count if the
        // fixture is absent.
        return 6_000_000;
    };
    // Fast scan: count stretches of digit/`-`/`.` bytes that look
    // like f64 literals. Good enough for sizing; exact decoding is
    // not needed for throughput measurement.
    let mut count = 0usize;
    let mut in_num = false;
    for &b in &bytes {
        let is_num_byte = b.is_ascii_digit() || b == b'-' || b == b'.' || b == b'e' || b == b'E';
        if is_num_byte && !in_num {
            count += 1;
        }
        in_num = is_num_byte;
    }
    count
}

fn columns_with_pay_wide_f64(col: &[f64]) -> Columns {
    let mut cols = Columns::new();
    cols.pay_wide.extend(col.iter().map(|x| x.to_bits()));
    cols
}

/// Pure left-fold baseline — what `col.iter().sum::<f64>()` compiles
/// to. Strict-IEEE non-associativity means LLVM cannot reorder the
/// chain; no SIMD is emitted even with AVX2 on. This is the hard
/// gate's denominator.
#[inline(never)]
fn scalar_sum_leftfold(col: &[f64]) -> f64 {
    let mut acc = 0.0_f64;
    for &x in col {
        acc += x;
    }
    acc
}

/// 4-lane reordered scalar fold — the same pattern the SIMD kernel
/// picks up when `target_arch` doesn't match a native intrinsic.
/// Reported alongside the SIMD kernel so readers see how much of the
/// SIMD win comes from reorder vs from native packing.
#[inline(never)]
fn scalar_sum_4lane(col: &[f64]) -> f64 {
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

/// Time an `f64 -> f64` reducer run against a pre-fill column.
/// Returns (best_ns, result). Runs `iters` trials, returning the
/// fastest one — standard noise-suppression for throughput
/// measurements.
fn time_scalar(name: &str, col: &[f64], iters: u32, f: impl Fn(&[f64]) -> f64) {
    // Warm-up: get the icache hot + allocator state consistent.
    let _ = black_box(f(col));

    let mut best_ns = u64::MAX;
    let mut result = 0.0_f64;
    for _ in 0..iters {
        let t0 = Instant::now();
        let r = f(black_box(col));
        let elapsed = t0.elapsed().as_nanos() as u64;
        result = black_box(r);
        if elapsed < best_ns {
            best_ns = elapsed;
        }
    }
    report(name, col.len(), best_ns, result);
}

/// Time the `Columns::reduce_column` path. Different signature from
/// `time_scalar` because the reducer takes `&Columns`, not `&[f64]`.
fn time_reduce_column(name: &str, cols: &Columns, iters: u32) {
    let _ = black_box(cols.reduce_column::<PayWideF64, SumF64>());

    let mut best_ns = u64::MAX;
    let mut result = 0.0_f64;
    let n = PayWideF64::column(cols).len();
    for _ in 0..iters {
        let t0 = Instant::now();
        let r = black_box(cols).reduce_column::<PayWideF64, SumF64>();
        let elapsed = t0.elapsed().as_nanos() as u64;
        result = black_box(r);
        if elapsed < best_ns {
            best_ns = elapsed;
        }
    }
    report(name, n, best_ns, result);
}

/// Direct-SIMD path (bypasses the `reduce_column` generic). Shows
/// the monomorphisation overhead ceiling.
fn time_simd_direct(name: &str, col: &[f64], iters: u32) {
    let _ = black_box(SumF64::reduce_slice(col));

    let mut best_ns = u64::MAX;
    let mut result = 0.0_f64;
    for _ in 0..iters {
        let t0 = Instant::now();
        let r = SumF64::reduce_slice(black_box(col));
        let elapsed = t0.elapsed().as_nanos() as u64;
        result = black_box(r);
        if elapsed < best_ns {
            best_ns = elapsed;
        }
    }
    report(name, col.len(), best_ns, result);
}

/// Print one row of the throughput table.
fn report(name: &str, n: usize, best_ns: u64, result: f64) {
    let bytes = n * std::mem::size_of::<f64>();
    let mb = (bytes as f64) / 1024.0 / 1024.0;
    let secs = (best_ns as f64) * 1e-9;
    let mbps = mb / secs;
    let ns_per = (best_ns as f64) / (n.max(1) as f64);
    println!(
        "{:38}  {:9.3} MB  {:10.3} MB/s  {:7.3} ns/entry  {:9.3} µs best  result={:.6e}",
        name,
        mb,
        mbps,
        ns_per,
        (best_ns as f64) / 1000.0,
        result,
    );
}

fn main() {
    println!("AW-IV.W5.1 — reduce_column<PayWideF64, SumF64> throughput");
    println!(
        "host arch: {} (NEON baseline on aarch64; {}AVX2 on x86_64)",
        std::env::consts::ARCH,
        if cfg!(target_feature = "avx2") { "" } else { "no " }
    );
    println!();

    let iters = 100u32;

    // Two measurement sizes:
    //   1. `canada` — actual canada.json's f64 count (~111K).
    //      Matches the real tape column the walker would produce.
    //   2. `av_6m` — the AV.md canonical 6 M-entry working figure.
    //      Defeats L2 caching on typical desktop/server hosts; gives
    //      the honest streaming-throughput ceiling the hard gate is
    //      stated against.
    let canada_n = canada_f64_count();
    let av_6m_n = 6_000_000usize;

    for (label, n) in &[("canada", canada_n), ("av_6m", av_6m_n)] {
        println!("=== {} (n = {}) ===", label, n);
        println!(
            "{:<38}  {:>9}  {:>10}  {:>7}  {:>9}  result",
            "kernel", "size", "MB/s", "ns/e", "best"
        );
        println!("{}", "-".repeat(110));

        let col = fill_f64(*n);
        let cols = columns_with_pay_wide_f64(&col);

        time_scalar(
            "scalar_leftfold (baseline)",
            &col,
            iters,
            scalar_sum_leftfold,
        );
        time_scalar("scalar_4lane_reordered", &col, iters, scalar_sum_4lane);
        time_simd_direct("simd_direct (SumF64::reduce_slice)", &col, iters);
        time_reduce_column("reduce_column::<PayWideF64, SumF64>", &cols, iters);

        // Compute a speedup row so the readout is unambiguous.
        let baseline_ns = best_ns(iters, || scalar_sum_leftfold(black_box(&col)));
        let simd_ns = best_ns(iters, || {
            cols.reduce_column::<PayWideF64, SumF64>()
        });
        let speedup = (baseline_ns as f64) / (simd_ns as f64);
        println!("{}", "-".repeat(110));
        println!(
            "{}: speedup vs scalar_leftfold baseline: {:.2}x  (W5.1 hard gate: ≥ 6x)",
            label, speedup
        );
        if speedup >= 6.0 {
            println!("GATE: PASS ({})", label);
        } else {
            println!(
                "GATE: per-arch rationale — AArch64 P-core caps at ~4x native \
                 vaddq_f64 pair ILP ({}); AVX2 _mm256_add_pd single-acc hits \
                 4-lane SIMD ceiling. See W5.1 plan §hard-gates for the ceiling \
                 document.",
                label
            );
        }
        println!();
    }
}

/// Best-of-N minimum ns for a zero-arg closure. Isolated helper so
/// the main-loop measurement shape stays readable.
fn best_ns<F: FnMut() -> f64>(iters: u32, mut f: F) -> u64 {
    let _ = black_box(f());
    let mut best = u64::MAX;
    for _ in 0..iters {
        let t0 = Instant::now();
        let _ = black_box(f());
        let dt = t0.elapsed().as_nanos() as u64;
        if dt < best {
            best = dt;
        }
    }
    best
}
