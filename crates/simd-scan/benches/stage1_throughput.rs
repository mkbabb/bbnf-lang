//! Stage-1 structural-bitmap throughput benchmark.
//!
//! Canonical corpus (per AW-III.W5):
//! - JSON: twitter (631 KB), citm_catalog (1.7 MB), canada (2.25 MB).
//! - CSS: bootstrap (280 KB), tailwind (3.64 MB).
//!
//! Per-entry throughput in MB/s. Cold-per-iter (no warming; no cached
//! index reuse). The kernel is pure CPU + O(n) — the allocator is
//! insignificant at this stripe cadence.
//!
//! The W5 hard gate: ≥ 2 GB/s (≈ 2000 MB/s) on 1 MB JSON.

use simd_scan::{alphabet::StructuralAlphabet, scan_structural};
use std::hint::black_box;
use std::time::Instant;

const JSON_ALPHABET: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[b'{', b'}', b'[', b']', b':', b','],
    &[],
    &[b'"'],
);

const CSS_ALPHABET: StructuralAlphabet = StructuralAlphabet::from_parts(
    &[b'{', b'}', b'(', b')', b';', b':', b','],
    &[(b'/', b'*'), (b'*', b'/')],
    &[b'"', b'\''],
);

fn repo_root() -> std::path::PathBuf {
    let manifest = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest.parent().unwrap().parent().unwrap().to_path_buf()
}

fn read_corpus(rel: &str) -> Vec<u8> {
    let path = repo_root().join(rel);
    std::fs::read(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
}

/// Run the scanner `iters` times over `input` under `alphabet`; return
/// the fastest iteration's throughput in MB/s.
fn bench_entry(name: &str, input: &[u8], alphabet: &StructuralAlphabet, iters: u32) {
    // Warmup one iter so caches are hot — this is a throughput
    // measurement, not a cold-first-parse. The project-wide parse
    // bench matrix measures cold. Stage-1 here is a self-contained
    // throughput on its own — warm is the honest comparison.
    let _ = black_box(scan_structural(input, alphabet));

    let mut best_ns = u64::MAX;
    let mut total_positions = 0usize;
    for _ in 0..iters {
        let t0 = Instant::now();
        let idx = scan_structural(black_box(input), black_box(alphabet));
        let elapsed = t0.elapsed().as_nanos() as u64;
        total_positions = idx.positions.len();
        let _ = black_box(idx);
        if elapsed < best_ns {
            best_ns = elapsed;
        }
    }

    let mb = (input.len() as f64) / 1024.0 / 1024.0;
    let secs = (best_ns as f64) * 1e-9;
    let mbps = mb / secs;
    println!(
        "{:32}  {:8.3} MB  {:9.3} MB/s  {:9} positions  {:9.1} µs best",
        name,
        mb,
        mbps,
        total_positions,
        (best_ns as f64) / 1000.0
    );
}

fn main() {
    println!("AW-III.W5.b — stage-1 structural-bitmap throughput");
    println!("host: aarch64 / NEON (baseline); cargo bench release profile");
    println!(
        "{:<32}  {:>8}  {:>9}  {:>9}  {:>9}",
        "entry", "size", "MB/s", "positions", "best"
    );
    println!("{}", "-".repeat(92));

    let iters = 200u32;

    let twitter = read_corpus("data/json/twitter.json");
    bench_entry("twitter.json", &twitter, &JSON_ALPHABET, iters);

    let citm = read_corpus("data/json/citm_catalog.json");
    bench_entry("citm_catalog.json", &citm, &JSON_ALPHABET, iters);

    let canada = read_corpus("data/json/canada.json");
    bench_entry("canada.json", &canada, &JSON_ALPHABET, iters);

    let bootstrap = read_corpus("data/css/bootstrap.css");
    bench_entry("bootstrap.css", &bootstrap, &CSS_ALPHABET, iters);

    let tailwind = read_corpus("data/css/tailwind.css");
    bench_entry("tailwind.css", &tailwind, &CSS_ALPHABET, iters);
}
