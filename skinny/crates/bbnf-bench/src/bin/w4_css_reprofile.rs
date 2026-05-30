//! SK-V17 W4 conditional re-profile harness.
//!
//! Drives the post-W1/W2/W3 rich-typed CSS Track-1 recognizer spine
//! (`parser::rich_summary` — build offset tape + lazily project the rich typed
//! CSSOM) in a tight cold loop so a real profiler (samply) can attribute
//! self-time. The W4 conditional gate (SPEC §7 / SYNTHESIS §3 L9) asks whether a
//! speculative checkpoint/rollback or recognition-control loop is a meaningful
//! self-time share on this plane; this bin is the workload samply records.
//!
//! Invocation:
//!     cargo build --release -p bbnf-bench --bin w4_css_reprofile
//!     samply record ./target/release/w4_css_reprofile <iters> <corpus>
//! corpus default = bootstrap (the W3-identified bracket-scan-richest corpus).

use std::hint::black_box;
use std::time::Instant;

use bbnf_bench::css_l4_corpus::load_all;
use runtime::generated_css_l4_declaration_values as track1;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let iters: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(20_000);
    let want = args.get(2).map(String::as_str).unwrap_or("bootstrap");

    let corpora = load_all().expect("load CSS L4 corpora");
    let corpus = corpora
        .iter()
        .find(|c| c.spec.id.contains(want))
        .unwrap_or_else(|| panic!("corpus {want} not found"));
    let source = std::str::from_utf8(&corpus.bytes).expect("utf8");
    let bytes = source.len();

    // Correctness pre-check (the rich tape path must parse the corpus).
    let pre = track1::parser::rich_summary(source).expect("rich tape parse");
    eprintln!(
        "w4-reprofile: corpus={} bytes={bytes} rules={} decls={} sel={} iters={iters}",
        corpus.spec.id, pre.rules, pre.declarations, pre.selectors
    );

    let start = Instant::now();
    let mut acc = 0u64;
    for _ in 0..iters {
        let s = track1::parser::rich_summary(black_box(source)).unwrap();
        acc ^= s.rules as u64 ^ (s.declarations as u64) << 8 ^ (s.selectors as u64) << 16;
    }
    let elapsed = start.elapsed();
    let total = (bytes as u128) * (iters as u128);
    let mbps = (total as f64 * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0);
    eprintln!(
        "w4-reprofile: {iters} iters in {:.2}s -> {:.0} Mbps (acc {acc})",
        elapsed.as_secs_f64(),
        mbps
    );
    println!("W4_REPROFILE corpus={} iters={iters} mbps={mbps:.1}", corpus.spec.id);
}
