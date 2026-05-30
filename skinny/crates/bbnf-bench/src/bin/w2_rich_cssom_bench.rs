//! SK-V17 W2 — the FAIR rich-CSSOM materialization measurement. Reports the cold
//! per-parse MEDIAN Mbps of the tape-routed CSS Track-1 RICH typed path
//! (`parser::rich_summary`, which builds the offset tape and lazily projects the
//! rich typed CSSOM — selectors + typed value-node counts atop the 4 structural
//! fields) against the lightningcss full-CSSOM comparator, per benched corpus.
//!
//! This is the W2 fair-comparison crux: the RICH lazy CSSOM projection vs
//! lightningcss's full CSSOM materialization. The W1 4-field track is also timed
//! so the rich rider's overhead vs the minimal cursor is visible in the same run.
//!
//! Invocation:
//!     cargo run --release --bin w2_rich_cssom_bench [N]   (W0/W1-comparable: fat LTO, cu=1)
//! N defaults to 200, asserts N >= 50 (telemetry honesty).

use std::hint::black_box;
use std::time::Instant;

use bbnf_bench::css_l4_corpus::load_all;
use lightningcss::stylesheet::{ParserOptions, StyleSheet};
use runtime::generated_css_l4_declaration_values as track1;

fn median(mut samples: Vec<f64>) -> f64 {
    samples.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let mid = samples.len() / 2;
    if samples.len() % 2 == 0 {
        (samples[mid - 1] + samples[mid]) / 2.0
    } else {
        samples[mid]
    }
}

/// One cold per-parse sample: time a single parse, return Mbps.
fn cold_sample(bytes: usize, parse: &mut dyn FnMut()) -> f64 {
    let start = Instant::now();
    parse();
    let ns = start.elapsed().as_nanos() as f64;
    (bytes as f64) * 8_000.0 / ns
}

fn main() {
    let n: usize = std::env::args()
        .nth(1)
        .and_then(|a| a.parse().ok())
        .unwrap_or(200);
    assert!(n >= 50, "telemetry honesty: N must be >= 50 (got {n})");

    let corpora = load_all().expect("load CSS L4 corpora");
    println!(
        "SK-V17 W2 RICH-typed CSSOM Track-1 (cold, N={n}, median Mbps) vs lightningcss full-CSSOM"
    );
    println!(
        "{:<26} {:>14} {:>14} {:>16} {:>10}",
        "corpus", "track1_rich", "track1_4field", "lightningcss", "rich/lcss"
    );

    for corpus in &corpora {
        let source = std::str::from_utf8(&corpus.bytes).expect("utf8");
        let bytes = source.len();

        // Correctness pre-check: the rich tape path must parse the corpus.
        let _ = track1::parser::rich_summary(source).expect("rich tape parse");

        let mut rich_samples = Vec::with_capacity(n);
        for _ in 0..n {
            rich_samples.push(cold_sample(bytes, &mut || {
                let summary = track1::parser::rich_summary(black_box(source)).unwrap();
                black_box(summary);
            }));
        }

        let mut field4_samples = Vec::with_capacity(n);
        for _ in 0..n {
            field4_samples.push(cold_sample(bytes, &mut || {
                let summary = track1::parser::summary(black_box(source)).unwrap();
                black_box(summary);
            }));
        }

        let mut lcss_samples = Vec::with_capacity(n);
        for _ in 0..n {
            lcss_samples.push(cold_sample(bytes, &mut || {
                let sheet =
                    StyleSheet::parse(black_box(source), ParserOptions::default()).unwrap();
                black_box(sheet.rules.0.len());
            }));
        }

        let rich = median(rich_samples);
        let field4 = median(field4_samples);
        let lcss = median(lcss_samples);
        println!(
            "{:<26} {:>14.1} {:>14.1} {:>16.1} {:>9.3}x",
            corpus.spec.id, rich, field4, lcss, rich / lcss
        );
    }
}
