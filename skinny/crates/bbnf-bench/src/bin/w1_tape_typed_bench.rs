//! SK-V17 W1 — informational tape-typed Track-1 measurement (NOT a speed
//! admission; W1 is equality-before-speed). Reports the cold per-parse MEDIAN
//! Mbps of the tape-routed CSS Track-1 typed path (`parser::summary`, which
//! builds the offset tape and lazily projects the 4-field structural summary)
//! against the lightningcss full-CSSOM comparator, per benched corpus.
//!
//! Invocation:
//!     cargo run --profile ax-iter -p bbnf-bench --bin w1_tape_typed_bench [N]
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
        "SK-V17 W1 tape-typed Track-1 (cold, N={n}, median Mbps) vs lightningcss full-CSSOM"
    );
    println!(
        "{:<26} {:>14} {:>16} {:>10}",
        "corpus", "track1_typed", "lightningcss", "ratio"
    );

    for corpus in &corpora {
        let source = std::str::from_utf8(&corpus.bytes).expect("utf8");
        let bytes = source.len();

        // Correctness pre-check: the tape path must parse the corpus.
        let _ = track1::parser::summary(source).expect("tape parse");

        let mut track1_samples = Vec::with_capacity(n);
        for _ in 0..n {
            track1_samples.push(cold_sample(bytes, &mut || {
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

        let t = median(track1_samples);
        let l = median(lcss_samples);
        println!(
            "{:<26} {:>14.1} {:>16.1} {:>9.3}x",
            corpus.spec.id, t, l, t / l
        );
    }
}
