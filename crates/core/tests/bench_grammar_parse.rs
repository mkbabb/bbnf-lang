//! Grammar parser benchmarks — generated (self-hosted) parser only.
//!
//! The hand-written combinator parser has been removed; only the generated
//! tape-first bootstrap parser remains.
//!
//! Run: cargo test -p bbnf --test bench_grammar_parse -- --nocapture

use std::time::Instant;

/// The self-hosted BBNF parser (generated from bbnf.bbnf via proc-macro).
use ::bbnf::grammar::generated::bbnf::*;

fn load_grammar(name: &str) -> &'static str {
    let candidates = [format!("grammar/{name}"), format!("../../grammar/{name}")];
    for path in &candidates {
        if let Ok(text) = std::fs::read_to_string(path) {
            return Box::leak(text.into_boxed_str());
        }
    }
    panic!("Failed to load grammar: {name}");
}

#[test]
fn bench_generated_parser() {
    let grammars = [
        ("json/json.bbnf", "JSON"),
        ("ebnf/ebnf.bbnf", "EBNF"),
        ("bnf/bnf.bbnf", "BNF"),
        ("misc/csv.bbnf", "CSV"),
    ];

    for (path, label) in &grammars {
        let source = load_grammar(path);
        let iterations = 2000;

        // ── Generated parser ─────────────────────────────────────
        {
            // Warm-up.
            let _ = BbnfBootstrap::parse(source);

            let start = Instant::now();
            for _ in 0..iterations {
                let _ = BbnfBootstrap::parse(source);
            }
            let elapsed = start.elapsed();
            let per_parse = elapsed / iterations as u32;
            let throughput =
                (source.len() as f64 * iterations as f64) / elapsed.as_secs_f64() / 1_000_000.0;
            eprintln!(
                "{label:>6}    generated: {per_parse:>8.1?}  ({throughput:.1} MB/s, {len} bytes)",
                len = source.len()
            );
        }

        eprintln!();
    }
}
