//! Grammar parser benchmarks — generated (self-hosted) parser only.
//!
//! The hand-written combinator parser has been removed; only the generated
//! bootstrap parser remains.
//!
//! Run: cargo test -p bbnf --test bench_grammar_parse -- --nocapture

use std::time::Instant;

use bbnf_derive::Parser;

/// The self-hosted BBNF parser (generated from bbnf.bbnf via proc-macro).
#[derive(Parser)]
#[parser(path = "../../grammar/bbnf/bbnf.bbnf")]
struct BbnfGenerated;

fn load_grammar(name: &str) -> &'static str {
    let candidates = [
        format!("grammar/{name}"),
        format!("../../grammar/{name}"),
    ];
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
            let ctx = __BbnfGeneratedEnumCtx::with_capacity(source.len() / 8);
            let _ = BbnfGenerated::grammar().parse_return_state_with_context(source, &ctx); // warm

            let start = Instant::now();
            for _ in 0..iterations {
                let ctx = __BbnfGeneratedEnumCtx::with_capacity(source.len() / 8);
                let _ = BbnfGenerated::grammar().parse_return_state_with_context(source, &ctx);
            }
            let elapsed = start.elapsed();
            let per_parse = elapsed / iterations as u32;
            let throughput = (source.len() as f64 * iterations as f64)
                / elapsed.as_secs_f64()
                / 1_000_000.0;
            eprintln!(
                "{label:>6}    generated: {per_parse:>8.1?}  ({throughput:.1} MB/s, {len} bytes)",
                len = source.len()
            );
        }

        eprintln!();
    }
}
