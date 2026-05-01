//! BBNF JSON TypeScript backend benchmark — codegen throughput.
//!
//! Measures grammar → TS source compilation speed. For native TS parse throughput,
//! see the generated Node.js benchmark script at `benches/ts/json_bench.mjs`.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};
use divan::black_box;

fn load_grammar() -> String {
    std::fs::read_to_string("../../grammar/json/json.bbnf").expect("failed to read json.bbnf")
}

fn ts_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    }
}

// ── Codegen throughput ──────────────────────────────────────────────────────

#[divan::bench]
fn ts_codegen(b: divan::Bencher) {
    let grammar = load_grammar();
    {
        // Warmup / sanity check.
        let output = compile_grammar_request(&grammar, &ts_request()).unwrap();
        match &output {
            CompileOutput::Ts(src) => {
                assert!(
                    src.contains("export function parse"),
                    "missing parse export in TS output"
                );
            }
            _ => panic!("expected TS output"),
        }
    }
    b.with_inputs(|| grammar.clone()).bench_values(|grammar| {
        let output = compile_grammar_request(black_box(&grammar), &ts_request()).unwrap();
        match output {
            CompileOutput::Ts(src) => black_box(src.len()),
            _ => unreachable!(),
        }
    });
}

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
