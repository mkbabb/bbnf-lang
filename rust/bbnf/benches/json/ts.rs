#![feature(cold_path)]

//! BBNF JSON TypeScript backend benchmark — codegen throughput.
//!
//! Measures grammar → TS source compilation speed. For native TS parse throughput,
//! see the generated Node.js benchmark script at `benches/ts/json_bench.mjs`.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

fn load_grammar() -> String {
    std::fs::read_to_string("../../grammar/json/json.bbnf")
        .expect("failed to read json.bbnf")
}

fn ts_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    }
}

// ── Codegen throughput ──────────────────────────────────────────────────────

fn ts_codegen(b: &mut Bencher) {
    let grammar = load_grammar();
    b.bytes = grammar.len() as u64;
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
    b.iter(|| {
        let output = compile_grammar_request(black_box(&grammar), &ts_request()).unwrap();
        match output {
            CompileOutput::Ts(src) => black_box(src.len()),
            _ => unreachable!(),
        }
    });
}

benchmark_group!(benches, ts_codegen);
benchmark_main!(benches);
