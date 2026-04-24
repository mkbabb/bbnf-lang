
//! BBNF CSS L4 TypeScript backend benchmark — codegen throughput.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use divan::counter::BytesCount;

fn grammar_paths() -> Vec<PathBuf> {
    vec![PathBuf::from("../../grammar/css/l4/stylesheet.bbnf")]
}

fn ts_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    }
}

#[divan::bench]
fn ts_codegen(b: divan::Bencher) {
    let paths = grammar_paths();
    let bytes = {
        let output = compile_paths_request(&paths, &ts_request()).unwrap();
        match &output {
            CompileOutput::Ts(src) => {
                assert!(
                    src.contains("export function parse"),
                    "missing parse export"
                );
                BytesCount::new(src.len())
            }
            _ => panic!("expected TS output"),
        }
    };
    b.counter(bytes).bench_local(|| {
        let output = compile_paths_request(&paths, &ts_request()).unwrap();
        match output {
            CompileOutput::Ts(src) => divan::black_box(src.len()),
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
