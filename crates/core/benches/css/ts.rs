
//! BBNF CSS L4 TypeScript backend benchmark — codegen throughput.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::path::PathBuf;

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_paths_request,
};
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

fn grammar_paths() -> Vec<PathBuf> {
    vec![PathBuf::from("../../grammar/css/l4/stylesheet.bbnf")]
}

fn ts_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Ts,
    }
}

fn ts_codegen(b: &mut Bencher) {
    let paths = grammar_paths();
    b.bytes = 0; // grammar input size not meaningful for codegen bench
    {
        let output = compile_paths_request(&paths, &ts_request()).unwrap();
        match &output {
            CompileOutput::Ts(src) => {
                assert!(
                    src.contains("export function parse"),
                    "missing parse export"
                );
                b.bytes = src.len() as u64;
            }
            _ => panic!("expected TS output"),
        }
    }
    b.iter(|| {
        let output = compile_paths_request(&paths, &ts_request()).unwrap();
        match output {
            CompileOutput::Ts(src) => black_box(src.len()),
            _ => unreachable!(),
        }
    });
}

benchmark_group!(benches, ts_codegen);
benchmark_main!(benches);
