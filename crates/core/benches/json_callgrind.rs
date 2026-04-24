#![cfg(feature = "iai")]

//! iai-callgrind instruction-count bench — Linux CI only (requires valgrind).
//!
//! Gated behind the `iai` feature so local dev-hosts (typically macOS) do not
//! try to compile iai-callgrind. Invoked by `.github/workflows/bench-iai.yml`
//! with `cargo bench --bench json_callgrind --features iai` on a Linux runner
//! after `sudo apt-get install valgrind`.

use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};
use iai_callgrind::{library_benchmark, library_benchmark_group, main};

fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

#[library_benchmark]
fn compile_json_iai() -> bbnf::pipeline::CompiledProgram {
    let source = std::fs::read_to_string(
        concat!(env!("CARGO_MANIFEST_DIR"), "/../../grammar/json/json.bbnf"),
    )
    .expect("grammar/json/json.bbnf read");
    compile_grammar_request(&source, &vm_request()).expect("compile_grammar_request")
}

library_benchmark_group!(
    name = compile;
    benchmarks = compile_json_iai
);

main!(library_benchmark_groups = compile);
