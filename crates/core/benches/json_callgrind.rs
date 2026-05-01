//! iai-callgrind instruction-count bench — Linux CI only (requires valgrind).
//!
//! Gated behind the `callgrind` feature and Linux so local dev-hosts
//! (typically macOS) can compile the bench target as a no-op. Invoked by
//! `.github/workflows/bench-iai.yml` with
//! `cargo bench -p bbnf --bench json_callgrind --features callgrind` on a
//! Linux runner after `sudo apt-get install valgrind`.

#[cfg(not(all(feature = "callgrind", target_os = "linux")))]
fn main() {}

#[cfg(all(feature = "callgrind", target_os = "linux"))]
use bbnf::pipeline::{CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request};
#[cfg(all(feature = "callgrind", target_os = "linux"))]
use iai_callgrind::{library_benchmark, library_benchmark_group, main};

#[cfg(all(feature = "callgrind", target_os = "linux"))]
fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

#[cfg(all(feature = "callgrind", target_os = "linux"))]
#[library_benchmark]
fn compile_json_iai() -> bbnf::pipeline::CompiledProgram {
    let source = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../grammar/json/json.bbnf"
    ))
    .expect("grammar/json/json.bbnf read");
    compile_grammar_request(&source, &vm_request()).expect("compile_grammar_request")
}

#[cfg(all(feature = "callgrind", target_os = "linux"))]
library_benchmark_group!(
    name = compile;
    benchmarks = compile_json_iai
);

#[cfg(all(feature = "callgrind", target_os = "linux"))]
main!(library_benchmark_groups = compile);
