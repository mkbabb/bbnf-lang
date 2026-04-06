//! Benchmarks the BBNF compile pipeline — grammar → IR → passes.
//!
//! Measures compile-time (CSP passes, lowering, optimization) not parse-time.

use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

use bbnf::pipeline::{
    CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
    compile_paths_request,
};

fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

fn grammar_path(name: &str) -> std::path::PathBuf {
    std::path::PathBuf::from(format!("../../grammar/{}", name))
}

// JSON grammar (9 rules, ~30 lines — smallest useful grammar, no imports)
fn compile_json(b: &mut Bencher) {
    let source = std::fs::read_to_string(grammar_path("json/json.bbnf")).unwrap();
    b.iter(|| {
        black_box(compile_grammar_request(&source, &vm_request()).unwrap());
    });
}

// EBNF grammar (51 lines — medium complexity, no imports)
fn compile_ebnf(b: &mut Bencher) {
    let source = std::fs::read_to_string(grammar_path("ebnf/ebnf.bbnf")).unwrap();
    b.iter(|| {
        black_box(compile_grammar_request(&source, &vm_request()).unwrap());
    });
}

// BBNF grammar (80 lines — self-describing, uses @import)
fn compile_bbnf(b: &mut Bencher) {
    let path = grammar_path("bbnf/bbnf.bbnf");
    b.iter(|| {
        black_box(compile_paths_request(&[path.clone()], &vm_request()).unwrap());
    });
}

// Google Sheets (115 lines — real-world grammar, uses @import)
fn compile_sheets(b: &mut Bencher) {
    let path = grammar_path("google-sheets/google-sheets.bbnf");
    b.iter(|| {
        black_box(compile_paths_request(&[path.clone()], &vm_request()).unwrap());
    });
}

benchmark_group!(
    compile,
    compile_json,
    compile_ebnf,
    compile_bbnf,
    compile_sheets,
);
benchmark_main!(compile);
