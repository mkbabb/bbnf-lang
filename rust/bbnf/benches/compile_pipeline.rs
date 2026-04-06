//! Benchmarks the BBNF compile pipeline — grammar → IR → passes.
//!
//! Measures compile-time (CSP passes, lowering, optimization) not parse-time.

use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

use bbnf::pipeline::{
    CompileOutput, CompileRequest, CompileTarget, PipelineOptions, compile_grammar_request,
};

fn load_grammar(name: &str) -> String {
    let path = format!("../../grammar/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

fn vm_request() -> CompileRequest {
    CompileRequest {
        options: PipelineOptions::default(),
        target: CompileTarget::Vm,
    }
}

// JSON grammar (9 rules, ~30 lines — smallest useful grammar)
fn compile_json(b: &mut Bencher) {
    let source = load_grammar("json/json.bbnf");
    b.iter(|| {
        black_box(compile_grammar_request(&source, &vm_request()).unwrap());
    });
}

// EBNF grammar (51 lines — medium complexity)
fn compile_ebnf(b: &mut Bencher) {
    let source = load_grammar("ebnf/ebnf.bbnf");
    b.iter(|| {
        black_box(compile_grammar_request(&source, &vm_request()).unwrap());
    });
}

// BBNF grammar (80 lines — self-describing, moderate complexity)
fn compile_bbnf(b: &mut Bencher) {
    let source = load_grammar("bbnf/bbnf.bbnf");
    b.iter(|| {
        black_box(compile_grammar_request(&source, &vm_request()).unwrap());
    });
}

// Google Sheets (115 lines — real-world grammar)
fn compile_sheets(b: &mut Bencher) {
    let source = load_grammar("google-sheets/google-sheets.bbnf");
    b.iter(|| {
        black_box(compile_grammar_request(&source, &vm_request()).unwrap());
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
