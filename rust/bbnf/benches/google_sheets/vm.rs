#![feature(cold_path)]

//! Google Sheets formula VM benchmark — bytecode interpreter.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};
use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;

const PATHOLOGICAL: &str = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))"#;

fn compiled() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar = std::fs::read_to_string("../../grammar/google-sheets/google-sheets.bbnf")
        .expect("failed to read google-sheets.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    (ir, program)
}

fn generate_large_formula(n_bindings: usize) -> String {
    let mut parts = Vec::with_capacity(n_bindings * 2 + 1);
    for i in 0..n_bindings {
        parts.push(format!("v{}", i));
        parts.push(format!(
            "IF(v{}>0, FILTER(A1:Z100, INDEX(A1:Z100,,{})>0), SUM(A1:A{}))",
            i, i + 1, i + 10
        ));
    }
    parts.push(format!("v{}", n_bindings - 1));
    format!("=LET({})", parts.join(", "))
}

fn compile(b: &mut Bencher) {
    let grammar = std::fs::read_to_string("../../grammar/google-sheets/google-sheets.bbnf")
        .expect("failed to read google-sheets.bbnf");
    b.bytes = grammar.len() as u64;
    b.iter(|| {
        let ir = compile_grammar(black_box(&grammar), &PipelineOptions::default()).unwrap();
        let _program = compile_bytecode(&ir);
    });
}

fn parse_pathological(b: &mut Bencher) {
    let (_ir, program) = compiled();
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(PATHOLOGICAL));
        let r = interp.run();
        assert!(r.success);
    });
}

fn parse_1kb(b: &mut Bencher) {
    let (_ir, program) = compiled();
    let input = generate_large_formula(10);
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn parse_10kb(b: &mut Bencher) {
    let (_ir, program) = compiled();
    let input = generate_large_formula(100);
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

benchmark_group!(benches, compile, parse_pathological, parse_1kb, parse_10kb);
benchmark_main!(benches);
