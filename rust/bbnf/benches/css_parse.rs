#![feature(cold_path)]

//! CSS parsing benchmarks.
//!
//! Throughput is reported over *consumed* bytes, not total file size.
//! The prettify grammar may not cover 100% of every stylesheet.

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use gorgeous::css::CssParser;
use parse_that::Parser;

/// Parse and return actual bytes consumed (for accurate throughput).
fn aot_consumed_bytes(input: &str) -> u64 {
    let parser = CssParser::stylesheet();
    let (_result, state) = parser.parse_return_state(input);
    state.offset as u64
}

// ── AOT ─────────────────────────────────────────────────────────────────────

fn aot_normalize(b: &mut Bencher) {
    let input = load_css("normalize.css");
    let parser = CssParser::stylesheet();
    b.bytes = aot_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_bootstrap(b: &mut Bencher) {
    let input = load_css("bootstrap.css");
    let parser = CssParser::stylesheet();
    b.bytes = aot_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_tailwind(b: &mut Bencher) {
    let input = load_css("tailwind.css");
    let parser = CssParser::stylesheet();
    b.bytes = aot_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

// ── VM ──────────────────────────────────────────────────────────────────────

fn vm_normalize(b: &mut Bencher) {
    let input = load_css("normalize.css");
    let (_ir, program) = compiled_css_vm();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_bootstrap(b: &mut Bencher) {
    let input = load_css("bootstrap.css");
    let (_ir, program) = compiled_css_vm();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_tailwind(b: &mut Bencher) {
    let input = load_css("tailwind.css");
    let (_ir, program) = compiled_css_vm();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

// ── Helpers ─────────────────────────────────────────────────────────────────

fn load_css(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

fn compiled_css_vm() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar = std::fs::read_to_string("../../grammar/css/css-stylesheet-pretty.bbnf")
        .expect("failed to read css-stylesheet-pretty.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    (ir, program)
}

benchmark_group!(aot_css, aot_normalize, aot_bootstrap, aot_tailwind);
benchmark_group!(vm_css, vm_normalize, vm_bootstrap, vm_tailwind);
benchmark_main!(aot_css, vm_css);
