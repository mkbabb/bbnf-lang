#![feature(cold_path)]

//! CSS parsing benchmarks.
//!
//! NOTE: The prettify grammar (`css-stylesheet-pretty.bbnf`) only consumes a
//! fraction of real-world stylesheets. Throughput is reported over the full
//! file size for consistency with the parse-that benchmark suite, but take the
//! numbers with a grain of salt. See `parse-that/benches/bbnf_css.rs` for the
//! fast grammar that covers 95%+ of bootstrap.css.

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use gorgeous::css::CssParser;

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

// ── AOT ─────────────────────────────────────────────────────────────────────

fn aot_normalize(b: &mut Bencher) {
    let input = load_css("normalize.css");
    let parser = CssParser::stylesheet();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_bootstrap(b: &mut Bencher) {
    let input = load_css("bootstrap.css");
    let parser = CssParser::stylesheet();
    b.bytes = input.len() as u64;
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

benchmark_group!(aot_css, aot_normalize, aot_bootstrap);
benchmark_group!(vm_css, vm_normalize, vm_bootstrap);
benchmark_main!(aot_css, vm_css);
