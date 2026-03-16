#![feature(cold_path)]

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use gorgeous::json::JsonParser;

fn load_json(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

fn compiled_json_vm() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar =
        std::fs::read_to_string("../../grammar/lang/json.bbnf").expect("failed to read json.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    (ir, program)
}

// ── AOT ─────────────────────────────────────────────────────────────────────

fn aot_data(b: &mut Bencher) {
    let input = load_json("data.json");
    let parser = JsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_twitter(b: &mut Bencher) {
    let input = load_json("twitter.json");
    let parser = JsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_citm_catalog(b: &mut Bencher) {
    let input = load_json("citm_catalog.json");
    let parser = JsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_canada(b: &mut Bencher) {
    let input = load_json("canada.json");
    let parser = JsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

// ── VM ──────────────────────────────────────────────────────────────────────

fn vm_data(b: &mut Bencher) {
    let input = load_json("data.json");
    let (_ir, program) = compiled_json_vm();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_twitter(b: &mut Bencher) {
    let input = load_json("twitter.json");
    let (_ir, program) = compiled_json_vm();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_citm_catalog(b: &mut Bencher) {
    let input = load_json("citm_catalog.json");
    let (_ir, program) = compiled_json_vm();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_canada(b: &mut Bencher) {
    let input = load_json("canada.json");
    let (_ir, program) = compiled_json_vm();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

benchmark_group!(aot_json, aot_data, aot_twitter, aot_citm_catalog, aot_canada);
benchmark_group!(vm_json, vm_data, vm_twitter, vm_citm_catalog, vm_canada);
benchmark_main!(aot_json, vm_json);
