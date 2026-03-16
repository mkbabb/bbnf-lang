#![feature(cold_path)]

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use gorgeous::google_sheets::{prettify_formula, GoogleSheetsParser};
use gorgeous::PrinterConfig;
use pprint::{pprint as render, pprint_ref};

// ── Formulas ────────────────────────────────────────────────────────────────

const PATHOLOGICAL: &str = r#"=LET(raw, A2:E1000, filtered, FILTER(raw, (INDEX(raw,,3)>100)*(INDEX(raw,,5)="Active")), sorted, SORT(filtered, 3, FALSE), IF(ROWS(sorted)>0, MAP(SEQUENCE(MIN(10, ROWS(sorted))), LAMBDA(i, INDEX(sorted, i, 1)&" - "&TEXT(INDEX(sorted, i, 3), "$#,##0"))), "No results"))"#;

// ── Helpers ─────────────────────────────────────────────────────────────────

fn compiled() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar = std::fs::read_to_string("../../grammar/lang/google-sheets.bbnf")
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
            i,
            i + 1,
            i + 10
        ));
    }
    parts.push(format!("v{}", n_bindings - 1));
    format!("=LET({})", parts.join(", "))
}

// ── VM Benchmarks ──────────────────────────────────────────────────────────

fn vm_compile(b: &mut Bencher) {
    let grammar = std::fs::read_to_string("../../grammar/lang/google-sheets.bbnf")
        .expect("failed to read google-sheets.bbnf");
    b.bytes = grammar.len() as u64;
    b.iter(|| {
        let ir = compile_grammar(black_box(&grammar), &PipelineOptions::default()).unwrap();
        let _program = compile_bytecode(&ir);
    });
}

fn vm_parse_pathological(b: &mut Bencher) {
    let (_ir, program) = compiled();
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(PATHOLOGICAL));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_parse_1kb(b: &mut Bencher) {
    let (_ir, program) = compiled();
    let input = generate_large_formula(10);
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_parse_10kb(b: &mut Bencher) {
    let (_ir, program) = compiled();
    let input = generate_large_formula(100);
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
    });
}

fn vm_format_pathological(b: &mut Bencher) {
    let (ir, program) = compiled();
    let printer = pprint::Printer::new(80, 2, false);
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(PATHOLOGICAL));
        let r = interp.run();
        assert!(r.success);
        let formatted = gorgeous::vm::format_value(
            &ir,
            r.value.as_ref().unwrap(),
            PATHOLOGICAL,
            printer,
        );
        assert!(formatted.is_some());
    });
}

fn vm_format_1kb(b: &mut Bencher) {
    let (ir, program) = compiled();
    let printer = pprint::Printer::new(80, 2, false);
    let input = generate_large_formula(10);
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut interp = Interpreter::new(&program, black_box(&input));
        let r = interp.run();
        assert!(r.success);
        let formatted = gorgeous::vm::format_value(
            &ir,
            r.value.as_ref().unwrap(),
            &input,
            printer,
        );
        assert!(formatted.is_some());
    });
}

// ── AOT Benchmarks ─────────────────────────────────────────────────────────

fn aot_parse_pathological(b: &mut Bencher) {
    let parser = GoogleSheetsParser::formula();
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| parser.parse(black_box(PATHOLOGICAL)).unwrap());
}

fn aot_parse_1kb(b: &mut Bencher) {
    let input = generate_large_formula(10);
    let parser = GoogleSheetsParser::formula();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_parse_10kb(b: &mut Bencher) {
    let input = generate_large_formula(100);
    let parser = GoogleSheetsParser::formula();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_format_pathological(b: &mut Bencher) {
    let config = PrinterConfig::new(80, 2);
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| prettify_formula(black_box(PATHOLOGICAL), &config).unwrap());
}

fn aot_format_pathological_cached(b: &mut Bencher) {
    let config = PrinterConfig::new(80, 2);
    let parser = GoogleSheetsParser::formula();
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| {
        let ast = parser.parse(black_box(PATHOLOGICAL)).unwrap();
        render(ast.to_doc(), config.to_printer())
    });
}

fn aot_format_1kb(b: &mut Bencher) {
    let input = generate_large_formula(10);
    let config = PrinterConfig::new(80, 2);
    b.bytes = input.len() as u64;
    b.iter(|| prettify_formula(black_box(&input), &config).unwrap());
}

fn aot_format_1kb_cached(b: &mut Bencher) {
    let input = generate_large_formula(10);
    let config = PrinterConfig::new(80, 2);
    let parser = GoogleSheetsParser::formula();
    b.bytes = input.len() as u64;
    b.iter(|| {
        let ast = parser.parse(black_box(&input)).unwrap();
        render(ast.to_doc(), config.to_printer())
    });
}

fn aot_format_10kb(b: &mut Bencher) {
    let input = generate_large_formula(100);
    let config = PrinterConfig::new(80, 2);
    b.bytes = input.len() as u64;
    b.iter(|| prettify_formula(black_box(&input), &config).unwrap());
}

// ── AOT phase-split benchmarks ─────────────────────────────────────────────

fn aot_pathological_to_doc_only(b: &mut Bencher) {
    let parser = GoogleSheetsParser::formula();
    let ast = parser.parse(PATHOLOGICAL).unwrap();
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| ast.to_doc());
}

fn aot_pathological_render_only(b: &mut Bencher) {
    let config = PrinterConfig::new(80, 2);
    let parser = GoogleSheetsParser::formula();
    let ast = parser.parse(PATHOLOGICAL).unwrap();
    let doc = ast.to_doc();
    b.bytes = PATHOLOGICAL.len() as u64;
    b.iter(|| pprint_ref(&doc, config.to_printer()));
}

fn aot_1kb_to_doc_only(b: &mut Bencher) {
    let input = generate_large_formula(10);
    let parser = GoogleSheetsParser::formula();
    let ast = parser.parse(&input).unwrap();
    b.bytes = input.len() as u64;
    b.iter(|| ast.to_doc());
}

fn aot_1kb_render_only(b: &mut Bencher) {
    let input = generate_large_formula(10);
    let config = PrinterConfig::new(80, 2);
    let parser = GoogleSheetsParser::formula();
    let ast = parser.parse(&input).unwrap();
    let doc = ast.to_doc();
    b.bytes = input.len() as u64;
    b.iter(|| pprint_ref(&doc, config.to_printer()));
}

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(
    vm_benches,
    vm_compile,
    vm_parse_pathological,
    vm_parse_1kb,
    vm_parse_10kb,
    vm_format_pathological,
    vm_format_1kb,
);

benchmark_group!(
    aot_benches,
    aot_parse_pathological,
    aot_parse_1kb,
    aot_parse_10kb,
    aot_format_pathological,
    aot_format_pathological_cached,
    aot_format_1kb,
    aot_format_1kb_cached,
    aot_format_10kb,
);

benchmark_group!(
    aot_phase_benches,
    aot_pathological_to_doc_only,
    aot_pathological_render_only,
    aot_1kb_to_doc_only,
    aot_1kb_render_only,
);

benchmark_main!(vm_benches, aot_benches, aot_phase_benches);
