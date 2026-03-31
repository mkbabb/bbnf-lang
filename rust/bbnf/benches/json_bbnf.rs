#![feature(cold_path)]

//! BBNF JSON parsing benchmarks — cold per-parse, three tiers.
//!
//! Fresh BumpArena + Parser per iteration. No warm-cache benchmarks.
//!
//! - **monolithic**: arena codegen, raw enum output (structural validation)
//! - **combinator**: parse_that built-in json_parser (fused number conversion, not BBNF)
//! - **vm**: bytecode interpreter

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[cfg(not(feature = "dhat-heap"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf::pipeline::{compile_grammar, PipelineOptions};
use bbnf_derive::Parser;
use bbnf_ir::compiler::compile as compile_bytecode;
use bbnf_ir::interpreter::Interpreter;
use parse_that::BumpArena;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", arena)]
struct JsonParser;

// Compile-time enum size audit: ensure the generated enum stays compact.
// Smaller enums → faster Vec operations (memcpy, reallocation).
const _: () = assert!(std::mem::size_of::<JsonParserEnum>() <= 48);

fn load_json(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

fn compiled_json_vm() -> (bbnf_ir::GrammarIR, bbnf_ir::bytecode::BytecodeProgram) {
    let grammar =
        std::fs::read_to_string("../../grammar/json/json.bbnf").expect("failed to read json.bbnf");
    let ir = compile_grammar(&grammar, &PipelineOptions::default()).unwrap();
    let program = compile_bytecode(&ir);
    (ir, program)
}

// ── Monolithic tier (cold per-parse) ────────────────────────────────

macro_rules! bench_monolithic {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            b.bytes = input.len() as u64;
            {
                let arena = BumpArena::<JsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = JsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    concat!($file, ": monolithic parse failed")
                );
            }
            b.iter(|| {
                let arena = BumpArena::<JsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = JsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_monolithic!(monolithic_data, "data.json");
bench_monolithic!(monolithic_twitter, "twitter.json");
bench_monolithic!(monolithic_citm, "citm_catalog.json");
bench_monolithic!(monolithic_canada, "canada.json");
bench_monolithic!(monolithic_data_xl, "data_xl.json");

// ── VM tier (bytecode interpreter) ──────────────────────────────────────────

macro_rules! bench_vm {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            let (_ir, program) = compiled_json_vm();
            b.bytes = input.len() as u64;
            {
                let mut interp = Interpreter::new(&program, &input);
                let r = interp.run();
                assert!(r.success, concat!($file, ": VM parse failed"));
            }
            b.iter(|| {
                let mut interp = Interpreter::new(&program, black_box(&input));
                let r = interp.run();
                assert!(r.success);
            });
        }
    };
}

bench_vm!(vm_data, "data.json");
bench_vm!(vm_twitter, "twitter.json");
bench_vm!(vm_citm, "citm_catalog.json");
bench_vm!(vm_canada, "canada.json");
bench_vm!(vm_data_xl, "data_xl.json");

// ── Combinator tier (parse_that built-in json_parser, not BBNF) ────
// Uses parse_that::json_value() which fuses number scanning with mantissa
// accumulation — numbers converted to f64 during parsing, no re-read.
// This is the fair comparison against sonic-rs (which also fuses).

macro_rules! bench_combinator {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_json($file);
            b.bytes = input.len() as u64;
            {
                let parser = parse_that::parsers::json::json_parser();
                assert!(
                    parser.parse(&input).is_some(),
                    concat!($file, ": json_value parse failed")
                );
            }
            b.iter(|| {
                let parser = parse_that::parsers::json::json_parser();
                let ast = parser.parse(black_box(&input)).unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_combinator!(combinator_data, "data.json");
bench_combinator!(combinator_twitter, "twitter.json");
bench_combinator!(combinator_citm, "citm_catalog.json");
bench_combinator!(combinator_canada, "canada.json");
bench_combinator!(combinator_data_xl, "data_xl.json");

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(
    monolithic,
    monolithic_data,
    monolithic_twitter,
    monolithic_citm,
    monolithic_canada,
    monolithic_data_xl
);
benchmark_group!(
    combinator,
    combinator_data,
    combinator_twitter,
    combinator_citm,
    combinator_canada,
    combinator_data_xl
);
benchmark_group!(vm, vm_data, vm_twitter, vm_citm, vm_canada, vm_data_xl);
benchmark_main!(monolithic, combinator, vm);
