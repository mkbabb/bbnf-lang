#![feature(cold_path)]

//! CSS parsing benchmarks — two tiers.
//!
//! - **L0 fast**: css-fast.bbnf — opaque spans, single regex ws, maximum throughput
//! - **L1.75 pretty**: css-stylesheet-pretty.bbnf — structural AST with @pretty directives

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "benches/grammars/css-fast.bbnf")]
struct CssFastParser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/css-stylesheet-pretty.bbnf", prettify, skip_recover)]
struct CssPrettyParser;

fn load_css(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

// ── L0 fast ─────────────────────────────────────────────────────────────────

/// Parse and return actual bytes consumed (for accurate throughput).
fn fast_consumed_bytes(input: &str) -> u64 {
    let parser = CssFastParser::stylesheet();
    let (_result, state) = parser.parse_return_state(input);
    state.offset as u64
}

fn pretty_consumed_bytes(input: &str) -> u64 {
    let parser = CssPrettyParser::stylesheet();
    let (_result, state) = parser.parse_return_state(input);
    state.offset as u64
}

fn aot_fast_normalize(b: &mut Bencher) {
    let input = load_css("normalize.css");
    let parser = CssFastParser::stylesheet();
    b.bytes = fast_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_fast_bootstrap(b: &mut Bencher) {
    let input = load_css("bootstrap.css");
    let parser = CssFastParser::stylesheet();
    b.bytes = fast_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_fast_tailwind(b: &mut Bencher) {
    let input = load_css("tailwind.css");
    let parser = CssFastParser::stylesheet();
    b.bytes = fast_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

// ── L1.75 pretty ────────────────────────────────────────────────────────────

fn aot_pretty_normalize(b: &mut Bencher) {
    let input = load_css("normalize.css");
    let parser = CssPrettyParser::stylesheet();
    b.bytes = pretty_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_pretty_bootstrap(b: &mut Bencher) {
    let input = load_css("bootstrap.css");
    let parser = CssPrettyParser::stylesheet();
    b.bytes = pretty_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn aot_pretty_tailwind(b: &mut Bencher) {
    let input = load_css("tailwind.css");
    let parser = CssPrettyParser::stylesheet();
    b.bytes = pretty_consumed_bytes(&input);
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

benchmark_group!(aot_fast, aot_fast_normalize, aot_fast_bootstrap, aot_fast_tailwind);
benchmark_group!(aot_pretty, aot_pretty_normalize, aot_pretty_bootstrap, aot_pretty_tailwind);
benchmark_main!(aot_fast, aot_pretty);
