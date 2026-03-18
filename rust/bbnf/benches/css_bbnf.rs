#![feature(cold_path)]

//! CSS parsing benchmarks — two BBNF tiers.
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

macro_rules! bench_fast {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let parser = CssFastParser::stylesheet();
            let (_result, state) = parser.parse_return_state(&input);
            let consumed_pct = state.offset * 100 / input.len().max(1);
            assert!(
                consumed_pct >= 95,
                concat!($file, ": fast parser only consumed {}%"),
                consumed_pct
            );
            b.bytes = state.offset as u64;
            b.iter(|| parser.parse(black_box(&input)).unwrap());
        }
    };
}

bench_fast!(fast_normalize, "normalize.css");
bench_fast!(fast_bootstrap, "bootstrap.css");
bench_fast!(fast_tailwind, "tailwind.css");

// ── L1.75 pretty ────────────────────────────────────────────────────────────

macro_rules! bench_pretty {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let parser = CssPrettyParser::stylesheet();
            let (_result, state) = parser.parse_return_state(&input);
            let consumed_pct = state.offset * 100 / input.len().max(1);
            assert!(
                consumed_pct >= 95,
                concat!($file, ": pretty parser only consumed {}%"),
                consumed_pct
            );
            b.bytes = state.offset as u64;
            b.iter(|| parser.parse(black_box(&input)).unwrap());
        }
    };
}

bench_pretty!(pretty_normalize, "normalize.css");
bench_pretty!(pretty_bootstrap, "bootstrap.css");
bench_pretty!(pretty_tailwind, "tailwind.css");

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(fast, fast_normalize, fast_bootstrap, fast_tailwind);
benchmark_group!(pretty, pretty_normalize, pretty_bootstrap, pretty_tailwind);
benchmark_main!(fast, pretty);
