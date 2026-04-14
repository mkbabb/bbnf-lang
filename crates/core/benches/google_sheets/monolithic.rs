
//! Google Sheets formula benchmark — monolithic codegen (tape-first).
//!
//! Stratified from simple cell references (simple.txt, 505B) through
//! deeply nested LET/LAMBDA/FILTER chains (stress.txt, 2.7KB).
//! Each formula is parsed individually; throughput is aggregate bytes/s.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[derive(Parser)]
#[parser(path = "../../grammar/google-sheets/google-sheets.bbnf", prettify)]
struct GoogleSheetsParser;

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

fn load(name: &str) -> String {
    let path = format!("../../data/sheets/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

/// Parse every non-empty line as an individual formula.
fn parse_all_formulas(input: &str) {
    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        GoogleSheetsParser::parse(line)
            .unwrap_or_else(|e| panic!("parse failed on {:?}: {:?}", &line[..line.len().min(40)], e));
    }
}

fn parse_simple(b: &mut Bencher) {
    let input = load("simple.txt");
    b.bytes = input.len() as u64;
    parse_all_formulas(&input); // verify
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() { continue; }
            let parsed = GoogleSheetsParser::parse(black_box(line)).unwrap();
            black_box(parsed);
        }
    });
}

fn parse_nested(b: &mut Bencher) {
    let input = load("nested.txt");
    b.bytes = input.len() as u64;
    parse_all_formulas(&input); // verify
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() { continue; }
            let parsed = GoogleSheetsParser::parse(black_box(line)).unwrap();
            black_box(parsed);
        }
    });
}

fn parse_stress(b: &mut Bencher) {
    let input = load("stress.txt");
    b.bytes = input.len() as u64;
    parse_all_formulas(&input); // verify
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        for line in input.lines() {
            let line = line.trim();
            if line.is_empty() { continue; }
            let parsed = GoogleSheetsParser::parse(black_box(line)).unwrap();
            black_box(parsed);
        }
    });
}

// ── Format benchmarks ──────────────────────────────────────────────

fn format_simple(b: &mut Bencher) {
    let input = load("simple.txt");
    let first_formula = input.lines().find(|l| !l.trim().is_empty()).unwrap().trim();
    let config = pprint::Printer::new(80, 2, false);
    b.bytes = first_formula.len() as u64;
    {
        let parser = GoogleSheetsParser::formula_prettify();
        let (result, state) = parser.parse_return_state(first_formula);
        assert!(result.is_some(), "format_simple: parse failed");
        assert_eq!(state.offset, first_formula.len());
    }
    b.iter(|| {
        let parser = GoogleSheetsParser::formula_prettify();
        let ops = parser.parse(black_box(first_formula)).unwrap();
        pprint::render(&ops, config)
    });
}

fn format_stress(b: &mut Bencher) {
    let input = load("stress.txt");
    let first_formula = input.lines().find(|l| !l.trim().is_empty()).unwrap().trim();
    let config = pprint::Printer::new(80, 2, false);
    b.bytes = first_formula.len() as u64;
    {
        let parser = GoogleSheetsParser::formula_prettify();
        let (result, state) = parser.parse_return_state(first_formula);
        assert!(result.is_some(), "format_stress: parse failed");
        assert_eq!(state.offset, first_formula.len());
    }
    b.iter(|| {
        let parser = GoogleSheetsParser::formula_prettify();
        let ops = parser.parse(black_box(first_formula)).unwrap();
        pprint::render(&ops, config)
    });
}

benchmark_group!(
    benches,
    parse_simple,
    parse_nested,
    parse_stress,
    format_simple,
    format_stress,
);
benchmark_main!(benches);
