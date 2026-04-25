
//! Google Sheets formula benchmark — monolithic codegen (tape-first).
//!
//! Stratified from simple cell references (simple.txt, 505B) through
//! deeply nested LET/LAMBDA/FILTER chains (stress.txt, 2.7KB).
//! Each formula is parsed individually; throughput is aggregate bytes/s.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use divan::counter::BytesCount;

use ::bbnf::grammar::generated::google_sheets::*;


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

#[divan::bench]
fn parse_simple(b: divan::Bencher) {
    let input = load("simple.txt");
    parse_all_formulas(&input); // verify
    let bytes = input.len();
    bench_with_timeout(
        b.counter(BytesCount::new(bytes)),
        limits::PARSE_DEFAULT,
        |input: String| {
            for line in input.lines() {
                let line = line.trim();
                if line.is_empty() { continue; }
                let parsed = GoogleSheetsParser::parse(divan::black_box(line)).unwrap();
                divan::black_box_drop(parsed);
            }
        },
        &input,
    );
}

#[divan::bench]
fn parse_nested(b: divan::Bencher) {
    let input = load("nested.txt");
    parse_all_formulas(&input); // verify
    let bytes = input.len();
    bench_with_timeout(
        b.counter(BytesCount::new(bytes)),
        limits::PARSE_DEFAULT,
        |input: String| {
            for line in input.lines() {
                let line = line.trim();
                if line.is_empty() { continue; }
                let parsed = GoogleSheetsParser::parse(divan::black_box(line)).unwrap();
                divan::black_box_drop(parsed);
            }
        },
        &input,
    );
}

#[divan::bench]
fn parse_stress(b: divan::Bencher) {
    let input = load("stress.txt");
    parse_all_formulas(&input); // verify
    let bytes = input.len();
    bench_with_timeout(
        b.counter(BytesCount::new(bytes)),
        limits::PARSE_DEFAULT,
        |input: String| {
            for line in input.lines() {
                let line = line.trim();
                if line.is_empty() { continue; }
                let parsed = GoogleSheetsParser::parse(divan::black_box(line)).unwrap();
                divan::black_box_drop(parsed);
            }
        },
        &input,
    );
}

// ── Format benchmarks ──────────────────────────────────────────────

#[divan::bench]
fn format_simple(b: divan::Bencher) {
    let input = load("simple.txt");
    let first_formula = input.lines().find(|l| !l.trim().is_empty()).unwrap().trim().to_string();
    let config = pprint::Printer::new(80, 2, false);
    {
        let parser = GoogleSheetsParser::formula_prettify();
        let (result, state) = parser.parse_return_state(&first_formula);
        assert!(result.is_some(), "format_simple: parse failed");
        assert_eq!(state.offset, first_formula.len());
    }
    let bytes = first_formula.len();
    b.counter(BytesCount::new(bytes))
        .with_inputs(|| first_formula.clone())
        .bench_values(|formula| {
            let parser = GoogleSheetsParser::formula_prettify();
            let ops = parser.parse(divan::black_box(&formula)).unwrap();
            pprint::render(&ops, config)
        });
}

#[divan::bench]
fn format_stress(b: divan::Bencher) {
    let input = load("stress.txt");
    let first_formula = input.lines().find(|l| !l.trim().is_empty()).unwrap().trim().to_string();
    let config = pprint::Printer::new(80, 2, false);
    {
        let parser = GoogleSheetsParser::formula_prettify();
        let (result, state) = parser.parse_return_state(&first_formula);
        assert!(result.is_some(), "format_stress: parse failed");
        assert_eq!(state.offset, first_formula.len());
    }
    let bytes = first_formula.len();
    b.counter(BytesCount::new(bytes))
        .with_inputs(|| first_formula.clone())
        .bench_values(|formula| {
            let parser = GoogleSheetsParser::formula_prettify();
            let ops = parser.parse(divan::black_box(&formula)).unwrap();
            pprint::render(&ops, config)
        });
}

fn main() {
    // Cold-per-parse (`sample_size = 1`) per workspace feedback
    // `no-warm-benches`. `skip_ext_time(true)` excludes the
    // `with_inputs` clone from the reported wall.
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
