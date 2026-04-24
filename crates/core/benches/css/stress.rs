
//! CSS pathological input stress benchmarks — cold per-parse (tape-first).
//!
//! Exercises generated worst-case inputs: many rules, wide selectors,
//! deep media nesting, and large declaration blocks.
//!
//! Fresh parser per iteration. No warm-cache benchmarks.

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[cfg(not(feature = "dhat-heap"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[path = "../generators/mod.rs"]
mod generators;

use divan::counter::BytesCount;

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover)]
struct CssFastParser;

// ── CSS rules: many separate rules ─────────────────────────────────────

macro_rules! bench_rules {
    ($name:ident, $count:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = generators::css_gen::many_rules($count);
            let bytes = BytesCount::new(input.len());
            {
                let parsed = CssFastParser::parse(&input)
                    .unwrap_or_else(|e| panic!("many_rules_{}: parse failed: {:?}", $count, e));
                divan::black_box(&parsed);
            }
            b.counter(bytes).bench_local(|| {
                let parsed = CssFastParser::parse(divan::black_box(&input)).unwrap();
                divan::black_box(parsed);
            });
        }
    };
}

bench_rules!(rules_100, 100);
bench_rules!(rules_1000, 1000);
bench_rules!(rules_5000, 5000);

// ── CSS declarations: single rule with many properties ─────────────────

macro_rules! bench_decls {
    ($name:ident, $count:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = generators::css_gen::many_declarations($count);
            let bytes = BytesCount::new(input.len());
            {
                let parsed = CssFastParser::parse(&input).unwrap_or_else(|e| {
                    panic!("many_declarations_{}: parse failed: {:?}", $count, e)
                });
                divan::black_box(&parsed);
            }
            b.counter(bytes).bench_local(|| {
                let parsed = CssFastParser::parse(divan::black_box(&input)).unwrap();
                divan::black_box(parsed);
            });
        }
    };
}

bench_decls!(decls_100, 100);
bench_decls!(decls_500, 500);

// ── CSS selectors: wide selector lists ─────────────────────────────────

macro_rules! bench_selectors {
    ($name:ident, $count:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = generators::css_gen::wide_selector_list($count);
            let bytes = BytesCount::new(input.len());
            {
                let parsed = CssFastParser::parse(&input).unwrap_or_else(|e| {
                    panic!("wide_selector_list_{}: parse failed: {:?}", $count, e)
                });
                divan::black_box(&parsed);
            }
            b.counter(bytes).bench_local(|| {
                let parsed = CssFastParser::parse(divan::black_box(&input)).unwrap();
                divan::black_box(parsed);
            });
        }
    };
}

bench_selectors!(selectors_100, 100);
bench_selectors!(selectors_500, 500);

// ── CSS nesting: @media query depth ────────────────────────────────────

macro_rules! bench_nesting {
    ($name:ident, $depth:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = generators::css_gen::media_query_nesting($depth);
            let bytes = BytesCount::new(input.len());
            {
                let parsed = CssFastParser::parse(&input).unwrap_or_else(|e| {
                    panic!("media_query_nesting_{}: parse failed: {:?}", $depth, e)
                });
                divan::black_box(&parsed);
            }
            b.counter(bytes).bench_local(|| {
                let parsed = CssFastParser::parse(divan::black_box(&input)).unwrap();
                divan::black_box(parsed);
            });
        }
    };
}

bench_nesting!(nesting_5, 5);
bench_nesting!(nesting_10, 10);
bench_nesting!(nesting_20, 20);

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
