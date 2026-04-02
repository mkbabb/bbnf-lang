#![feature(cold_path)]

//! CSS pathological input stress benchmarks — cold per-parse.
//!
//! Exercises generated worst-case inputs: many rules, wide selectors,
//! deep media nesting, and large declaration blocks.
//!
//! Fresh BumpSlab + Parser per iteration. No warm-cache benchmarks.

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[cfg(not(feature = "dhat-heap"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[path = "../generators/mod.rs"]
mod generators;

use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover, slab)]
struct CssFastParser;

// ── CSS rules: many separate rules ─────────────────────────────────────

macro_rules! bench_rules {
    ($name:ident, $count:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::css_gen::many_rules($count);
            b.bytes = input.len() as u64;
            {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let (result, state) =
                    parser.parse_return_state_with_context(&input, &slab);
                assert!(result.is_some(), "many_rules_{}: parse failed", $count);
                assert_eq!(
                    state.offset,
                    input.len(),
                    "many_rules_{}: incomplete parse ({}/{})",
                    $count,
                    state.offset,
                    input.len(),
                );
            }
            b.iter(|| {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let ast = parser
                    .parse_with_context(black_box(&input), &slab)
                    .unwrap();
                black_box(&ast as *const _);
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
        fn $name(b: &mut Bencher) {
            let input = generators::css_gen::many_declarations($count);
            b.bytes = input.len() as u64;
            {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let (result, state) =
                    parser.parse_return_state_with_context(&input, &slab);
                assert!(
                    result.is_some(),
                    "many_declarations_{}: parse failed",
                    $count,
                );
                assert_eq!(
                    state.offset,
                    input.len(),
                    "many_declarations_{}: incomplete parse ({}/{})",
                    $count,
                    state.offset,
                    input.len(),
                );
            }
            b.iter(|| {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let ast = parser
                    .parse_with_context(black_box(&input), &slab)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_decls!(decls_100, 100);
bench_decls!(decls_500, 500);

// ── CSS selectors: wide selector lists ─────────────────────────────────

macro_rules! bench_selectors {
    ($name:ident, $count:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::css_gen::wide_selector_list($count);
            b.bytes = input.len() as u64;
            {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let (result, state) =
                    parser.parse_return_state_with_context(&input, &slab);
                assert!(
                    result.is_some(),
                    "wide_selector_list_{}: parse failed",
                    $count,
                );
                assert_eq!(
                    state.offset,
                    input.len(),
                    "wide_selector_list_{}: incomplete parse ({}/{})",
                    $count,
                    state.offset,
                    input.len(),
                );
            }
            b.iter(|| {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let ast = parser
                    .parse_with_context(black_box(&input), &slab)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_selectors!(selectors_100, 100);
bench_selectors!(selectors_500, 500);

// ── CSS nesting: @media query depth ────────────────────────────────────

macro_rules! bench_nesting {
    ($name:ident, $depth:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::css_gen::media_query_nesting($depth);
            b.bytes = input.len() as u64;
            {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let (result, state) =
                    parser.parse_return_state_with_context(&input, &slab);
                assert!(
                    result.is_some(),
                    "media_query_nesting_{}: parse failed",
                    $depth,
                );
                assert_eq!(
                    state.offset,
                    input.len(),
                    "media_query_nesting_{}: incomplete parse ({}/{})",
                    $depth,
                    state.offset,
                    input.len(),
                );
            }
            b.iter(|| {
                let slab =
                    __CssFastParserEnumCtx::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet();
                let ast = parser
                    .parse_with_context(black_box(&input), &slab)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_nesting!(nesting_5, 5);
bench_nesting!(nesting_10, 10);
bench_nesting!(nesting_20, 20);

// ── Groups ─────────────────────────────────────────────────────────────

benchmark_group!(css_rules, rules_100, rules_1000, rules_5000);
benchmark_group!(css_declarations, decls_100, decls_500);
benchmark_group!(css_selectors, selectors_100, selectors_500);
benchmark_group!(css_nesting, nesting_5, nesting_10, nesting_20);

benchmark_main!(css_rules, css_declarations, css_selectors, css_nesting);
