#![feature(cold_path)]

//! CSS parsing benchmarks — cold per-parse, two BBNF tiers.
//!
//! All benches construct a fresh BumpArena + Parser per iteration.
//!
//! - **fast_arena**: css-fast.bbnf — opaque spans, @ws SIMD whitespace
//! - **pretty_arena**: css-stylesheet-pretty.bbnf — structural AST with @pretty directives

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};
use parse_that::BumpArena;

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "benches/grammars/css-fast.bbnf", arena)]
struct CssFastParser;

#[derive(Parser)]
#[parser(
    path = "../../grammar/css/css-stylesheet-pretty.bbnf",
    prettify,
    skip_recover,
    arena
)]
struct CssPrettyParser;

fn load_css(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

// ── L0 fast (arena, cold per-parse) ─────────────────────────────────────────

macro_rules! bench_fast_arena {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let (bytes, consumed_pct) = {
                let arena = BumpArena::<CssFastParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet_arena();
                let (_result, state) = parser.parse_return_state_with_context(&input, &arena);
                (state.offset as u64, state.offset * 100 / input.len().max(1))
            };
            assert!(
                consumed_pct >= 95,
                concat!($file, ": fast arena parser only consumed {}%"),
                consumed_pct
            );
            b.bytes = bytes;
            b.iter(|| {
                let arena = BumpArena::<CssFastParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssFastParser::stylesheet_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_fast_arena!(fast_arena_normalize, "normalize.css");
bench_fast_arena!(fast_arena_bootstrap, "bootstrap.css");
bench_fast_arena!(fast_arena_tailwind, "tailwind.css");

// ── L1.75 pretty ────────────────────────────────────────────────────────────

// ── L1.75 pretty (arena, cold per-parse) ────────────────────────────────────

macro_rules! bench_pretty_arena {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load_css($file);
            let (bytes, consumed_pct) = {
                let arena = BumpArena::<CssPrettyParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssPrettyParser::stylesheet_arena();
                let (_result, state) = parser.parse_return_state_with_context(&input, &arena);
                (state.offset as u64, state.offset * 100 / input.len().max(1))
            };
            assert!(
                consumed_pct >= 95,
                concat!($file, ": pretty arena parser only consumed {}%"),
                consumed_pct
            );
            b.bytes = bytes;
            b.iter(|| {
                let arena = BumpArena::<CssPrettyParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssPrettyParser::stylesheet_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench_pretty_arena!(pretty_arena_normalize, "normalize.css");
bench_pretty_arena!(pretty_arena_bootstrap, "bootstrap.css");
bench_pretty_arena!(pretty_arena_tailwind, "tailwind.css");

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(
    fast_arena,
    fast_arena_normalize,
    fast_arena_bootstrap,
    fast_arena_tailwind
);
benchmark_group!(
    pretty_arena,
    pretty_arena_normalize,
    pretty_arena_bootstrap,
    pretty_arena_tailwind
);
benchmark_main!(fast_arena, pretty_arena);
