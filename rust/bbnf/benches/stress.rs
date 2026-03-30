#![feature(cold_path)]

//! Pathological input stress benchmarks — cold per-parse.
//!
//! Exercises generated worst-case inputs: deep nesting, wide collections,
//! long strings, escape-heavy content, and mixed-type arrays.
//!
//! Fresh BumpArena + Parser per iteration. No warm-cache benchmarks.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod generators;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf_derive::Parser;
use parse_that::BumpArena;

#[derive(Parser)]
#[parser(path = "benches/grammars/json.bbnf", arena)]
struct BbnfJsonParser;

// ── JSON depth: deeply nested objects ───────────────────────────────────

macro_rules! bench_depth_obj {
    ($name:ident, $depth:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::json_gen::deeply_nested_objects($depth);
            b.bytes = input.len() as u64;
            {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    "depth_obj_{}: parse failed",
                    $depth,
                );
            }
            b.iter(|| {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_depth_obj!(depth_obj_100, 100);
bench_depth_obj!(depth_obj_500, 500);
bench_depth_obj!(depth_obj_1000, 1000);

// ── JSON depth: deeply nested arrays ────────────────────────────────────

macro_rules! bench_depth_arr {
    ($name:ident, $depth:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::json_gen::deeply_nested_arrays($depth);
            b.bytes = input.len() as u64;
            {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    "depth_arr_{}: parse failed",
                    $depth,
                );
            }
            b.iter(|| {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_depth_arr!(depth_arr_100, 100);
bench_depth_arr!(depth_arr_500, 500);
bench_depth_arr!(depth_arr_1000, 1000);

// ── JSON width: wide arrays ─────────────────────────────────────────────

macro_rules! bench_wide_arr {
    ($name:ident, $count:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::json_gen::wide_array($count);
            b.bytes = input.len() as u64;
            {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    "wide_arr_{}: parse failed",
                    $count,
                );
            }
            b.iter(|| {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_wide_arr!(wide_arr_100, 100);
bench_wide_arr!(wide_arr_1000, 1000);
bench_wide_arr!(wide_arr_10000, 10000);

// ── JSON width: wide objects ────────────────────────────────────────────

macro_rules! bench_wide_obj {
    ($name:ident, $count:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::json_gen::wide_object($count);
            b.bytes = input.len() as u64;
            {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    "wide_obj_{}: parse failed",
                    $count,
                );
            }
            b.iter(|| {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_wide_obj!(wide_obj_100, 100);
bench_wide_obj!(wide_obj_1000, 1000);
bench_wide_obj!(wide_obj_10000, 10000);

// ── JSON strings: long string content ───────────────────────────────────

macro_rules! bench_strings {
    ($name:ident, $count:expr, $len:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::json_gen::long_strings($count, $len);
            b.bytes = input.len() as u64;
            {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    "strings_{}x{}: parse failed",
                    $count,
                    $len,
                );
            }
            b.iter(|| {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_strings!(strings_100x64, 100, 64);
bench_strings!(strings_100x256, 100, 256);
bench_strings!(strings_100x1024, 100, 1024);

// ── JSON escapes: unicode-heavy strings ─────────────────────────────────

macro_rules! bench_escapes {
    ($name:ident, $count:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::json_gen::escape_heavy($count);
            b.bytes = input.len() as u64;
            {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    "escapes_{}: parse failed",
                    $count,
                );
            }
            b.iter(|| {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_escapes!(escapes_100, 100);
bench_escapes!(escapes_1000, 1000);

// ── JSON mixed: alternating value types ─────────────────────────────────

macro_rules! bench_mixed {
    ($name:ident, $count:expr) => {
        fn $name(b: &mut Bencher) {
            let input = generators::json_gen::mixed_types($count);
            b.bytes = input.len() as u64;
            {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                assert!(
                    parser.parse_with_context(&input, &arena).is_some(),
                    "mixed_{}: parse failed",
                    $count,
                );
            }
            b.iter(|| {
                let arena =
                    BumpArena::<BbnfJsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = BbnfJsonParser::value_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench_mixed!(mixed_1000, 1000);
bench_mixed!(mixed_10000, 10000);

// ── Groups ──────────────────────────────────────────────────────────────

benchmark_group!(
    json_depth,
    depth_obj_100,
    depth_obj_500,
    depth_obj_1000,
    depth_arr_100,
    depth_arr_500,
    depth_arr_1000
);
benchmark_group!(
    json_width,
    wide_arr_100,
    wide_arr_1000,
    wide_arr_10000,
    wide_obj_100,
    wide_obj_1000,
    wide_obj_10000
);
benchmark_group!(
    json_strings,
    strings_100x64,
    strings_100x256,
    strings_100x1024,
    escapes_100,
    escapes_1000
);
benchmark_group!(json_mixed, mixed_1000, mixed_10000);

benchmark_main!(json_depth, json_width, json_strings, json_mixed);
