#![feature(cold_path)]

//! BBNF CSS monolithic arena benchmark — cold per-parse (pretty.bbnf).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};
use parse_that::BumpArena;

#[derive(Parser)]
#[parser(path = "../../grammar/css/pretty.bbnf", skip_recover, arena)]
struct CssPrettyParser;

fn load(name: &str) -> String {
    let path = format!("../../data/css/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

macro_rules! bench {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            let (bytes, consumed_pct) = {
                let arena =
                    BumpArena::<CssPrettyParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssPrettyParser::stylesheet_arena();
                let (_result, state) = parser.parse_return_state_with_context(&input, &arena);
                (state.offset as u64, state.offset * 100 / input.len().max(1))
            };
            assert!(
                consumed_pct >= 95,
                concat!($file, ": only consumed {}%"),
                consumed_pct
            );
            b.bytes = bytes;
            b.iter(|| {
                let arena =
                    BumpArena::<CssPrettyParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = CssPrettyParser::stylesheet_arena();
                let ast = parser
                    .parse_with_context(black_box(&input), &arena)
                    .unwrap();
                black_box(&ast as *const _);
            });
        }
    };
}

bench!(normalize, "normalize.css");
bench!(bootstrap, "bootstrap.css");
bench!(tailwind, "tailwind.css");

benchmark_group!(benches, normalize, bootstrap, tailwind);
benchmark_main!(benches);
