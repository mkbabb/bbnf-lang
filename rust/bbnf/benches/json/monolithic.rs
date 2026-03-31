#![feature(cold_path)]

//! BBNF JSON monolithic arena benchmark — cold per-parse.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};
use bbnf_derive::Parser;
use parse_that::BumpArena;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", arena)]
struct JsonParser;

const _: () = assert!(std::mem::size_of::<JsonParserEnum>() <= 48);

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

macro_rules! bench {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            b.iter(|| {
                let arena = BumpArena::<JsonParserArenaEnum<'_>>::with_capacity(input.len() / 32);
                let parser = JsonParser::value_arena();
                let ast = parser.parse_with_context(black_box(&input), &arena).unwrap();
                black_box(ast as *const _);
            });
        }
    };
}

bench!(data, "data.json");
bench!(twitter, "twitter.json");
bench!(citm, "citm_catalog.json");
bench!(canada, "canada.json");
bench!(data_xl, "data_xl.json");

benchmark_group!(benches, data, twitter, citm, canada, data_xl);
benchmark_main!(benches);
