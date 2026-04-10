#![feature(cold_path)]

//! BBNF JSON monolithic slab benchmark — cold per-parse.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf", slab)]
struct JsonParser;

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

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
            {
                let ctx = __JsonParserEnumCtx::with_capacity(input.len() / 32);
                let parser = JsonParser::value();
                let (result, state) = parser.parse_return_state_with_context(&input, &ctx);
                assert!(result.is_some(), concat!($file, ": parse failed"));
                assert!(
                    state.offset >= input.trim_end().len(),
                    concat!($file, ": incomplete parse ({}/{})"),
                    state.offset,
                    input.len(),
                );
            }
            bench_with_timeout(b, limits::JSON_PARSE, || {
                let ctx = __JsonParserEnumCtx::with_capacity(input.len() / 32);
                let parser = JsonParser::value();
                let ast = parser.parse_with_context(black_box(&input), &ctx).unwrap();
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
