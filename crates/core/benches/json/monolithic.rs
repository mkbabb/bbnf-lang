
//! BBNF JSON monolithic benchmark — cold per-parse (tape-first).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

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
                let parsed = JsonParser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                black_box(&parsed);
            }
            bench_with_timeout(b, limits::JSON_PARSE, || {
                let parsed = JsonParser::parse(black_box(&input)).unwrap();
                black_box(parsed);
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
