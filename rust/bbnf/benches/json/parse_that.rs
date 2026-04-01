#![feature(cold_path)]

//! parse_that built-in JSON parser benchmark — baseline comparison (not BBNF).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

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
                let parser = parse_that::parsers::json::json_parser();
                let ast = parser.parse(black_box(&input)).unwrap();
                black_box(&ast as *const _);
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
