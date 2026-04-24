
//! parse_that built-in JSON parser benchmark — baseline comparison (not BBNF).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use divan::black_box;

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

macro_rules! bench {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            {
                let parser = parse_that::parsers::json::json_parser();
                let (result, state) = parser.parse_return_state(&input);
                assert!(
                    result.is_some(),
                    concat!($file, ": parse_that parse failed")
                );
                assert!(
                    state.offset >= input.trim_end().len(),
                    concat!($file, ": parse_that incomplete parse ({}/{})"),
                    state.offset,
                    input.len(),
                );
            }
            b.with_inputs(|| input.clone()).bench_values(|input| {
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

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
