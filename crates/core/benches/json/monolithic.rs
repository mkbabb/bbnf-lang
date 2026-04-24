
//! BBNF JSON monolithic benchmark — cold per-parse (tape-first).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use divan::black_box;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

#[path = "../common/validate.rs"]
mod validate;

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
                let parsed = JsonParser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                // Structural sanity: root is compound, tape is non-trivial.
                let view = parsed.view();
                validate::assert_root_kind_compound(&view.cursor(), $file);
                validate::assert_record_count_range(parsed.tape(), 1, 10_000_000, $file);
                black_box(&parsed);
            }
            bench_with_timeout(
                b,
                limits::JSON_PARSE,
                |input: String| {
                    let parsed = JsonParser::parse(black_box(&input)).unwrap();
                    black_box(parsed);
                },
                &input,
            );
        }
    };
}

// AU.6.6: rename `data` to `data_s` so `--bench data` no longer
// matches `data_xl` as a prefix. Bencher 0.1.5's filter is a
// substring match — the previous `data` / `data_xl` pair collapsed
// both entries into one profile run, hiding per-dataset attribution.
bench!(data_s, "data.json");
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
