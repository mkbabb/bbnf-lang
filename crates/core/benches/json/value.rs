
//! JSON value benchmark — honest side-by-side comparison.
//!
//! Puts BBNF monolithic parse (typed AST, arena-allocated) next to
//! sonic-rs `Value` parse (SIMD, full unescape, arena-allocated)
//! on the same datasets in one bench binary so the numbers are
//! directly comparable with identical measurement overhead.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

// ── BBNF monolithic (typed AST, arena-allocated) ───────────────────────────

macro_rules! bench_bbnf {
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

bench_bbnf!(bbnf_data, "data.json");
bench_bbnf!(bbnf_twitter, "twitter.json");
bench_bbnf!(bbnf_citm, "citm_catalog.json");
bench_bbnf!(bbnf_canada, "canada.json");
bench_bbnf!(bbnf_data_xl, "data_xl.json");

// ── sonic-rs (SIMD, arena-allocated, full unescape) ────────────────────────

macro_rules! bench_sonic {
    ($name:ident, $file:expr) => {
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            sonic_rs::from_str::<sonic_rs::Value>(&input)
                .expect(concat!($file, ": sonic-rs parse failed"));
            b.iter(|| sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap());
        }
    };
}

bench_sonic!(sonic_data, "data.json");
bench_sonic!(sonic_twitter, "twitter.json");
bench_sonic!(sonic_citm, "citm_catalog.json");
bench_sonic!(sonic_canada, "canada.json");
bench_sonic!(sonic_data_xl, "data_xl.json");

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(
    bench_bbnf,
    bbnf_data,
    bbnf_twitter,
    bbnf_citm,
    bbnf_canada,
    bbnf_data_xl,
);
benchmark_group!(
    bench_sonic,
    sonic_data,
    sonic_twitter,
    sonic_citm,
    sonic_canada,
    sonic_data_xl,
);

benchmark_main!(bench_bbnf, bench_sonic);
