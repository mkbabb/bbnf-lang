//! sk-v27 LW3.PERF-1d (RULINGS 2026-11-14f PERF1D-B2 (b)): the json `data_s`
//! case read with 10x the iterations of `benches/json/monolithic.rs`
//! (`sample_size(10)` in place of 1; the same body, timeout guard, allocator
//! and sample count). Copied into `crates/core/examples/` of the base and the
//! cure trees for the gate read only; never committed.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use divan::black_box;

use ::bbnf::grammar::generated::json::*;

#[path = "../benches/json/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

#[divan::bench]
fn data_s(b: divan::Bencher) {
    let path = bbnf_corpus::path("json/data.json").display().to_string();
    let input = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e));
    assert!(JsonParser::parse(&input).is_ok());
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

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(10)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
