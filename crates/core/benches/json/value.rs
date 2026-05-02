//! JSON value benchmark -- grammar-owned semantic parity.
//!
//! The retained lanes compare BBNF's emitted document/value/path APIs
//! directly with sonic-rs. Earlier scaffolding lanes are removed so
//! profiling targets the substrate AZ-II keeps.
//!
//! - Lazy lane (`bbnf_get_twitter` vs `sonic_get_twitter`): parse
//!   `twitter.json` and extract `["statuses", 0, "text"]`.
//! - Eager lane (`bbnf_value_<fx>` vs `sonic_value_<fx>`): parse and
//!   materialize a full typed value tree on both sides.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use divan::black_box;

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

use ::bbnf::grammar::generated::json::*;

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

// ── Lazy lane (`Document::get::<&str>` vs `sonic_rs::get`) ─────────────────
//
// Twitter-only: path `["statuses", 0, "text"]` extracts a single `&str`
// leaf. Both parties do the minimum work: parse enough of the input to
// reach the leaf, then return it. sonic's `get` is its lazy-query API
// (pointer-walk without materialising downstream nodes); bbnf's
// `Document::get::<T>(path)` is the grammar-emitted path-query surface.
// The bench pairs the two so the per-iter wall time is directly comparable.
//
// Note: bbnf's `parse()` currently materializes the document before
// extraction; the lazy comparison is the grammar-owned path-query API
// over the parsed document.

use bbnf::path;

#[divan::bench]
fn bbnf_get_twitter(b: divan::Bencher) {
    let input = load("twitter.json");
    {
        let parsed = JsonParser::parse(&input)
            .unwrap_or_else(|e| panic!("twitter.json: parse failed: {:?}", e));
        let segs = path!["statuses", 0_usize, "text"];
        let p = bbnf::runtime::Path::new(segs);
        black_box(parsed.get::<&str>(p));
    }
    bench_with_timeout(
        b,
        limits::JSON_PARSE,
        |input: String| {
            let parsed = JsonParser::parse(black_box(&input)).unwrap();
            let segs = path!["statuses", 0_usize, "text"];
            let p = bbnf::runtime::Path::new(segs);
            let got: Option<&str> = parsed.get(p);
            black_box(got);
            black_box(parsed);
        },
        &input,
    );
}

#[divan::bench]
fn sonic_get_twitter(b: divan::Bencher) {
    let input = load("twitter.json");
    // Warm-up: sonic_rs::get accepts the path as an `IntoIterator` of
    // `Index` values. Mixed string/integer keys round-trip via the
    // `JsonPointer!` macro or a heterogeneous vec of `PointerNode`
    // values — bbnf_get_twitter's path is `statuses.0.text`, a field
    // / index / field mix.
    {
        let node_path: sonic_rs::PointerTree = {
            let mut t = sonic_rs::PointerTree::new();
            t.add_path(sonic_rs::pointer!["statuses", 0usize, "text"]);
            t
        };
        let nodes = sonic_rs::get_many(&input, &node_path)
            .unwrap_or_else(|e| panic!("twitter.json: sonic_rs::get_many warm-up failed: {e}"));
        black_box(nodes);
    }
    b.with_inputs(|| input.clone()).bench_values(|input| {
        // Use `sonic_rs::get` with a path iterator — the idiomatic
        // lazy-extract entry. `JsonInput::from_subset` slices `&input`
        // so the returned `LazyValue<'_>` borrows from it.
        let got = sonic_rs::get(
            black_box(&input),
            sonic_rs::pointer!["statuses", 0usize, "text"],
        );
        let _ = black_box(got);
    });
}

// ── AY.W3c.1 Eager lane (`Document::to_value` vs `sonic_rs::from_str`) ──────
//
// Materialises the full typed value tree on both sides. bbnf uses
// `Document::to_value()`; sonic uses `from_str::<sonic_rs::Value>`.
// The ratio `bbnf_value_twitter / sonic_value_twitter` is AY's headline
// BEAT-sonic metric. Cold per-parse discipline (no warm benches per
// `feedback_no_warm_benches`): every iter reparses + rematerialises.

macro_rules! bench_bbnf_value {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            {
                let parsed = JsonParser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                let v = parsed.to_value();
                black_box(v);
            }
            bench_with_timeout(
                b,
                limits::JSON_PARSE,
                |input: String| {
                    let parsed = JsonParser::parse(black_box(&input)).unwrap();
                    let v = parsed.to_value();
                    black_box(v);
                    black_box(parsed);
                },
                &input,
            );
        }
    };
}

bench_bbnf_value!(bbnf_value_data_s, "data.json");
bench_bbnf_value!(bbnf_value_twitter, "twitter.json");
bench_bbnf_value!(bbnf_value_citm, "citm_catalog.json");
bench_bbnf_value!(bbnf_value_canada, "canada.json");
// AZ-IV.W6 carve: bbnf_value_data_xl exceeds the 1s JSON_PARSE
// wall-clock guard under fat-LTO `[profile.bench]` (~2.4-2.6s/iter
// observed reproducibly across 4 runs). divan's panic propagation
// aborts every subsequent bench (sonic_value_*) before they can
// measure. Carving so the rest run cleanly. data_xl is recorded
// separately in `docs/benchmarks/post-AZ-IV.json` as WATCHDOG_HALT
// with the observed per-iter wall and named hotspot
// (`<bbnf::grammar::generated::json::JsonParser>::parse` walking the
// 21MB document materialising f64 payloads at every `__value` Number
// branch — same hotspot as `bbnf_value_canada` scaled 9.4x in input).
// Reproducible: cargo bench --profile bench -p bbnf --features
// competitor --bench json_value (with bench_bbnf_value!(bbnf_value_data_xl, ...)
// reactivated).
// bench_bbnf_value!(bbnf_value_data_xl, "data_xl.json");

macro_rules! bench_sonic_value {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            sonic_rs::from_str::<sonic_rs::Value>(&input)
                .expect(concat!($file, ": sonic-rs parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap()
            });
        }
    };
}

bench_sonic_value!(sonic_value_data_s, "data.json");
bench_sonic_value!(sonic_value_twitter, "twitter.json");
bench_sonic_value!(sonic_value_citm, "citm_catalog.json");
bench_sonic_value!(sonic_value_canada, "canada.json");
bench_sonic_value!(sonic_value_data_xl, "data_xl.json");

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
