#![feature(cold_path)]

//! JSON parsing competitor benchmarks.
//!
//! Compares BBNF AOT against serde_json, sonic-rs, simd-json, and jiter
//! on the same datasets. No global allocator override — uses system default.

use bencher::{benchmark_group, benchmark_main, black_box, Bencher};

use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "benches/grammars/json.bbnf")]
struct BbnfJsonParser;

fn load_json(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {}: {}", path, e))
}

// ── BBNF AOT ────────────────────────────────────────────────────────────────

fn bbnf_data(b: &mut Bencher) {
    let input = load_json("data.json");
    let parser = BbnfJsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn bbnf_canada(b: &mut Bencher) {
    let input = load_json("canada.json");
    let parser = BbnfJsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn bbnf_twitter(b: &mut Bencher) {
    let input = load_json("twitter.json");
    let parser = BbnfJsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

fn bbnf_citm(b: &mut Bencher) {
    let input = load_json("citm_catalog.json");
    let parser = BbnfJsonParser::value();
    b.bytes = input.len() as u64;
    b.iter(|| parser.parse(black_box(&input)).unwrap());
}

// ── serde_json ──────────────────────────────────────────────────────────────

fn serde_data(b: &mut Bencher) {
    let input = load_json("data.json");
    b.bytes = input.len() as u64;
    b.iter(|| serde_json::from_str::<serde_json::Value>(black_box(&input)).unwrap());
}

fn serde_canada(b: &mut Bencher) {
    let input = load_json("canada.json");
    b.bytes = input.len() as u64;
    b.iter(|| serde_json::from_str::<serde_json::Value>(black_box(&input)).unwrap());
}

fn serde_twitter(b: &mut Bencher) {
    let input = load_json("twitter.json");
    b.bytes = input.len() as u64;
    b.iter(|| serde_json::from_str::<serde_json::Value>(black_box(&input)).unwrap());
}

fn serde_citm(b: &mut Bencher) {
    let input = load_json("citm_catalog.json");
    b.bytes = input.len() as u64;
    b.iter(|| serde_json::from_str::<serde_json::Value>(black_box(&input)).unwrap());
}

// ── sonic-rs ────────────────────────────────────────────────────────────────

fn sonic_data(b: &mut Bencher) {
    let input = load_json("data.json");
    b.bytes = input.len() as u64;
    b.iter(|| sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap());
}

fn sonic_canada(b: &mut Bencher) {
    let input = load_json("canada.json");
    b.bytes = input.len() as u64;
    b.iter(|| sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap());
}

fn sonic_twitter(b: &mut Bencher) {
    let input = load_json("twitter.json");
    b.bytes = input.len() as u64;
    b.iter(|| sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap());
}

fn sonic_citm(b: &mut Bencher) {
    let input = load_json("citm_catalog.json");
    b.bytes = input.len() as u64;
    b.iter(|| sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap());
}

// ── simd-json ───────────────────────────────────────────────────────────────

fn simd_data(b: &mut Bencher) {
    let input = load_json("data.json");
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut bytes = input.as_bytes().to_vec();
        simd_json::to_owned_value(black_box(&mut bytes)).unwrap()
    });
}

fn simd_canada(b: &mut Bencher) {
    let input = load_json("canada.json");
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut bytes = input.as_bytes().to_vec();
        simd_json::to_owned_value(black_box(&mut bytes)).unwrap()
    });
}

fn simd_twitter(b: &mut Bencher) {
    let input = load_json("twitter.json");
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut bytes = input.as_bytes().to_vec();
        simd_json::to_owned_value(black_box(&mut bytes)).unwrap()
    });
}

fn simd_citm(b: &mut Bencher) {
    let input = load_json("citm_catalog.json");
    b.bytes = input.len() as u64;
    b.iter(|| {
        let mut bytes = input.as_bytes().to_vec();
        simd_json::to_owned_value(black_box(&mut bytes)).unwrap()
    });
}

// ── jiter ───────────────────────────────────────────────────────────────────

fn jiter_data(b: &mut Bencher) {
    let input = load_json("data.json");
    b.bytes = input.len() as u64;
    b.iter(|| jiter::JsonValue::parse(black_box(input.as_bytes()), false).unwrap());
}

fn jiter_canada(b: &mut Bencher) {
    let input = load_json("canada.json");
    b.bytes = input.len() as u64;
    b.iter(|| jiter::JsonValue::parse(black_box(input.as_bytes()), false).unwrap());
}

fn jiter_twitter(b: &mut Bencher) {
    let input = load_json("twitter.json");
    b.bytes = input.len() as u64;
    b.iter(|| jiter::JsonValue::parse(black_box(input.as_bytes()), false).unwrap());
}

fn jiter_citm(b: &mut Bencher) {
    let input = load_json("citm_catalog.json");
    b.bytes = input.len() as u64;
    b.iter(|| jiter::JsonValue::parse(black_box(input.as_bytes()), false).unwrap());
}

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(bench_bbnf, bbnf_data, bbnf_canada, bbnf_twitter, bbnf_citm);
benchmark_group!(bench_serde, serde_data, serde_canada, serde_twitter, serde_citm);
benchmark_group!(bench_sonic, sonic_data, sonic_canada, sonic_twitter, sonic_citm);
benchmark_group!(bench_simd, simd_data, simd_canada, simd_twitter, simd_citm);
benchmark_group!(bench_jiter, jiter_data, jiter_canada, jiter_twitter, jiter_citm);

benchmark_main!(bench_bbnf, bench_serde, bench_sonic, bench_simd, bench_jiter);
