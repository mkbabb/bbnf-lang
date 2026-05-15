//! Profiling binary for the direct-to-struct workload gate.
//!
//! Invocation:
//!     cargo build --release -p bbnf-bench --bin profile_direct
//!     samply record --save-only -o profile.json.gz ./target/release/profile_direct 10000 twitter track1

use std::env;
use std::path::PathBuf;
use std::time::Instant;

fn main() {
    let args: Vec<String> = env::args().collect();
    let iters: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(10_000);
    let corpus = args
        .get(2)
        .cloned()
        .unwrap_or_else(|| "twitter".to_string());
    let mode = args.get(3).map(String::as_str).unwrap_or("track1");
    let path = if corpus.contains('/') || corpus.ends_with(".json") {
        PathBuf::from(&corpus)
    } else if mode.starts_with("real_typed_") {
        bbnf_bench::real_typed_struct::locate_fixture(&corpus)
    } else {
        locate_fixture(&corpus)
    };

    eprintln!("profile-direct: corpus={corpus} mode={mode} path={path:?} iters={iters}");
    let bytes = std::fs::read(&path).expect("failed to read fixture");
    let input = std::str::from_utf8(&bytes).expect("fixture is not UTF-8");
    eprintln!("profile-direct: fixture size = {} bytes", bytes.len());

    for _ in 0..16 {
        run_once(mode, &corpus, input, &bytes);
    }

    eprintln!("profile-direct: starting timed loop");
    let start = Instant::now();
    let mut checksum = 0_u64;
    for _ in 0..iters {
        checksum ^= run_once(
            mode,
            &corpus,
            std::hint::black_box(input),
            std::hint::black_box(&bytes),
        );
    }
    let elapsed = start.elapsed();
    let total_bytes = (bytes.len() as u128) * (iters as u128);
    let mbps = (total_bytes as f64 * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0);
    eprintln!(
        "profile-direct: {iters} iters in {:.2}s -> {:.0} Mbps (digest cksum {checksum})",
        elapsed.as_secs_f64(),
        mbps
    );
}

fn run_once(mode: &str, corpus: &str, input: &str, bytes: &[u8]) -> u64 {
    let digest = match mode {
        "track1" => bbnf_bench::direct_struct::track1_digest(input),
        "track2" => bbnf_bench::direct_struct::track2_digest(input),
        "sonic" => bbnf_bench::direct_struct::sonic_digest(bytes),
        "serde" => bbnf_bench::direct_struct::serde_digest(bytes),
        "real_typed_track1" | "real_typed_track2" | "real_typed_sonic" | "real_typed_serde" => {
            return real_typed_checksum(corpus, mode, input, bytes);
        }
        other => panic!(
            "unknown mode {other}; expected track1|track2|sonic|serde|real_typed_track1|real_typed_track2|real_typed_sonic|real_typed_serde"
        ),
    }
    .expect("direct digest failed");
    std::hint::black_box(
        digest.fingerprint
            ^ digest.objects
            ^ digest.arrays
            ^ digest.strings
            ^ digest.numbers
            ^ digest.string_bytes,
    )
}

fn real_typed_checksum(corpus: &str, mode: &str, input: &str, bytes: &[u8]) -> u64 {
    let fixture = bbnf_bench::real_typed_struct::fixture_for_name(corpus)
        .unwrap_or_else(|| panic!("real typed mode {mode} does not support corpus {corpus}"));
    let output = match mode {
        "real_typed_track1" => bbnf_bench::real_typed_struct::track1_typed(fixture, input),
        "real_typed_track2" => bbnf_bench::real_typed_struct::track2_typed(fixture, input),
        "real_typed_sonic" => bbnf_bench::real_typed_struct::sonic_typed(fixture, bytes),
        "real_typed_serde" => bbnf_bench::real_typed_struct::serde_typed(fixture, bytes),
        _ => unreachable!("real_typed_checksum called for non-real-typed mode"),
    }
    .expect("real typed parse failed");
    std::hint::black_box(bbnf_bench::real_typed_struct::typed_checksum(&output))
}

fn locate_fixture(name: &str) -> PathBuf {
    let manifest = env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| env::current_dir().unwrap());
    for dir in manifest.ancestors() {
        let candidate = dir
            .join("crates/test-fixtures/corpus/json")
            .join(format!("{name}.json"));
        if candidate.exists() {
            return candidate;
        }
        let candidate = dir.join("test_data").join(format!("{name}.json"));
        if candidate.exists() {
            return candidate;
        }
    }
    panic!("could not locate fixture {name}.json under crates/test-fixtures/corpus/json");
}
