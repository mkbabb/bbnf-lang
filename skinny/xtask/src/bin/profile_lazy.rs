//! Profiling binary: drives `runtime::generated_json::parse` over the twitter
//! fixture in a tight loop. Targeted by samply / cargo-flamegraph to surface
//! the actual cycle sinks of the lazy-offset tape parse path.
//!
//! Invocation:
//!     cargo build --release -p xtask --bin profile-lazy
//!     samply record ./target/release/profile-lazy [iters]
//!
//! Default iteration count is sized to give ~30s on the twitter corpus when
//! Track 1 is ~11.8 Gbps.

use std::env;
use std::path::PathBuf;
use std::time::Instant;

fn main() {
    let args: Vec<String> = env::args().collect();
    let iters: usize = args
        .get(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(60_000);
    let corpus = args
        .get(2)
        .cloned()
        .unwrap_or_else(|| "twitter".to_string());

    // If corpus is an absolute or relative path containing '/' or ending in
    // ".json", treat it as a literal path; otherwise look it up by name.
    let path = if corpus.contains('/') || corpus.ends_with(".json") {
        PathBuf::from(&corpus)
    } else {
        locate_fixture(&corpus)
    };
    eprintln!("profile-lazy: corpus={corpus} path={path:?} iters={iters}");

    let bytes = std::fs::read(&path).expect("failed to read fixture");
    let input = std::str::from_utf8(&bytes).expect("fixture is not UTF-8");
    eprintln!("profile-lazy: fixture size = {} bytes", bytes.len());

    // Warmup
    for _ in 0..16 {
        let root = runtime::generated_json::parse(input).expect("parse failed");
        std::hint::black_box(root);
    }

    eprintln!("profile-lazy: starting timed loop");
    let start = Instant::now();
    let mut total_offsets: u64 = 0;
    for _ in 0..iters {
        let root = runtime::generated_json::parse(std::hint::black_box(input))
            .expect("parse failed");
        total_offsets = total_offsets.wrapping_add(root.tape().offsets().len() as u64);
        std::hint::black_box(root);
    }
    let elapsed = start.elapsed();
    let total_bytes = (bytes.len() as u128) * (iters as u128);
    let mbps = (total_bytes as f64 * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0);
    eprintln!(
        "profile-lazy: {iters} iters in {:.2}s -> {:.0} Mbps (offset cksum {})",
        elapsed.as_secs_f64(),
        mbps,
        total_offsets
    );
}

fn locate_fixture(name: &str) -> PathBuf {
    // Walk up from CARGO_MANIFEST_DIR to skinny root.
    let manifest = env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| env::current_dir().unwrap());
    let mut dir = manifest.as_path();
    loop {
        let candidate = dir.join("crates/test-fixtures/corpus/json").join(format!("{name}.json"));
        if candidate.exists() {
            return candidate;
        }
        match dir.parent() {
            Some(p) => dir = p,
            None => break,
        }
    }
    // Fallback: cwd-relative
    let cwd_candidate = PathBuf::from(format!(
        "crates/test-fixtures/corpus/json/{name}.json"
    ));
    if cwd_candidate.exists() {
        return cwd_candidate;
    }
    panic!("could not locate fixture {name}.json under crates/test-fixtures/corpus/json");
}
