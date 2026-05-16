//! SK-V3 Wave 2 capacity-plan probe binary.
//!
//! Drives the four `BBNF_CAPACITY_PLAN` plans across a corpus, reporting:
//!   - throughput Mbps;
//!   - mean offset-vector capacity vs structural-count delta;
//!   - peak resident set (rusage maxrss);
//!   - allocation-count proxy: the parser's offset_capacity_bytes growth.
//!
//! Run e.g.:
//!   BBNF_CAPACITY_PLAN=B ./target/release/capacity-probe 50000 update-center
//!
//! When the corpus arg is `all`, the binary sweeps all four plans across
//! `update-center`, `random`, `unicode_escapes`, `github_events`.

use std::env;
use std::path::PathBuf;
use std::time::Instant;

const CORPORA: &[&str] = &[
    "update-center",
    "random",
    "unicode_escapes",
    "github_events",
];
const PLANS: &[&str] = &["A", "B", "C", "D"];

fn main() {
    let args: Vec<String> = env::args().collect();
    let iters: usize = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(75_000);
    let corpus_arg = args
        .get(2)
        .cloned()
        .unwrap_or_else(|| "update-center".into());
    let plan_arg = args.get(3).cloned();

    let corpora: Vec<String> = if corpus_arg == "all" {
        CORPORA.iter().map(|s| s.to_string()).collect()
    } else {
        vec![corpus_arg]
    };

    let plans: Vec<String> = match plan_arg.as_deref() {
        Some("all") | None => PLANS.iter().map(|s| s.to_string()).collect(),
        Some(p) => vec![p.to_string()],
    };

    println!(
        "{:<18} {:<14} {:>12} {:>10} {:>12} {:>14} {:>12}",
        "corpus", "plan", "Mbps", "src_KiB", "offsets", "cap_bytes", "maxrss_KiB"
    );
    for corpus in &corpora {
        let path = locate_fixture(corpus);
        let bytes = std::fs::read(&path).expect("read fixture");
        let src_kib = bytes.len() / 1024;
        let input = std::str::from_utf8(&bytes).expect("UTF-8 fixture");

        for plan in &plans {
            env::set_var("BBNF_CAPACITY_PLAN", plan);

            // Warmup
            for _ in 0..16 {
                let root = runtime::generated_json::parse(input).expect("parse");
                std::hint::black_box(root);
            }

            let rss_before = max_rss_kib();
            let start = Instant::now();
            let mut offset_count: u64 = 0;
            let mut cap_bytes_sum: u64 = 0;
            for _ in 0..iters {
                let root =
                    runtime::generated_json::parse(std::hint::black_box(input)).expect("parse");
                offset_count = offset_count.wrapping_add(root.tape().offsets().len() as u64);
                cap_bytes_sum =
                    cap_bytes_sum.wrapping_add(root.tape().offset_capacity_bytes() as u64);
                std::hint::black_box(root);
            }
            let elapsed = start.elapsed();
            let rss_after = max_rss_kib();
            let total_bytes = (bytes.len() as u128) * (iters as u128);
            let mbps = (total_bytes as f64 * 8.0) / (elapsed.as_secs_f64() * 1_000_000.0);
            let avg_offsets = offset_count / iters as u64;
            let avg_cap = cap_bytes_sum / iters as u64;

            println!(
                "{:<18} {:<14} {:>12.0} {:>10} {:>12} {:>14} {:>12}",
                corpus,
                plan_label(plan),
                mbps,
                src_kib,
                avg_offsets,
                avg_cap,
                rss_after.max(rss_before),
            );
        }
    }
}

fn plan_label(p: &str) -> &'static str {
    match p {
        "A" => "A:sampled",
        "B" => "B:exact",
        "C" => "C:simd",
        "D" => "D:grow",
        _ => "?",
    }
}

fn max_rss_kib() -> u64 {
    // libc rusage on macOS: maxrss is in bytes; on Linux it's KiB.
    unsafe {
        let mut usage: libc_rusage = std::mem::zeroed();
        if rusage_get(&mut usage) != 0 {
            return 0;
        }
        let raw = usage.ru_maxrss as u64;
        if cfg!(target_os = "macos") {
            raw / 1024
        } else {
            raw
        }
    }
}

#[repr(C)]
struct libc_rusage {
    ru_utime: [i64; 2],
    ru_stime: [i64; 2],
    ru_maxrss: i64,
    _padding: [i64; 14],
}

extern "C" {
    fn getrusage(who: i32, usage: *mut libc_rusage) -> i32;
}

unsafe fn rusage_get(usage: *mut libc_rusage) -> i32 {
    const RUSAGE_SELF: i32 = 0;
    getrusage(RUSAGE_SELF, usage)
}

fn locate_fixture(name: &str) -> PathBuf {
    let manifest = env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| env::current_dir().unwrap());
    for dir in manifest.ancestors() {
        let candidate = dir.join("test_data").join(format!("{name}.json"));
        if candidate.exists() {
            return candidate;
        }
        let alt = dir
            .join("crates/test-fixtures/corpus/json")
            .join(format!("{name}.json"));
        if alt.exists() {
            return alt;
        }
    }
    let cwd = PathBuf::from(format!("test_data/{name}.json"));
    if cwd.exists() {
        return cwd;
    }
    panic!("could not locate fixture {name}.json");
}
