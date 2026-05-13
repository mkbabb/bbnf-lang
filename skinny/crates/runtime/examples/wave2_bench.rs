//! Standalone Track-1 throughput probe used to A/B the SK-V3 Wave 2
//! eventcursor prototype against the legacy `generated` dispatch path.
//!
//! Build & run baseline:
//!     cargo run --release -p runtime --example wave2_bench \
//!         -- <corpus.json> [<corpus.json> ...]
//!
//! Build & run prototype:
//!     cargo run --release -p runtime --features eventcursor \
//!         --example wave2_bench -- <corpus.json> [<corpus.json> ...]
//!
//! Output (CSV to stdout):
//!     mode,corpus,bytes,iters,median_ns,mean_ns,mbps_median,mbps_mean
//!
//! Methodology: per corpus, sample 200 iterations with a 50-iteration
//! warm-up, take the min batch of 25 to filter context-switch noise, and
//! report both median and mean.  The same allocator (mimalloc) and the same
//! `black_box` discipline as `bench_json_parity` are used so results are
//! directly comparable to criterion's `track1_generated` row.

use runtime::generated_json;
use std::env;
use std::hint::black_box;
use std::time::Instant;

#[cfg(target_os = "macos")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[cfg(feature = "eventcursor")]
const MODE: &str = "eventcursor";
#[cfg(not(feature = "eventcursor"))]
const MODE: &str = "baseline";

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: wave2_bench <corpus.json> [<corpus.json> ...]");
        std::process::exit(2);
    }
    println!("mode,corpus,bytes,iters,median_ns,mean_ns,mbps_median,mbps_mean");
    for path in &args[1..] {
        match bench_file(path) {
            Ok(row) => println!("{row}"),
            Err(e) => eprintln!("{path}: {e}"),
        }
    }
}

fn bench_file(path: &str) -> Result<String, String> {
    let bytes = std::fs::read(path).map_err(|e| e.to_string())?;
    std::str::from_utf8(&bytes).map_err(|e| e.to_string())?;
    let size = bytes.len();
    let name = std::path::Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(path)
        .to_string();

    // Adapt iteration count by corpus size so total wall time stays close to
    // ~1.5 s per corpus and we exit promptly within the 60-min cap.
    let target_total_ms = 1500u128;
    let probe = run_once(&bytes)?;
    let iters_total = ((target_total_ms * 1_000_000) / probe).max(20).min(20_000) as usize;
    let warmup = (iters_total / 5).max(10);
    let measured = iters_total.max(warmup + 30);

    for _ in 0..warmup {
        let _ = run_once(&bytes)?;
    }

    let mut samples: Vec<u128> = Vec::with_capacity(measured);
    for _ in 0..measured {
        samples.push(run_once(&bytes)?);
    }
    samples.sort_unstable();
    let median = samples[samples.len() / 2] as f64;
    let mean = samples.iter().sum::<u128>() as f64 / samples.len() as f64;
    let mbps_median = size as f64 / median * 1_000.0;
    let mbps_mean = size as f64 / mean * 1_000.0;

    Ok(format!(
        "{MODE},{name},{size},{measured},{median:.0},{mean:.0},{mbps_median:.0},{mbps_mean:.0}"
    ))
}

#[inline(never)]
fn run_once(bytes: &[u8]) -> Result<u128, String> {
    let input = std::str::from_utf8(bytes).map_err(|e| e.to_string())?;
    let t0 = Instant::now();
    let root = generated_json::parse(black_box(input)).map_err(|e| format!("{e:?}"))?;
    black_box(&root);
    let dt = t0.elapsed().as_nanos();
    drop(root);
    Ok(dt.max(1))
}
