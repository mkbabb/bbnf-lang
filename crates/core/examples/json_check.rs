#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use ::bbnf::grammar::generated::json::*;

/// `json_check [CORPUS_DIR]`: with a directory, inputs are read from it only (a record names its
/// input by content hash, so the harness passes a tracked, hashed corpus); without one, the
/// corpus resolver (`bbnf_corpus`, provided by `cargo xtask corpus fetch`) applies.
fn bench_file(dir: Option<&str>, name: &str) {
    let candidates: Vec<String> = match dir {
        Some(d) => vec![format!("{}/{}", d.trim_end_matches('/'), name)],
        None => vec![bbnf_corpus::path(&format!("json/{}", name)).display().to_string()],
    };
    let path = match candidates.iter().find(|p| std::path::Path::new(p).exists()) {
        Some(p) => p.clone(),
        None => {
            eprintln!("skip {} (not found)", name);
            return;
        }
    };
    let input = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(_) => {
            eprintln!("skip {}", name);
            return;
        }
    };
    let len = input.len();

    // Warm-up — single parse to prime caches and validate the input.
    let _ = std::hint::black_box(
        JsonParser::parse(std::hint::black_box(&input)).expect("parse failed"),
    );

    // Cold — fresh document + parser state per iteration. The parser
    // returns an owning `JsonDocument` from the generated `parse`
    // entry point.
    let n = if len > 1_000_000 { 5 } else { 20 };
    let start = std::time::Instant::now();
    for _ in 0..n {
        let parsed = JsonParser::parse(std::hint::black_box(&input)).expect("parse failed");
        let _ = std::hint::black_box(&parsed);
    }
    let cold = start.elapsed() / n as u32;

    let mb = |d: std::time::Duration| len as f64 / d.as_secs_f64() / 1e6;
    println!("{:25} {:>8}B  cold:{:>6.0} MB/s", name, len, mb(cold));
}

fn main() {
    let dir = std::env::args().nth(1);
    for name in [
        "data.json",
        "twitter.json",
        "citm_catalog.json",
        "canada.json",
        "data_xl.json",
    ] {
        bench_file(dir.as_deref(), name);
    }
}
