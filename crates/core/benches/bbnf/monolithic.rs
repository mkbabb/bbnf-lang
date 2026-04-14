
//! BBNF self-hosting parse benchmark — cold per-parse (tape-first).
//!
//! Parses `.bbnf` grammar files using the generated bootstrap parser.
//! This IS self-hosting: the parser parses its own grammar definition
//! language. Stratified from trivial (json.bbnf, 483B) through the
//! full CSS L4 grammar (15 files, 32KB).

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

use bbnf::grammar::generated::BbnfBootstrap;

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

fn grammar_path(name: &str) -> std::path::PathBuf {
    let manifest = env!("CARGO_MANIFEST_DIR");
    std::path::PathBuf::from(manifest)
        .join("../../grammar")
        .join(name)
}

fn load(name: &str) -> String {
    let path = grammar_path(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path.display(), e))
}

/// Load all `.bbnf` files under a directory, concatenated.
/// For @import grammars, the bootstrap parser parses each file
/// independently; concatenation measures aggregate parse throughput.
fn load_dir(dir: &str) -> Vec<(String, String)> {
    let base = grammar_path(dir);
    let mut files: Vec<(String, String)> = std::fs::read_dir(&base)
        .unwrap_or_else(|e| panic!("{}: {}", base.display(), e))
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("bbnf") {
                let name = path.file_name()?.to_str()?.to_owned();
                let content = std::fs::read_to_string(&path).ok()?;
                Some((name, content))
            } else {
                None
            }
        })
        .collect();
    files.sort_by(|a, b| a.0.cmp(&b.0));
    files
}

// ── Single-file grammars ───────────────────────────────────────────

fn json(b: &mut Bencher) {
    let input = load("json/json.bbnf");
    b.bytes = input.len() as u64;
    {
        BbnfBootstrap::parse(&input)
            .unwrap_or_else(|e| panic!("json.bbnf: parse failed: {:?}", e));
    }
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        let parsed = BbnfBootstrap::parse(black_box(&input)).unwrap();
        black_box(parsed);
    });
}

fn ebnf(b: &mut Bencher) {
    let input = load("ebnf/ebnf.bbnf");
    b.bytes = input.len() as u64;
    {
        BbnfBootstrap::parse(&input)
            .unwrap_or_else(|e| panic!("ebnf.bbnf: parse failed: {:?}", e));
    }
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        let parsed = BbnfBootstrap::parse(black_box(&input)).unwrap();
        black_box(parsed);
    });
}

fn css_pretty(b: &mut Bencher) {
    let input = load("css/pretty.bbnf");
    b.bytes = input.len() as u64;
    {
        BbnfBootstrap::parse(&input)
            .unwrap_or_else(|e| panic!("css/pretty.bbnf: parse failed: {:?}", e));
    }
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        let parsed = BbnfBootstrap::parse(black_box(&input)).unwrap();
        black_box(parsed);
    });
}

fn google_sheets(b: &mut Bencher) {
    let input = load("google-sheets/google-sheets.bbnf");
    b.bytes = input.len() as u64;
    {
        BbnfBootstrap::parse(&input)
            .unwrap_or_else(|e| panic!("google-sheets.bbnf: parse failed: {:?}", e));
    }
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        let parsed = BbnfBootstrap::parse(black_box(&input)).unwrap();
        black_box(parsed);
    });
}

// ── @import grammars (multi-file) ──────────────────────────────────

fn bbnf_self(b: &mut Bencher) {
    let files = load_dir("bbnf");
    let total_bytes: usize = files.iter().map(|(_, c)| c.len()).sum();
    b.bytes = total_bytes as u64;
    for (name, content) in &files {
        BbnfBootstrap::parse(content)
            .unwrap_or_else(|e| panic!("{}: parse failed: {:?}", name, e));
    }
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        for (_, content) in &files {
            let parsed = BbnfBootstrap::parse(black_box(content)).unwrap();
            black_box(parsed);
        }
    });
}

fn css_l4_grammar(b: &mut Bencher) {
    let files = load_dir("css/l4");
    let total_bytes: usize = files.iter().map(|(_, c)| c.len()).sum();
    b.bytes = total_bytes as u64;
    for (name, content) in &files {
        BbnfBootstrap::parse(content)
            .unwrap_or_else(|e| panic!("{}: parse failed: {:?}", name, e));
    }
    bench_with_timeout(b, limits::PARSE_DEFAULT, || {
        for (_, content) in &files {
            let parsed = BbnfBootstrap::parse(black_box(content)).unwrap();
            black_box(parsed);
        }
    });
}

benchmark_group!(
    benches,
    json,
    ebnf,
    css_pretty,
    google_sheets,
    bbnf_self,
    css_l4_grammar,
);
benchmark_main!(benches);
