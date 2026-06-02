//! SK-V17 S-P1 P1-D samply profiling driver: benched CSS track1 path only.
//! Loops `track1::parser::parse_full` (= `emit_full_parse`, the live benched
//! CSS Track 1 recognition+count scan) over the SK-V14 corpora so samply can
//! attribute self-time to the scan leaves (`find_component_delim`,
//! `consume_balanced_at`, `skip_ws_comments`) and the String emit.
//!
//! Run: samply record --save-only -o /tmp/skv17-p1d-track1.json.gz -- \
//!        target/release/css_track1_profile <iters>

use bbnf_bench::css_l4_corpus::load_all;
use mimalloc::MiMalloc;
use runtime::generated_css_l4_declaration_values as track1;
use std::hint::black_box;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn main() {
    let iters: usize = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(4000);
    let corpora = load_all().expect("load SK-V14 corpora");
    let srcs: Vec<String> = corpora
        .iter()
        .map(|c| String::from_utf8(c.bytes.clone()).unwrap())
        .collect();
    let mut acc = 0usize;
    for _ in 0..iters {
        for s in &srcs {
            let out = track1::parser::parse_full(black_box(s)).expect("parse");
            acc = acc.wrapping_add(black_box(out.len()));
        }
    }
    println!("acc={acc}");
}
