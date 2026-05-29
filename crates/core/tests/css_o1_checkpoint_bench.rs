//! Cold single-sheet retime exposing the O(1) checkpoint win.
//!
//! The prior speculative-parse checkpoint deep-cloned the growing
//! `StyleSheet.rules` Vec on every checkpoint site, giving O(N^2) cost on
//! a single large sheet. This bench measures `CssL4Parser::parse` cold
//! (one warmup-free sample) over the full `data/css/bootstrap.css`
//! single-sheet input, the figure the diagnostic stub lifted 62x.

use bbnf::grammar::generated::css_l4::CssL4Parser;
use std::hint::black_box;
use std::path::Path;
use std::time::Instant;

fn time_one(source: &str) -> (f64, usize) {
    let start = Instant::now();
    let doc = CssL4Parser::parse(source).expect("parse bootstrap single sheet");
    black_box(&doc);
    let elapsed = start.elapsed();
    let bytes = source.len();
    let mbps = (bytes as f64) / elapsed.as_secs_f64() / 1_000_000.0;
    (mbps, bytes)
}

#[test]
fn css_o1_checkpoint_bootstrap_single_sheet_cold() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for rel in ["data/css/bootstrap.css", "skinny/corpora/css-l4-sk-v14/bootstrap-5.3.3.min.css"] {
        let path = root.join(rel);
        let Ok(source) = std::fs::read_to_string(&path) else {
            eprintln!("CSS_O1_BENCH skip missing={rel}");
            continue;
        };
        let (mbps, bytes) = time_one(&source);
        eprintln!("CSS_O1_BENCH file={rel} bytes={bytes} cold_mbps={mbps:.3}");
    }
}
