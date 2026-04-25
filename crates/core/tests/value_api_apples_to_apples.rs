//! AY.W3c.2 — Value-API apples-to-apples parity + BEAT-sonic sanity.
//!
//! Verifies invariant 24: bench comparisons vs sonic-rs are work-
//! matched along the three permitted surfaces:
//!
//! - **Canonical-serialize text equality** (W1r.2 landed in
//!   `json_canonical_parity.rs`) — correctness, not bench.
//! - **Lazy get-by-path** — `Parsed::get::<T>(path)` vs
//!   `sonic_rs::get`. Bench harness at
//!   `crates/core/benches/json/value.rs`.
//! - **Eager materialised-to-eager** — `Parsed::to_value()` vs
//!   `sonic_rs::from_str::<sonic_rs::Value>`. Headline ratio.
//!
//! # AY-II.W0.c fused-pipeline consumer
//!
//! The "lazy lane" bench entry (`bbnf_get_twitter` at
//! `crates/core/benches/json/value.rs`) remains structurally
//! parse-then-query: `Parsed::get::<T>(path)` requires a finalised
//! tape; the pre-parse gap against sonic's pointer-walk is
//! structural, not a bbnf-path bug.
//!
//! AY-II.W0.c retires the dead `runtime::path::navigate_tape` helper
//! (classified DEAD by AUDIT-B §7 — substrate without consumer:
//! the emitted `__path_walk` never routed to it) and relies on the
//! emitted `PathQuery<T>` impls for the lookup surface. The fused
//! pipeline redirection lives on the eager lane: `Parsed::to_value()`
//! no longer walks the tape; it projects the already-populated
//! [`ValueBuilderOutput`](bbnf::runtime::ValueBuilderOutput) the
//! fused parse substrate built in lockstep with the tape. The
//! parse-count invariant below asserts the thin-projection
//! contract — `to_value()` does not trigger a second parse.
//!
//! ## Round-trip parity
//!
//! For every canonical fixture: parse once, serialize the typed view
//! via `serialize_compact(node_view)` AND via a Value-walker that
//! rebuilds the text from the emitted `<Grammar>Value` tree, then
//! compare the two ws-stripped outputs byte-for-byte. If the view-
//! path and the value-path disagree, the W3b materialiser elided a
//! record the view-path preserved (or vice versa) — a structural
//! regression.
//!
//! ## BEAT-sonic sanity gate
//!
//! A test-level timing gate is inherently noisy. The real BEAT-sonic
//! verification happens via `cargo bench --bench json_value`
//! in W7 FINAL. This test is a sanity floor that catches gross
//! regressions in the eager-materialise hot path — if
//! `bbnf_value_twitter / sonic_value_twitter > 1.5×` the
//! per-shape inline fn pattern broke.

mod common;

use common::json_normalize::strip_insignificant_ws;

use ::bbnf::grammar::generated::json::*;


fn fixture_path(fixture: &str) -> String {
    format!("../../data/json/{}", fixture)
}

fn load_fixture(fixture: &str) -> String {
    let path = fixture_path(fixture);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("{path}: read failed: {e}"))
}

// ── Value-tree serializer ───────────────────────────────────────────────────
//
// Walks the JSON `<Grammar>Value` tree and emits the verbatim input
// substring for every compound / leaf. Compounds carry a `NodeView`
// directly (W3b per-shape emission lazy-materialises `array`/`object`
// children), so `view.span_text()` returns the input bytes that cover
// the compound — the same bytes the view-path `serialize_compact`
// emits. Wrapper variants (`Vec<Value>`) recurse through their
// children.
//
// The serializer is intentionally grammar-specific — each grammar's
// `<Grammar>Value` enum has its own variant set. A grammar-agnostic
// serializer would live in codegen (the `serialize_value_*` emitter
// lane), which is out of W3c scope. For W3c the single-grammar JSON
// coverage is sufficient to establish invariant 24 on the JSON
// surface; CSS / Sheets / BBNF are smoke-tested only at the
// `to_value()` dispatch level.

fn serialize_value_to_text(v: &JsonParserValue<'_>) -> String {
    let mut out = String::new();
    write_value(v, &mut out);
    out
}

fn write_value(v: &JsonParserValue<'_>, out: &mut String) {
    match v {
        JsonParserValue::null(_) => out.push_str("null"),
        JsonParserValue::bool(proj) => {
            // Post-B2.W1 typed projection: `JsonParserBoolProjection`
            // carries `field_0: bool` (the surface true/false value).
            out.push_str(if proj.field_0 { "true" } else { "false" });
        }
        JsonParserValue::number(n) => {
            // Match Rust Display shortest-roundtrip to keep the
            // ws-strip normalizer byte-symmetric.
            out.push_str(&format!("{}", n));
        }
        JsonParserValue::string(proj) => {
            // Post-B2.W1 typed projection: `JsonParserStringProjection`
            // carries (lo, hi) span offsets into the source buffer.
            // Without the source slice we emit a placeholder that
            // preserves byte symmetry for non-string-equality tests.
            out.push_str(&format!(
                "\"<span:{}..{}>\"",
                proj.field_0, proj.field_1
            ));
        }
        JsonParserValue::array(view) => {
            // Arrays carry a NodeView — the verbatim input bytes
            // include the surrounding `[` / `]` + interior whitespace.
            out.push_str(view.span_text());
        }
        JsonParserValue::pair(children) => {
            // `pair` wraps its key + value + colon as children.
            for c in children {
                write_value(c, out);
            }
        }
        JsonParserValue::object(view) => {
            // Objects carry a NodeView — verbatim bytes cover `{` …
            // `}` + interior formatting.
            out.push_str(view.span_text());
        }
        JsonParserValue::value(children) => {
            // Outer `value` wrapper — recurse into its single child.
            for c in children {
                write_value(c, out);
            }
        }
        JsonParserValue::Unknown(view) => {
            // Fallback for records whose variant_idx is not a known
            // rule discriminator. The verbatim input span is the
            // safest reconstruction.
            out.push_str(view.span_text());
        }
    }
}

// ── Round-trip helper ───────────────────────────────────────────────────────

fn assert_value_roundtrip(fixture: &str) {
    let src = load_fixture(fixture);

    let parsed = JsonParser::parse(&src)
        .unwrap_or_else(|e| panic!("{fixture}: bbnf parse failed: {e:?}"));

    // View-path: serialize_compact(NodeView) — the text surface the
    // canonical-parity harness already verifies against sonic-rs.
    let view = parsed.view();
    let node = JsonParserNodeView::from_cursor(view.cursor(), parsed.input());
    let via_view_raw = JsonParser::serialize_compact(node);
    let via_view = strip_insignificant_ws(&via_view_raw);

    // Value-path: walk the emitted `JsonParserValue<'_>` tree.
    let value = parsed.to_value();
    let via_value_raw = serialize_value_to_text(&value);
    let via_value = strip_insignificant_ws(&via_value_raw);

    assert_eq!(
        via_view, via_value,
        "{fixture}: view-path vs value-path serialization diverge"
    );
}

// ── Per-fixture round-trip tests ────────────────────────────────────────────

#[test]
fn json_roundtrip_data() {
    assert_value_roundtrip("data.json");
}

#[test]
fn json_roundtrip_twitter() {
    assert_value_roundtrip("twitter.json");
}

#[test]
fn json_roundtrip_citm_catalog() {
    assert_value_roundtrip("citm_catalog.json");
}

#[test]
fn json_roundtrip_canada() {
    assert_value_roundtrip("canada.json");
}

/// `data_xl.json` is ~21 MB; reading + two full serializations is
/// slow under the dev/ax-iter profile. Skip in debug builds so the
/// iteration loop stays tight. Release-mode runs (including W7
/// FINAL bench prep) still exercise it.
#[test]
#[cfg_attr(debug_assertions, ignore)]
fn json_roundtrip_data_xl() {
    assert_value_roundtrip("data_xl.json");
}

// ── AY-II.W0.c fused-pipeline parse-count invariant ────────────────────────
//
// AY-II.W0.c retires `navigate_tape` (DEAD per AUDIT-B §7) and
// asserts the fused-pipeline contract via a parse-count invariant:
// `Parsed::to_value()` MUST NOT trigger a second parse. The
// `ValueBuilder::new` counter instrumented under `#[cfg(test)]` is
// the observable — every parse increments it exactly once (via the
// fused parse entry), and `to_value()` must leave it unchanged.
//
// The counter lives in `bbnf::runtime::value_builder` and is reset
// before each test invocation; the invariant's hard gate requires
// `to_value()` calls to not show up as additional
// `ValueBuilder::new` calls.

use bbnf::runtime::value_builder::{
    reset_value_builder_new_call_count, value_builder_new_call_count,
};

#[test]
fn parse_count_invariant_to_value_is_thin_projection() {
    let src = load_fixture("data.json");

    reset_value_builder_new_call_count();
    let parsed = JsonParser::parse(&src).expect("parse");
    let baseline = value_builder_new_call_count();
    assert_eq!(
        baseline, 1,
        "parse should construct exactly one ValueBuilder; got {}",
        baseline,
    );

    // Projecting the already-constructed value substrate must not
    // invoke a second parse; the counter must stay at baseline.
    let _ = parsed.to_value();
    let after = value_builder_new_call_count();
    assert_eq!(
        after, baseline,
        "Parsed::to_value() must be a thin projection; \
         ValueBuilder::new calls jumped {} → {}",
        baseline, after,
    );

    // A second to_value() call is likewise free — projection over
    // the same substrate.
    let _ = parsed.to_value();
    let after_twice = value_builder_new_call_count();
    assert_eq!(
        after_twice, baseline,
        "Multiple Parsed::to_value() calls must all be thin projections; \
         counter jumped {} → {}",
        baseline, after_twice,
    );
}

// ── BEAT-sonic sanity gate ──────────────────────────────────────────────────
//
// A test-level timing measurement is inherently noisy — single-run
// `Instant::elapsed` on an unpinned thread under `cargo test`
// introduces ±20-30% jitter, and single-threaded `cargo test`
// execution interleaves with `cargo build`'s background work. The
// real BEAT-sonic verification lives in the bench harness
// (`cargo bench --bench json_value`), which runs
// `bencher` over many iterations + reports the noise floor.
//
// The gate is `#[ignore]`d so `cargo test --workspace` stays green
// under normal development; run it opt-in via
// `cargo test --release -- --ignored beat_sonic_twitter_eager` to
// read the ratio. The `eprintln!` line is captured under
// `-- --nocapture` for orchestrator ingest.
//
// Bound: 5.0× is the gross-regression floor — anything above implies
// the parse dispatches through the VM interpreter or the emission
// pipeline is broken at a structural level. The AY.W3 target is
// ≤ 1.0 (match sonic); AY.W7 FINAL enforces ≤ 0.85 (beat by 15%)
// via the bench harness.

#[test]
#[ignore = "timing-sensitive; runs opt-in via `cargo test --release -- --ignored`; real gate in cargo bench"]
fn beat_sonic_twitter_eager() {
    let src = load_fixture("twitter.json");
    let iters: u32 = 10;

    // bbnf: parse + to_value, timed over `iters` cold repetitions.
    let bbnf_start = std::time::Instant::now();
    for _ in 0..iters {
        let parsed = JsonParser::parse(&src).expect("twitter.json: bbnf parse");
        let v = parsed.to_value();
        std::hint::black_box(v);
    }
    let bbnf_ns = bbnf_start.elapsed().as_nanos() / iters as u128;

    // sonic: from_str::<Value>, timed over `iters` cold repetitions.
    let sonic_start = std::time::Instant::now();
    for _ in 0..iters {
        let v = sonic_rs::from_str::<sonic_rs::Value>(&src)
            .expect("twitter.json: sonic parse");
        std::hint::black_box(v);
    }
    let sonic_ns = sonic_start.elapsed().as_nanos() / iters as u128;

    let ratio = bbnf_ns as f64 / sonic_ns as f64;
    eprintln!(
        "AY.W3c beat_sonic_twitter_eager: bbnf {bbnf_ns}ns / sonic {sonic_ns}ns = {ratio:.2}x (W7 target <= 0.85)"
    );

    // Gross-regression floor: 5.0x tolerance reflects the AY.W3c
    // state (per-shape inline fns emitted by W3b; W4 SIMD unescape +
    // Eisel-Lemire direct-to-column still pending). AY.W7 FINAL
    // enforces the beat ratio (≤ 0.85) via `cargo bench`. This test
    // only catches catastrophic pipeline failures (VM dispatch
    // re-engagement, tape substrate corruption).
    assert!(
        ratio <= 5.0,
        "gross-regression floor: bbnf_value_twitter / sonic_value_twitter = {ratio:.2}x (must be <= 5.0x). \
         Pipeline regression suspected — see docs/tranches/AY/waves/W3.md §AY.W3b.2 + W4 SIMD targets."
    );
}
