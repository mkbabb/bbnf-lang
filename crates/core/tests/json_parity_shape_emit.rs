//! AW-V.W3.4 — JSON parity: shape emitter vs walker.
//!
//! The W3.2 shape emitter is active for JSON: `JsonGrammar::parse` routes
//! through `parse_JsonGrammar_value` (the top-level dispatcher + per-shape
//! fns) when `has_full_shape_coverage(ir)` admits it. This suite exercises
//! the shape-emitter path on every production JSON fixture and asserts
//! its parse result is equivalent to the parse produced by the sibling
//! walker path (`dta_run_JsonGrammar`, i.e. the
//! `__dta_walker_inline::run` surface the codegen surfaces as a stable
//! per-grammar symbol).
//!
//! # Parity semantics
//!
//! The two code paths emit structurally different tapes at the raw
//! column level: the walker reserves the root compound at offset 0
//! (pre-order), whereas the shape emitter pushes compounds post-order
//! so the root record lands at `tape.len() - 1`. Both paths produce the
//! SAME parse in every semantic sense — same total record count, same
//! root kind / variant / child count / span, and every descendant
//! record matches when the cursor traversals are walked in lockstep.
//!
//! "Byte-identical" as a naive column-for-column diff would therefore
//! be architecturally nonsensical — the underlying record orderings
//! are intrinsically different and mechanical W3.2 churn cannot
//! reconcile them. The right parity assertion is _the trees_ the
//! consumer sees are equal: this file walks both cursors recursively
//! and compares every structural field at every depth.
//!
//! # Invocation scheme
//!
//! Both paths are emitted alongside each other by `emit_grammar_impl`
//! in `crates/core/src/backend/rust/emitter/grammar.rs`. `parse()`
//! routes through the shape dispatcher when coverage is total; the
//! walker remains callable as `dta_run_<grammar>`. [`parse_via_walker`]
//! replays the walker-branch body of the emitted `parse()` against
//! [`dta_run_JsonGrammar`] so the resulting tape is structurally what
//! the legacy (pre-shape) hot path would have produced.

#![allow(dead_code)]

use std::fs;

use bbnf::runtime::{tape as rt, Parsed};
use bbnf_derive::Parser;
use rt::{PayloadStream, Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset};

// AU.2.4 parity with other test files: every `#[derive(Parser)]` site
// that includes CSS L4 must expose `css_types::parse_hex_color`. We
// don't include CSS here but keep the scaffold for symmetry with the
// sibling tape_parity file so future additions drop in cleanly.
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_s: &str) -> u32 {
        0
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonGrammar;

// ─── Fixture loader ──────────────────────────────────────────────────

/// Load a JSON fixture from `data/json/<name>`. Tolerant of workspace-
/// root vs crate-dir invocation, matching the sibling `tape_parity`
/// harness.
fn load(relpath: &str) -> String {
    let candidates = [
        format!("../../data/{}", relpath),
        format!("../data/{}", relpath),
        format!("data/{}", relpath),
        relpath.to_string(),
    ];
    for path in &candidates {
        if let Ok(contents) = fs::read_to_string(path) {
            return contents;
        }
    }
    panic!(
        "could not find data file '{}'; tried: {:?}",
        relpath, candidates
    );
}

// ─── Walker-path parser ──────────────────────────────────────────────
//
// The walker surface is the `dta_run_JsonGrammar` fn the codegen
// emits alongside the shape dispatcher — a thin wrapper over
// `__dta_walker_inline::run` with a stable per-grammar symbol. The
// shape-coverage gate in `has_full_shape_coverage` routes the
// inherent `parse()` through the shape dispatcher for JSON; this
// helper forces the walker path instead so the two paths can be
// compared head-to-head.

/// Parse `input` through the walker path and return a `Parsed`
/// carrying the produced tape. Mirrors the walker-branch body of the
/// emitted `parse()` fn from `emit_grammar_impl`:
///
/// 1. Allocate a sized `TapeBuilder` + enable inline frame depth.
/// 2. Build `PayloadStream` via the grammar profile's leaves-per-byte.
/// 3. Run the stage-1 structural scan (`scan_structural` +
///    `StructuralAlphabet::from_profile`).
/// 4. Call `dta_run_JsonGrammar` with the builder's columns /
///    frame-depth refs + the PSI stream.
/// 5. `psi.fill_columns` the typed-payload columns.
/// 6. `builder.finish()` to derive sib-skip / frame-depth / arena.
///
/// The re-exported `psi_with_capacity` would collide with CSS /
/// BBNF / Sheets / EBNF siblings if this file later includes them
/// in the same scope; the collision is avoided by calling
/// `PayloadStream::with_capacity_for` directly against the
/// grammar's associated-constant profile. `JsonGrammar::GRAMMAR_PROFILE`
/// exists precisely for this disambiguation per the doc comment in
/// `parsed.rs` on multi-grammar test files.
fn parse_via_walker(input: &str) -> Parsed<'_, JsonGrammar> {
    let profile = &JsonGrammar::GRAMMAR_PROFILE;
    let mut builder = TapeBuilder::with_capacity(profile.capacity_for(input.len()));
    builder.enable_inline_frame_depth();
    let mut psi = PayloadStream::with_capacity_for(profile, input.len());

    // Stage-1 structural pre-pass — alphabet is a `const fn` of the
    // profile, so the construction is compile-time-cheap.
    let alphabet = bbnf::runtime::scan::StructuralAlphabet::from_profile(profile);
    let idx = bbnf::runtime::scan::scan_structural(input.as_bytes(), &alphabet);

    let root_off = {
        let (columns, frame_depth) = builder.columns_and_frame_depth_mut();
        dta_run_JsonGrammar(input.as_bytes(), &idx, columns, &mut psi, frame_depth)
    }
    .expect("walker-path parse must succeed on well-formed JSON");

    psi.fill_columns(input.as_bytes(), builder.columns_mut(), profile);
    let tape = builder.finish().expect("tape finalise must succeed");
    Parsed::new(tape, input, root_off)
}

/// Parse `input` through the shape-emitter path — the grammar's
/// inherent `parse()` routes here when `has_full_shape_coverage`
/// admits full dispatch (JSON does post-W3.2).
fn parse_via_shapes(input: &str) -> Parsed<'_, JsonGrammar> {
    JsonGrammar::parse(input).expect("shape-path parse must succeed on well-formed JSON")
}

// ─── Cursor-walk parity ──────────────────────────────────────────────

/// Structural signature of a single tape record — the fields a
/// consumer reads through a [`TapeCursor`]. Spans are normalised
/// against the shared input, so a post-order vs pre-order emission
/// that yields the same parsed tree produces the same sequence of
/// signatures when the walk is pre-order-DFS starting from each
/// path's respective `root_offset`.
#[derive(Debug, PartialEq, Eq)]
struct NodeSig {
    kind: TapeKind,
    variant_idx: u8,
    meta_idx: u8,
    span_lo: u32,
    span_hi: u32,
    child_count: usize,
}

impl NodeSig {
    fn from_cursor(cursor: TapeCursor<'_>) -> Self {
        let (lo, hi) = cursor.span();
        Self {
            kind: cursor.kind(),
            variant_idx: cursor.variant_idx(),
            meta_idx: cursor.meta_idx(),
            span_lo: lo,
            span_hi: hi,
            child_count: cursor.children().count(),
        }
    }
}

/// Path through the tree, used in divergence reporting.
///
/// Each entry is the (child-index, kind) of the step taken into the
/// next record. `fixture: value.object[3].pair.value.string` is
/// much more useful than a raw `TapeOffset`.
#[derive(Debug, Default)]
struct Breadcrumb {
    steps: Vec<(usize, TapeKind)>,
}

impl Breadcrumb {
    fn push(&mut self, child_idx: usize, kind: TapeKind) {
        self.steps.push((child_idx, kind));
    }

    fn pop(&mut self) {
        self.steps.pop();
    }

    fn render(&self) -> String {
        if self.steps.is_empty() {
            return "<root>".to_string();
        }
        self.steps
            .iter()
            .map(|(idx, kind)| format!("[{}:{:?}]", idx, kind))
            .collect::<Vec<_>>()
            .join(" / ")
    }
}

/// Recursively compare two cursors in lockstep. Asserts every
/// structural field matches at the current depth and every child
/// matches below.
///
/// Pre-order DFS ensures that if post-order vs pre-order emission
/// yields the same consumer-visible tree, this walk confirms
/// equivalence despite the differing raw record orderings.
fn assert_cursor_parity(
    fixture: &str,
    walker: TapeCursor<'_>,
    shapes: TapeCursor<'_>,
    crumb: &mut Breadcrumb,
) {
    let w = NodeSig::from_cursor(walker);
    let s = NodeSig::from_cursor(shapes);
    assert_eq!(
        w,
        s,
        "{}: node diverged at {} — walker = {:?}, shapes = {:?}",
        fixture,
        crumb.render(),
        w,
        s,
    );

    // Walk children in lockstep. `children()` yields direct
    // descendants in emission order — the same logical order on
    // both paths, since the emission-order invariant is fixed at the
    // grammar level (Seq-left-to-right / Alt-chosen-branch).
    let walker_children: Vec<TapeCursor<'_>> = walker.children().collect();
    let shapes_children: Vec<TapeCursor<'_>> = shapes.children().collect();
    assert_eq!(
        walker_children.len(),
        shapes_children.len(),
        "{}: child-count diverged at {} — walker = {}, shapes = {}",
        fixture,
        crumb.render(),
        walker_children.len(),
        shapes_children.len(),
    );

    for (i, (wc, sc)) in walker_children
        .iter()
        .zip(shapes_children.iter())
        .enumerate()
    {
        crumb.push(i, wc.kind());
        assert_cursor_parity(fixture, *wc, *sc, crumb);
        crumb.pop();
    }
}

// ─── Per-fixture parity harness ──────────────────────────────────────

/// Parse `fixture` through both paths, assert walker and shape-emitter
/// produce equivalent parses, and spot-check the top-level shape.
fn assert_json_parity(fixture: &str) {
    let input = load(&format!("json/{}", fixture));

    let walker_parsed = parse_via_walker(&input);
    let shapes_parsed = parse_via_shapes(&input);

    // Sanity: both tapes are non-empty. An empty tape on a real
    // fixture would be a silent regression that bypasses the cursor
    // walk below.
    assert!(
        walker_parsed.tape().len() > 0,
        "{}: walker produced empty tape",
        fixture
    );
    assert!(
        shapes_parsed.tape().len() > 0,
        "{}: shape emitter produced empty tape",
        fixture
    );

    // Total record count — coarse gate. The two paths emit in
    // different orders (pre- vs post-), but the same grammar
    // structure must produce the same record _count_.
    assert_eq!(
        walker_parsed.tape().len(),
        shapes_parsed.tape().len(),
        "{}: total record count diverged — walker = {}, shapes = {}",
        fixture,
        walker_parsed.tape().len(),
        shapes_parsed.tape().len(),
    );

    // Zero-divergence assertion: walk both cursors in lockstep.
    // Per the AW-V.md §W3.4 hard gate — every descendant record
    // must agree in (kind, variant_idx, meta_idx, span, child_count).
    let walker_cursor = cursor_for(walker_parsed.tape(), walker_parsed.root_offset());
    let shapes_cursor = cursor_for(shapes_parsed.tape(), shapes_parsed.root_offset());
    let mut crumb = Breadcrumb::default();
    assert_cursor_parity(fixture, walker_cursor, shapes_cursor, &mut crumb);
}

/// Construct a cursor at `offset` on `tape`. Wrapper around
/// `TapeCursor::new` kept tight for readability at the call site.
#[inline]
fn cursor_for(tape: &Tape, offset: TapeOffset) -> TapeCursor<'_> {
    TapeCursor::new(tape, offset)
}

// ─── Per-fixture tests ───────────────────────────────────────────────

#[test]
fn json_data_walker_vs_shape() {
    assert_json_parity("data.json");
}

#[test]
fn json_twitter_walker_vs_shape() {
    assert_json_parity("twitter.json");
}

#[test]
fn json_citm_walker_vs_shape() {
    assert_json_parity("citm_catalog.json");
}

#[test]
fn json_canada_walker_vs_shape() {
    assert_json_parity("canada.json");
}

#[test]
fn json_data_xl_walker_vs_shape() {
    assert_json_parity("data_xl.json");
}
