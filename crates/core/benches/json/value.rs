//! JSON value benchmark — honest side-by-side comparison.
//!
//! Puts BBNF monolithic parse + full tape walk next to sonic-rs
//! `Value` parse (SIMD, full unescape, arena-allocated) on the same
//! datasets in one bench binary so the numbers are directly
//! comparable with identical measurement overhead.
//!
//! AU.3.2: the bbnf side now walks the tape after parse, reading
//! `payload_f64` on numeric leaves, `payload_bool` on bool leaves,
//! `payload_u8` on null leaves, and the new `payload_string` on
//! string leaves. This matches the work sonic-rs does in
//! `from_str::<Value>` (materialising a full typed value tree), so
//! the bbnf/sonic ratio becomes an honest comparison.
//!
//! AU.6.6: the small-variant benches are named `*_data_s` so
//! `bencher`'s substring filter doesn't conflate `data` with
//! `data_xl` in per-entry profile attribution.
//!
//! AW-V.W3-bench-fix: the shape-emitter `parse()` path builds a tape
//! + walks it — the tape construction's structural + PSI bookkeeping
//! adds ~2× overhead vs the prototype's visitor-direct path. This
//! bench adds the visitor lane (`bbnf_visitor_*`) that exercises
//! `JsonParser::parse_with_visitor::<V>` — bypasses the tape entirely,
//! materialising into a packed `ValueVisitor` that mirrors the
//! prototype's `json_prototype::ValueVisitor` shape. Gate: within
//! ±5% of prototype's `proto_value_*` on every entry.
//!
//! AY.W3c.1: adds two Value-API perf lanes matching invariant 24
//! (apples-to-apples).
//!
//! - **Lazy lane** (`bbnf_get_twitter` vs `sonic_get_twitter`): parses
//!   `twitter.json` and extracts a single scalar via path query
//!   `["statuses", 0, "text"]`. bbnf uses the document-owned
//!   `get::<&str>(path)`; sonic uses `sonic_rs::get` (the
//!   lazy get-by-path entry). 2 bench entries — twitter only.
//! - **Eager lane** (`bbnf_value_<fx>` vs `sonic_value_<fx>`):
//!   materialises the full typed value tree. bbnf uses the
//!   document-owned `to_value()` API; sonic uses
//!   `sonic_rs::from_str::<sonic_rs::Value>`. 10 bench entries — 5
//!   fixtures × 2 parsers.
//!
//! The BEAT-sonic headline is `bbnf_value_twitter / sonic_value_twitter`
//! on the eager lane; target at AY close is ≤ 0.85 (beat by 15%). The
//! existing `bench_bbnf` (tape-walk) and `bench_sonic` (typed value)
//! lanes remain as the raw-parse comparison; the new `bench_bbnf_value`
//! / `bench_sonic_value` lanes are the Value-API headline.

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf::runtime::tape::{
    ArrayVisitor, GrammarVisitor, KeywordVisitor, NumberVisitor, ObjectVisitor, StringVisitor,
    Tape, TapeCursor, TapeKind, TapeOffset,
};
use divan::black_box;
use json_prototype::{self as proto, parse_json};

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

use ::bbnf::grammar::generated::json::*;

fn load(name: &str) -> String {
    let path = format!("../../data/json/{}", name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path, e))
}

// ── BBNF monolithic (typed AST, arena-allocated) + tape walk ───────────────
//
// The tape walk reaches every leaf and reads its payload, mirroring the
// work sonic-rs does in `from_str::<Value>`. Any shortcut that skips
// the walk makes the comparison dishonest — see AU.3.2 narrative.

/// Recursive descent that touches every tape record, exercising the
/// `payload_*` accessors so the compiler cannot elide the reads.
///
/// Returns a sum that folds in every leaf payload so `black_box` has a
/// meaningful value to consume; the sum's exact meaning doesn't
/// matter, only that every payload read is observed.
fn walk_tape(tape: &Tape, root: TapeOffset, input: &str) -> u64 {
    let cursor = TapeCursor::new(tape, root);
    walk_cursor(tape, cursor, input)
}

fn walk_cursor(tape: &Tape, cursor: TapeCursor<'_>, input: &str) -> u64 {
    let rec = cursor.record();
    let mut acc: u64 = rec.span_lo as u64;
    match rec.kind() {
        TapeKind::Span => {
            // AU.3.1: decoded JSON strings live in the arena; read
            // via `payload_string`. AU.3.2 / W6: borrow-safe strings
            // (no `\` byte in the source span) skip the arena read
            // entirely — `payload_string_with_source` slices
            // `input[span_lo + 1 .. span_hi - 1]` directly. Owned
            // strings (escape-bearing) still go through the arena
            // frame. Both branches return `&str` without a copy and
            // skip UTF-8 validation on the hot path (decoder kernel
            // contract).
            if let Some(s) = tape.payload_string_with_source(rec, input.as_bytes()) {
                acc = acc.wrapping_add(s.len() as u64);
            } else {
                acc = acc.wrapping_add(rec.span_hi as u64);
            }
        }
        TapeKind::Regex => {
            // Numeric leaf — `number` rule with `-> f64`.
            if let Some(f) = tape.payload_f64(rec) {
                acc = acc.wrapping_add(f.to_bits());
            }
        }
        TapeKind::Literal => {
            // Bool / null constants — `payload_bool` for bool and
            // `payload_u8` for null (the `"null" -> 0u8` sentinel).
            if let Some(b) = tape.payload_bool(rec) {
                acc = acc.wrapping_add(b as u64);
            } else if let Some(u) = tape.payload_u8(rec) {
                acc = acc.wrapping_add(u as u64);
            }
        }
        TapeKind::Epsilon => {}
        TapeKind::KvPair => {
            if let Some(bytes) = tape.payload_bytes(rec, 16) {
                acc = acc.wrapping_add(bytes.len() as u64);
            }
        }
        _ => {
            // AU.3.2: `cursor.children_zero_alloc()` returns a
            // 24-byte `ChildIter` that walks the tape backward via
            // `child_off` links, yielding children in reverse source
            // order without any heap allocation. The walker
            // accumulates into a `u64` via `wrapping_add`, so child
            // order is irrelevant; the bench/sonic ratio gains come
            // from eliminating the per-compound `Vec` allocation
            // that `cursor.children()` (the source-order accessor)
            // incurs.
            for child in cursor.children_zero_alloc() {
                acc = acc.wrapping_add(walk_cursor(tape, child, input));
            }
        }
    }
    // Suppress unused warning on `input` — kept in the signature for
    // future walkers that need span-to-text resolution.
    let _ = input;
    acc
}

macro_rules! bench_bbnf {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            {
                let parsed = JsonParser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                let view = parsed.view();
                let root_off = view.cursor().offset();
                let v = walk_tape(parsed.tape(), root_off, &input);
                black_box(v);
            }
            bench_with_timeout(
                b,
                limits::JSON_PARSE,
                |input: String| {
                    let parsed = JsonParser::parse(black_box(&input)).unwrap();
                    let view = parsed.view();
                    let root_off = view.cursor().offset();
                    let v = walk_tape(parsed.tape(), root_off, black_box(&input));
                    black_box(v);
                    black_box(parsed);
                },
                &input,
            );
        }
    };
}

bench_bbnf!(bbnf_data_s, "data.json");
bench_bbnf!(bbnf_twitter, "twitter.json");
bench_bbnf!(bbnf_citm, "citm_catalog.json");
bench_bbnf!(bbnf_canada, "canada.json");
bench_bbnf!(bbnf_data_xl, "data_xl.json");

// ── sonic-rs (SIMD, arena-allocated, full unescape) ────────────────────────

macro_rules! bench_sonic {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            sonic_rs::from_str::<sonic_rs::Value>(&input)
                .expect(concat!($file, ": sonic-rs parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap()
            });
        }
    };
}

bench_sonic!(sonic_data_s, "data.json");
bench_sonic!(sonic_twitter, "twitter.json");
bench_sonic!(sonic_citm, "citm_catalog.json");
bench_sonic!(sonic_canada, "canada.json");
bench_sonic!(sonic_data_xl, "data_xl.json");

// ── BBNF visitor-path (AW-V.W3-bench-fix) ──────────────────────────────────
//
// The shape-emitter-produced `JsonParser::parse_with_visitor::<V>`
// entry bypasses the tape entirely; visitor method calls materialise
// directly into a packed `ValueVisitor` whose layout mirrors
// `json_prototype::ValueVisitor`. Gate: within ±5% of the
// prototype's `proto_value_*` on every entry.

/// Local packed `ValueVisitor` mirroring
/// `json_prototype::ValueVisitor`'s shape while implementing the
/// `tape` per-shape visitor trait hierarchy. Borrow-path strings
/// reference the input directly when pointer identity resolves inside
/// the registered window; escape-decoded strings copy into the
/// document arena. Compound slot pre-reservation + in-place
/// back-patch matches sonic-rs's `DocumentVisitor`.
struct ValueVisitor {
    doc: proto::Document,
    input_lo: usize,
    input_hi: usize,
    compounds: Vec<CompoundMark>,
}

#[derive(Copy, Clone)]
struct CompoundMark {
    kind: CompoundKind,
    slot_idx: u32,
}

#[derive(Copy, Clone)]
enum CompoundKind {
    Array,
    Object,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ValueVisitorError;

impl ValueVisitor {
    #[inline]
    fn with_input(input: &[u8]) -> Self {
        let input_lo = input.as_ptr() as usize;
        let input_hi = input_lo + input.len();
        Self {
            doc: proto::Document::with_input_len(input.len()),
            input_lo,
            input_hi,
            compounds: Vec::new(),
        }
    }

    #[inline]
    fn finish(self) -> proto::Document {
        self.doc
    }

    #[inline(always)]
    fn intern_str(&mut self, bytes: &[u8]) -> proto::StringSpan {
        let ptr = bytes.as_ptr() as usize;
        if ptr >= self.input_lo && ptr + bytes.len() <= self.input_hi {
            proto::StringSpan::borrowed(bytes.as_ptr(), bytes.len())
        } else {
            let offset = self.doc.arena.len() as u32;
            self.doc.arena.extend_from_slice(bytes);
            proto::StringSpan::arena(offset, bytes.len())
        }
    }

    #[inline(always)]
    fn emit(&mut self, v: proto::Value) {
        if self.compounds.is_empty() {
            self.doc.root = Some(v);
        } else {
            self.doc.nodes.push(v);
        }
    }
}

impl GrammarVisitor for ValueVisitor {
    type Error = ValueVisitorError;
}

impl ObjectVisitor for ValueVisitor {
    #[inline(always)]
    fn begin_object(&mut self) -> Result<(), Self::Error> {
        let slot_idx = self.doc.nodes.len() as u32;
        self.doc.nodes.push(proto::Value::Null);
        self.compounds.push(CompoundMark {
            kind: CompoundKind::Object,
            slot_idx,
        });
        Ok(())
    }

    #[inline(always)]
    fn key(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let span = self.intern_str(bytes);
        self.doc.nodes.push(proto::Value::String(span));
        Ok(())
    }

    #[inline(always)]
    fn end_object(&mut self) -> Result<(), Self::Error> {
        let frame = self.compounds.pop().ok_or(ValueVisitorError)?;
        debug_assert!(matches!(frame.kind, CompoundKind::Object));
        let slot_idx = frame.slot_idx as usize;
        let child_start = slot_idx + 1;
        let subtree_len = self.doc.nodes.len() - child_start;
        let value = proto::Value::Object(proto::NodeSpan {
            start: child_start as u32,
            subtree_len: subtree_len as u32,
            entries: 0,
        });
        self.doc.nodes[slot_idx] = value;
        if self.compounds.is_empty() {
            self.doc.root = Some(value);
        }
        Ok(())
    }
}

impl ArrayVisitor for ValueVisitor {
    #[inline(always)]
    fn begin_array(&mut self) -> Result<(), Self::Error> {
        let slot_idx = self.doc.nodes.len() as u32;
        self.doc.nodes.push(proto::Value::Null);
        self.compounds.push(CompoundMark {
            kind: CompoundKind::Array,
            slot_idx,
        });
        Ok(())
    }

    #[inline(always)]
    fn end_array(&mut self) -> Result<(), Self::Error> {
        let frame = self.compounds.pop().ok_or(ValueVisitorError)?;
        debug_assert!(matches!(frame.kind, CompoundKind::Array));
        let slot_idx = frame.slot_idx as usize;
        let child_start = slot_idx + 1;
        let subtree_len = self.doc.nodes.len() - child_start;
        let value = proto::Value::Array(proto::NodeSpan {
            start: child_start as u32,
            subtree_len: subtree_len as u32,
            entries: 0,
        });
        self.doc.nodes[slot_idx] = value;
        if self.compounds.is_empty() {
            self.doc.root = Some(value);
        }
        Ok(())
    }
}

impl StringVisitor for ValueVisitor {
    #[inline(always)]
    fn string(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let span = self.intern_str(bytes);
        self.emit(proto::Value::String(span));
        Ok(())
    }
}

impl NumberVisitor for ValueVisitor {
    #[inline(always)]
    fn number_f64(&mut self, value: f64) -> Result<(), Self::Error> {
        self.emit(proto::Value::Number(proto::Number(value)));
        Ok(())
    }
}

impl KeywordVisitor for ValueVisitor {
    #[inline(always)]
    fn bool(&mut self, value: bool) -> Result<(), Self::Error> {
        self.emit(proto::Value::Bool(value));
        Ok(())
    }

    #[inline(always)]
    fn null(&mut self) -> Result<(), Self::Error> {
        self.emit(proto::Value::Null);
        Ok(())
    }
}

macro_rules! bench_bbnf_visitor {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            {
                let mut visitor = ValueVisitor::with_input(input.as_bytes());
                JsonParser::parse_with_visitor(&input, &mut visitor).unwrap_or_else(|e| {
                    panic!(concat!($file, ": parse_with_visitor failed: {:?}"), e)
                });
                black_box(visitor.finish());
            }
            bench_with_timeout(
                b,
                limits::JSON_PARSE,
                |input: String| {
                    let mut visitor = ValueVisitor::with_input(black_box(input.as_bytes()));
                    JsonParser::parse_with_visitor(black_box(&input), &mut visitor).unwrap();
                    let doc: proto::Document = visitor.finish();
                    black_box(doc);
                },
                &input,
            );
        }
    };
}

bench_bbnf_visitor!(bbnf_visitor_data_s, "data.json");
bench_bbnf_visitor!(bbnf_visitor_twitter, "twitter.json");
bench_bbnf_visitor!(bbnf_visitor_citm, "citm_catalog.json");
bench_bbnf_visitor!(bbnf_visitor_canada, "canada.json");
bench_bbnf_visitor!(bbnf_visitor_data_xl, "data_xl.json");

// ── Prototype lane (reference) ──────────────────────────────────────────────
//
// Runs the prototype's `parse_json::<ValueVisitor>` in the same
// binary so the orchestrator can compute per-entry ratios vs
// `bbnf_visitor_*` from one `cargo bench` invocation.

macro_rules! bench_proto_value {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            {
                let mut visitor = proto::ValueVisitor::with_input(input.as_bytes());
                parse_json(input.as_bytes(), &mut visitor)
                    .unwrap_or_else(|e| panic!(concat!($file, ": proto parse failed: {:?}"), e));
                black_box(visitor.finish());
            }
            b.with_inputs(|| input.clone()).bench_values(|input| {
                let mut visitor = proto::ValueVisitor::with_input(black_box(input.as_bytes()));
                parse_json(black_box(input.as_bytes()), &mut visitor).unwrap();
                let doc: proto::Document = visitor.finish();
                black_box(doc);
            });
        }
    };
}

bench_proto_value!(proto_value_data_s, "data.json");
bench_proto_value!(proto_value_twitter, "twitter.json");
bench_proto_value!(proto_value_citm, "citm_catalog.json");
bench_proto_value!(proto_value_canada, "canada.json");
bench_proto_value!(proto_value_data_xl, "data_xl.json");

// ── AY.W3c.1 Lazy lane (`Document::get::<&str>` vs `sonic_rs::get`) ─────────
//
// Twitter-only: path `["statuses", 0, "text"]` extracts a single `&str`
// leaf. Both parties do the minimum work: parse enough of the input to
// reach the leaf, then return it. sonic's `get` is its lazy-query API
// (pointer-walk without materialising downstream nodes); bbnf's
// `Document::get::<T>(path)` is the grammar-emitted path-query surface.
// The bench pairs the two so the per-iter wall time is directly comparable.
//
// Note: bbnf's `parse()` currently builds the full tape — the "lazy"
// aspect is in the post-parse scalar extraction; W5+ may introduce a
// true bail-out lazy parse.

use bbnf::path;

#[divan::bench]
fn bbnf_get_twitter(b: divan::Bencher) {
    let input = load("twitter.json");
    {
        let parsed = JsonParser::parse(&input)
            .unwrap_or_else(|e| panic!("twitter.json: parse failed: {:?}", e));
        let segs = path!["statuses", 0_usize, "text"];
        let p = bbnf::runtime::Path::new(segs);
        black_box(parsed.get::<&str>(p));
    }
    bench_with_timeout(
        b,
        limits::JSON_PARSE,
        |input: String| {
            let parsed = JsonParser::parse(black_box(&input)).unwrap();
            let segs = path!["statuses", 0_usize, "text"];
            let p = bbnf::runtime::Path::new(segs);
            let got: Option<&str> = parsed.get(p);
            black_box(got);
            black_box(parsed);
        },
        &input,
    );
}

#[divan::bench]
fn sonic_get_twitter(b: divan::Bencher) {
    let input = load("twitter.json");
    // Warm-up: sonic_rs::get accepts the path as an `IntoIterator` of
    // `Index` values. Mixed string/integer keys round-trip via the
    // `JsonPointer!` macro or a heterogeneous vec of `PointerNode`
    // values — bbnf_get_twitter's path is `statuses.0.text`, a field
    // / index / field mix.
    {
        let node_path: sonic_rs::PointerTree = {
            let mut t = sonic_rs::PointerTree::new();
            t.add_path(sonic_rs::pointer!["statuses", 0usize, "text"]);
            t
        };
        let nodes = sonic_rs::get_many(&input, &node_path)
            .unwrap_or_else(|e| panic!("twitter.json: sonic_rs::get_many warm-up failed: {e}"));
        black_box(nodes);
    }
    b.with_inputs(|| input.clone()).bench_values(|input| {
        // Use `sonic_rs::get` with a path iterator — the idiomatic
        // lazy-extract entry. `JsonInput::from_subset` slices `&input`
        // so the returned `LazyValue<'_>` borrows from it.
        let got = sonic_rs::get(
            black_box(&input),
            sonic_rs::pointer!["statuses", 0usize, "text"],
        );
        let _ = black_box(got);
    });
}

// ── AY.W3c.1 Eager lane (`Document::to_value` vs `sonic_rs::from_str`) ──────
//
// Materialises the full typed value tree on both sides. bbnf uses
// `Document::to_value()`; sonic uses `from_str::<sonic_rs::Value>`.
// The ratio `bbnf_value_twitter / sonic_value_twitter` is AY's headline
// BEAT-sonic metric. Cold per-parse discipline (no warm benches per
// `feedback_no_warm_benches`): every iter reparses + rematerialises.

macro_rules! bench_bbnf_value {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            {
                let parsed = JsonParser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                let v = parsed.to_value();
                black_box(v);
            }
            bench_with_timeout(
                b,
                limits::JSON_PARSE,
                |input: String| {
                    let parsed = JsonParser::parse(black_box(&input)).unwrap();
                    let v = parsed.to_value();
                    black_box(v);
                    black_box(parsed);
                },
                &input,
            );
        }
    };
}

bench_bbnf_value!(bbnf_value_data_s, "data.json");
bench_bbnf_value!(bbnf_value_twitter, "twitter.json");
bench_bbnf_value!(bbnf_value_citm, "citm_catalog.json");
bench_bbnf_value!(bbnf_value_canada, "canada.json");
bench_bbnf_value!(bbnf_value_data_xl, "data_xl.json");

macro_rules! bench_sonic_value {
    ($name:ident, $file:expr) => {
        #[divan::bench]
        fn $name(b: divan::Bencher) {
            let input = load($file);
            sonic_rs::from_str::<sonic_rs::Value>(&input)
                .expect(concat!($file, ": sonic-rs parse failed"));
            b.with_inputs(|| input.clone()).bench_values(|input| {
                sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap()
            });
        }
    };
}

bench_sonic_value!(sonic_value_data_s, "data.json");
bench_sonic_value!(sonic_value_twitter, "twitter.json");
bench_sonic_value!(sonic_value_citm, "citm_catalog.json");
bench_sonic_value!(sonic_value_canada, "canada.json");
bench_sonic_value!(sonic_value_data_xl, "data_xl.json");

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
