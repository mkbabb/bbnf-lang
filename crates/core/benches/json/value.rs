
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

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use bbnf::runtime::tape::{Tape, TapeCursor, TapeKind, TapeOffset};
use bbnf_derive::Parser;
use bencher::{Bencher, benchmark_group, benchmark_main, black_box};

#[path = "../common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

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
            // via `payload_string`. The accessor returns a `&str`
            // pointing into the arena without copying; we fold its
            // length into the accumulator so the compiler cannot
            // elide the arena read. AU.3.2: `payload_string` skips
            // UTF-8 validation on the hot path (decoder kernel
            // contract) so the accessor is a single bounds check +
            // slice + transmute.
            if let Some(s) = tape.payload_string(rec) {
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
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            {
                let parsed = JsonParser::parse(&input)
                    .unwrap_or_else(|e| panic!(concat!($file, ": parse failed: {:?}"), e));
                let view = parsed.view();
                let root_off = view.cursor().offset();
                let v = walk_tape(parsed.tape(), root_off, &input);
                black_box(v);
            }
            bench_with_timeout(b, limits::JSON_PARSE, || {
                let parsed = JsonParser::parse(black_box(&input)).unwrap();
                let view = parsed.view();
                let root_off = view.cursor().offset();
                let v = walk_tape(parsed.tape(), root_off, black_box(&input));
                black_box(v);
                black_box(parsed);
            });
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
        fn $name(b: &mut Bencher) {
            let input = load($file);
            b.bytes = input.len() as u64;
            sonic_rs::from_str::<sonic_rs::Value>(&input)
                .expect(concat!($file, ": sonic-rs parse failed"));
            b.iter(|| sonic_rs::from_str::<sonic_rs::Value>(black_box(&input)).unwrap());
        }
    };
}

bench_sonic!(sonic_data_s, "data.json");
bench_sonic!(sonic_twitter, "twitter.json");
bench_sonic!(sonic_citm, "citm_catalog.json");
bench_sonic!(sonic_canada, "canada.json");
bench_sonic!(sonic_data_xl, "data_xl.json");

// ── Groups ──────────────────────────────────────────────────────────────────

benchmark_group!(
    bench_bbnf,
    bbnf_data_s,
    bbnf_twitter,
    bbnf_citm,
    bbnf_canada,
    bbnf_data_xl,
);
benchmark_group!(
    bench_sonic,
    sonic_data_s,
    sonic_twitter,
    sonic_citm,
    sonic_canada,
    sonic_data_xl,
);

benchmark_main!(bench_bbnf, bench_sonic);
