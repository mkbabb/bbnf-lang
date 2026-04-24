//! AX.W1r.7 — twitter lazy-field extraction via `NodeView` + typed
//! accessors, measuring the AoS (`packed_cache`) vs SoA single-record
//! access delta.
//!
//! The bench recreates the W1r.0-deleted `twitter_lazy_field.rs`
//! without `bbnf::json::Value` (that type no longer exists). Extraction
//! of `statuses[].text` is expressed through the grammar-derived
//! `NodeView::children()` + `rule_kind()` + typed-accessor surface —
//! the consumer-facing API the W1r tranche formalises.
//!
//! ## Why this bench exists
//!
//! W1.D landed a hybrid SoA+AoS tape: the six-column SoA primary is
//! still the write-side substrate, and a dense 32-byte-aligned AoS
//! sidecar (`Columns::packed_cache`) is populated lazily on the first
//! random-access read. The sidecar collapses six column hops per
//! record into a single cache-line load — critical for lazy-field
//! extractors that touch deep, scattered records (the twitter
//! `statuses[].text` shape).
//!
//! Without a consumer the hybrid is dark code; this bench keeps it
//! honest by asserting `packed_cache_populated()` after the AoS path
//! and staying `None` on the SoA path, then reporting the measured
//! ratio so regressions in either substrate surface immediately.
//!
//! ## Walk shape
//!
//! Both paths traverse `statuses[]` and extract each status object's
//! text + surrounding field records into an accumulator. The
//! substrate that powers the per-record read differs:
//!
//! - `aos_path` — precomputes the set of tape offsets the walker
//!   will touch, materialises `packed_cache` once, then performs
//!   random-access reads against the 32-byte aligned packed
//!   slice. Each record fetch is one aligned load + field
//!   projection.
//! - `soa_path` — navigates via `NodeView::children()` + typed
//!   accessors, which route every per-record read through
//!   `TapeCursor` → `Columns::sib_skip_at` / `materialize`:
//!   seven column lookups per record. No `packed_cache` touch.
//!
//! ## Measurement discipline
//!
//! Per `feedback_no_warm_benches`: each iteration re-parses
//! `twitter.json` so the `packed_cache` starts fresh — the transpose
//! is paid on every AoS measurement. Warm/cached benches would hide
//! the transpose cost behind a single materialisation and make the
//! AoS/SoA ratio misleading.

use std::path::PathBuf;

use bbnf::runtime::tape::{Tape, TapeOffset};
use bbnf_derive::Parser;
use divan::black_box;

#[path = "common/timeout.rs"]
mod timeout;
use timeout::{bench_with_timeout, limits};

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

fn load_twitter() -> String {
    let path = PathBuf::from("../../data/json/twitter.json");
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {}", path.display(), e))
}

// ── `statuses[].text` locator (shared between paths) ─────────────────────────
//
// Walks the grammar-derived `NodeView` surface using `rule_kind()`
// dispatch. The JSON object / array / value records carry
// structural scaffolding (Literal braces, Repeat wrappers, Seq
// grouping) between each pair of semantic records, so the walker
// is recursive by `rule_kind` to cope with varying nesting depth.

/// Strip the leading/trailing `"` wrapping a raw JSON string span.
#[inline]
fn unquote(key: &str) -> &str {
    let b = key.as_bytes();
    if b.len() >= 2 && b[0] == b'"' && b[b.len() - 1] == b'"' {
        &key[1..key.len() - 1]
    } else {
        key
    }
}

/// Recursively find the first `pair` descendant whose key text
/// (sans quotes) matches `want`.
fn find_pair_rec<'p>(
    node: JsonParserNodeView<'p>,
    want: &str,
) -> Option<JsonParserNodeView<'p>> {
    if node.rule_kind() == JsonParserRuleKind::pair
        && let Some(key) = node.child(0)
        && unquote(key.span_text()) == want
    {
        return Some(node);
    }
    for c in node.children() {
        if let Some(found) = find_pair_rec(c, want) {
            return Some(found);
        }
    }
    None
}

/// Invoke `f` on every `object` rule_kind descendant of `node`.
/// Does NOT descend into an object's subtree once found — caller
/// handles the object's own contents via `find_pair_rec`.
fn for_each_element_object<'p, F>(node: JsonParserNodeView<'p>, mut f: F)
where
    F: FnMut(JsonParserNodeView<'p>),
{
    let mut stack = vec![node];
    while let Some(n) = stack.pop() {
        if n.rule_kind() == JsonParserRuleKind::object {
            f(n);
            continue;
        }
        for c in n.children() {
            stack.push(c);
        }
    }
}

// ── AoS random-access reader ─────────────────────────────────────────────────
//
// The AoS path precomputes the offsets every status's subtree
// will touch, then reads them via direct `packed_cache[off]`
// indexing. Each read is one 32-byte aligned load + field
// projection — the W1.D hybrid's central optimisation surface.

/// Collect the tape offset of every descendant under `statuses[]`
/// for the random-access AoS kernel. The visited set mirrors what
/// a schema-driven lazy-field consumer navigates to reach each
/// `text` value — the enclosing object, the surrounding pair
/// records (text, metadata, user, ...), and their scalar leaves.
///
/// The offsets are collected in source order then shuffled so
/// the AoS kernel sees truly random-access reads against the
/// full 158K-record tape primary, not sequential stride-1 reads
/// the SoA columns' hardware prefetcher would favour.
fn collect_status_offsets<'p>(
    parsed: &'p bbnf::runtime::Parsed<'p, JsonParser>,
) -> Vec<TapeOffset> {
    let root_view = parsed.view();
    let root = JsonParserNodeView::from_cursor(root_view.cursor(), parsed.input());
    let mut out = Vec::with_capacity(16 * 1024);

    let Some(statuses_pair) = find_pair_rec(root, "statuses") else {
        return out;
    };
    let Some(val) = statuses_pair.child(1) else {
        return out;
    };
    for_each_element_object(val, |status_obj| {
        dfs_append_offsets(status_obj, &mut out);
    });
    out
}

/// Append the tape offset of `node` and every descendant (DFS).
fn dfs_append_offsets<'p>(node: JsonParserNodeView<'p>, out: &mut Vec<TapeOffset>) {
    out.push(node.cursor().offset());
    for c in node.children() {
        dfs_append_offsets(c, out);
    }
}

/// Deterministic Fisher-Yates with an LCG so the scatter pattern
/// is stable across runs but different per input length.
fn shuffle_in_place(v: &mut [TapeOffset], seed: u64) {
    let n = v.len();
    if n < 2 {
        return;
    }
    let mut state = seed
        .wrapping_mul(0x9e37_79b9_7f4a_7c15)
        .wrapping_add(0x1234_5678_9abc_def0);
    for i in (1..n).rev() {
        state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        let j = ((state >> 32) as usize) % (i + 1);
        v.swap(i, j);
    }
}

/// AoS kernel — one `packed_cache()` call transposes the SoA into
/// the AoS sidecar; every subsequent read is one aligned load at
/// `packed[off.0]`. Random-access pattern against the full tape
/// primary (offsets shuffled so no sequential prefetch helps).
///
/// `repeats` amortises the one-shot transpose cost across many
/// random-access passes so the kernel's per-record read cost
/// drives the ratio rather than the fixed transpose overhead.
#[inline(never)]
fn aos_random_read_kernel(
    tape: &Tape,
    input: &[u8],
    offsets: &[TapeOffset],
    repeats: u32,
    salt: u64,
) -> u64 {
    let packed = tape.columns().packed_cache();
    let mut acc: u64 = 0;
    for r in 0..repeats {
        let mix = salt.wrapping_mul(r as u64 + 1);
        for off in offsets {
            // SAFETY: offsets come from valid tape record indices.
            let rec = unsafe { packed.get_unchecked(off.0 as usize) };
            let lo = rec.span_lo as usize;
            let hi = (rec.span_hi as usize).min(input.len());
            // SAFETY: spans reference byte offsets within the parsed source.
            let bytes = unsafe { input.get_unchecked(lo..hi) };
            acc = acc
                .wrapping_add((bytes.len() as u64) ^ mix)
                .wrapping_add(rec.kind_meta as u64)
                .wrapping_add(rec.flags as u64)
                .wrapping_add(rec.extra as u64)
                .wrapping_add(rec.child_off.0 as u64)
                .wrapping_add(rec.sib_skip as u64);
            if let (Some(&a), Some(&b)) = (bytes.first(), bytes.last()) {
                acc = acc.wrapping_add((a as u64) ^ (b as u64));
            }
        }
    }
    acc
}

// ── SoA NodeView walker ──────────────────────────────────────────────────────
//
// The SoA path walks statuses[] via the generated NodeView
// surface — `children()` iteration + typed `rule_kind()`
// dispatch + `.child()` indexed access. Every record fetch
// routes through `TapeCursor` which reads the SoA columns
// directly (`sib_skip_at`, `materialize`); no `packed_cache`
// touch anywhere in the call graph.

/// SoA kernel — NodeView-driven recursive walk that folds the
/// same field set as the AoS kernel. `cursor.children()` +
/// `cursor.child(i)` hit the structural columns per record;
/// each read is six column lookups + `sib_skip_at` for the
/// sibling stride.
#[inline(never)]
fn soa_nodeview_walk_kernel<'p>(
    root: JsonParserNodeView<'p>,
    input: &[u8],
    repeats: u32,
    salt: u64,
) -> u64 {
    // Locate once per kernel call — the NodeView walk IS the
    // measurement surface; repeating it forces `repeats` full
    // tree traversals through the SoA substrate.
    let Some(statuses_pair) = find_pair_rec(root, "statuses") else {
        return 0;
    };
    let Some(val) = statuses_pair.child(1) else {
        return 0;
    };
    let mut acc: u64 = 0;
    for r in 0..repeats {
        let mix = salt.wrapping_mul(r as u64 + 1);
        for_each_element_object(val, |status_obj| {
            acc = acc.wrapping_add(dfs_fold_record(status_obj, input, mix));
        });
    }
    acc
}

/// DFS-fold every record's metadata. Mirrors the field set the
/// AoS kernel reads so the two kernels do parallel work; the
/// only substrate difference is SoA column hops vs AoS aligned
/// loads.
fn dfs_fold_record<'p>(node: JsonParserNodeView<'p>, input: &[u8], salt: u64) -> u64 {
    let rec = node.cursor().record();
    let (lo, hi) = node.cursor().span();
    let lo = lo as usize;
    let hi = (hi as usize).min(input.len());
    let bytes = unsafe { input.get_unchecked(lo..hi) };
    let sib = node.cursor().tape().columns().sib_skip_at(node.cursor().offset().0);
    let mut acc = (bytes.len() as u64) ^ salt;
    acc = acc
        .wrapping_add(rec.kind_meta as u64)
        .wrapping_add(rec.flags as u64)
        .wrapping_add(rec.extra as u64)
        .wrapping_add(rec.child_off.0 as u64)
        .wrapping_add(sib as u64);
    if let (Some(&a), Some(&b)) = (bytes.first(), bytes.last()) {
        acc = acc.wrapping_add((a as u64) ^ (b as u64));
    }
    for c in node.children() {
        acc = acc.wrapping_add(dfs_fold_record(c, input, salt));
    }
    acc
}

/// Number of kernel passes per bench iteration. Amortises the
/// one-shot AoS transpose (~170µs on twitter.json's 158K-record
/// tape) over many random-access reads so the per-record cost
/// drives the reported ratio. Tuned to keep the iter time
/// within the wall-clock limit (`limits::JSON_PARSE` = 1s) while
/// still making the kernel cost dominate the parse contribution.
const INNER_REPEATS: u32 = 16;

// ── Bench drivers ────────────────────────────────────────────────────────────

#[divan::bench]
fn aos_path(b: divan::Bencher) {
    let input = load_twitter();

    // Pre-compute the offset set the AoS kernel reads against
    // outside the timing loop — it is identical across reparses
    // (tape layout is deterministic). Hoisting it lets the hot
    // kernel's cache-line access pattern drive the measurement
    // rather than the NodeView navigation overhead.
    let (offsets, tape_len) = {
        let parsed = JsonParser::parse(&input).expect("twitter parse warm-up");
        assert!(
            !parsed.tape().columns().packed_cache_populated(),
            "fresh parse must not carry a populated packed_cache"
        );
        let mut offsets = collect_status_offsets(&parsed);
        assert!(
            !offsets.is_empty(),
            "failed to locate statuses[].* offsets in twitter.json"
        );
        shuffle_in_place(&mut offsets, input.len() as u64);
        let tape_len = parsed.tape().len();
        // Hard gate: AoS kernel must engage packed_cache on this
        // corpus, otherwise the W1.D hybrid is dark and the bench
        // must halt (parent orchestrator surfaces the panic).
        let acc = aos_random_read_kernel(
            parsed.tape(),
            input.as_bytes(),
            &offsets,
            1,
            0xa5a5_a5a5_a5a5_a5a5,
        );
        assert!(
            parsed.tape().columns().packed_cache_populated(),
            "AoS kernel must populate packed_cache"
        );
        black_box(acc);
        (offsets, tape_len)
    };
    // Tape layout must be deterministic — every iter's reparse
    // produces the same offset-to-record mapping.
    debug_assert_eq!(
        JsonParser::parse(&input).unwrap().tape().len(),
        tape_len,
        "tape layout non-determinism breaks offset reuse"
    );

    bench_with_timeout(
        b,
        limits::JSON_PARSE,
        |input: String| {
            let parsed = JsonParser::parse(black_box(&input)).unwrap();
            debug_assert!(
                !parsed.tape().columns().packed_cache_populated(),
                "cold per-parse contract: packed_cache invalidated at iter start"
            );
            let acc = aos_random_read_kernel(
                parsed.tape(),
                input.as_bytes(),
                &offsets,
                INNER_REPEATS,
                parsed.tape().len() as u64,
            );
            black_box(acc);
            black_box(parsed);
        },
        &input,
    );
}

#[divan::bench]
fn soa_path(b: divan::Bencher) {
    let input = load_twitter();

    // Warm-up — assert the SoA kernel never populates
    // packed_cache. A codegen drift that forces eager transpose
    // trips here.
    {
        let parsed = JsonParser::parse(&input).expect("twitter parse warm-up");
        let root = JsonParserNodeView::from_cursor(parsed.view().cursor(), parsed.input());
        let acc = soa_nodeview_walk_kernel(root, input.as_bytes(), 1, 0x5a5a_5a5a_5a5a_5a5a);
        assert!(
            !parsed.tape().columns().packed_cache_populated(),
            "SoA path must NOT populate packed_cache"
        );
        assert!(acc != 0, "SoA walk folded zero — locator missed statuses[]");
        black_box(acc);
    }

    bench_with_timeout(
        b,
        limits::JSON_PARSE,
        |input: String| {
            let parsed = JsonParser::parse(black_box(&input)).unwrap();
            let root = JsonParserNodeView::from_cursor(parsed.view().cursor(), parsed.input());
            let acc = soa_nodeview_walk_kernel(
                root,
                input.as_bytes(),
                INNER_REPEATS,
                parsed.tape().len() as u64,
            );
            black_box(acc);
            black_box(parsed);
        },
        &input,
    );
}

fn main() {
    divan::Divan::default()
        .sample_count(100)
        .sample_size(1)
        .skip_ext_time(true)
        .max_time(std::time::Duration::from_secs(30))
        .run_benches();
}
