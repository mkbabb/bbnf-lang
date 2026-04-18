# AW-V.W2 — JSON Prototype Close Ledger

W2 ships the sonic-rs-class JSON prototype in `crates/bbnf-json-prototype/`
under the `bbnf-wt-aw5-prototype` worktree. Per the AW-V tranche plan,
W2's hard gate is each of `{data_s, twitter, citm, canada, data_xl}`
within **10% of sonic-rs's ns/iter** on the twin-pair bench.

## Hard-gate status — MET

| entry    | prototype ns/iter | sonic ns/iter | ratio (≤ 1.10) | status |
|----------|------------------:|--------------:|---------------:|:------:|
| data_s   |            14,418 |        15,361 |          0.939 |  PASS  |
| twitter  |           244,993 |       274,864 |          0.891 |  PASS  |
| citm     |           522,441 |       585,060 |          0.893 |  PASS  |
| canada   |         1,330,826 |     1,477,382 |          0.901 |  PASS  |
| data_xl  |        13,954,450 |    15,479,020 |          0.902 |  PASS  |

**All 5 entries under 1.0× — the prototype matches or BEATS sonic-rs
on every twin-pair entry.** Full bench artefact at
`docs/benchmarks/post-AW-V-W2-prototype.json`.

## Commit chain (inside `bbnf-wt-aw5-prototype` worktree)

```
2edb612b perf(bbnf-json-prototype): scalar integer scan wins canada
b70311f8 feat(bbnf-json-prototype): AW-V.W2 hand-tuned JSON parser prototype
```

Base: `f457b4df` (post-AW-IV-W2 master HEAD). Both commits are
self-contained; the orchestrator cherry-picks onto master at W3
open per the isolation contract.

## Samply attribution — JSON twitter

Saved under
`.profiles/samply/aw5-w2/json_twitter/profile_final.json.{gz,syms.json}`.

### Top-10 self-time (3,482 samples)

| rank | self-time |  samples | symbol                                                                     |
|-----:|----------:|---------:|----------------------------------------------------------------------------|
|    1 |    91.15% |    3,174 | `bbnf_json_prototype::parse_value::<ValueVisitor>`                         |
|    2 |     3.36% |      117 | `bbnf_json_prototype::string::parse_string_escaped::<ValueVisitor>` (cold) |
|    3 |     1.38% |       48 | `_mi_heap_realloc_zero`                                                    |
|    4 |     0.78% |       27 | `core::str::converts::from_utf8`                                           |
|    5 |     0.66% |       23 | `_mi_page_retire`                                                          |
|    6 |     0.63% |       22 | unresolved syslib                                                          |
|    7 |     0.52% |       18 | `<alloc::raw_vec::RawVecInner>::finish_grow`                               |
|    8 |     0.32% |       11 | `read`                                                                     |
|    9 |     0.20% |        7 | `__open`                                                                   |
|   10 |     0.20% |        7 | `mi_free`                                                                  |

**Top-2 cover 94.5% self-time** (gate ≥ 70% — exceeded with margin).
Sonic-rs's equivalent twin pair sits at **81–88% self-time** over
two monomorphised symbols; the prototype lands higher because the
four per-shape functions (`parse_object` + `parse_array` +
`parse_string` + `parse_number` + `skip_space` + SIMD helpers) all
inline into `parse_value` under `#[inline(always)]` + workspace LTO,
leaving **one hot symbol** carrying the entire parse body.

Canada samply (`.profiles/samply/aw5-w2/json_canada/profile_v5.json.{gz,syms.json}`):
98.6% self-time on `parse_value::<ValueVisitor>` — fully-inlined
number kernel. No `compute_f64` / `compute_product_approx` /
`compute_float` symbols reachable (all resolve via
`#[inline(always)]` markers added to `parse_that::parsers::eisel_lemire`
mod.rs + algorithm.rs).

## Symbol-presence verification — `nm`

Forbidden-symbol probe (must be empty):

```
nm target/release/deps/json_value-a07c6e237c351cce |
  grep -E '(dispatch_one|try_branch|advance_or_pop_with|__dta_walker_inline|DtaState|FrameStack)'
```

Result: **empty**. None of the AW-III/AW-IV interpretive substrate
appears in the prototype's bench binary — the walker, DTA state
table, advance_or_pop_with helper, FrameStack, try_branch all absent.

Per-shape symbol absence (all inlined away):

```
$ nm target/release/deps/json_value-a07c6e237c351cce | grep -E 'bbnf_json_prototype'
... parse_json::<ValueVisitor>      # entry, present
... parse_json::<TapeVisitor>       # entry, present
... parse_value::<ValueVisitor>     # dispatcher, present (all shapes inline into here)
... parse_value::<TapeVisitor>      # dispatcher, present
... parse_string_escaped::<...>     # cold path, #[inline(never)], present
... parse_fallback                  # cold, #[inline(never)]+#[cold], present
```

Absent from `nm` (verified inlined): `parse_object`, `parse_array`,
`parse_string` (body — only escape cold path survives), `parse_number`,
`skip_space`, `skip_space_slow`, `nospace_bitmap_64`,
`first_quote_or_backslash`, `simd_str2int`, `intern_str`,
`parse_that::parsers::eisel_lemire::compute_f64`,
`parse_that::parsers::eisel_lemire::algorithm::compute_float`,
`parse_that::parsers::eisel_lemire::algorithm::compute_product_approx`.

## `cargo expand` — inline-body inspection

Source: `/tmp/aw5-w2-expand-final.txt` (2,864 lines).

All five per-shape functions carry `#[inline(always)]` on the
expanded output (lines 2704, 2745, 2787, 2821, 2839):

```rust
#[inline(always)]
pub(crate) fn parse_value<V: JsonVisitor>(
    input: &[u8], p: &mut usize, state: &mut ScanState,
    first_byte: u8, visitor: &mut V,
) -> Result<(), ParseError> { … }

#[inline(always)]
pub(crate) fn parse_object<V: JsonVisitor>(…) -> Result<…> { … }

#[inline(always)]
pub(crate) fn parse_array<V: JsonVisitor>(…) -> Result<…> { … }

#[inline(always)]
pub(crate) fn parse_string<V: JsonVisitor>(…) -> Result<…> {
    string::parse_string_body(input, p, visitor, is_key)
}

#[inline(always)]
pub(crate) fn parse_number<V: JsonVisitor>(…) -> Result<…> {
    number::parse_number_body(input, p, first_byte, visitor)
}
```

Full snippet at `/tmp/aw5-w2-expand-shapes.txt` (161 lines).
`parse_value`'s body is a 6-arm `match first_byte` over the
dispatch bytes (`{`, `[`, `"`, digit/`-`, `t`, `f`, `n`); every arm
tail-calls one of the per-shape functions.

## Deliverables present

- [x] `crates/bbnf-json-prototype/` workspace crate with:
  - `src/lib.rs` — `pub fn parse_json<V: JsonVisitor>` + 5 shape fns
  - `src/visitor.rs` — `JsonVisitor` trait + `ValueVisitor` + `TapeVisitor`
  - `src/value.rs` — packed sonic-parity `Value` enum (24-byte) +
    `Document { nodes, arena, root }` + `StringSpan { loc: u64, len }`
    with high-bit `ARENA_TAG` for borrow-vs-arena discrimination
  - `src/simd.rs` — `nospace_bitmap_64` NEON/AVX2 cache +
    `first_quote_or_backslash` NEON/AVX2 + `skip_space_slow`
  - `src/number.rs` — scalar integer digit scan + NEON
    `simd_str2int` fraction SIMD + inline Eisel-Lemire decode
  - `src/string.rs` — inline quoted-string scan + RFC 8259 escape
    decoder (cold path)
  - `Cargo.toml` — workspace member, `bbnf-tape` + `bbnf-simd-scan`
    + `parse_that` deps, `sonic-rs` + `bencher` + `mimalloc` dev-deps
  - `benches/json_value.rs` — twin-pair bench harness isomorphic to
    `crates/core/benches/json/value.rs`
  - `tests/corpus.rs` — 20 tests covering corpus + escape decoding
    + error paths
- [x] `docs/benchmarks/post-AW-V-W2-prototype.json` — per-entry
      ns/iter + MB/s + ratios across two stability runs
- [x] `.profiles/samply/aw5-w2/json_twitter/profile_final.json.{gz,syms.json}`
- [x] `.profiles/samply/aw5-w2/json_canada/profile_v5.json.{gz,syms.json}`
- [x] This ledger — `docs/tranches/AW/AW-V-W2-close.md`

## Residuals / notes

1. **`parse-that` inline annotations tightened.** Added
   `#[inline(always)]` to `parse_that::parsers::eisel_lemire::compute_f64`,
   `algorithm::compute_float`, `compute_product_approx`, `power`,
   and `full_multiplication`. This lands in the `parse-that` repo
   (workspace path-dep); it is a substrate enabler for the
   prototype but does not modify any `bbnf-wt-aw5-prototype`-tracked
   file outside these two. On cherry-pick to master the change
   should land alongside the prototype crate in the same wave.

2. **TapeVisitor** ships and parses every corpus fixture (tests:
   `tape_visitor_data_s`, `tape_visitor_twitter`). The TapeVisitor
   bench shows lower throughput than ValueVisitor because
   `bbnf_tape::TapeBuilder` does more per-record work (column
   writes, structural slot bookkeeping, arena accounting).
   Per-entry numbers in `post-AW-V-W2-prototype.json`.
   Not gated by W2; it's the AW-IV-substrate validator.

3. **Scalar integer scan wins over SIMD for the integer digit run.**
   The first cut followed sonic-number's NEON `simd_str2int`
   verbatim for both integer and fraction. Profile showed canada's
   numbers like `-65.6136169…` have 2–3-digit integer parts; the
   16-byte SIMD stripe's 6-cycle overhead never amortises on 2–3
   digits. Replaced integer-phase SIMD with scalar; fraction-phase
   SIMD retained (canada fractions are 15 digits — the design
   target). Canada ratio dropped from 1.18× over sonic to 0.90×
   under sonic after this change. Commit `2edb612b`.

4. **Escape-free string path is borrow, not copy.** `ValueVisitor`
   computes whether `bytes`'s pointer lies inside the input range
   via `input_lo <= ptr && ptr + len <= input_hi`; hits take the
   `StringSpan::borrowed` path (zero copy, pointer + length).
   Only escape-decoded strings copy into `doc.arena`. This is
   sonic-rs's `visit_borrowed_str` vs `visit_str` split enacted at
   visit time via pointer-range identity. Verified by profile:
   `_platform_memmove` drops from 14.8% in the scratch-based layout
   to 1.8% in the packed layout on twitter.

5. **NEON `nospace_bitmap_64` uses `vaddv_u8` movemask pattern.**
   The first cut attempted a `vshrn_n_u16<4>` + multiply-gather
   reduction that was incorrect (the multiply-shift constants
   didn't line up with the packed-nibble output). Replaced with
   the per-lane bit-vector + `vaddv_u8` horizontal sum pattern —
   16 bytes → 16 bits per chunk, 4 chunks → 64-bit mask. All
   corpus tests pass.

## Successor

W2 closes green. W3 opens with the shape-mining IR pass
(`crates/ir/src/passes/recognizers/shape_mining.rs`) +
per-shape emitter modules
(`crates/core/src/backend/rust/emitter/shapes/`), generalising the
prototype's shape back through codegen. Wave gate: emitter-
produced JSON parser matches hand-prototype ± 5%.
