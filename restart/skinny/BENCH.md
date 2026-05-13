# Skinny Spec — Bench and Parity Harness

This document is one of four quadrants of the skinny implementation spec for
`bbnf-lang`. The skinny exists to validate the V1 architectural premise — the
SOTA-viability claim — before tranches A-J commit. Sister quadrants:

- `restart/skinny/SUBSTRATE.md` — `Tape`, `ValueRef`, `DocumentView`, payload
  arena, `bbnf-simd` integration contract.
- `restart/skinny/COMPILER.md` — Grammar IR subset, BIR subset, HM-only type
  checker, `codegen::rust` path.
- `restart/skinny/WORKSPACE.md` — Cargo workspace, member list, per-crate LOC
  budgets for skinny, build/test commands.
- `restart/skinny/BENCH.md` — this document.

The skinny ships ONE grammar end-to-end: `json`. The bench harness here is the
arbiter that decides whether tranches A-J dispatch, dispatch with codegen
focus, or block on substrate redesign.

The full V1 spec lives at `restart/ARCHITECTURE.md` and `restart/MASTER-PLAN.md`.
SOTA anchors live at `restart/corpora/SOTA.md` and Lock 8
(`restart/locks/14-LOCKS.md:48`). This document binds the skinny bench harness
to those anchors without reproducing them.

---

## §1 The dual-track contract

The skinny harness measures TWO bbnf-side parser implementations against a
shared substrate, plus three external competitors. The two bbnf-side parsers
are the heart of the slice.

### 1.1 Track 1 — Generated parser (the skinny vertical)

Produced by:

```
grammars/json.bbnf
  -> bbnf-parse  (Grammar IR subset; see COMPILER.md §2)
  -> passes      (HM-only type pass; see COMPILER.md §4)
  -> bbnf-bir    (Backend IR subset; see COMPILER.md §3)
  -> codegen::rust  (Rust lowerer, skinny subset; see COMPILER.md §5)
  -> emits crates/runtime/src/grammars/json/generated.rs (committed source
     artefact; Lock 6: xtask emits committed source artefacts)
```

Track 1 is the *generated* JSON parser: the answer to "did our entire
compiler-plus-codegen pipeline produce a SOTA-class parser from a BBNF grammar?"

Public surface:

```rust
// crates/runtime/src/grammars/json/generated.rs (emitted)
pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError<'i>>;
```

`JsonRoot<'i>` owns a sealed `JsonDocument<'i>` / `Tape<'i>` snapshot and
typed `ValueRef` projections borrow that tape per `restart/ARCHITECTURE.md:1597`.
The generated parser consumes `runtime::tape`
and `bbnf-simd` exactly as the hand-coded substrate parser does. No bypass,
no shortcuts, no per-grammar Rust outside `crates/runtime/src/grammars/json/`.

### 1.2 Track 2 — Hand-coded substrate parser (the substrate ceiling probe)

Produced by:

```
crates/bbnf-bench/src/track2/json.rs (handwritten Rust; the author writes
  calls into runtime::tape and `bbnf-simd` directly, expressing the JSON
  grammar as direct Rust against the same APIs codegen will emit)
```

LOC is **measurement-driven, not constraint-driven**. A reference-class
hand-coded JSON parser using the substrate APIs is expected to land at
800-1,500 LOC; the prior 500 LOC ceiling was a constraint that risked
either substrate-API over-use (substrate becomes covert hand-coded parser)
or arbitrary budget-overrun resolution. Replaced by §10.6 substrate-API
correspondence checklist that gates on what Track 2 *calls*, not how
short it is.

Track 2 is the *hand-coded* JSON parser using the SAME `runtime::tape` runtime
and the SAME `bbnf-simd` structural-scan dispatcher that Track 1's generator
emits against. The author is allowed to use every substrate API the codegen
emits, but no codegen runs. Substrate access is via `TapeBuilder<'a>` per
`SUBSTRATE.md` §8 — the named-inversion API contract that V1 graduation
closes (per `INDEX.md` deviation ledger row 6); the skinny consumes the
inverted contract directly and the graduation is mechanical.

Public surface — identical shape to Track 1:

```rust
// crates/bbnf-bench/src/track2/json.rs (handwritten)
pub fn parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError<'i>>;
```

`JsonRoot<'i>` is the same type — exported from
`runtime::grammars::json` — so both tracks return interchangeable values.
The harness asserts token-stream parity and byte-equal materialised output
across tracks for every fixture (see §3.4 parity oracle).

### 1.3 The contract — what changes, what does not

| Invariant | Track 1 (generated) | Track 2 (hand-coded) |
|---|---|---|
| `runtime::tape` substrate | identical | identical |
| `bbnf-simd` structural scan | identical | identical |
| payload arena layout | identical | identical |
| `ValueRef` / `DocumentView` shape | identical | identical |
| `parse(&str) -> Result<JsonRoot<'i>, ParseError>` shape | identical | identical |
| Source of the parse-driver code | emitted by `codegen::rust` | written by hand |
| Code-shape gate | `xtask lint-loc` against the per-grammar generated budget | substrate-API correspondence checklist (§10.6) — Track 2 calls only what codegen emits |

### 1.4 What the deltas measure

Two deltas; each tells the user a different lever to pull.

- **Delta A — codegen overhead:** `Track1_time / Track2_time`. If A is large
  (> 1.20×), the codegen path is leaving performance on the floor. Lever:
  tranche F (Rust lowerer) or tranche H (cost model + recogniser tuning).
- **Delta B — substrate ceiling:** `Track2_time / S`, where `S` is the
  fastest in-run sonic-rs / simd-json anchor for that corpus. If B is
  large (> 1.10×), the substrate itself is the floor. Lever: tranche B
  (runtime substrate) and tranche H.W1 (SIMD scan). This is the failure
  mode that NO-GOs the entire skinny.

Measuring A and B separately is the whole point of the skinny. A single
end-to-end number does not answer "did codegen waste it" vs "did the substrate
fail." Two numbers do.

### 1.5 Track 2 is not a strawman

Track 2 must use every substrate API the generator emits. If the generator
emits a hand-rolled string interner and Track 2 uses `String::new()`, the
comparison is meaningless. The author of Track 2 is the same author (or
review-coupled to) the author of the substrate, so the substrate APIs Track 2
calls are the substrate APIs codegen will emit. The hand-coded parser is
structured to mirror what the codegen template would emit for JSON's grammar:
recursive-descent over object/array/string/number/literal recognisers, each
consuming `&Tape<'i>` cursor advances and recording raw spans into the tape.
The JSON hot path must leave the payload arena empty; decoded scalar values
are lazy accessor results, not parse-time arena writes.

A code review checklist sits in §10.6 to guard against Track 2 cheating.

---

## §2 Comparator baselines and workload planes

The competitor set is fixed by Lock 8 (`restart/locks/14-LOCKS.md:48`) and
extended by the 2026-05-12 SOTA-BEAT research packet. Skinny pins the JSON-line
subset only, but reports two planes:

- **Rust in-process plane**: sonic-rs, simd-json, serde_json, Track 1, Track 2.
  These rows classify the skinny gate because they share one Rust harness.
- **Native SOTA reference plane**: yyjson, simdjson C++, and asmjson. These rows
  are not mixed into the Rust harness timing, but they bind the SOTA-BEAT target
  and profile shape: cycles/byte, hot-leaf count, materialization mode, strict
  versus permissive validation, and SIMD/ASM primitive class.

### 2.1 sonic-rs (lazy materialisation)

Repository: `cloudwego/sonic-rs`.

Cargo entry (in `crates/bbnf-bench/Cargo.toml`):

```toml
[dev-dependencies]
sonic-rs = { version = "=0.5", default-features = false, features = ["sort_keys"] }
```

Pin: `=0.5` exact version; default-features off to avoid the optional
`unstable` feature drift. Re-pin on each baseline refresh; record the version
in the per-row metadata block (§5).

APIs used:

- `sonic_rs_anchor`: the fastest unchecked/anchor-mode parse API exposed by
  the pinned crate version, recorded with its exact symbol name in metadata.
- `sonic_rs_checked`: `sonic_rs::from_slice::<sonic_rs::Value>(bytes)` or the
  strict checked equivalent exposed by the pinned crate version.

The threshold matrix uses `sonic_rs_anchor`; the checked row is a parity and
debugging row. We deliberately do NOT use `sonic_rs::LazyValue` as the
threshold anchor; the bbnf generated parser builds a typed root with typed
projections borrowing the sealed tape, so a lazy-value-only comparison would not
bind the same materialisation contract.

Why sonic-rs is the primary competitor on twitter / citm / canada: SOTA.md
shows sonic-rs is the JSON Rust line leader on M1 Pro with 436 µs / 854 µs /
3.144 ms (`restart/corpora/SOTA.md:50-56`). Lock 8 names it.

### 2.2 simd-json (Rust port of simdjson)

Repository: `simd-lite/simd-json`.

Cargo entry:

```toml
[dev-dependencies]
simd-json = { version = "=0.13", default-features = false, features = ["serde_impl"] }
```

APIs used:

- `simd_json_borrowed`: `simd_json::to_borrowed_value(&mut bytes)`.
- `simd_json_owned`: `simd_json::to_owned_value(&mut bytes)`.

Rationale: SOTA.md does not identify which simd-json API produced the anchor.
The skinny records both and uses the faster in-run simd-json row for `S`;
the result table keeps the borrowed/owned distinction explicit.

Note: `to_borrowed_value` mutates the input buffer in place; the harness owns
a per-iteration `Vec<u8>` clone of the corpus bytes to avoid measuring
preceding-iteration mutations on subsequent iterations. The clone happens
inside the criterion `iter_batched` setup phase and is NOT charged to the
parse time. See §7.2.

### 2.3 serde_json (control: typed, no SIMD)

Repository: `serde-rs/json`.

Cargo entry:

```toml
[dev-dependencies]
serde_json = { version = "=1.0.117", default-features = false, features = ["std"] }
```

API used: `serde_json::from_slice::<serde_json::Value>(bytes)` — fully
materialised, no SIMD. Purpose: anchors the "no SIMD" baseline so the harness
can attribute substrate wins to SIMD specifically rather than to algorithmic
choice. SOTA.md records serde_json M1 Pro as 831 µs twitter / 1.376 ms citm /
4.988 ms canada (`restart/corpora/SOTA.md:54-56`).

The serde_json gap to sonic-rs gives the user a known SIMD-dividend reference
point. If bbnf's substrate ceiling is, say, 480 µs on twitter, the user reads
that against (sonic-rs 436 µs, simd-json 424 µs, serde_json 831 µs) and knows
"we are SIMD-class, slightly behind sonic-rs, well ahead of serde_json."

### 2.4 yyjson (native DOM, no-SIMD i-cache reference)

yyjson is a native C comparator, not a Rust harness row. It is retained because
the expanded profiles show a useful ceiling for scalar eager DOM: dense inline
code, low branch count, and an ~18 KiB hot parser can beat SIMD-heavy Rust rows
on some corpora. The skinny uses yyjson as a code-shape and i-cache reference:
hot function size, cycles/byte, and parse-only DOM throughput. It must not be
treated as proof that BBNF should build an eager JSON-only DOM; the general
architecture remains tape/direct union.

### 2.5 asmjson (native AVX-512 assembly, strictness-separated reference)

asmjson is the native SOTA target on x86_64 AVX-512-class hardware. It is not
available on the Apple Silicon host and must be reported in a strictness plane:
permissive rows cannot be compared against BBNF strict JSON correctness rows.
The transferable findings are grammar-neutral primitive shape: fused byte-class
masks, direct-threaded state dispatch, mask-held parser state, branchless offset
or event emission, and scalar/SWAR fallback. These findings feed Lock 16 and the
global `BackendShape::CollapsedStage`; they do not authorize JSON-specific code
in generic crates.

### 2.4 What is NOT in the competitor set for skinny

- **simdjson C++:** out of scope for a Rust harness. C++ FFI introduces
  cross-language noise that defeats the controlled comparison. The simdjson
  number anchors the structural-scan microbenchmark target (§4) but does not
  appear as a parse-time competitor row.
- **lightning-css:** CSS, not JSON. Defers to V1 H.W4.
- **tree-sitter:** different output shape (CST vs typed root). Defers to V1
  diagnostic ledger only.
- **jq, oj, json-parser-c:** not architecturally adjacent.

### 2.5 Competitor configuration table

| Competitor | Crate | Version pin | API | Materialisation |
|---|---|---|---|---|
| sonic-rs | `sonic-rs` | `=0.5` | `anchor` + `checked` wrapper rows | eager-typed / lazy raw-slice modes recorded |
| simd-json | `simd-json` | `=0.13` | `to_borrowed_value` + `to_owned_value` | borrowed + owned |
| serde_json | `serde_json` | `=1.0.117` | `from_slice::<Value>` | eager-owned |
| yyjson | native sidecar | commit + compiler recorded | parse DOM / read API sidecar | eager DOM |
| simdjson C++ | native sidecar | commit + compiler recorded | DOM + On-Demand sidecar | DOM tape / structural-index On-Demand |
| asmjson | native sidecar | commit + compiler recorded | strict/permissive sidecar | AVX-512 native DOM |
| Track 2 (bbnf hand) | (workspace) | (commit) | `parse(&str)` | typed root owning sealed Tape |
| Track 1 (bbnf gen) | (workspace) | (commit) | `parse(&str)` | typed root owning sealed Tape |

A note on materialisation match: bbnf's `JsonRoot<'i>` owns the sealed tape and
returns typed projections over it. That is neither sonic-rs's eager-typed
`Value` nor simd-json's `BorrowedValue`. The honest framing: bbnf builds a tape
and typed root, but JSON scalar
materialisation remains lazy and the payload arena must report zero writes
on the hot path. We benchmark it head-to-head with sonic-rs and simd-json
in-run anchors and report the materialisation mode per row; `S` is the
minimum of the anchor rows, not a stale static table value.

---

## §3 Corpus tiers

Skinny records two corpus tiers:

- **Historical triad**: twitter, citm_catalog, canada. This tier validates the
  tape/direct substrate against the canonical sonic-rs / simdjson community
  set. It passed after lazy-offset tape and local hot-path specialization.
- **Expanded SOTA-BEAT gate**: the full throughput corpus below. This tier is
  the current dispatch arbiter. `skinny/RESULTS.md` records overall
  **N-direct / NoGo**. The parse/tape plane has hard G rows on `twitter`,
  `random`, `unicode_mixed`, and `unicode_basic`; the sink-only
  direct-to-struct workload now passes 6 of 17 rows after the UTF-validation
  and integer-classification redress, but 11 rows remain below sonic-rs direct.

### 3.1 Corpus inventory (canonical 17-corpus expansion; 2026-05-12)

The original three-corpus set (twitter / citm / canada) is the simdjson-community canonical anchor but overfits the bench to (a) string-heavy + light-Unicode, (b) deep-object + light-numerics, (c) float-dense. The expanded set adds escape-density, full-Unicode, structural-stress, object-key-dispatch, and adversarial corpora; corpus-shape diversity is the empirical guard against per-corpus overfit (per `skinny/profile/skinny-expanded/PROFILE-REPORT.md` — the 14-corpus profile reveals that marine_ik is the worst-Mbps corpus, not canada; skinny is NOT float-overfit). The 17-row inventory below is the canonical throughput-corpus bench surface; this aligns with `restart/skinny/audit/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` §1 (the V3 authority's expanded SOTA-BEAT corpus). The 14-corpus profile at `skinny/profile/skinny-expanded/` is the historical sub-sample (pre-Wave-2 expansion); the bench harness now reports all 17 rows.

| Corpus | Purpose | Bytes (approx) | Hot path | Source |
|---|---|---|---|---|
| `twitter.json` | small, payload-rich | ~616 KB | string-heavy object/array nesting, light Unicode | simdjson-data |
| `citm_catalog.json` | medium, structural | ~1.7 MB | deep object trees, key-value heavy | simdjson-data |
| `canada.json` | large, array-of-numbers | ~2.2 MB | float-array dispatch, GeoJSON-style | simdjson-data |
| `apache_builds.json` | object-heavy build metadata | ~127 KB | object-key dispatch, light Unicode | simdjson-data |
| `github_events.json` | mixed event stream | ~65 KB | object + array, ASCII text | simdjson-data |
| `update-center.json` | nested config | ~533 KB | deep nesting, repeated keys | simdjson-data |
| `mesh.json` | wide arrays + floats | ~724 KB | 3D mesh data, float-array dispatch | simdjson-data |
| `random.json` | uniform structure stress | ~510 KB | random object/array distribution | simdjson-data |
| `gsoc-2018.json` | medium object | ~3.3 MB | structured GSoC application records | simdjson-data |
| `marine_ik.json` | deep animation rig | ~3.0 MB | deeply-nested object hierarchy, **worst-Mbps corpus per skinny-expanded profile** | simdjson-data |
| `instruments.json` | medium structured | ~220 KB | object-keyed instrument catalog | simdjson-data |
| `numbers.json` | numeric edge stress | ~150 KB | float / int / NaN edge cases | simdjson-data |
| `unicode_mixed.json` | UTF-8 stress | ~1.05 MB | ASCII + Latin-1 + BMP CJK + emoji + escapes; raw multibyte UTF-8 | synthesized + sonic-rs testdata `string_unicode.json` lineage |
| `unicode_escapes.json` | escape-density stress | ~1.05 MB | control + escape-heavy, ASCII-encoded with `\uXXXX` + surrogate pairs; **anomaly corpus: sonic-rs LazyValue collapses to 364 Mbps here (5× worse than its Value-DOM)** | synthesized + sonic-rs testdata `string_escaped.json` lineage |
| `unicode_basic.json` | raw UTF-8 baseline | ~1.05 MB | plain multibyte strings without escape density | synthesized |
| `distinct_values.json` | value-shape spread | ~70 KB | many distinct scalar/object/array shapes | synthesized |
| `y_string_unicode.json` | conformance-throughput bridge | ~140 KB | JSONTestSuite string correctness shape under timed parse | JSONTestSuite-derived |

**Per-corpus profile evidence**: `skinny/profile/skinny-expanded/PROFILE-REPORT.md` (expanded skinny baseline), `skinny/profile/sonic-rs-expanded/PROFILE-REPORT.md` (sonic-rs Value-DOM + LazyValue × inlined + noinline measurements), `skinny/profile/simdjson-expanded/` (expanded corpora; stage1/stage2 sub-decomposition per corpus), `skinny/profile/yyjson/PROFILE-REPORT.md` (yyjson reference; the no-SIMD SOTA-class anchor at 3687 MiB/s twitter / 0.91 c/B), `skinny/profile/rapidjson/PROFILE-REPORT.md` + `skinny/profile/serde_json/PROFILE-REPORT.md` (floor comparators; 449-805 MiB/s range = the recursive-descent ceiling without SIMD/LTO).

**JSONTestSuite conformance bundle** (separate from throughput corpora): 95 `y_string_*` + 95 `y_structure_*` files at `/tmp/jsontestsuite-research/JSONTestSuite/test_parsing/` exercise parse-time correctness (UTF-8 validity, surrogate-pair handling, non-character codepoints, overlong sequences). Bench harness opens these in conformance mode (parse-only, no throughput); failures count toward the `BBNF-UTF8-INVALID-AT-PARSE` and `BBNF-UNICODE-NONCHAR-CODEPOINT` diagnostic gates per ARCH §7.4.

**Synthesized adversarial corpora** (reproducible from seed via `xtask/src/bin/corpus_gen.rs`):
- `gen/deep_nest_1024.json` — 1024-deep `[[…]]` for structural recursion stress
- `gen/wide_object_10k.json` — 10K distinct keys for object-dispatch stress
- `gen/unicode_planes_all.json` — every Unicode plane (BMP + supplementary + emoji); 1/2/3/4-byte UTF-8 mix

### 3.2 Corpus sourcing

Canonical sources (record exact URL + retrieval timestamp in the manifest):

- `twitter.json`: `https://github.com/simdjson/simdjson/tree/master/jsonexamples/twitter.json`
- `citm_catalog.json`: `https://github.com/simdjson/simdjson/tree/master/jsonexamples/citm_catalog.json`
- `canada.json`: `https://github.com/simdjson/simdjson/tree/master/jsonexamples/canada.json`

Files land at `tests/fixtures/json/twitter.json`, `tests/fixtures/json/citm_catalog.json`,
`tests/fixtures/json/canada.json`. SHA-256 hashes are captured at fixture-load
time (the fixture loader hashes the file contents and writes the digest into
the per-row metadata block; the harness FAILS if the loaded hash diverges
from the expected hash committed in `tests/fixtures/json/manifest.toml`).

`tests/fixtures/json/manifest.toml` shape (committed, but the actual hash
values land at fixture-acquisition time, not at this spec time — leaving them
as `<sha256-pending>` here is intentional):

```toml
[fixtures.twitter]
path = "twitter.json"
size_bytes = 631_514
sha256 = "<sha256-pending: capture at fixture acquisition>"
source_url = "https://github.com/simdjson/simdjson/blob/master/jsonexamples/twitter.json"

[fixtures.citm_catalog]
path = "citm_catalog.json"
size_bytes = 1_727_204
sha256 = "<sha256-pending: capture at fixture acquisition>"
source_url = "https://github.com/simdjson/simdjson/blob/master/jsonexamples/citm_catalog.json"

[fixtures.canada]
path = "canada.json"
size_bytes = 2_251_051
sha256 = "<sha256-pending: capture at fixture acquisition>"
source_url = "https://github.com/simdjson/simdjson/blob/master/jsonexamples/canada.json"
```

Sizes above are simdjson community-canonical and may shift by tens of bytes;
the harness binds the SHA-256 not the size. Once the fixtures are pulled, the
SHA-256 fields are filled in and committed; from that point any drift is a
test-fail signal, not a regression.

### 3.3 Per-corpus skinny targets

The user's skinny target table (provided in the prompt) sets BEAT and PARITY
floors. PARITY is the V1-correctness floor (matches sonic-rs envelope per
ARCHITECTURE.md §4 amendment); BEAT is the J.W1 audacious target. The skinny
gates against PARITY for GO; BEAT routes to V1 H tranche body.

| Corpus | Beat target (Track 2) | Parity floor (Track 2) | sonic-rs anchor | simd-json anchor |
|---|---|---|---|---|
| `twitter.json` | ≤ 380 µs | ≤ 480 µs | 436 µs | 424 µs |
| `citm_catalog.json` | ≤ 750 µs | ≤ 950 µs | 854 µs | 831 µs |
| `canada.json` | ≤ 2.8 ms | ≤ 3.2 ms | 3.144 ms | 3.226 ms |

Beat vs parity per `restart/MASTER-PLAN.md:145-154`: parity demonstrates V1
correctness; beat is the user-mandated stretch target. The skinny matrix in
§6 binds outcomes to both.

Note: the per-corpus sonic-rs / simd-json times in the table above are
illustrative anchors against the M1 Pro baseline; the gate computes `S`
from in-run measurements per §6 notation, never from this static table.
`skinny/RESULTS.md` currently records sonic-rs at 20,810 / 24,910 / 12,658
Mbps across twitter / citm / canada, which differ from the static anchors
by 14-40% in either direction (run conditions per metadata schema §5.1).
The static table is preserved for historical anchoring and reader context;
the in-run minimum binds the verdict.

### 3.4 The parity oracle

For every fixture and every iteration, the harness checks Track 1 and Track 2
emit byte-equal token streams `(kind, span, payload_class)` and byte-equal
serialised output (round-trip via `serde_json::to_string` applied to a
normalised projection of `JsonRoot`). The parity oracle is NOT inside the
timed region; it runs once per fixture during harness setup and then a
coarse-grained sample (1-in-100 iterations) inside the timed region for drift
detection. A parity failure is a hard FAIL — bench rows do not publish. This
is the safety net against "Track 1 is fast because it elides correctness."

Parity oracle pseudocode (lives at `crates/bbnf-bench/src/parity.rs`):

```rust
pub fn assert_parity<'i>(input: &'i str) -> Result<(), ParityError> {
    let t1 = runtime::grammars::json::parse(input)?; // Track 1 (generated)
    let t2 = bbnf_bench::track2::json::parse(input)?; // Track 2 (hand-coded)
    assert_eq!(token_stream(&t1), token_stream(&t2));
    assert_eq!(t1.payload_arena_writes(), 0);
    assert_eq!(t2.payload_arena_writes(), 0);
    assert_eq!(t1.payload_arena_allocations(), 0);
    assert_eq!(t2.payload_arena_allocations(), 0);
    let s1 = serialize_canonical(&t1);
    let s2 = serialize_canonical(&t2);
    if s1 != s2 {
        return Err(ParityError::Divergence { /* span */ });
    }
    Ok(())
}
```

`serialize_canonical` produces RFC-8259-stable JSON output (sorted keys,
canonical float repr, escape-pair normalisation). It exists once in
`crates/bbnf-bench/src/parity.rs`; it is not used in the timed bench paths.

---

## §4 The structural-scan microbenchmark

Separate from the end-to-end parse bench. Measures `bbnf-simd`'s structural-
index throughput in Mbps on the JSON corpus bytes. Targets per
`restart/ARCHITECTURE.md:1519`:

- ≥ 40000 Mbps on M-series NEON
- ≥ 56000 Mbps on x86 AVX2

### 4.1 What it measures

The structural index pass: the bit-parallel SIMD scan over input bytes that
identifies pseudo-structural characters (`{ } [ ] : , "`), maintains the
quote-state bitmap, and emits the index of every structural offset. This is
simdjson's Stage 1; bbnf's `bbnf-simd` provides the equivalent.

Microbench input: raw fixture bytes. Output: a `Vec<u32>` of structural
offsets. We measure the scan time, the input byte count, and divide.
Mbps = bytes_processed * 8000 / wall_time_ns.

### 4.2 Scalar parity hash — per-corpus, not just twitter

Per `restart/audit/pass-3-runtime/PASS-3.md:476`: in exact mode, a SIMD
positive that disagrees with the scalar offset vector is a correctness
failure before tape emission. The microbench enforces parity on **each**
of the three corpora — twitter (small, payload-rich), citm (medium,
deep-structural), canada (large, array-tail). A SIMD bug that manifests
only on long inputs or on specific quote-state-density patterns slips
past a twitter-only check.

```rust
for fixture in [&twitter_bytes, &citm_bytes, &canada_bytes] {
    let simd_offsets = bbnf_simd::scan_json_structurals(fixture);
    let scalar_offsets = bbnf_simd::scalar::scan_json_structurals(fixture);
    let simd_hash = blake3::hash(bytemuck::cast_slice(&simd_offsets));
    let scalar_hash = blake3::hash(bytemuck::cast_slice(&scalar_offsets));
    assert_eq!(simd_hash, scalar_hash,
        "SIMD/scalar parity hash mismatch on {fixture_name}");
}
```

Each per-corpus parity hash is recorded as a separate metadata field
(§5.1 `scalar_parity_hash_<corpus>`). A scan that fails parity on any
corpus FAILS the gate regardless of throughput.

### 4.3 Throughput floors

| ISA | Floor | Anchor |
|---|---|---|
| M-series NEON | ≥ 40000 Mbps | simdjson OD on Apple Silicon |
| x86 AVX2 | ≥ 56000 Mbps | simdjson OD on Intel Skylake |

Below floor → NO-GO at substrate level even if Track 2 parse is fast (because
the substrate floor is what holds at scale; a parse that hits parity on
twitter but cannot scale to canada-size or bigger inputs is a false signal).

### 4.4 Microbench harness (lives at `crates/bbnf-bench/benches/simd_scan.rs`)

Sketch — per-corpus parity check + throughput rows for each fixture so
the Mbps floor is verified at the input size that exercises the kernel
hardest (canada at 2.2 MB):

```rust
use criterion::{black_box, Criterion, Throughput};

fn bench_structural_scan(c: &mut Criterion) {
    let fixtures = [
        ("twitter", std::fs::read("tests/fixtures/json/twitter.json").unwrap()),
        ("citm", std::fs::read("tests/fixtures/json/citm_catalog.json").unwrap()),
        ("canada", std::fs::read("tests/fixtures/json/canada.json").unwrap()),
    ];

    // Per-corpus parity check (out of timed region; a mismatch on any
    // corpus FAILS the gate regardless of throughput).
    for (name, bytes) in &fixtures {
        let simd = bbnf_simd::scan_json_structurals(bytes);
        let scalar = bbnf_simd::scalar::scan_json_structurals(bytes);
        assert_eq!(
            blake3::hash(bytemuck::cast_slice(&simd)),
            blake3::hash(bytemuck::cast_slice(&scalar)),
            "SIMD/scalar parity hash mismatch on {name}"
        );
    }

    let mut g = c.benchmark_group("simd/structural_scan");
    for (name, bytes) in &fixtures {
        g.throughput(Throughput::Bytes(bytes.len() as u64));
        g.bench_function(format!("{name}/simd"), |b| {
            b.iter(|| bbnf_simd::scan_json_structurals(black_box(bytes)));
        });
        g.bench_function(format!("{name}/scalar"), |b| {
            b.iter(|| bbnf_simd::scalar::scan_json_structurals(black_box(bytes)));
        });
    }
    g.finish();
}
```

The Mbps floor is gated against the **canada** row (largest input;
worst-case kernel load). twitter and citm rows are recorded for cross-
input variance reporting but the floor binds at canada.

Criterion's `Throughput::Bytes` may report bytes/s in its native output.
The gate converts elapsed time to Mbps and gates against the Mbps floor.

---

## §5 Reproducibility schema

Every bench row must serialise the schema below. A bench row with any field
missing FAILS the gate. Source: `restart/MASTER-PLAN.md:159-168` and
`restart/ARCHITECTURE.md:1514-1519`.

### 5.1 Required fields

| Field | Source | Example |
|---|---|---|
| `schema_version` | constant; bumped on any field add/remove/rename | `"2"` |
| CPU model | `sysctl -n machdep.cpu.brand_string` (macOS); `lscpu` (Linux) | `Apple M1 Pro` |
| Microarchitecture | derived from CPU model | `arm64-firestorm/icestorm` |
| OS + kernel | `uname -a` | `Darwin 25.4.0` |
| RUSTFLAGS | environment, captured | `-C target-cpu=native` |
| target-cpu | from RUSTFLAGS | `native` (M1 Pro: `apple-m1`) |
| Profile | Cargo profile name | `release` |
| Input SHA-256 | `sha256sum` of fixture | `<digest>` |
| Input size | `metadata.len()` | `631514` |
| Competitor crate | `Cargo.lock` lookup | `sonic-rs` |
| Competitor version | `Cargo.lock` lookup | `0.5.0` |
| bbnf commit | `git rev-parse HEAD` | `<commit>` |
| Warmup samples | criterion config | `3` (default) |
| Warmup time | criterion config | `3.0 s` |
| Sample size | criterion config | `100` (default) |
| Measurement time | criterion config | `5.0 s` (default; canada uses `8.0 s`) |
| Confidence interval | criterion default | `0.95` |
| Outlier rejection | criterion default | `iqr` (Tukey 1.5×) |
| Statistical method | criterion default | `bootstrap`, 100 000 resamples |
| Track | `track1_generated` / `track2_handcoded` / `competitor` | `track1_generated` |
| Materialisation mode | for bbnf rows: `typed_root_over_tape` | `typed_root_over_tape` |
| Parse mode | bbnf rows: `parse(&str)` prevalidation per ARCH §11 | `parse_str_prevalidate` |
| Source ownership | `borrowed` / `owned` / `arena` | `borrowed` |
| Allocator | global allocator used by the process | `mimalloc` |
| Plan variant | `canonical` / `host_call_dispatch_overhead` / `host_call_eager_decode` / `alternate_scalar_plan` / `alternate_dispatch_table_plan` / `alternate_pext_mask_plan` / `cold_first_parse` | `canonical` |
| Host-call mode | `none` / `direct` / `registry_callhost_dispatch_only` / `registry_callhost_eager_decode` | `none` |
| Arena writes | payload arena write counter for bbnf rows; competitors record `n/a` | `0` |
| Payload allocations | payload arena allocation counter for bbnf rows; competitors record `n/a` | `0` |
| Scalar parity hash (simd rows; per-corpus) | blake3 of offsets, keyed by corpus | `{twitter:<digest>, citm:<digest>, canada:<digest>}` |
| Peak RSS (bytes) | `getrusage(RUSAGE_SELF).ru_maxrss` post-bench, scaled by platform unit | `42_336_256` |
| Cold-cache mode | `warm` / `aarch64_dc_civac` / `aarch64_clear_cache_fallback` / `x86_64_clflush_stride64` / `x86_64_clflushopt_stride64` | `warm` |

A row missing any field is INVALID and removed from the dataset before the
threshold matrix runs.

### 5.1.1 Strictness disclosure columns (SK-V5 Wave 0 deliverable)

Per `restart/skinny/audit/SK-V5-COHORT/skv5-B3-native-sidecars.md` and
`restart/skinny/audit/GRAND-SYNTHESIS-SK-V5.md` §4, the current
`skinny/RESULTS.md` reads the N-direct verdict as a pure throughput delta
against sonic-rs / simdjson / yyjson / asmjson. It is partly a contract delta:
asmjson SWAR is permissive (accepts `0x00..0x1F` as whitespace, passes
unescaped controls inside string bodies); bbnf Track 1 parse is deferred on
UTF-8 validation at the scan stage (strict only at view materialization);
RapidJSON default flags do not validate UTF-8. Comparing strict-bbnf to
permissive-asmjson without disclosure misreads the gate. Four additional
columns are required on every bench row so the existing rows can be honestly
compared. Authority: `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V5.md`
§2.2.

| Column | Domain | Meaning |
|---|---|---|
| `Strictness` | `strict` / `permissive` / `deferred` | RFC 8259 conformance plane the parser commits to on the timed path |
| `parse_utf8` | `scan-boundary` / `view-boundary` / `none` | where UTF-8 well-formedness is validated relative to the parse boundary |
| `escape_complete` | `yes` / `no` | does the parser fully scan strings for unescaped controls (`0x00..0x1F` inside string bodies) per RFC 8259 §7? |
| `flaw_probe` | one-line text | summary of where the parser diverges from strict RFC 8259, citing a JSONTestSuite test ID where applicable; empty when fully strict |

Per-parser baseline values populated at row emission time:

| Parser | Strictness | parse_utf8 | escape_complete | flaw_probe |
|---|---|---|---|---|
| bbnf Track 1 (generated) | `strict` | `view-boundary` | `yes` | deferred for parse-only because UTF-8 fall-through is scalar and shape-dependent; mark honestly per row when scan-stage validator is bypassed |
| sonic-rs | `strict` | `scan-boundary` | `yes` | (empty) |
| simdjson C++ | `strict` | `scan-boundary` | `yes` | (empty) |
| yyjson default | `strict` | `scan-boundary` | `yes` | (empty) |
| asmjson SWAR | `permissive` | `none` | `no` | accepts 0x00..0x1F as whitespace; passes unescaped controls inside strings |
| RapidJSON default | `permissive` | `none` | `no` | default flags skip UTF-8 validation; controls pass inside strings |
| serde_json | `strict` | `scan-boundary` | `yes` | (empty) |

The four columns are emitted by the `Sidecar` trait in
`bbnf-bench/src/lib.rs` and propagate through every native sidecar row.
Missing values FAIL the schema gate at §5.3 the same way any required field
does. Outcome classification at §6 reads these columns as advisory context;
strictness-disjoint comparisons cannot ratify a `BEAT` claim, only a
`PARITY-PLUS-CONTRACT-DELTA` annotation.

### 5.2 Capture mechanism

`crates/bbnf-bench/src/metadata.rs`:

```rust
pub struct RowMetadata {
    schema_version: &'static str,    // "2" — bumped on field schema change
    cpu_model: String,
    cpu_arch: String,
    os_kernel: String,
    rustflags: String,
    target_cpu: String,
    profile: String,
    input_sha256: String,
    input_bytes: u64,
    competitor_crate: Option<String>,
    competitor_version: Option<String>,
    bbnf_commit: String,
    warmup_samples: u32,
    warmup_time_s: f64,
    sample_size: u32,
    measurement_time_s: f64,
    confidence_interval: f64,
    outlier_rejection: String,
    statistical_method: String,
    track: TrackTag,
    materialisation: String,
    parse_mode: String,
    source_ownership: String,
    allocator: String,
    plan_variant: String,
    host_call_mode: String,
    arena_writes: Option<u64>,
    payload_allocations: Option<u64>,
    scalar_parity_hash_twitter: Option<String>,
    scalar_parity_hash_citm: Option<String>,
    scalar_parity_hash_canada: Option<String>,
    peak_rss_bytes: Option<u64>,
    cold_cache_mode: String,
}

impl RowMetadata {
    pub fn capture_host_metadata() -> HostFacts;
    pub fn from_criterion(b: &Bench, host: &HostFacts) -> Self;
    pub fn write_toml(&self, path: &Path) -> io::Result<()>;
}
```

Host capture runs ONCE per bench invocation (not per iteration); per-row
populates from criterion + the prebuilt `HostFacts`. Output goes to a TOML
mirror at `target/criterion/<benchgroup>/<benchname>/metadata.toml` alongside
the criterion JSON report.

### 5.3 Schema enforcement gate

`crates/bbnf-bench/src/gate.rs`: the post-bench gate (§8.2) reads every
`metadata.toml`, validates `schema_version` matches the gate's compiled-in
expected version, validates all required fields are present and non-empty,
and FAILS the bench run if any are missing. The gate runs in CI before the
threshold matrix. Missing-field rows or version-mismatched rows are removed
from the dataset before threshold comparison even runs — schema enforcement
precedes outcome classification. A `schema_version` bump in the gate
without a corresponding RowMetadata update is itself a hard FAIL: the gate
refuses to classify across schema versions silently.

---

## §6 Go/no-go threshold matrix

The matrix below is the entire reason the skinny exists. Each row binds
(Track 2 outcome) × (Track 1 outcome) → verdict + action. The user gets a
single classified outcome plus a recommended next step.

The matrix uses the fastest in-run sonic-rs/simd-json anchor because Lock 8
names both as JSON SOTA comparators. Static SOTA.md numbers calibrate the
expected envelope; the gate classifies against rows measured in the same run.

Notation:

- `S = min(sonic_rs_anchor_time, simd_json_borrowed_time, simd_json_owned_time)`
  per corpus, measured in the same run and recorded with API/mode metadata.
- `T_README` = the per-corpus README spec target: 380 µs (twitter), 750 µs
  (citm), 2.8 ms (canada) per `restart/README.md:332` and
  `restart/MASTER-PLAN.md` SOTA gate rows.
- `BEAT_BOUND = min(S × 0.95, T_README)` — outcome A binds on the stricter of
  the two so a "GO — beat" verdict cannot ratify a result the README spec
  considers a miss.
- Track 2 is the substrate ceiling; Track 1 is the generated parser.

### 6.1 The full matrix

| Outcome ID | Track 2 (substrate ceiling) | Track 1 (generated) | Verdict | Action |
|---|---|---|---|---|
| **A — Beat-and-parity** | ≤ BEAT_BOUND | ≤ Track 2 × 1.10 | GO — full V1 SOTA-beat likely | Dispatch tranches A-J on schedule. SOTA-beat probability ≥ 70% before H tranche entry. The README spec target is **part of the gate**, not a separate aspiration. |
| **B — Beat substrate, parity codegen** | ≤ BEAT_BOUND | ≤ Track 2 × 1.15 | GO — full V1 likely | Dispatch tranches A-J. SOTA-beat probability 50-70% pending H.W3 tuning. |
| **C — Substrate parity, codegen acceptable** | ≤ S × 1.05 | ≤ Track 2 × 1.15 | GO — V1 parity likely | Dispatch tranches A-J. SOTA-beat probability 30-50%; route SOTA-beat to H tranche body. |
| **D — Substrate parity, codegen gap** | ≤ S × 1.05 | > Track 2 × 1.15 and ≤ Track 2 × 1.50 | GO with codegen focus | Dispatch tranches A-J. Tranche F (Rust lowerer) and tranche H absorb extra iteration; pre-allocate +1 wave to F. |
| **E — Substrate parity, codegen failure** | ≤ S × 1.05 | > Track 2 × 1.50 | CONDITIONAL — codegen hold | Allow only A-prep work that does not commit the Rust-lowering shape. Hold F until codegen prototype lands a < 1.20× ratio. Reopen Lock 5 if F retry fails. |
| **F-positive — Codegen matches hand on borderline-weak substrate** | S × 1.05 < Track 2 ≤ S × 1.10 | ≤ Track 2 × 1.05 | CONDITIONAL — substrate warning, codegen positive | Dispatch A/workspace-only and tranche B substrate work; pre-allocate +1 wave to H.W1 (SIMD scan tuning). The codegen ratio is a *positive* finding (the generator is competitive with hand-coded). The Lock 1 reopen is precautionary, not driven by codegen evidence. SOTA-beat probability < 30% before substrate fix. |
| **F-noise — Borderline-weak substrate, codegen indistinguishable from hand within bench noise** | S × 1.05 < Track 2 ≤ S × 1.10 | Track 2 × 1.05 < Track 1 ≤ Track 2 × 1.10 | CONDITIONAL — substrate warning | Same dispatch posture as F-positive; the codegen ratio falls within the Track 1 95% CI upper bound's overlap of Track 2 × 1.05 — unclassifiable as positive or gap. Re-run on bare-metal before committing to dispatch posture. |
| **F-codegen-gap — Borderline-weak substrate, codegen overhead atop weak substrate** | S × 1.05 < Track 2 ≤ S × 1.10 | > Track 2 × 1.10 | CONDITIONAL — substrate warning + codegen gap | Same substrate posture as F-positive (Lock 1 reopen precautionary; +1 wave to H.W1) PLUS codegen attention: pre-allocate +1 wave to F (Rust lowerer) since the generator is widening the gap relative to hand. SOTA-beat probability < 30%. |
| **G — Substrate failure** (collapsed prior G + H) | > S × 1.10 | any | NO-GO — substrate redesign or materialization-plan change | Block tranche B dispatch. Reopen the Lock 1 amendment surface only after checking whether the miss is a retained-substrate problem or an implementation-plan problem. (Track 1 ratio is informational only when Track 2 is a gap — the codegen rides the failed substrate; no separate "both fail" outcome.) **Disposition note (2026-05-12):** eager-token alternates (dispatch-table REDRESS-17, 12-byte token REDRESS-18, pair-token fusion REDRESS-16) were measured-and-rejected. Lazy-offset tape plus local hot-path specialization cleared the historical triad, but the expanded parse corpus still has G rows and the full gate is currently N-direct / NoGo because direct typed emission is not SOTA-class. Future outcome-G classifications first run the workload split in §6.6 and the profile packet in §10.8; Lock 1 is amended only if `OffsetTape`, `EventTape`, `SinkOnly`, and `CollapsedStage` are all insufficient for the rule class. |
| **G-fusion-quality — Event-cursor / fusion-quality gap** (new 2026-05-12) | ≤ S × 1.10 (substrate within parity envelope) | hot-leaf count ≥ 5 at ≥10% self-time AND comparator-anchored hot-leaf count ≤ 2 | NO-GO — typed-event cursor or primitive fusion required | The substrate is within the parity envelope but the generated parser carries dispatch, cursor, string, or number overhead the comparator does not. Do **not** reopen Lock 1. The architectural lever is the typed event cursor over the tape projection plus grammar-neutral primitive fusion: `bbnf-simd::{ByteClassPlan, KernelSet}`, `parse-that/string`, `parse-that/unicode`, and `parse-that/number`. Comparator anchors: sonic-rs = 1 hot leaf (`skinny/profile/sonic-rs-v2/PROFILE-REPORT.md` §(d)), simdjson = 2 hot leaves (`skinny/profile/simdjson-v2/PROFILE-REPORT.md` §(a)), yyjson ≈ one i-cache-resident scalar parse driver. Dispatch the SOTA-BEAT implementation packet against the skinny workspace; no new directive and no parallel substrate. **Two-pathology-class taxonomy (Wave 2 Agent 2 finding, 2026-05-12)**: the current G/NoGo corpus splits into two diagnostic sub-classes within `G-fusion-quality`, each prescriptive of a distinct NEON kernel fix — see §6.1.1 below. |
| **I — Parity oracle fail** | n/a | n/a | NO-GO — correctness fail | Block tranche dispatch. Track 1 and Track 2 disagree on materialised output for at least one fixture; codegen is incorrect. Investigate divergence before any further bench claims. |
| **J — Reproducibility schema fail** | n/a | n/a | INVALID — re-run | Bench row missing required schema fields or schema_version mismatch; classification unsafe. Re-instrument and re-run. |
| **K — SIMD parity hash fail** | n/a | n/a | NO-GO — correctness fail | The structural-scan SIMD path produces offsets disagreeing with scalar on **any** corpus (twitter / citm / canada); substrate is silently corrupt. Block all dispatch until SIMD codepath fixed. |
| **L — SIMD throughput fail** | n/a | n/a | NO-GO — SIMD floor fail | Structural scan on **canada** (largest input; binding row) below floor (40000 Mbps NEON / 56000 Mbps AVX2). Even if Track 2 parse hits parity, the substrate ceiling will fail at scale. Block dispatch until the SIMD floor is restored, then re-run the full matrix. |
| **M — Memory residency fail** | n/a | n/a | NO-GO — peak RSS exceeds floor | Track 2 (or Track 1) peak RSS > 3 × the fastest competitor's peak RSS on canada. Substrate that hits SOTA-class throughput at 3× memory is not viable for concurrent-parse workloads (web servers, batch ingestion). Block dispatch until substrate memory profile is fixed. The 3× multiplier is the V1 J.W1 J-side floor projected back to skinny gate; a tighter ratio is encouraged but not required. |
| **N-direct — Direct projection throughput fail** | n/a | n/a | NO-GO — direct typed emission is not SOTA-class | The `direct_to_struct` workload is correctness-green but either Track 1 direct or Track 2 direct is slower than `sonic-rs * 1.10` in time. The current sink-only digest parser removes the retained view walk from the timed BBNF rows; after duplicate UTF-8 validation was removed and scanner-owned integer classification landed, 6 of 17 rows pass and 11 still miss sonic-rs direct. Block SOTA-BEAT dispatch until direct-only API shapes close exact float/string/Unicode materialization under `SinkOnly`. This is separate from outcome G: a parse-only win cannot ratify the BBNF direct-to-struct premise. |

### 6.1.1 G-fusion-quality two-pathology-class taxonomy (Wave 2 Agent 2 finding)

The current expanded-gate misses split into two diagnostic pathology classes within `G-fusion-quality`. Each class prescribes a distinct NEON kernel fix; the classes are diagnostic sub-classes of the broader `G-fusion-quality` outcome and route to the SOTA-BEAT implementation packet (`restart/skinny/audit/SOTA-BEAT-DESIGN.md` §6 falsifiability matrix) without reopening Lock 1.

| Pathology class | Symptom | Affected corpora | Prescribed kernel | Falsifier |
|---|---|---|---|---|
| **`tiny_string_loop`** | Throughput collapses on object-key-dispatch + short-string corpora because the structural-scan SIMD inner loop pays per-chunk fixed cost not amortised by tiny strings. Hot-leaf count ≥ 5 with `match_json_string` / `scan_inside_string` dominating. | `github_events`, `update_center`, `apache_builds`, `instruments` | NEON LD4-interleaved 4-channel classifier (Lock 16 row, Validark 2024) + `vbcaxq_u8` ternary bitwise reduction (Lock 16 row, ARMv8.2-A SHA3); compress fixed-cost preamble into the per-chunk pipeline. | Kernel lands; corpora cross S anchor. Falsified if the kernel lands but the corpora still miss S — routes to `EventTape` materialization probe per ARCH §7.3. |
| **`hex_decode`** | Throughput collapses on escape-density + Unicode-escape corpora because `\uXXXX` hex decode runs scalar per-codepoint. Hot-leaf count ≥ 5 with `decode_escapes` / `handle_unicode_codepoint` / `from_u32` dominating. | `unicode_escapes`, `y_string_unicode`, `random` (escape-heavy paths) | NEON `vqtbl4q_u8` 64-byte hex-nibble lookup (Lock 16 row, Lemire 2019) + `vshrn_n_u16` movemask compaction for escape-byte locations + branchless surrogate-pair recombination. | Kernel lands; corpora cross S anchor. Falsified if the kernel lands but the corpora still miss S — routes to lazy-borrow `HasEsc` flag re-derivation per `COMPILER.md` §3.3 Primitive 3. |

Both pathology classes share the comparator-anchored hot-leaf count threshold (≥ 5 at ≥ 10% self-time while sonic-rs = 1 and simdjson = 2). The `G-fusion-quality` classifier emits the pathology-class label as a diagnostic field on the bench row; CI consumes both the outcome and the sub-class.

### 6.1.2 M5 Max DOM-class anchors (Wave 2 re-baselined; 2026-05-12)

The Wave 2 re-baseline pass re-anchored DOM-class throughputs on M5 Max. The numbers below are the binding `S` candidates per corpus and are used when the in-run anchor measurement falls within their 95% CI. They are not a substitute for the in-run anchor; they are the cross-run sanity floor.

| Corpus | M5 Max DOM-class throughput (Mbps) | Source |
|---|---:|---|
| `twitter` | 22071 | Wave 2 re-baseline; sonic-rs Value-DOM |
| `citm_catalog` | 29959 | Wave 2 re-baseline; sonic-rs Value-DOM |
| `canada` | 14051 | Wave 2 re-baseline; sonic-rs Value-DOM |
| `github_events` | 20709 | Wave 2 re-baseline; sonic-rs Value-DOM |
| `update-center` | 18538 | Wave 2 re-baseline; sonic-rs Value-DOM |
| `random` | 12373 | Wave 2 re-baseline; sonic-rs Value-DOM |
| `unicode_escapes` | 17079 | Wave 2 re-baseline; sonic-rs Value-DOM |
| `y_string_unicode` | 11120 | Wave 2 re-baseline; sonic-rs Value-DOM |

Cross-reference: `restart/skinny/audit/SOTA-BEAT-DESIGN.md` §6 (falsifiability matrix) carries the per-class pathology falsifier rows; failed kernels route to materialization-plan probes at §7.8.2 alongside `alternate_event_cursor_plan` / `alternate_capacity_plan` / `alternate_primitive_kernel_plan`.

### 6.2 Reading the matrix

The matrix is decided per corpus. The classifier checks correctness/schema/
floor rows first (I, J, K, L, M), then throughput rows. The verdict for the
historical triad is reported separately from the expanded SOTA-BEAT gate. The
dispatch verdict for skinny is the WORST outcome across the expanded throughput
corpus plus the structural-scan microbench, the memory floor, and the masking
probes in §7.8.
Examples:

- All three corpora outcome A, structural scan ≥ floor, memory within
  3 × competitor → outcome A overall.
- twitter outcome A, citm outcome C, canada outcome D → outcome D overall
  (the worst).
- canada or any expanded blocker outcome G → expanded gate outcome G overall,
  no matter what twitter / citm say.
- any direct-to-struct workload row emitting `N-direct` → overall
  `N-direct / NoGo`, even if parse-only rows pass.
- Parity oracle fail anywhere → outcome I, blocking everything.
- Peak RSS > 3 × fastest competitor on canada → outcome M, blocking
  regardless of throughput.

This is intentional: the skinny is not "average performance," it is "worst-
case viability for the V1 architectural premise." A parser that smokes
twitter but flames out on canada has failed the substrate generality test,
and the V1 architecture has to support all three corpora at once.

### 6.2.1 Classification order

The classifier fires in this order; the first matching outcome wins:

1. J (schema fail) — fail closed.
2. I (parity oracle fail) — correctness gate.
3. K (SIMD parity fail) — correctness gate.
4. L (SIMD throughput fail on canada) — substrate floor.
5. M (peak RSS > 3 × competitor) — substrate viability.
6. G (Track 2 > S × 1.10) — substrate gap.
7. N-direct (any direct-to-struct Track 1/Track 2 row slower than sonic-rs direct × 1.10 in time) — typed-emission gap.
8. F-positive / F-noise / F-codegen-gap (Track 2 ∈ (S × 1.05, S × 1.10]) — substrate warning, by Track 1 sub-band: F-positive when Track 1 ≤ Track 2 × 1.05; F-noise when the Track 1 95% CI upper bound overlaps Track 2 × 1.05 (and Track 1 ≤ Track 2 × 1.10); F-codegen-gap when Track 1 > Track 2 × 1.10.
9. E (Track 2 ≤ S × 1.05, Track 1 > Track 2 × 1.50) — codegen failure.
10. D (Track 2 ≤ S × 1.05, Track 1 ∈ (Track 2 × 1.15, Track 2 × 1.50]) — codegen gap.
11. C (Track 2 ≤ S × 1.05, Track 1 ≤ Track 2 × 1.15) — parity acceptable.
12. B (Track 2 ≤ BEAT_BOUND, Track 1 ≤ Track 2 × 1.15) — beat substrate.
13. A (Track 2 ≤ BEAT_BOUND, Track 1 ≤ Track 2 × 1.10) — beat-and-parity.

The order is deliberate: correctness/floor failures dominate; substrate
gaps dominate codegen issues (a fast generator on a broken substrate is
not viable); codegen issues only matter when the substrate floor is met.

**Measured gate split (2026-05-12, SK-V3 Wave 0/1 + direct workload).**
`skinny/RESULTS.md` records two facts that must stay visible. The expanded
parse corpus still has hard **G / NoGo** rows: `twitter`, `random`,
`unicode_mixed`, and `unicode_basic`. Many shape rows are already viable:
`citm_catalog`, `canada`, `apache_builds`, `github_events`, `mesh`,
`gsoc-2018`, `marine_ik`, and `numbers` classify as A / GO; `update_center`,
`instruments`, `unicode_escapes`, `distinct_values`, and `y_string_unicode`
classify as C / GO. Structural-only canada remains above the 40000 Mbps floor
at 69075 Mbps. The overall verdict is now **N-direct / NoGo** because the
`direct_to_struct` workload is correctness-green; 6 of 17 rows pass after the
UTF-validation and integer-classification redress, and 11 remain
throughput-red against sonic-rs direct. Accepted implementation wins are lazy offset
tape, sparse flags, direct spare-capacity offset writes, cold errors, SWAR digit
and plain-string runs, fused comma/close delimiter consumption,
newline-indent space-run skipping, `parse_value_at`, short plain-string fast
path, Track 2 inline parity, strict `bbnf-simd` checkasm, and parse-that
string/unicode closure. Rejected routes remain rejected: eager-token revival,
sidecar structural-index typed-parser prepass, active 16-byte tiny-string
parser dispatch, separator elision, generic SWAR whitespace skipper,
12-byte/width churn, and dispatch-table/function-pointer alternates.

### 6.2.2 Workload split for current NO-GO rows

Every expanded-gate rerun reports these workload modes per corpus:

| Workload | What it proves | Required rows |
|---|---|---|
| `parse_only` | raw parser/tape/direct ceiling | Track 1, Track 2, sonic-rs, simd-json, serde_json |
| `parse_full_traversal` | all strings/numbers/arrays touched, exposing lazy work | Track 1, sonic-rs Value-DOM, simdjson DOM, yyjson sidecar |
| `path_lookup` | cursor/direct projection and key lookup cost | Track 1 path, sonic-rs pointer/LazyValue, simdjson On-Demand pointer |
| `direct_to_struct` | BBNF typed emission premise | Track 1 generated runtime/codegen `SinkOnly` direct, Track 2 independent hand-coded sink over runtime event/sink traits, retained-view parity oracle, sonic-rs serde struct; outcome `N-direct` if either BBNF direct track is slower than `sonic-rs * 1.10` in time. A bench-private Track 1 parser is INVALID for SK-V4 close. |
| `unicode_string_float` | string decode, UTF-8, escapes, number materialization | `unicode_*`, `numbers`, `canada`, JSONTestSuite-derived rows |
| `memory` | retained substrate cost | peak RSS, offset/event counts, payload bytes, allocations |
| `cycles_per_byte` | native SOTA comparability | samply or perf c/B for hot rows |

### 6.3 Honest accounting

The user instruction was to "not assume that any of these other libs have
'magic' SIMD facilities" — but the matrix MUST still call NO-GO honestly.
Outcomes G / I / J / K / L / M / N-direct exist precisely because the bench is the
arbiter, not the plan. If the substrate genuinely fails the SOTA gate, the
matrix says so; the spec does not bias toward GO.

That said: outcome G blocks tranche B and full A-J dispatch, but does not
forbid workspace-only prep that cannot commit substrate APIs. The user gets a
clear blocker rather than a false green light.

### 6.4 What outcome A looks like in numbers

For twitter.json on M1 Pro, if the in-run sonic-rs anchor = 436 µs and the
fastest simd-json row = 424 µs, S = min = 424 µs.

- `T_README` (twitter) = 380 µs.
- `BEAT_BOUND = min(S × 0.95, T_README) = min(403 µs, 380 µs) = 380 µs`.
- Track 2 ≤ 380 µs (substrate beats AND meets README spec).
- Track 1 ≤ 1.10 × Track 2 ≤ 418 µs (codegen overhead ≤ 10%).

The bind on `T_README` is the corrective: at the prior S × 0.95 bound
(403 µs), a Track 2 of 400 µs would have ratified outcome A — but the
README spec calls 400 µs a parity-not-beat result. The new BEAT_BOUND
prevents that ratification. Outcome A now means "meets the README spec
beat target AND beats the in-run competitor floor."

Per-corpus BEAT_BOUND values:

| Corpus | S (typical) | S × 0.95 | T_README | BEAT_BOUND |
|---|---:|---:|---:|---:|
| twitter | 424 µs | 403 µs | 380 µs | **380 µs** (T_README binds) |
| citm | 831 µs | 789 µs | 750 µs | **750 µs** (T_README binds) |
| canada | 3.144 ms | 2.987 ms | 2.8 ms | **2.8 ms** (T_README binds) |

In all three corpora the README spec target is stricter than S × 0.95;
T_README binds outcome A across the board. Outcomes B and C use S × 0.95
and S × 1.05 unchanged — they are the parity-class verdicts and the
README spec is not part of their definition.

### 6.5 What the matrix does not classify

The matrix covers parse throughput. It does not cover:

- Full nine-grammar LOC budget compliance (defers to F.W3; see §9 omissions).
- Memory residency / peak RSS (instrumented but report-only at skinny;
  V1 J.W1 owns the gate).
- Allocator selection drift across runs (skinny pins `mimalloc` for the
  bbnf side; competitors use their own defaults; reported but not gated).

---

## §7 Criterion harness layout

Single bench file: `crates/bbnf-bench/benches/json_parity.rs`.

### 7.1 Bench groups

```text
crates/bbnf-bench/benches/json_parity.rs
  group: json/twitter
    bench: track1_generated
    bench: track2_handcoded
    bench: sonic_rs_anchor
    bench: sonic_rs_checked
    bench: simd_json_borrowed
    bench: simd_json_owned
    bench: serde_json
  group: json/citm
    bench: track1_generated
    bench: track2_handcoded
    bench: sonic_rs_anchor
    bench: sonic_rs_checked
    bench: simd_json_borrowed
    bench: simd_json_owned
    bench: serde_json
  group: json/canada
    bench: track1_generated
    bench: track2_handcoded
    bench: sonic_rs_anchor
    bench: sonic_rs_checked
    bench: simd_json_borrowed
    bench: simd_json_owned
    bench: serde_json

  group: json/probes/<corpus>
    bench: host_call_dispatch_overhead   # per-call microbench, isolated
    bench: host_call_eager_decode        # gross-time JSON variant
    bench: alternate_scalar_plan         # confirmatory (the active alternate)
    bench: alternate_dispatch_table_plan # INVALID post-iteration; see §7.8.2 + skinny/REDRESS.md item 17
    bench: alternate_pext_mask_plan      # x86_64 only; unimplemented in skinny (V1 H.W2 owns)
    bench: cold_first_parse              # report-only; cache-flushed

crates/bbnf-bench/benches/simd_scan.rs (separate file, separate harness)
  group: simd/structural_scan
    bench: twitter/simd
    bench: twitter/scalar
```

### 7.2 Warmup, sample, measurement

Criterion configuration uses these values; each is recorded in per-row
metadata. Defaults for criterion 0.5 are explicit:

```rust
use criterion::{Criterion, BenchmarkId, Throughput};
use std::time::Duration;

fn json_config() -> Criterion {
    Criterion::default()
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(5))
        .sample_size(100)
        .confidence_level(0.95)
        .significance_level(0.05)
        .noise_threshold(0.02)
}

fn canada_config() -> Criterion {
    json_config()
        .measurement_time(Duration::from_secs(8))   // longer; canada is 2.2 MB
        .sample_size(50)
}
```

Outlier rejection uses criterion's default Tukey IQR fence (1.5×). Bootstrap
resampling at 100 000 default. These are recorded in metadata; downstream
gate code reads them rather than baking them in.

### 7.3 Per-iteration setup

simd-json's `to_borrowed_value` mutates input, so per-iteration the input
must be cloned. Criterion's `iter_batched` separates setup from timing:

```rust
b.iter_batched(
    || bytes.clone(),                  // setup (untimed)
    |mut buf| simd_json::to_borrowed_value(&mut buf).unwrap(),
    BatchSize::LargeInput,
);
```

The other competitors (`from_slice`) and bbnf's `parse(&str)` are non-
mutating and use `b.iter(|| black_box(...))` directly. Track 1 / Track 2 use:

```rust
b.iter(|| {
    let root = runtime::grammars::json::parse(black_box(input))?;
    black_box(&root);
    Ok::<(), _>(())
});
```

`black_box` prevents the optimizer from eliding the parse. The result type is
acknowledged so that the parse function signature remains `Result<T, E>`.

### 7.4 Throughput reporting

```rust
g.throughput(Throughput::Bytes(input.len() as u64));
```

Criterion records elapsed time; the gate converts every parse and scan row to
Mbps for the JSON report.

### 7.5 Output sinks

Criterion writes JSON reports to `target/criterion/<group>/<bench>/`. The
TOML metadata mirror writes alongside, at
`target/criterion/<group>/<bench>/metadata.toml`. Both are inputs to the
post-bench gate.

### 7.6 Cargo.toml entry

`crates/bbnf-bench/Cargo.toml`:

```toml
[package]
name = "bbnf-bench"
version = "0.0.0"
edition = "2021"
publish = false

[lib]
path = "src/lib.rs"

[[bench]]
name = "json_parity"
harness = false
path = "benches/json_parity.rs"

[[bench]]
name = "simd_scan"
harness = false
path = "benches/simd_scan.rs"

[dependencies]
runtime = { path = "../runtime" }
bbnf-simd = { path = "../bbnf-simd" }
blake3 = "1"
sha2 = "0.10"
toml = "0.8"
serde = { version = "1", features = ["derive"] }
bytemuck = "1"
mimalloc = { version = "0.1", default-features = false }

[dev-dependencies]
criterion = { version = "0.5", features = ["html_reports"] }
sonic-rs = { version = "=0.5", default-features = false, features = ["sort_keys", "utf8_lossy"] }
simd-json = { version = "=0.13", default-features = false, features = ["serde_impl"] }
serde_json = { version = "=1.0.117", default-features = false, features = ["std"] }
```

### 7.7 The full bench file sketch

```rust
// crates/bbnf-bench/benches/json_parity.rs
use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion, Throughput};
use std::time::Duration;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

mod fixtures {
    use std::path::PathBuf;
    pub struct Fixture { pub name: &'static str, pub path: PathBuf, pub bytes: Vec<u8>, pub sha256: String }
    pub fn load_all() -> Vec<Fixture> { /* reads tests/fixtures/json/manifest.toml, validates SHA-256 */ }
}

mod metadata {
    use bbnf_bench::metadata::*;
    pub fn capture_host() -> HostFacts { HostFacts::probe() }
}

fn run_corpus(c: &mut Criterion, fix: &fixtures::Fixture, host: &metadata::HostFacts) {
    let mut g = c.benchmark_group(format!("json/{}", fix.name));
    g.throughput(Throughput::Bytes(fix.bytes.len() as u64));
    if fix.name == "canada" { g.measurement_time(Duration::from_secs(8)).sample_size(50); }
    let input = std::str::from_utf8(&fix.bytes).unwrap();

    // Track 1: generated.
    g.bench_function("track1_generated", |b| {
        b.iter(|| { let r = runtime::grammars::json::parse(black_box(input)).unwrap(); black_box(&r); });
    });
    // Track 2: hand-coded.
    g.bench_function("track2_handcoded", |b| {
        b.iter(|| { let r = bbnf_bench::track2::json::parse(black_box(input)).unwrap(); black_box(&r); });
    });
    // sonic-rs anchor + checked rows.
    g.bench_function("sonic_rs_anchor", |b| {
        b.iter(|| { let v = bbnf_bench::competitors::sonic_anchor(black_box(&fix.bytes)).unwrap(); black_box(v); });
    });
    g.bench_function("sonic_rs_checked", |b| {
        b.iter(|| { let v: sonic_rs::Value = sonic_rs::from_slice(black_box(&fix.bytes)).unwrap(); black_box(v); });
    });
    // simd-json (mutates input, clone per-iter).
    g.bench_function("simd_json_borrowed", |b| {
        b.iter_batched(|| fix.bytes.clone(), |mut buf| { let v = simd_json::to_borrowed_value(&mut buf).unwrap(); black_box(v); }, BatchSize::LargeInput);
    });
    g.bench_function("simd_json_owned", |b| {
        b.iter_batched(|| fix.bytes.clone(), |mut buf| { let v = simd_json::to_owned_value(&mut buf).unwrap(); black_box(v); }, BatchSize::LargeInput);
    });
    // serde_json.
    g.bench_function("serde_json", |b| {
        b.iter(|| { let v: serde_json::Value = serde_json::from_slice(black_box(&fix.bytes)).unwrap(); black_box(v); });
    });

    // Write per-row metadata for every bench just executed.
    for bench_name in ["track1_generated", "track2_handcoded", "sonic_rs_anchor", "sonic_rs_checked", "simd_json_borrowed", "simd_json_owned", "serde_json"] {
        metadata::write_row(host, fix, bench_name);
    }
    g.finish();
}

fn benches(c: &mut Criterion) {
    let host = metadata::capture_host();
    let fixtures = fixtures::load_all();

    // Parity oracle once per fixture before any timing.
    for fix in &fixtures {
        let s = std::str::from_utf8(&fix.bytes).unwrap();
        bbnf_bench::parity::assert_parity(s).expect("parity oracle FAILED");
    }

    for fix in &fixtures { run_corpus(c, fix, &host); }
}

criterion_group! { name = json_parity; config = Criterion::default().warm_up_time(Duration::from_secs(3)).measurement_time(Duration::from_secs(5)).sample_size(100); targets = benches }
criterion_main!(json_parity);
```

`crates/bbnf-bench/benches/simd_scan.rs` is a parallel shorter file using
the §4.4 sketch.

### 7.8 Masking probes

Probes exist to bound skinny omissions that could otherwise hide V1 costs.
Each probe answers a *specific question* with a *specific threshold*.
Aggregate "median > 2%" thresholds are forbidden — they conflate distinct
masking modes.

#### 7.8.1 Host-call probes (two probes, two questions)

The skinny is host-fn-free per `COMPILER.md` §1.3. The V1 JSON grammar
(ARCH §12.2) has a parse-time `decode_json_string_to_arena` host call.
Two distinct masking modes:

**Probe A — `host_call_dispatch_overhead`** (per-call microbench, isolated).
Question: how much does `CallHost` registry dispatch cost per invocation,
in isolation from the work being dispatched? Implementation: a
microbench that calls a no-op host function `N` times via the registry,
and `N` times via direct call; reports `(registry_time - direct_time) / N`
as ns/call. Threshold: ≤ 50 ns/call on M1 Pro. Rationale for the
threshold: a worst-case un-inlined virtual call on M1 Pro is ~5-10 ns
plus a registry hash-map lookup at ~20-40 ns, for a worst-case envelope of
~30-50 ns. The measured prototype reports ~0.7 ns/call (skinny/RESULTS.md
masking-probes table) — the registry call site inlines on release, so the
dispatch collapses to roughly a single load + indirect branch and the
threshold holds with three orders of magnitude headroom. The 50 ns ceiling
leaves room for V1 grammars where the registry path is not inlined. Pass:
probe ≤ 50 ns/call. Fail: probe > 50 ns/call, RESULTS marks dispatch
overhead as MASKING and notes that V1 grammars with many `@host fn` calls
per parse will pay more than the skinny measures.

**Probe B — `host_call_eager_decode`** (gross-time variant, JSON-shaped).
Question: when the V1 grammar's eager string decode runs through the
registry, how does end-to-end parse time compare? Implementation: a
JSON variant that emits `CallHost decode_string_to_arena` for every
string token at parse time, identical to V1 grammar shape. Threshold: the
gate fires a MASKING signal when the eager-decode row exceeds Track 1's
gross time by a per-corpus envelope: **> 1.15× T1 on twitter** (string-heavy,
700+ string fields, 5% escape density), **> 1.08× T1 on citm** (key-value
heavy but mostly short strings), **> 1.02× T1 on canada** (numeric, few
strings). These thresholds replace an earlier predictive-band rationale
(prior expectations of 5-15% / 3-8% / < 2% deltas) that the iteration
empirically refuted — the measured prototype reports gross-time penalties
of 1.74× / 1.30× / 1.22× across twitter / citm / canada
(`skinny/RESULTS.md` masking-probes rows; the row is correctly classified
as MASKING by the gate).

The thresholds are now disposition rules, not predictions: any eager-decode
row beyond the envelope marks the parse-time eager-decode work as MASKING
for the substrate; below the envelope, the row is reported but does not
fire a MASKING signal. A low outlier still warrants inspection (the work
may have been elided by the compiler), and the gate records the explicit
ratio for each corpus.

Premise commitment (post-iteration): V1 JSON must keep string decode lazy
in the substrate / view layer. The eager-decode rows in `skinny/RESULTS.md`
show 22-74% gross-time penalty across the three corpora; that magnitude
of SOTA hit is severe enough that "accept the SOTA hit" is no longer a
viable alternative. The isolated `host_call_dispatch_overhead` row owns
registry dispatch cost; this Probe B row owns the eager-decode-work cost
and the disjunction is closed by measurement, not preference.

#### 7.8.2 Alternate-plan probes — measured plan space (post-expanded gate)

The alternate-plan probes bound the missing cost-driven rewrite axis.
**Post-expanded framing**: scalar remains confirmatory; the dispatch-table
alternate was empirically invalidated (the SK prototype's first row duplicated
canonical Track 1, and a real 256-entry function-pointer table regressed); the
PEXT mask alternate remains an x86-only research row. The expanded parse G rows
and the full `N-direct / NoGo` gate mean this section is no longer allowed to
claim the cost-model axis is orthogonal. It must also carry materialization-plan,
event-cursor, primitive, and capacity-policy probes before a SOTA-BEAT claim is
FAITHFUL.

The probes verify that the canonical typed-event + alt-dispatch plan is not
dominated by named alternates within the implementation envelope. They cannot
establish that no plan elsewhere in the cost-model space would be faster; a
probe win emits `BBNF-COST-PLAN-NOT-CANONICAL` and routes to H.W3/H.W4 rather
than being folded into a vague tuning bucket.

Three alternates, with the dispatch-table row INVALID post-iteration and
the PEXT row unimplemented in the skinny:

| Alternate | Mechanism | Status (post-iteration) | Question answered |
|---|---|---|---|
| `alternate_scalar_plan` | Pure scalar recursive descent; no SIMD scan; byte-by-byte alt dispatch. | Active; passes confirmatory (canonical wins by 38-52% across three corpora per `skinny/RESULTS.md`). | Confirms SIMD adds value on JSON. |
| `alternate_dispatch_table_plan` | SIMD scan; alt dispatch via a 256-entry direct table instead of match-arm. | **INVALID** per `skinny/REDRESS.md` item 17 — the SK prototype's first row duplicated canonical Track 1 (probe was a no-op duplicate); a real 256-entry function-pointer table was then implemented and regressed against canonical. Canonical Rust `match` is the load-bearing dispatch; LLVM owns the branch-table lowering for byte-disjoint alts. | (No question remains open at the skinny scope.) |
| `alternate_pext_mask_plan` | x86_64 only; uses BMI2 PEXT to extract structural-bit masks instead of structural index of offsets. | Not implemented in the skinny; reported as `missing` in `skinny/RESULTS.md`. Deferred to V1 H.W2 — the skinny is silent on this axis. | (Would test a different cost-model selection on Intel; not a skinny conclusion.) |
| `alternate_event_cursor_plan` | Generated parser consumes `Tape::offsets` through a typed cursor instead of source-byte `cursor` + `skip_ws`/`peek`. | Required for SK-V3. Fresh samply profiles show `parse_value_at` dominates `random` and `unicode_escapes`; this row must exist before the expanded gate can close. | Tests whether the current G rows are codegen/substrate-consumption overhead rather than primitive limits. |
| `alternate_capacity_plan` | Capacity policy variants for offset and sparse-flag vectors: sampled capacity, exact prepass, one-shot SIMD pre-scan, and grow-only geometric reserve. | Plan D has landed: `Vec::with_capacity(256)` plus geometric grow is the production default, while sampled/exact/one-shot pre-scan routes remain rejected-route probes. Keep the row for regression and for re-examination after event-cursor/codegen changes; do not treat capacity as the current SOTA blocker. | Tests whether builder allocation policy has become a hidden SOTA gap after a substrate/codegen change. |
| `alternate_primitive_kernel_plan` | Scalar/SWAR/NEON/x86 kernel selection for string, Unicode, whitespace, digit, and byte-class primitives. | Required for SK-V3; kernel choice is grammar-neutral and belongs in `bbnf-simd`/`parse-that`, not generated JSON code. | Tests whether the current plan overfits to one JSON hot path and misses other grammar token classes. |

Probe verdicts:

- **Confirmatory pass:** `canonical ≤ alternate_scalar_plan`. The canonical
  plan is not dominated by the scalar alternate within the implementation
  envelope; cost-driven-rewrites cut is FAITHFUL on the scalar axis. The
  dispatch-table axis has no skinny-side question post-invalidation; the
  PEXT axis defers to V1 H.W2.
- **Invalid probe:** any alternate-plan row that calls the canonical parser, or
  cannot prove a distinct implementation, reports `INVALID` and is excluded
  from MASKING / FAITHFUL classification. The `alternate_dispatch_table_plan`
  row is recorded as `INVALID duplicate-probe disabled; real function-pointer
  table regressed` in `skinny/RESULTS.md` and the gate honours that label
  without re-classifying.
- **Cost-model masking signal:** the scalar alternate ≤ canonical × 0.95
  on any corpus would fire MASKING for the scalar axis and route a
  cost-model recovery lever to V1 H.W2 / H.W3. The current measurement
  does not fire this signal.

The probes are **not** e-graph outputs and do not validate the cost-model
machinery itself; they validate that the canonical plan is not
self-defeating against the alternate that the skinny retains an active
implementation for. Calling the probes "adversarial" overstates them;
calling them "confirmatory only" is honest after the dispatch-table
invalidation.

#### 7.8.3 Cold-cache first-parse probe (per-corpus)

`json/probes/<corpus>/cold_first_parse` measures parse latency on a cold
L1 + L2 + L3 cache. Implementation: each iteration uses
`criterion::Bencher::iter_custom` with explicit cache eviction between
iterations. The eviction primitive is platform-specific:

- **aarch64 (M1 Pro reference)**: a `dc civac` ("data cache clean and
  invalidate by virtual address to point of coherency") loop walks the
  corpus bytes plus the parser's hot-data regions (tape buffer, structural
  index buffer) at 64-byte cache-line granularity, followed by a `dsb ish`
  data-synchronisation barrier and `isb` instruction-synchronisation
  barrier. As a portable fallback, `libc::__clear_cache(start, end)` over
  the same ranges combined with allocating + touching a buffer ≥ L3 cache
  size induces L1 + L2 + L3 pressure.
- **x86_64**: a `_mm_clflush` (or `_mm_clflushopt`) loop walks the corpus
  bytes and parser hot-data regions at 64-byte stride
  (`for addr in (start..end).step_by(64) { _mm_clflush(addr) }`), followed
  by `_mm_mfence` to retire the flushes before the timed region begins.

Qualifier: TLB and branch-predictor state are not cooled by this probe;
the cold / warm ratio reported here is the dCache + iCache delta only.
The cold cost of TLB fills and branch-predictor priming flows into the
warm baseline once the dispatch table has been touched a few thousand
times during criterion's warmup phase, so the reported ratio under-counts
true first-request latency. The metadata field `cold_cache_mode` records which
primitive ran (`aarch64_dc_civac`, `aarch64_clear_cache_fallback`,
`x86_64_clflush_stride64`, or `x86_64_clflushopt_stride64`) so the row's
disposition is reproducible across platforms.

Question: how much does warm-cache benchmarking flatter the result? Many
production workloads (web servers, CLI tools, batch ingestion) parse one
document per request — first-byte latency matters, not steady-state.

Threshold: `cold_first_parse_us ≤ track1_generated_us × 2.0` per corpus.
Cold parses up to 2× warm parse is expected (instruction cache misses,
branch predictor unprimed, cold dispatch table). > 2× indicates a
substrate that performs only when warm; documented as a substrate
sensitivity finding routed to V1 J.W1 close gate. < 1.2× is the
inconclusive band: either the dCache + iCache eviction did not actually
cool the relevant lines on the host CPU model, or the workload is
instruction-cache-dominated and the natural cold / warm ratio is
legitimately near 1.0×. The gate emits an `INCONCLUSIVE` disposition on
the cold-cache row in this case (distinct from `PASS`); the row's
pass/inconclusive disposition is reproducible from the recorded
`cold_cache_mode` metadata.

This probe is *report-only*; it does not gate the matrix. The skinny's
SOTA premise is the warm-cache contest sonic-rs and simd-json compete in.
Cold-cache is recorded for V1 J.W1 to consume.

### 7.9 Correctness gates (Lock 9 + JSONTestSuite conformance + UTF-8 validation)

The 2026-05-12 corpus expansion + asm-string-unicode + skinny-expanded agents surfaced two correctness gaps that must be closed before any SOTA-BEAT throughput claim is honest:

**Gate 1 — UTF-8 validation at scan stage, not view time**. The current skinny binary panics on `i_string_invalid_utf-8.json`, `i_string_overlong_sequence_2_bytes.json`, `i_string_truncated-utf-8.json`, `i_string_iso_latin_1.json` (raw 0xE9) because `view.rs:203, 229` does `std::str::from_utf8(...).expect("parser input is UTF-8")` while the scan emits no UTF-8 validation pass. The fix lands at scan stage inside the `bbnf-simd` boundary via the `simdutf8` crate (Keiser-Lemire 2020 "Validating UTF-8 In Less Than One Instruction Per Byte"; admissible per Lock 16 algorithm-class citation). Per `skinny/profile/skinny-expanded/PROFILE-REPORT.md`, UTF-8 validation is **0.00% self-time** on every current corpus — moving it to scan stage costs nothing measurable but closes the correctness gap. Per `skinny/profile/simdjson-expanded/PROFILE-REPORT.md`, simdjson's scan-time UTF-8 validation costs 0-35% depending on multibyte density; on pure-ASCII corpora the validator's overhead is ≤ 2% of total cycles.

Verification: `cargo run --release --bin parity_oracle -- jsontestsuite/test_parsing/` must exit 0 with `i_string_*` files producing `BBNF-UTF8-INVALID-AT-PARSE` diagnostic (not panic).

**Gate 2 — Non-character codepoints admit per RFC 8259**. The current skinny `parse-that-regex/src/lib.rs:352` uses `char::from_u32` which rejects non-characters (`U+FDD0..U+FDEF`, `U+nFFFE`, `U+nFFFF` for `n` in `0..=0x10`) and surrogate-pair-only codepoints. JSONTestSuite marks `y_string_unicode_U+10FFFE_nonchar.json` as `y_` (must accept). The fix is `char::from_u32` → manual codepoint construction admitting non-characters; emits `BBNF-UNICODE-NONCHAR-CODEPOINT` as a warning (not error) per RFC 8259.

Verification: `y_string_unicode*.json` files at `/tmp/jsontestsuite-research/JSONTestSuite/test_parsing/` must parse-OK + view-as-`&str`-or-`Cow` OK + emit zero errors (warnings admissible).

**Gate 3 — Surrogate-pair handling correctness**. Per skinny's current `parse-that-regex/src/lib.rs:324-347` strict surrogate-pair handling is correct and matches simdjson/yyjson semantics; verified by JSONTestSuite `y_string_accepted_surrogate_pair.json` (`["𐐷"]` → U+10437 𐐷) decoding correctly. Per the simdjson-expanded profile, surrogate-pair decode (`handle_unicode_codepoint`) is 0% on every real-world corpus — only fires on synthesized escape-heavy corpora. **No change required; keep strict.**

**Gate 4 — Float-bit-exact parity** (for canada, numbers, mesh, marine_ik corpora): `JsonNumber::as_f64()` must produce bit-exact match against sonic-rs (Eisel-Lemire) and serde_json (lexical-core) on every canonical number-bearing corpus. Last-bit ULP discrepancy is a hard parse-time failure under skinny. Current skinny uses `std::str::parse::<f64>` which is correct for normal IEEE-754 but loses ULP precision on subnormals; spec carries this gate to enforce the Eisel-Lemire wrap landing per `parse-that/float/eisel_lemire.rs` (wraps `fast-float2` crate per Lock 11 + Lock 16 algorithm-class citation).

**Conformance bundle test** lives at `skinny/crates/test-fixtures/corpus/jsontestsuite_y_pack/` (95 files; ~140 KB total); `cargo run -p xtask -- check-conformance` exits 0 only when all 95 parse-OK.

### 7.10 Comparative-profile primitive (samply hot-leaf + cycle-per-byte attribution)

Every full bench run produces a comparative samply profile against sonic-rs and simdjson on the same three corpora; the output lives at `skinny/profile/skinny-v{N}/`, `skinny/profile/sonic-rs-v{N}/`, `skinny/profile/simdjson-v{N}/`. This primitive is load-bearing post-2026-05-12: the six-agent comparative cohort showed that **wall-clock throughput alone does not distinguish substrate ceiling from codegen template shape**. The hot-leaf-count gate at §6 outcome class `G-fusion-quality` requires comparator-anchored measurement.

**Methodology (load-bearing; reproducible)**:

- samply ≥ 0.13 with `--save-only --unstable-presymbolicate -o <file>.json.gz`.
- 1 kHz sampling rate; ≥ 30 s of bench-loop CPU per corpus.
- `[profile.release]` carries `debug = true` per `feedback_samply_symbols` (Lock 15).
- Symbol resolution via per-lib `symbol_table` (rva, size) binary-search; do NOT trust funcTable.name strings (they mix inline-frame caller attribution).
- Rust demangling via `rustfilt`; C++ via `c++filt` + `addr2line`.
- Comparators built with two profiles each: **inlined** (canonical wall-clock) and **noinline** (leaf-level technique attribution). The inlined build classifies fusion quality; the noinline build classifies per-primitive cycle budget.

**Required artefact set per bench run**:

| Path | Content | Purpose |
|---|---|---|
| `skinny/profile/skinny-v{N}/{corpus}.profile.json.gz` + `.syms.json` | Skinny samply profile per corpus | Self-attribution |
| `skinny/profile/sonic-rs-v{N}/{corpus}.{inlined,noinline}.profile.json.gz` + `.syms.json` | sonic-rs samply profile per corpus, both builds | Comparator anchor; per-technique cycle budget |
| `skinny/profile/simdjson-v{N}/{corpus}.{inlined,noinline}.profile.json.gz` + `.syms.json` | simdjson samply profile per corpus, both builds | Stage1/stage2 architectural-shape verification |
| `skinny/profile/skinny-v{N}/PROFILE-REPORT.md` | Skinny report with sections (a)-(f) | Self-classification per §7.10 contract below |
| `skinny/profile/sonic-rs-v{N}/PROFILE-REPORT.md` + `noinline.patch` | sonic-rs report + the patch flipping `#[inline(always)]` → `#[inline(never)]` on the SIMD inner kernel | Reproducible noinline build |
| `skinny/profile/simdjson-v{N}/PROFILE-REPORT.md` | simdjson report with stage1/stage2 sub-decomposition | Architectural-shape verification |
| `skinny/profile/COMPARISON-v{N}.md` | Cross-parser hot-leaf table + cycle-per-byte table | Gate evaluation |

**Required report sections per PROFILE-REPORT.md**:

(a) Per-corpus top 15 hot functions by self-time.
(b) Per-corpus top 15 hot functions by inclusive-time.
(c) Function-class attribution. Classes: `parse_driver` / `structural_scan_simd` / `whitespace_skip_simd` / `string_simd` / `number_simd` / `utf8_validation` / `tape_assembly` / `view_materialization` / `allocation` / `memmove_memcmp` / `other`. Percentages per class per corpus.
(d) Hot-leaf count metric: count of distinct symbols at ≥ 10% self-time per corpus. **Comparator anchors: sonic-rs = 1, simdjson = 2**. Skinny gate: ≤ 3 hot leaves at ≥10% on twitter (SOTA-BEAT validation); ≤ 4 at ≥10% (Phase 1 validation only).
(e) Methodology section: samply version, sampling rate, iter counts per corpus, build flags. Reproducible.
(f) Per-corpus single-sentence honest take.

**Cycle-per-byte gate (comparator-anchored)**:

Wall-clock Mbps depends on host clock speed and drifts across dev hardware (M1 Pro vs M5 Max vs Zen 4 vs Intel). The cycle-per-byte (c/B) metric is host-clock-invariant: `c/B = ns_per_byte × clock_freq_GHz`. Comparator anchors on twitter (verified at `skinny/profile/simdjson-v2/PROFILE-REPORT.md` §(d) + `skinny/profile/sonic-rs-v2/PROFILE-REPORT.md` §(f)):

- simdjson: 1.142 c/B total (stage1 0.629 + stage2 0.377 + outlined 0.131); **floor at simdjson's algorithm ≈ 0.4-0.5 c/B**.
- sonic-rs LazyValue path (the 18552 Mbps reference): ~1.5 c/B inferred from 2.32 GB/s ÷ 4 GHz Apple-Silicon mean clock.
- Skinny final gate (twitter Track 1 21552 Mbps): ~1.30 c/B inferred from ~2.57 GiB/s ÷ 3.5 GHz.

| Gate | Twitter c/B | Comparator interpretation |
|---|---|---|
| Phase 1 validation | ≤ 1.9 c/B | within 1.65× simdjson floor |
| Phase 2 SOTA-BEAT sonic-rs | ≤ 1.4 c/B | within 1.22× simdjson floor |
| Phase 3 SOTA-BEAT simdjson on x86_64 AVX-512 | ≤ 0.9 c/B | below simdjson floor on platform with VBMI2 |
| Phase 4 asmjson-class | ≤ 0.45 c/B | parity with asmjson 10.93 GiB/s on Zen 4 |

The cycle-per-byte gate fires alongside the Mbps gate and the hot-leaf-count gate. A passing Mbps gate with a failing c/B gate (which can happen on faster hardware) is a *valid* result for the host environment but does **not** discharge the architectural target — the parser carries structural inefficiency masked by raw clock speed. A passing c/B gate with a failing Mbps gate suggests host hardware below the published-benchmark host class; document and continue.

**Per-host disposition**:

The skinny gates run on **arm64 Apple Silicon as the primary host** (M-series; M1 Pro is the README-pinned target, but M3/M4/M5 hosts produce comparable c/B on this architecture). The x86_64 measurements run on the secondary CI host when the AVX-512 VBMI2 path is implemented (Phase 3); the AVX-2 fallback runs on any commodity x86_64 host. SWAR scalar fallback (`bbnf-simd/scalar/`) ships parity tests but is not on the gate path.

---

## §8 CI integration

The bench runs in CI as a nightly job (not on every PR; bench noise on
non-bare-metal CI runners makes per-PR gating impractical). Local runs are
the gold standard; CI runs the regression detection.

### 8.1 The bench command

```sh
RUSTFLAGS="-C target-cpu=native" \
  cargo bench --bench json_parity --bench simd_scan -p bbnf-bench -- \
  --output-format=bencher \
  | tee bench-output.txt
```

`--output-format=bencher` emits libtest-style `test ... bench: N ns/iter (+/- M)`
lines. The same run also writes the criterion JSON report and the TOML
metadata mirror, which is what the gate actually consumes.

`-p bbnf-bench` keeps the bench dispatch scoped; cargo bench in a workspace
without that flag triggers every crate that has a `[[bench]]` entry. We
have only one, but the discipline matters for future skinny additions.

#### 8.1.1 PGO disclosure

The skinny build does not use profile-guided optimisation. Both bbnf and
the competitor crates (sonic-rs, simd-json, serde_json) compile from
source through cargo with `-C target-cpu=native` and `lto = "thin"`.
PGO is not on the build chain, but neither is it on the competitors'
build chain in this run — this is a fair head-to-head of out-of-the-box
release builds.

What it is *not*: the skinny does not measure what `cargo install
sonic-rs` produces from a published wheel that may have been PGO-tuned
at upstream release time. If the V1 plan adopts PGO at J.W1 publication,
the SOTA rows must re-run with PGO on both sides — `RUSTFLAGS="-Cprofile-use=
... -C target-cpu=native"` against a `cargo pgo run` flow. Out of skinny
scope; recorded in metadata as `pgo_mode: "none"`.

### 8.2 Post-bench gate

`crates/bbnf-bench/src/bin/gate.rs`:

```rust
fn main() -> Result<(), Box<dyn Error>> {
    let advisory = args.contains("--advisory");     // CI-only throughput advisory
    let rows = collect_rows("target/criterion")?;   // reads JSON + metadata.toml
    let outcomes = classify_all(&rows, &threshold_matrix()); // §6 + workloads
    let worst = worst_outcome(outcomes.iter().copied());
    let hard_failure = outcomes.iter().copied().find(|outcome| {
        matches!(outcome, IParityOracleFail | JSchemaFail | KSimdParityHashFail)
    });
    write_results_md(&rows, worst)?;                // RESULTS.md content (§10)
    let exit_outcome = if advisory { hard_failure } else { worst };
    if let Some(outcome) = exit_outcome {
        std::process::exit(exit_code_for(outcome.verdict()));
    }
    Ok(())
}
```

Exit codes:

- 0: GO (CI passes, dispatch authorised).
- 2: schema enforcement failed (re-run required).
- 5: NO-GO. In local mode this includes substrate, direct, memory, parity, and
  SIMD-parity failures. In advisory CI mode, only parity and SIMD-parity
  correctness failures still exit 5; throughput/memory NO-GO rows render but
  do not fail CI.
- 6: CONDITIONAL (manual amendment required before dispatch).

CI green requires exit 0. Conditional outcomes are intentionally non-green:
they produce RESULTS.md and a precise action, but they do not authorize skinny
dispatch.

### 8.3 CI bench is advisory non-gating; local bench is authoritative

CI runners are typically slower than local M1 Pro by 1.3-1.7× depending on
runner generation and contention. Rather than maintain a per-runner discount
table that the gate must read and apply, the skinny adopts a simpler
discipline: **CI bench is advisory non-gating; the local bench on the
reference platform is authoritative**. CI exists to detect regressions in
the harness itself (build green, fixtures load, the matrix evaluator runs
without panic, RESULTS.md renders) and to surface gross throughput
collapses; it does NOT decide tranche dispatch. Any NO-GO requires a local
re-run on the reference M1 Pro before the verdict is final, and any GO
requires a corresponding local-bench artefact in the audit trail.

CI is configured to report classification on its own measurements but to
exit success regardless of throughput verdict (parity / schema / SIMD-hash
correctness gates still hard-fail in CI). Local runs hard-fail on NO-GO
per §8.2 exit codes. This removes ~50 LOC of per-runner discount apparatus
from `gate.rs` and keeps the authoritative gate co-located with the
reference platform where the threshold matrix was calibrated.

### 8.4 GitHub Actions workflow sketch

`.github/workflows/skinny-bench.yml`:

```yaml
name: skinny-bench
on:
  schedule: [{ cron: "0 6 * * *" }]
  workflow_dispatch:
jobs:
  bench:
    runs-on: macos-14
    timeout-minutes: 30
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - run: |
          RUSTFLAGS="-C target-cpu=native" \
            cargo bench --bench json_parity --bench simd_scan -p bbnf-bench
      - run: cargo run -p bbnf-bench --bin gate -- --advisory  # CI is advisory non-gating per §8.3; correctness gates still hard-fail
      - uses: actions/upload-artifact@v4
        with:
          name: criterion-report
          path: target/criterion/
      - uses: actions/upload-artifact@v4
        with:
          name: results
          path: skinny/RESULTS.md
```

The workflow is named `skinny-bench` not `bench` so it does not collide
with the V1 bench workflow that the J tranche stands up. After the skinny
graduates, this workflow renames or absorbs into J.W1.

---

## §9 What this skinny bench omits

The skinny intentionally does NOT measure several axes that V1 does. Each
omission has an explicit impact statement on the SOTA-viability conclusion.

### 9.1 No CSS gates — but a CSS prior probe (substrate-only)

Full lightning-css comparison defers to V1 H.W4 (the H tranche owns the
CSS SOTA close). The skinny does not run codegen for CSS L4. However,
there is a **CSS prior probe** that is cheap to add and substantially
sharpens the SOTA-viability conclusion:

`json/probes/css_prior/bootstrap` parses `bootstrap.css` (canonical
~143 KB CSS3 fixture used by lightning-css) using a hand-coded
substrate-only walker — no codegen, no full BBNF compile, no Grammar IR.
The walker uses the *same* `runtime::tape` + `bbnf-simd` substrate the
JSON skinny uses. Question: does the substrate generalise to a non-JSON
structurally-different grammar without degrading throughput by an order
of magnitude?

Threshold: walker parse time ≤ lightning-css × 1.5 on bootstrap.css. The
threshold is generous because the walker is hand-rolled CSS without any
of the cost-model / Pratt / SIMD-recogniser tuning the V1 H.W4 gate will
apply. Pass at 1.5× indicates the substrate generalises; 2× to 5× indicates
substrate has hidden costs on non-JSON shapes; > 5× indicates the
substrate is JSON-shaped, not language-shaped.

Probe scope: handwritten CSS walker ≤ 600 LOC inside
`crates/bbnf-bench/src/track2/css_prior.rs`, gated by the same §10.6
substrate-API correspondence checklist. Probe is *report-only* — it
does not gate the matrix — but RESULTS records the multiplier and the
SOTA-beat-on-CSS probability update. A skinny that hits JSON outcome A
but degrades 3× on CSS bootstrap should *lower* the V1 SOTA-beat
probability for the CSS row, not assume JSON parity transfers.

Defer if implementation budget excludes this probe; record explicit
"deferred to V1 H.W4 entry gate" in RESULTS rather than assuming
generality.

### 9.2 No incremental parsing bench

Defers to V1 I tranche. Impact: the skinny measures cold parse only. The
substrate's incremental edit performance is a different question with a
different competitor set (tree-sitter is the leader there, not sonic-rs).
Skinny outcomes do not predict incremental performance; the I tranche has
its own bench gates.

### 9.3 Skinny-local LOC gates only

The skinny gates the local scope it can honestly measure: generated JSON
runtime output must stay ≤ 4,000 LOC and the Track 2 handwritten probe must
stay inside `bbnf-bench` under the substrate-API correspondence checklist.
The `bbnf-bench` aggregate cap is 3,300 LOC per WORKSPACE.md. The nine-grammar generated-LOC ceiling
defers to F.W3. Impact: a JSON pass does not prove the full V1 generated LOC
budget, but it does prevent the skinny from hiding codegen verbosity in the
one generated grammar it actually emits.

### 9.4 No WASM bench

Defers to V2 alongside the V2 `WasmBackend: Backend` impl per Lock 5
amendment. Impact: the skinny does not validate the WASM lower path.
The Rust line is the V1 SOTA close path per `restart/locks/14-LOCKS.md:48`
amendment; WASM SOTA is post-V1.

### 9.5 No Pratt / full recogniser auto-detection bench

The skinny grammar (JSON) has no operator chain — Pratt does not apply.
The structural-scan microbench (§4) covers the SIMD recogniser cost on the
JSON structural alphabet, which is the relevant SIMD application for JSON.
The auto-detection cost-model bench (decide whether to emit Pratt or SIMD
for a candidate rule) defers to H.W2. Impact: the skinny does not validate
the cost-model dispatch logic; that is a V1 H tranche question. JSON's hot
path uses SIMD scan unconditionally on x86_64 / aarch64 in skinny;
auto-detection is bypassed by hardcoding the strategy in the skinny
codegen path. The alternate-plan probes (§7.8.2) bound only the JSON cost-plan
cut on the axes the skinny retains an active implementation for (scalar);
they do not validate the full V1 recognizer miner.

### 9.6 Memory residency floor (now a hard gate; outcome M)

Recovered from prior "instrumented but report-only" status. The harness
records `getrusage(RUSAGE_SELF).ru_maxrss` per bench invocation
(`peak_rss_bytes` field in §5.1) and **gates** at outcome M: peak RSS on
canada (the largest-input row) > 3 × the fastest competitor's peak RSS
is NO-GO. Rationale: a substrate that hits SOTA-class throughput at 3×
memory is not viable for concurrent-parse workloads (web servers, batch
ingestion). 3× is a generous floor — sonic-rs's lazy materialisation
deliberately trades parse time for memory, and a tape + direct substrate
should land within 2× sonic-rs at the architectural shape, with 3× as a
soft floor.

Forward-projection (post-iteration, now empirically computable from
`skinny/RESULTS.md` materialisation rows): canada tape lands at ~2.68 MB
logical / 3.57 MB allocated (167,196 tokens × 16 bytes/token plus
private-`Vec` slack); the typed root at ~3-5 MB; payload arena writes
and allocations are zero on the hot path. Total skinny canada peak ~5.7-8.6
MB versus sonic-rs canada peak ~5-7 MB (community-anchored lazy
materialisation) — the substrate operates at ~1.1-1.4× sonic-rs canada
peak. The 3× outcome M floor leaves ~2× headroom against the measured
substrate profile; the gate is a safety net for substrate-shape drift,
not a primary close-row gate.

V1 J.W1 retains the strict gate (≤ 1.5× competitor); skinny gates at the
generous 3× floor as a substrate viability signal, not a closing-row gate.
This is the redress of the prior "memory is not skinny's question"
defer — substrate viability *is* skinny's question, and 10×-memory
substrate is not viable.

### 9.7 No multi-core / parallel parse bench

NDJSON multithreaded (simdjson 28000 Mbps) is not a V1 target. Skinny
single-threaded only. No omission impact: V1 is single-threaded parse.

### 9.8 No diagnostic / error-path bench

Error recovery (`BBNF-RECOVERY*` per PASS-3 §6b) defers to tranche I.
Impact: skinny benches valid input only. Error-path performance
characteristics are a separate concern.

### 9.9 No path / select API bench

`path!` / `select!` macro performance defers to G tranche. Impact:
skinny measures parse-to-typed-root. The path query API is V1 G-owned.

---

## §10 Verdict-writing template — `skinny/RESULTS.md`

After the bench runs, the gate (§8.2) renders a results document. This
document is the single arbitration record the user reads to decide
"dispatch tranches A-J or not."

### 10.1 Single-sentence verdict

The first line of RESULTS.md is the verdict sentence. Examples:

- `verdict: substrate viable, codegen overhead at 1.08x — full V1 dispatch authorised (outcome B)`
- `verdict: substrate parity, codegen gap at 1.34x — dispatch with codegen focus, +1 wave to F (outcome D)`
- `verdict: substrate gap at 1.18x sonic-rs — NO-GO, redesign substrate before tranche B (outcome G)`
- `verdict: parity oracle failure on canada.json — NO-GO, codegen correctness bug, fix before any further bench claim (outcome I)`

The sentence is the gate's classify-and-render output. It is not editorial.

### 10.2 RESULTS.md shape

```markdown
# Skinny Bench Results

verdict: <single-sentence outcome>

## Outcome classification

Outcome ID: <A|B|C|D|E|F-positive|F-noise|F-codegen-gap|G|I|J|K|L|M|N-direct>

Per-corpus outcomes:
- twitter: <ID>
- citm: <ID>
- canada: <ID>
- structural_scan: <pass|fail with Mbps>

Overall: worst-case across corpora = <ID>.

## Reproducibility metadata

| Field | Value |
|---|---|
| Run timestamp | <UTC> |
| CPU | <model + arch> |
| OS | <uname -a> |
| RUSTFLAGS | <value> |
| Profile | release |
| bbnf commit | <sha> |
| sonic-rs version | <pinned> |
| simd-json version | <pinned> |
| serde_json version | <pinned> |

## Per-corpus measurements

### twitter.json (size <bytes>, sha256 <digest>)

| Bench | Median (us) | 95% CI | Mbps | vs sonic-rs |
|---|---|---|---:|---:|
| Track 1 (generated) | <m> | [<lo>, <hi>] | <mbps> | <ratio>x |
| Track 2 (hand-coded) | <m> | [<lo>, <hi>] | <mbps> | <ratio>x |
| sonic-rs anchor | <m> | [<lo>, <hi>] | <mbps> | 1.00x |
| sonic-rs checked | <m> | [<lo>, <hi>] | <mbps> | <ratio>x |
| simd-json borrowed | <m> | [<lo>, <hi>] | <mbps> | <ratio>x |
| simd-json owned | <m> | [<lo>, <hi>] | <mbps> | <ratio>x |
| serde_json | <m> | [<lo>, <hi>] | <mbps> | <ratio>x |

Codegen overhead (T1/T2): <ratio>x
Substrate ceiling (T2/S): <ratio>x
Payload arena writes/allocations: Track 1 <w>/<a>, Track 2 <w>/<a> (must all be 0)
Tape materialization: Track 1 <tokens> tokens, <logical_bytes> logical tape bytes (<x>x input), <allocated_bytes> allocated tape bytes (<y>x input), <payload_bytes> payload bytes; pairs <n>, opens <n>, closes <n>, scalars <n>, sibling-skips <n>
Beat target (≤ 380 µs): <met|missed>
Parity floor (≤ 480 µs): <met|missed>

Outcome: <ID>

### citm_catalog.json (size <bytes>, sha256 <digest>)
[same schema]

### canada.json (size <bytes>, sha256 <digest>)
[same schema]

### simd/structural_scan (canada.json, sha256 <digest>)

| Bench | Mbps | Floor | Pass |
|---|---|---|---|
| simd | <mbps> | 40000 (NEON) / 56000 (AVX2) | <yes|no> |
| scalar | <mbps> | (parity) | <yes|no> |
| Parity hash match | <yes|no> | required | <pass|fail> |

## Masking probes

### Host-call probes (per §7.8.1)

| Probe | Result | Threshold | Signal |
|---|---:|---|---|
| host_call_dispatch_overhead (ns/call) | <ns> | ≤ 50 ns/call | <PASS \| MASKING> |
| host_call_eager_decode twitter | <ratio>x vs T1 | > 1.15× T1 fires MASKING | <reported \| MASKING> |
| host_call_eager_decode citm | <ratio>x vs T1 | > 1.08× T1 fires MASKING | <reported \| MASKING> |
| host_call_eager_decode canada | <ratio>x vs T1 | > 1.02× T1 fires MASKING | <reported \| MASKING> |

### Alternate-plan probes — confirmatory only (per §7.8.2)

| Probe | Corpus | Result | Disposition |
|---|---|---:|---|
| alternate_scalar_plan | <corpus> | <ratio>x vs canonical | canonical ≤ alternate (SIMD adds value) |
| alternate_dispatch_table_plan | <corpus> | INVALID | INVALID per `skinny/REDRESS.md` item 17 — duplicate-probe disabled; real function-pointer table regressed; canonical Rust `match` is the load-bearing dispatch |
| alternate_pext_mask_plan (x86_64 only) | <corpus> | <missing \| ratio>x vs canonical | missing in skinny; V1 H.W2 owns |

### Cold-cache probe (report-only; per §7.8.3)

| Probe | Corpus | Cold (us) | Warm (us) | Cold/Warm | Disposition |
|---|---|---:|---:|---:|---|
| cold_first_parse | twitter | <us> | <us> | <ratio>x | <PASS \| INCONCLUSIVE \| FAIL> (< 1.2× INCONCLUSIVE; 1.2-2.0× PASS; > 2.0× FAIL) |
| cold_first_parse | citm | <us> | <us> | <ratio>x | <PASS \| INCONCLUSIVE \| FAIL> |
| cold_first_parse | canada | <us> | <us> | <ratio>x | <PASS \| INCONCLUSIVE \| FAIL> |

### CSS prior probe (per §9.1; report-only if implemented)

| Probe | Result | Threshold | Pass |
|---|---:|---|---|
| css_prior bootstrap.css | <ms> | ≤ lightning-css × 1.5 | <yes|no|deferred> |

## Action

<gate-rendered action sentence per the matrix row>

## Probabilities update

Prior to skinny: SOTA-beat probability <P_pre>%, V1-parity probability <Q_pre>%.
Post-skinny: SOTA-beat probability <P_post>%, V1-parity probability <Q_post>%.

The probabilities are updated per §10.3 mapping below; they are not
free-floating estimates.
```

### 10.3 Probability-update mapping

Outcome → (V1-parity probability, SOTA-beat probability) update relative to
the pre-skinny prior. The pre-skinny prior is the user's V9.1-hardening
READY-verdict default; the skinny is the evidence event.

| Outcome | V1-parity P(post) | SOTA-beat P(post) | Notes |
|---|---|---|---|
| A | ≥ 0.95 | ≥ 0.70 | Meets README spec target + tight codegen |
| B | ≥ 0.95 | 0.50-0.70 | Beats S × 0.95 (may miss README target) + acceptable codegen |
| C | ≥ 0.85 | 0.30-0.50 | Parity substrate + acceptable codegen |
| D | 0.70-0.85 | 0.20-0.40 | Codegen gap; F absorbs |
| E | 0.50-0.70 | 0.10-0.30 | Conditional; F held |
| F-positive | 0.50-0.70 | < 0.30 | Substrate borderline-weak; codegen positive (generator competitive with hand) |
| F-noise | 0.40-0.60 | < 0.30 | Substrate borderline-weak; codegen indistinguishable from hand within Track 1 95% CI overlap of Track 2 × 1.05 |
| F-codegen-gap | 0.35-0.55 | < 0.25 | Substrate borderline-weak; codegen widens the gap (Track 1 > Track 2 × 1.10); requires +1 wave to F alongside the substrate +1 wave to H.W1 |
| G | < 0.30 | < 0.05 | Substrate failure (collapsed prior G + H) |
| I | n/a | n/a | Correctness fail; rerun |
| J | n/a | n/a | Schema fail; rerun |
| K | n/a | n/a | Correctness fail; rerun |
| L | < 0.30 | < 0.05 | SIMD floor failed on canada; re-run after substrate fix |
| M | < 0.30 | < 0.05 | Peak RSS > 3× competitor; substrate not viable for concurrent-parse |
| N-direct | < 0.50 for direct-only APIs | < 0.05 | Typed emission misses sonic-rs direct; require `SinkOnly` generated field writes before SOTA-BEAT dispatch |

The user can re-anchor the prior elsewhere, but the skinny supplies the
evidence in a form that the prior consumes.

Note (post-iteration): a prior version of this section carried a row for
"`alternate_pext_mask_plan` < canonical × 0.90 on x86_64 → cross-platform
plan divergence". The alternate-plan probes at §7.8.2 do not currently
bound cross-platform plan divergence: the dispatch-table candidate is
INVALID per `skinny/REDRESS.md` item 17, and the PEXT-mask candidate is
unimplemented in the skinny (V1 H.W2 owns). The skinny is silent on
cross-platform plan divergence; no probability-mapping row applies, and
the residual question routes to V1 H.W2 input rather than a skinny
probability update.

### 10.4 Where RESULTS.md lives

`skinny/RESULTS.md` in the prototype workspace. The gate writes (overwrites)
it on every run. The historical results live in `skinny/target/criterion/`
archived per-run; the canonical RESULTS.md is the latest. The spec authority
remains under `restart/skinny/`.

### 10.5 Reading order

The user reads RESULTS.md in this order:

1. Verdict sentence (line 1).
2. Outcome ID + per-corpus outcome breakdown.
3. Reproducibility metadata (sanity check the run conditions).
4. Per-corpus tables (the data).
5. Action sentence (what to do).
6. Probabilities (what to believe about V1 readiness).

The gate writes them in that order so the user does not have to scroll past
data to reach the verdict.

### 10.6 Track 2 review checklist (sits at the end of RESULTS.md as a
permanent-but-static section)

Before any RESULTS.md is honoured, a code-review checklist sits adjacent to
the bench. The reviewer signs that Track 2 used the same substrate as
Track 1; the bench results are gated on the signature. The checklist:

```text
Track 2 substrate-API correspondence checklist
- [ ] Parse/tape Track 2 calls runtime::tape APIs only (no separate buffer
      struct, no shadow tape implementation, no parallel arena).
- [ ] Direct Track 2 calls runtime event/sink APIs only (no retained-view walk
      in timed direct rows, no bench-private substitute for generated Track 1).
- [ ] Track 1 direct rows call generated runtime/codegen `SinkOnly`, not
      `bbnf-bench` private parser code.
- [ ] Track 2 calls `bbnf-simd` byte-class / typed-event APIs (no inline byte loop that
      duplicates structural-scan logic).
- [ ] Track 2 records raw scalar spans and leaves the payload arena empty on
      the JSON hot path, matching the generated parser (zero arena writes
      and zero allocations per the parity oracle).
- [ ] Track 2 returns JsonRoot<'i> from runtime::grammars::json's typed root
      (no parallel typed root).
- [ ] Track 2 calls the same set of substrate APIs the codegen template
      emits. Reviewer cross-references the codegen template (§COMPILER §6.3
      and the emitted runtime/src/grammars/json/) against the Track 2
      source and confirms no Track 2 call has no codegen-emitted analog.
- [ ] Track 2 obeys Lock 13 per-file ≤ 500 LOC (file split is allowed
      across crates/bbnf-bench/src/track2/json/ if needed; total LOC is
      measurement-driven, not capped).
- [ ] Track 2 passes the parity oracle on all three fixtures.
- [ ] Track 2 author signature: <name>, date <YYYY-MM-DD>.
- [ ] Track 1 codegen template author signature: <name>, date <YYYY-MM-DD>.
- [ ] Reviewer signature confirming substrate-API correspondence:
      <name>, date <YYYY-MM-DD>.
```

A RESULTS.md with the checklist unsigned is INVALID for tranche dispatch
purposes. The signatures are the substrate-fairness oracle.

---

## §11 Crate ownership

Per the skinny workspace (defer to `restart/skinny/WORKSPACE.md` for the
canonical list; this section names only the bench-relevant crates):

| Crate | Owner concern | Notes |
|---|---|---|
| `crates/bbnf-bench/` | this document | Criterion harness, fixtures loader, parity oracle, gate binary, metadata writer, Track 2 hand-coded probe |
| `crates/runtime/` (Tape) | SUBSTRATE.md | Shared substrate; bench is an external consumer |
| `crates/runtime/src/grammars/json/` | COMPILER.md | Generated from `grammars/json.bbnf`; emits `parse(&str) -> Result<JsonRoot, ParseError>` |
| `crates/bbnf-simd/` | SUBSTRATE.md | Structural scan dispatcher; bench microbenches it |

`crates/bbnf-bench/` source layout:

```
crates/bbnf-bench/
  Cargo.toml
  src/
    lib.rs
    metadata.rs        # RowMetadata, HostFacts capture
    parity.rs          # cross-track parity oracle
    materialization.rs # lazy-tape materialization report
    probes.rs          # masking/cold/host-call probe manifest
    report.rs          # RESULTS.md renderer
    scan.rs            # scalar/SIMD scan report helpers
    gate.rs            # threshold matrix classifier
    bin/gate.rs        # CI-invoked gate binary
    bin/profile_direct.rs
    direct_struct.rs   # sink-only direct digest proof and parity oracle
    track2/            # handwritten substrate ceiling probe
      json.rs          # ≤ 500 LOC
  benches/
    json_parity.rs
    simd_scan.rs
```

### 11.1 LOC budget for `bbnf-bench`

The skinny LOC ceiling for `crates/bbnf-bench/` is set in WORKSPACE.md.
Indicative per-file budget (the executable `xtask lint-loc` gate enforces the
aggregate `bbnf-bench` cap plus the Track 2 cap; these rows localize pressure
inside the aggregate budget):

- `metadata.rs`: ≤ 450 LOC (schema_version + per-corpus parity + RSS + cold_cache_mode add fields)
- `parity.rs`: ≤ 120 LOC
- `materialization.rs`: ≤ 150 LOC
- `probes.rs`: ≤ 90 LOC
- `report.rs`: ≤ 350 LOC
- `scan.rs`: ≤ 60 LOC
- `lib.rs`: ≤ 20 LOC
- `gate.rs`: ≤ 430 LOC (matrix expansion: F-split, G-collapse, M-add, BEAT_BOUND classifier)
- `bin/gate.rs`: ≤ 580 LOC (renders fastest-anchor `S`, subprocess RSS probes, persisted SIMD parity metadata, masking probes, direct-to-struct workload rows, and CI advisory exit handling)
- `direct_struct.rs`: ≤ 650 LOC (sink-only direct projection proof; retained-view parity oracle; exact Track 1 / Track 2 / serde digest equality, scanner-owned integer classification, and sonic-rs shape parity)
- `bin/profile_direct.rs`: ≤ 100 LOC (focused direct-sink profiling harness for Track 1, Track 2, sonic-rs, and serde rows)
- `track2/json.rs`: ≤ 500 LOC by the executable `xtask lint-loc` gate; Lock 13 split allowed if needed.
- `track2/css_prior.rs` (optional CSS prior probe per §9.1): ≤ 600 LOC, file split allowed.
- `benches/json_parity.rs`: ≤ 430 LOC (probe additions: distinct dispatch if available, eager_decode, pext, cold_first_parse, direct-to-struct timed rows)
- `benches/simd_scan.rs`: ≤ 150 LOC (per-corpus parity)

Total: target ≤ ~3,300 LOC. Track 2's measurement-driven LOC and the
direct-to-struct workload proof are the largest variables; reference-class
hand-coded JSON parsers using substrate APIs land at 800-1,500 LOC. The
earlier 500 LOC ceiling on Track 2 was constraint-driven and risked either a
substrate-API thin parser (substrate becomes covert hand-coded parser) or an
arbitrary budget overrun. The substrate-API correspondence checklist (§10.6)
gates on what Track 2 calls, not how short it is.

---

## §12 Failure modes and mitigations

Five failure modes the harness must resist.

### 12.1 Allocator drift across runs

**Failure:** the user runs the bench once with the system allocator and
once with `mimalloc`; numbers shift by 5-15%; classification drifts across
matrix rows.

**Mitigation:** `crates/bbnf-bench/Cargo.toml` depends on `mimalloc` and the
bench file installs `mimalloc` as the global allocator at startup.
Competitors (sonic-rs / simd-json / serde_json) inherit the global allocator.
Recorded in metadata.

### 12.2 CPU frequency scaling

**Failure:** the M1 Pro thermal-throttles after 5-10 minutes of continuous
bench, dropping numbers by 3-8%; classification flickers between matrix
rows.

**Mitigation:** the bench command pre-runs a 30-second warmup loop on
`canada.json` to bring the CPU into thermal steady state before timed
measurement begins. The warmup is documented but not gated.

### 12.3 RUSTFLAGS drift

**Failure:** local dev runs without `target-cpu=native` (or with an older
target-cpu like `apple-m1` that excludes recent NEON intrinsics); numbers
diverge from the reference run.

**Mitigation:** the gate reads `RUSTFLAGS` from metadata. If `target-cpu`
is anything other than `native` (or the platform-native equivalent
`apple-m1` / `skylake-avx512` / etc.), classification emits a
CONDITIONAL flag.

### 12.4 Criterion noise threshold drift

**Failure:** criterion's default 2% noise threshold is too loose for
sub-µs differences; iteration-to-iteration drift across a build session
re-classifies between matrix rows.

**Mitigation:** `noise_threshold(0.02)` is set explicitly. Borderline rows
(within 5% of a matrix boundary) trigger a CONDITIONAL classification;
the gate emits "result is within noise of <next outcome up>; re-run for
confirmation" rather than committing to either side.

### 12.5 Fixture corruption / source URL rot

**Failure:** `twitter.json` upstream changes (line ending, trailing
newline, encoding); the SHA-256 fails; the bench cannot even load the
fixture.

**Mitigation:** SHA-256 is the binding contract per §3.2. A fixture
mismatch is a hard FAIL of fixture loading (exit 1). Re-fetching
deliberately requires a manifest update; drift is not silent.

### 12.6 Track 2 cheating

**Failure:** Track 2 author bypasses `runtime::tape`, hand-rolls a
faster-but-equivalent buffer, beats sonic-rs in the bench, and the
substrate ceiling number is fictitious — what bbnf actually generates
through codegen will not have access to that hand-rolled buffer.

**Mitigation:** §10.6 review checklist + signature. Track 2 source under
≤ 500 LOC keeps it review-tractable. CI runs a structural test
(`crates/bbnf-bench/tests/track2_uses_substrate.rs`) that greps Track 2
source for `runtime::tape::` and `bbnf_simd::` import statements and
fails if either is missing; greps for `Vec<u8>` allocation outside of
documented points and warns. It also asserts the source path is
`crates/bbnf-bench/src/track2/json.rs`, so no shadow hand-coded runtime crate
can become the measured Track 2. Not perfect but raises the cheating cost.

---

## §13 Open questions and contradictions in source authority

This section documents anywhere the spec sources disagree, so the bench
spec does not silently pick a side.

### 13.1 Rust toolchain pin

`restart/MASTER-PLAN.md` does not pin a Rust version. `restart/ARCHITECTURE.md`
mentions "edition = 2021" by implication (existing crates). The skinny
bench pins `edition = "2021"` per existing repo convention but does not
pin a `rust-toolchain.toml` MSRV; the M1 Pro reference run uses stable
latest at the time of run (recorded in metadata as
`rustc -V` output). If the user wants MSRV pinning at skinny level,
WORKSPACE.md owns it and BENCH.md inherits.

### 13.2 simd-json API choice

`restart/corpora/SOTA.md:14` says simd-json is "Tape-then-typed," but does
not specify which simd-json API produced the M1 Pro measurement. The skinny
therefore records both `to_borrowed_value` and `to_owned_value`; `S` uses the
faster row measured in the same run. This avoids baking an undocumented API
assumption into the threshold matrix.

### 13.3 sonic-rs feature flag set

SOTA.md treats the sonic-rs row as the fast anchor. The skinny records a
`sonic_rs_anchor` row using the fastest unchecked/anchor-mode API exposed by
the pinned crate and a `sonic_rs_checked` row using strict checked parsing.
If the pinned crate cannot expose a distinct anchor mode, the wrapper records
that fact and `sonic_rs_checked` becomes the anchor with an explicit
metadata flag. The feature set and exact function symbol are recorded in
metadata.

### 13.4 PASS-3 §7 vs ARCHITECTURE §11 row name

PASS-3 §7 names the rows `json/twitter/borrowed`, `json/twitter/tape_cursor`
(`restart/audit/pass-3-runtime/PASS-3.md:498-499`). ARCHITECTURE §11 names
them `json/twitter` flat (`restart/ARCHITECTURE.md:1514`). The skinny uses
the flat ARCHITECTURE convention because skinny only measures one
materialisation profile (typed root). The PASS-3 sub-rows split borrowed
vs tape_cursor; the skinny's `parse(&str)` returns a typed root owning the
sealed tape, with borrowed `ValueRef` projections. The naming difference is
documented; classification is unambiguous.

### 13.5 V1 H.W3 vs J.W1 ownership of SOTA close

`restart/MASTER-PLAN.md:140` says "the SOTA close gates measure the Rust
line only at H.W3 and H.W4 per Lock 8 amendment." Line 154 says "J.W1 close
gates record the parity-not-beat outcome." So SOTA close runs at H.W3
(beat target) and J.W1 (final closing gate). The skinny is upstream of
both; skinny outcomes flow into the H.W3 gate as an early warning.
The skinny does not redefine V1 ownership; it provides early-warning
information that H tranche entry consumes.

---

## §14 Bench harness — invocation cheat sheet

For developers running the skinny bench locally.

```sh
# One-time fixture setup.
cd tests/fixtures/json
curl -L -o twitter.json https://github.com/simdjson/simdjson/raw/master/jsonexamples/twitter.json
curl -L -o citm_catalog.json https://github.com/simdjson/simdjson/raw/master/jsonexamples/citm_catalog.json
curl -L -o canada.json https://github.com/simdjson/simdjson/raw/master/jsonexamples/canada.json
sha256sum *.json   # paste into manifest.toml

# Build the runtime.
cargo xtask check-json
cargo xtask regen-json     # emits crates/runtime/src/grammars/json/generated.rs
cargo build --release -p runtime -p bbnf-bench

# Run benches.
RUSTFLAGS="-C target-cpu=native" \
  cargo bench --bench json_parity --bench simd_scan -p bbnf-bench

# Run the gate to render RESULTS.md.
cargo run --release -p bbnf-bench --bin gate

# Read the verdict.
head -1 skinny/RESULTS.md
```

---

## §15 Bench harness scope summary

| Question | Answered | Method |
|---|---|---|
| Does the substrate reach SOTA-class throughput on JSON? | partial / NO overall | Track 2 vs sonic-rs / simd-json; current parse rows include A/C wins but hard G rows remain on `twitter`, `random`, `unicode_mixed`, and `unicode_basic` |
| Does the codegen path preserve the substrate's throughput? | mostly yes | Track 1 vs Track 2 ratio (F-positive / F-noise / F-codegen-gap sub-bands when substrate is borderline-weak; A / B / C / D / E when substrate is at parity) |
| Does the SIMD scan match its scalar reference on every corpus? | yes | per-corpus parity hash equality (twitter / citm / canada) |
| Does the SIMD scan reach simdjson-class Mbps on the largest input? | yes | structural_scan microbench gated on canada row |
| Are bench results reproducible? | yes | reproducibility schema enforcement + `schema_version` field |
| Is Track 1 byte-equal to Track 2 on output? | yes | parity oracle |
| Is direct-to-struct SOTA-class? | NO | sink-only direct rows are correctness-green; 6 of 17 pass and 11 emit `N-direct` against sonic-rs direct |
| Is the host-fn-free skinny grammar masking V1 dispatch cost? | yes | two probes — per-call dispatch overhead (`host_call_dispatch_overhead`) PASSES at < 1 ns/call; gross-time eager-decode variant (`host_call_eager_decode`) FIRES MASKING per §7.8.1 envelopes, forcing V1 JSON to keep decode lazy |
| Is the single-plan extraction masking cost-model wins? | confirmatory only | scalar alternate passes (canonical wins); dispatch-table alternate INVALID per `skinny/REDRESS.md` item 17 (real function-pointer table regressed; canonical Rust `match` is load-bearing); PEXT-mask alternate unimplemented in skinny — defers to V1 H.W2 |
| Is cold-cache parse latency acceptable? | report-only | cold_first_parse probe per corpus |
| Is the substrate viable for concurrent-parse workloads? | yes | peak RSS gated at outcome M (≤ 3× competitor on canada) |
| Does the substrate generalise beyond JSON? | report-only | optional CSS prior probe at bootstrap.css |
| Are CSS SOTA gates cleared? | NO (defers to V1 H.W4) | CSS prior probe is a substrate-generality signal, not a CSS SOTA verdict |
| Is incremental parsing performance acceptable? | NO (defers to V1 I) | n/a — committed tape is private and immutable; mutable reuse belongs to the V1 I `TapeBuilder` path documented in INDEX.md deviation ledger |
| Is generated JSON LOC inside budget? | yes | `xtask lint-loc` / gate metadata; V1 nine-grammar scale defers to F.W3 |
| Is WASM lower path measured? | NO (defers to V2) | n/a — see §9.4 |
| Is the build PGO-tuned? | NO (out-of-the-box LTO release for both bbnf and competitors) | recorded as `pgo_mode: "none"`; J.W1 may re-run with PGO |

The skinny answers the leading question — JSON-line SOTA viability — with
honest threshold gates. The current answer is not ready: parse/tape has four G
rows and direct typed emission is `N-direct / NoGo`.

---

End of `restart/skinny/BENCH.md`.
