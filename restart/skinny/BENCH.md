# Skinny Spec — Bench and Parity Harness

This document is one of four quadrants of the skinny implementation spec for
`bbnf-lang`. The skinny exists to validate the V1 architectural premise — the
SOTA-viability claim — before tranches A-J commit. Sister quadrants:

- `restart/skinny/SUBSTRATE.md` — `Tape`, `ValueRef`, `DocumentView`, payload
  arena, `simd-scan` integration contract.
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

`JsonRoot<'i>` is a typed root over `&'i Tape<'i>` per
`restart/ARCHITECTURE.md:1597`. The generated parser consumes `runtime::tape`
and `simd-scan` exactly as the hand-coded substrate parser does. No bypass,
no shortcuts, no per-grammar Rust outside `crates/runtime/src/grammars/json/`.

### 1.2 Track 2 — Hand-coded substrate parser (the substrate ceiling probe)

Produced by:

```
crates/bbnf-bench/src/track2/json.rs (handwritten Rust; the author writes
  calls into runtime::tape and simd-scan directly, expressing the JSON
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
and the SAME `simd-scan` structural-scan dispatcher that Track 1's generator
emits against. The author is allowed to use every substrate API the codegen
emits, but no codegen runs.

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
| `simd-scan` structural scan | identical | identical |
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

## §2 Three competitor baselines

The competitor set is fixed by Lock 8 (`restart/locks/14-LOCKS.md:48`).
Skinny pins the JSON-line subset only.

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
threshold anchor; the bbnf generated parser builds a typed root over the tape,
so a lazy-value-only comparison would not bind the same materialisation
contract.

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
| sonic-rs | `sonic-rs` | `=0.5` | `anchor` + `checked` wrapper rows | eager-typed |
| simd-json | `simd-json` | `=0.13` | `to_borrowed_value` + `to_owned_value` | borrowed + owned |
| serde_json | `serde_json` | `=1.0.117` | `from_slice::<Value>` | eager-owned |
| Track 2 (bbnf hand) | (workspace) | (commit) | `parse(&str)` | typed root over Tape |
| Track 1 (bbnf gen) | (workspace) | (commit) | `parse(&str)` | typed root over Tape |

A note on materialisation match: bbnf's `JsonRoot<'i>` over `&Tape<'i>` is
neither sonic-rs's eager-typed `Value` nor simd-json's `BorrowedValue`. The
honest framing: bbnf builds a tape and typed root, but JSON scalar
materialisation remains lazy and the payload arena must report zero writes
on the hot path. We benchmark it head-to-head with sonic-rs and simd-json
in-run anchors and report the materialisation mode per row; `S` is the
minimum of the anchor rows, not a stale static table value.

---

## §3 Three corpora

Skinny uses the canonical sonic-rs / simdjson trio: twitter, citm_catalog,
canada. These three span small/medium/large and payload-rich/structural/
array-of-numbers — the orthogonal axes that stress the substrate differently.

### 3.1 Corpus inventory

| Corpus | Purpose | Bytes (approx) | Hot path |
|---|---|---|---|
| `twitter.json` | small, payload-rich | ~616 KB | string-heavy object/array nesting, UTF-8 |
| `citm_catalog.json` | medium, structural | ~1.7 MB | deep object trees, key-value heavy |
| `canada.json` | large, array-of-numbers | ~2.2 MB | float-array dispatch, GeoJSON-style |

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

Separate from the end-to-end parse bench. Measures `simd-scan`'s structural-
index throughput in GB/s on twitter.json bytes. Targets per
`restart/ARCHITECTURE.md:1519`:

- ≥ 5 GB/s on M-series NEON
- ≥ 7 GB/s on x86 AVX2

### 4.1 What it measures

The structural index pass: the bit-parallel SIMD scan over input bytes that
identifies pseudo-structural characters (`{ } [ ] : , "`), maintains the
quote-state bitmap, and emits the index of every structural offset. This is
simdjson's Stage 1; bbnf's `simd-scan` provides the equivalent.

Microbench input: `twitter.json` raw bytes. Output: a `Vec<u32>` of
structural offsets. We measure the scan time, the input byte count, and
divide. GB/s = bytes_processed / wall_time.

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
    let simd_offsets = simd_scan::structural_index_simd(fixture);
    let scalar_offsets = simd_scan::structural_index_scalar(fixture);
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
| M-series NEON | ≥ 5 GB/s | simdjson OD on Apple Silicon |
| x86 AVX2 | ≥ 7 GB/s | simdjson OD on Intel Skylake |

Below floor → NO-GO at substrate level even if Track 2 parse is fast (because
the substrate floor is what holds at scale; a parse that hits parity on
twitter but cannot scale to canada-size or bigger inputs is a false signal).

### 4.4 Microbench harness (lives at `crates/bbnf-bench/benches/simd_scan.rs`)

Sketch — per-corpus parity check + throughput rows for each fixture so
the GB/s floor is verified at the input size that exercises the kernel
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
        let simd = simd_scan::structural_index_simd(bytes);
        let scalar = simd_scan::structural_index_scalar(bytes);
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
            b.iter(|| simd_scan::structural_index_simd(black_box(bytes)));
        });
        g.bench_function(format!("{name}/scalar"), |b| {
            b.iter(|| simd_scan::structural_index_scalar(black_box(bytes)));
        });
    }
    g.finish();
}
```

The GB/s floor is gated against the **canada** row (largest input;
worst-case kernel load). twitter and citm rows are recorded for cross-
input variance reporting but the floor binds at canada.

Criterion's `Throughput::Bytes` automatically reports GB/s in the output.
Post-bench parser (§8.2) extracts the GB/s figure and gates against the floor.

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
| Cold-cache mode | `warm` / `cold_clflush` / `cold_l1_evict` | `warm` |

A row missing any field is INVALID and removed from the dataset before the
threshold matrix runs.

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
| **F-noise — Borderline-weak substrate, codegen indistinguishable from hand within bench noise** | S × 1.05 < Track 2 ≤ S × 1.10 | Track 2 × 1.05 < Track 1 ≤ Track 2 × 1.10 | CONDITIONAL — substrate warning | Same dispatch posture as F-positive; the codegen ratio is within noise (criterion `noise_threshold(0.02)` plus 5% headroom) and unclassifiable as positive or gap. Re-run on bare-metal before committing to dispatch posture. |
| **G — Substrate failure** (collapsed prior G + H) | > S × 1.10 | any | NO-GO — substrate redesign | Block tranche B dispatch. Reopen Lock 1 (Tape/direct substrate) amendment. Re-run skinny after substrate revision before any A-J dispatch. (Track 1 ratio is informational only when Track 2 is a gap — the codegen rides the failed substrate; no separate "both fail" outcome.) |
| **I — Parity oracle fail** | n/a | n/a | NO-GO — correctness fail | Block tranche dispatch. Track 1 and Track 2 disagree on materialised output for at least one fixture; codegen is incorrect. Investigate divergence before any further bench claims. |
| **J — Reproducibility schema fail** | n/a | n/a | INVALID — re-run | Bench row missing required schema fields or schema_version mismatch; classification unsafe. Re-instrument and re-run. |
| **K — SIMD parity hash fail** | n/a | n/a | NO-GO — correctness fail | The structural-scan SIMD path produces offsets disagreeing with scalar on **any** corpus (twitter / citm / canada); substrate is silently corrupt. Block all dispatch until SIMD codepath fixed. |
| **L — SIMD throughput fail** | n/a | n/a | NO-GO — SIMD floor fail | Structural scan on **canada** (largest input; binding row) below floor (5 GB/s NEON / 7 GB/s AVX2). Even if Track 2 parse hits parity, the substrate ceiling will fail at scale. Block dispatch until the SIMD floor is restored, then re-run the full matrix. |
| **M — Memory residency fail** | n/a | n/a | NO-GO — peak RSS exceeds floor | Track 2 (or Track 1) peak RSS > 3 × the fastest competitor's peak RSS on canada. Substrate that hits SOTA-class throughput at 3× memory is not viable for concurrent-parse workloads (web servers, batch ingestion). Block dispatch until substrate memory profile is fixed. The 3× multiplier is the V1 J.W1 J-side floor projected back to skinny gate; a tighter ratio is encouraged but not required. |

### 6.2 Reading the matrix

The matrix is decided per-corpus. The classifier checks correctness/schema/
floor rows first (I, J, K, L, M), then throughput rows. The verdict for the
skinny is the WORST outcome across the three corpora plus the structural-scan
microbench, the memory floor, and the masking probes in §7.8.
Examples:

- All three corpora outcome A, structural scan ≥ floor, memory within
  3 × competitor → outcome A overall.
- twitter outcome A, citm outcome C, canada outcome D → outcome D overall
  (the worst).
- canada outcome G → outcome G overall, no matter what twitter / citm say.
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
7. F-positive / F-noise (Track 2 ∈ (S × 1.05, S × 1.10]) — substrate warning, by Track 1 sub-band.
8. E (Track 2 ≤ S × 1.05, Track 1 > Track 2 × 1.50) — codegen failure.
9. D (Track 2 ≤ S × 1.05, Track 1 ∈ (Track 2 × 1.15, Track 2 × 1.50]) — codegen gap.
10. C (Track 2 ≤ S × 1.05, Track 1 ≤ Track 2 × 1.15) — parity acceptable.
11. B (Track 2 ≤ BEAT_BOUND, Track 1 ≤ Track 2 × 1.15) — beat substrate.
12. A (Track 2 ≤ BEAT_BOUND, Track 1 ≤ Track 2 × 1.10) — beat-and-parity.

The order is deliberate: correctness/floor failures dominate; substrate
gaps dominate codegen issues (a fast generator on a broken substrate is
not viable); codegen issues only matter when the substrate floor is met.

### 6.3 Honest accounting

The user instruction was to "not assume that any of these other libs have
'magic' SIMD facilities" — but the matrix MUST still call NO-GO honestly.
Outcomes G and H exist precisely because the bench is the arbiter, not the
plan. If the substrate genuinely fails the SOTA gate, the matrix says so;
the spec does not bias toward GO.

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
    bench: alternate_scalar_plan         # confirmatory
    bench: alternate_dispatch_table_plan # confirmatory
    bench: alternate_pext_mask_plan      # x86_64 only; plausibly-better
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

Criterion reports both elapsed time AND GB/s automatically; both go to the
JSON report.

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
simd-scan = { path = "../simd-scan" }
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
as ns/call. Threshold: ≤ 50 ns/call on M1 Pro (one virtual call + table
lookup is canonically ~10-30 ns; 50 is generous). Pass: probe ≤ 50 ns/call.
Fail: probe > 50 ns/call, RESULTS marks dispatch overhead as MASKING and
notes that V1 grammars with many `@host fn` calls per parse will pay more
than the skinny measures.

**Probe B — `host_call_eager_decode`** (gross-time variant, JSON-shaped).
Question: when the V1 grammar's eager string decode runs through the
registry, how does end-to-end parse time compare? Implementation: a
JSON variant that emits `CallHost decode_string_to_arena` for every
string token at parse time, identical to V1 grammar shape. Threshold:
ratio against Track 1 declared explicitly per corpus; expected delta
**5-15% on twitter** (string-heavy, 700+ string fields, 5% escape density),
**3-8% on citm** (key-value heavy but mostly short strings), **< 2% on
canada** (numeric, few strings). Pass: probe within the per-corpus
expected band. Fail: probe **outside** the expected band — high or low.
A high outlier means eager decode costs more than expected on that
corpus and V1 SOTA probability drops; a low outlier means the eager-
decode work was elided (compiler optimised it away because the result
went unused) and the probe is invalid as written. RESULTS records pass /
fail with explicit ratio for each corpus.

#### 7.8.2 Alternate-plan probes — explicitly confirmatory

The alternate-plan probes bound the missing cost-driven rewrite axis. They
are **confirmatory, not adversarial**: they verify the canonical structural-
index + alt-dispatch plan is not dominated by other plausible plans within
the implementation envelope. They cannot establish that no plan elsewhere
in the cost-model space would be faster — that is what V1 H tranche owns.

Three alternates:

| Alternate | Mechanism | Question answered |
|---|---|---|
| `alternate_scalar_plan` | Pure scalar recursive descent; no SIMD scan; byte-by-byte alt dispatch. | Confirms SIMD adds value on JSON. Expected: substantially slower than canonical. |
| `alternate_dispatch_table_plan` | SIMD scan; alt dispatch via a 256-entry direct-jump table instead of match-arm. | Confirms LLVM's match-arm codegen on byte-disjoint alts is ≈ direct table. Expected: within 2-5% of canonical. |
| `alternate_pext_mask_plan` | x86_64 only; uses BMI2 PEXT to extract structural-bit masks instead of structural index of offsets. | Tests a *plausibly-better* shape that the V1 cost model might select on Intel. Expected: comparable to canonical or up to 10% faster on x86 AVX2-class hardware; may be slower on M1 Pro NEON (PEXT not available). |

Probe verdicts:

- **Confirmatory pass:** `canonical ≤ alternate_scalar_plan` AND
  `canonical ≤ alternate_dispatch_table_plan × 1.02` AND
  (`alternate_pext_mask_plan ≤ canonical × 1.05` on x86_64, or row reports
  N/A on aarch64). The canonical plan is not dominated within the
  implementation envelope; cost-driven-rewrites cut is FAITHFUL.
- **Cost-model masking signal:** any alternate ≤ canonical × 0.95
  on any corpus. RESULTS marks the cut as MASKING and routes a cost-model
  recovery lever to V1 H.W2/H.W3.
- **Inverted dominance:** `alternate_pext_mask_plan` < canonical × 0.90
  on x86_64. The cost model would clearly select PEXT on Intel; the
  skinny's hand-curated canonical plan is pessimal there, but the SOTA
  conclusion on M1 Pro stands. RESULTS notes the cross-platform plan
  divergence as a tranche-H input.

The probes are **not** e-graph outputs and do not validate the cost-model
machinery itself; they validate that the canonical plan is not
self-defeating. Calling them "adversarial" overstates them; calling them
"confirmatory with one plausibly-better candidate" is honest.

#### 7.8.3 Cold-cache first-parse probe (per-corpus)

`json/probes/<corpus>/cold_first_parse` measures parse latency on a cold
L1 + L2 + L3 cache. Implementation: each iteration uses
`criterion::Bencher::iter_custom` with explicit cache eviction between
iterations (allocate and touch a buffer ≥ L3 cache size, then drop;
on macOS `std::hint::black_box` the buffer through `core::arch::aarch64::__dsb`
or x86_64 `_mm_clflush` over the corpus bytes). Question: how much does
warm-cache benchmarking flatter the result? Many production workloads
(web servers, CLI tools, batch ingestion) parse one document per
request — first-byte latency matters, not steady-state.

Threshold: `cold_first_parse_us ≤ track1_generated_us × 2.0` per corpus.
Cold parses up to 2× warm parse is expected (instruction cache misses,
branch predictor unprimed, cold dispatch table). > 2× indicates a
substrate that performs only when warm; documented as a substrate
sensitivity finding routed to V1 J.W1 close gate. < 1.2× is suspicious —
likely the cache eviction did not actually cool the relevant lines;
RESULTS notes the row as inconclusive rather than passing.

This probe is *report-only*; it does not gate the matrix. The skinny's
SOTA premise is the warm-cache contest sonic-rs and simd-json compete in.
Cold-cache is recorded for V1 J.W1 to consume.

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
fn main() -> ExitCode {
    let report_dir = "target/criterion";
    let rows = collect_rows(report_dir)?;          // reads JSON + metadata.toml
    schema_enforce(&rows)?;                         // §5.3; missing field => exit 2
    parity_oracle_check(&rows)?;                   // §3.4; divergence => exit 3
    simd_parity_hash_check(&rows)?;                // §4.2; mismatch => exit 4
    masking_probe_check(&rows)?;                   // §7.8; annotates MASKING signals
    let outcome = classify(&rows, &threshold_matrix());  // §6
    println!("{}", render_results_md(&outcome));   // RESULTS.md content (§10)
    match outcome.verdict {
        Verdict::Go(_) => ExitCode::SUCCESS,
        Verdict::Conditional(_) => ExitCode::from(6),
        Verdict::NoGo(_) => ExitCode::from(5),
        Verdict::Invalid => ExitCode::from(2),
    }
}
```

Exit codes:

- 0: GO (CI passes, dispatch authorised).
- 2: schema enforcement failed (re-run required).
- 3: parity oracle failed (correctness bug).
- 4: SIMD parity hash failed (correctness bug).
- 5: NO-GO (substrate or codegen failure per matrix).
- 6: CONDITIONAL (manual amendment required before dispatch).

CI green requires exit 0. Conditional outcomes are intentionally non-green:
they produce RESULTS.md and a precise action, but they do not authorize skinny
dispatch.

### 8.3 CI runner discount

CI runners are typically slower than local M1 Pro by 1.3-1.7× depending on
runner generation and contention. The skinny gates against the CI-discounted
threshold matrix when running on CI; the local-run threshold is the
authoritative one. Discount factors:

| Runner | JSON parse discount | Reasoning |
|---|---|---|
| GitHub Actions `macos-14` (Apple Silicon) | × 1.15 | virtualisation + shared host noise |
| GitHub Actions `ubuntu-latest` (x86_64) | × 1.40 | shared cores, variable boost |
| Self-hosted bare metal | × 1.00 | (gold standard) |

Discount is applied to the threshold, not the measured time; a CI-measured
twitter Track 2 of 480 µs against a discounted threshold of (424 × 0.95 ×
1.15 = 463 µs) is a fail by ~17 µs — not a gold-case scenario, requires
a local re-run before NO-GO is final.

The discount table is committed at `crates/bbnf-bench/runners.toml` and
read by the gate. The runner detector reads `RUNNER_OS` /
`GITHUB_ACTIONS` / `BBNF_BENCH_RUNNER` env vars.

### 8.4 Local override

A developer can run the bench locally with `BBNF_BENCH_RUNNER=local` to get
authoritative thresholds bypassing the discount. CI sets the discount
automatically; local devs default to local.

### 8.5 GitHub Actions workflow sketch

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
      - run: cargo run -p bbnf-bench --bin gate
      - uses: actions/upload-artifact@v4
        with:
          name: criterion-report
          path: target/criterion/
      - uses: actions/upload-artifact@v4
        with:
          name: results
          path: restart/skinny/RESULTS.md
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
The walker uses the *same* `runtime::tape` + `simd-scan` substrate the
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
stay ≤ 500 LOC inside `bbnf-bench`. The nine-grammar generated-LOC ceiling
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
codegen path. The alternate-plan probe (§7.8) bounds only the JSON cost-plan
cut; it does not validate the full V1 recognizer miner.

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

V1 J.W1 retains the strict gate (≤ 1.5× competitor); skinny gates at the
generous 3× floor as a substrate viability signal, not a closing-row gate.
This is the redress of the prior "memory is not skinny's question"
defer — substrate viability *is* skinny's question, and 10×-memory
substrate is not viable.

### 9.7 No multi-core / parallel parse bench

NDJSON multithreaded (simdjson 3.5 GB/s) is not a V1 target. Skinny
single-threaded only. No omission impact: V1 is single-threaded parse.

### 9.8 No diagnostic / error-path bench

Error recovery (`BBNF-RECOVERY*` per PASS-3 §6b) defers to tranche I.
Impact: skinny benches valid input only. Error-path performance
characteristics are a separate concern.

### 9.9 No path / select API bench

`path!` / `select!` macro performance defers to G tranche. Impact:
skinny measures parse-to-typed-root. The path query API is V1 G-owned.

---

## §10 Verdict-writing template — `restart/skinny/RESULTS.md`

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

Outcome ID: <A|B|C|D|E|F|G|H|I|J|K|L>

Per-corpus outcomes:
- twitter: <ID>
- citm: <ID>
- canada: <ID>
- structural_scan: <pass|fail with GB/s>

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

| Bench | Median (µs) | 95% CI | GB/s | vs sonic-rs |
|---|---|---|---|---|
| Track 1 (generated) | <m> | [<lo>, <hi>] | <gb> | <ratio>x |
| Track 2 (hand-coded) | <m> | [<lo>, <hi>] | <gb> | <ratio>x |
| sonic-rs anchor | <m> | [<lo>, <hi>] | <gb> | 1.00x |
| sonic-rs checked | <m> | [<lo>, <hi>] | <gb> | <ratio>x |
| simd-json borrowed | <m> | [<lo>, <hi>] | <gb> | <ratio>x |
| simd-json owned | <m> | [<lo>, <hi>] | <gb> | <ratio>x |
| serde_json | <m> | [<lo>, <hi>] | <gb> | <ratio>x |

Codegen overhead (T1/T2): <ratio>x
Substrate ceiling (T2/S): <ratio>x
Payload arena writes/allocations: Track 1 <w>/<a>, Track 2 <w>/<a> (must all be 0)
Beat target (≤ 380 µs): <met|missed>
Parity floor (≤ 480 µs): <met|missed>

Outcome: <ID>

### citm_catalog.json (size <bytes>, sha256 <digest>)
[same schema]

### canada.json (size <bytes>, sha256 <digest>)
[same schema]

### simd/structural_scan (twitter.json, sha256 <digest>)

| Bench | GB/s | Floor | Pass |
|---|---|---|---|
| simd | <gb> | 5 (NEON) / 7 (AVX2) | <yes|no> |
| scalar | <gb> | (parity) | <yes|no> |
| Parity hash match | <yes|no> | required | <pass|fail> |

## Masking probes

### Host-call probes (per §7.8.1)

| Probe | Result | Threshold | Pass |
|---|---:|---|---|
| host_call_dispatch_overhead (ns/call) | <ns> | ≤ 50 ns/call | <yes|no> |
| host_call_eager_decode twitter | <ratio>x vs T1 | 1.05-1.15 expected | <yes|no> |
| host_call_eager_decode citm | <ratio>x vs T1 | 1.03-1.08 expected | <yes|no> |
| host_call_eager_decode canada | <ratio>x vs T1 | < 1.02 expected | <yes|no> |

### Alternate-plan probes — confirmatory (per §7.8.2)

| Probe | Corpus | Result | Confirmation |
|---|---|---:|---|
| alternate_scalar_plan | <corpus> | <ratio>x vs canonical | canonical ≤ alternate (SIMD adds value) |
| alternate_dispatch_table_plan | <corpus> | <ratio>x vs canonical | canonical ≤ alternate × 1.02 |
| alternate_pext_mask_plan (x86_64 only) | <corpus> | <ratio>x vs canonical | alternate ≤ canonical × 1.05 (or N/A on aarch64) |

### Cold-cache probe (report-only; per §7.8.3)

| Probe | Corpus | Cold (µs) | Warm (µs) | Cold/Warm | Sensitivity |
|---|---|---:|---:|---:|---|
| cold_first_parse | twitter | <us> | <us> | <ratio>x | < 2.0× expected |
| cold_first_parse | citm | <us> | <us> | <ratio>x | < 2.0× expected |
| cold_first_parse | canada | <us> | <us> | <ratio>x | < 2.0× expected |

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
| F-noise | 0.40-0.60 | < 0.30 | Substrate borderline-weak; codegen indistinguishable from hand within bench noise |
| G | < 0.30 | < 0.05 | Substrate failure (collapsed prior G + H) |
| I | n/a | n/a | Correctness fail; rerun |
| J | n/a | n/a | Schema fail; rerun |
| K | n/a | n/a | Correctness fail; rerun |
| L | < 0.30 | < 0.05 | SIMD floor failed on canada; re-run after substrate fix |
| M | < 0.30 | < 0.05 | Peak RSS > 3× competitor; substrate not viable for concurrent-parse |

The user can re-anchor the prior elsewhere, but the skinny supplies the
evidence in a form that the prior consumes.

### 10.4 Where RESULTS.md lives

`restart/skinny/RESULTS.md`. The gate writes (overwrites) it on every run.
The historical results live in `target/criterion/` archived per-run; the
canonical RESULTS.md is the latest.

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
- [ ] Track 2 calls runtime::tape APIs only (no separate buffer struct,
      no shadow tape implementation, no parallel arena).
- [ ] Track 2 calls simd-scan structural_index (no inline byte loop that
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
| `crates/simd-scan/` | SUBSTRATE.md | Structural scan dispatcher; bench microbenches it |

`crates/bbnf-bench/` source layout:

```
crates/bbnf-bench/
  Cargo.toml
  src/
    lib.rs
    fixtures.rs        # manifest loader, SHA-256 verify
    metadata.rs        # RowMetadata, HostFacts capture
    parity.rs          # cross-track parity oracle
    gate.rs            # threshold matrix classifier
    bin/gate.rs        # CI-invoked gate binary
    track2/            # handwritten substrate ceiling probe
      json.rs          # ≤ 500 LOC
  benches/
    json_parity.rs
    simd_scan.rs
  runners.toml         # CI discount factors
```

### 11.1 LOC budget for `bbnf-bench`

The skinny LOC ceiling for `crates/bbnf-bench/` is set in WORKSPACE.md.
Indicative budget:

- `fixtures.rs`: ≤ 120 LOC
- `metadata.rs`: ≤ 250 LOC (schema_version + per-corpus parity + RSS + cold_cache_mode add fields)
- `parity.rs`: ≤ 100 LOC
- `gate.rs`: ≤ 350 LOC (matrix expansion: F-split, G-collapse, M-add, BEAT_BOUND classifier)
- `bin/gate.rs`: ≤ 60 LOC
- `track2/json/` (Lock 13 split if needed): handwritten JSON parser, measurement-driven LOC; no constraint cap. Each file ≤ 500 LOC per Lock 13.
- `track2/css_prior.rs` (optional CSS prior probe per §9.1): ≤ 600 LOC, file split allowed.
- `benches/json_parity.rs`: ≤ 250 LOC (probe additions: dispatch, eager_decode, pext, cold_first_parse)
- `benches/simd_scan.rs`: ≤ 150 LOC (per-corpus parity)

Total: target ≤ ~2,200 LOC. Track 2's measurement-driven LOC is the
largest variable; reference-class hand-coded JSON parsers using
substrate APIs land at 800-1,500 LOC. The earlier 500 LOC ceiling on
Track 2 was constraint-driven and risked either a substrate-API thin
parser (substrate becomes covert hand-coded parser) or an arbitrary
budget overrun. The substrate-API correspondence checklist (§10.6) gates
on what Track 2 calls, not how short it is.

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
`apple-m1` / `skylake-avx512` / etc., enumerated in `runners.toml`),
classification emits a CONDITIONAL flag.

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
source for `runtime::tape::` and `simd_scan::` import statements and
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
vs tape_cursor; the skinny's `parse(&str)` returns a typed root over the
tape, which is structurally the borrowed row. The naming difference is
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
head -1 restart/skinny/RESULTS.md
```

---

## §15 Bench harness scope summary

| Question | Answered | Method |
|---|---|---|
| Does the substrate reach SOTA-class throughput on JSON? | yes | Track 2 vs sonic-rs / simd-json (BEAT_BOUND-anchored outcome A) |
| Does the codegen path preserve the substrate's throughput? | yes | Track 1 vs Track 2 ratio (F-positive vs F-noise sub-band) |
| Does the SIMD scan match its scalar reference on every corpus? | yes | per-corpus parity hash equality (twitter / citm / canada) |
| Does the SIMD scan reach simdjson-class GB/s on the largest input? | yes | structural_scan microbench gated on canada row |
| Are bench results reproducible? | yes | reproducibility schema enforcement + `schema_version` field |
| Is Track 1 byte-equal to Track 2 on output? | yes | parity oracle |
| Is the host-fn-free skinny grammar masking V1 dispatch cost? | yes | two probes — per-call dispatch overhead + gross-time eager-decode variant |
| Is the single-plan extraction masking cost-model wins? | confirmatory | three probes including a plausibly-better PEXT-mask alternate on x86_64 |
| Is cold-cache parse latency acceptable? | report-only | cold_first_parse probe per corpus |
| Is the substrate viable for concurrent-parse workloads? | yes | peak RSS gated at outcome M (≤ 3× competitor on canada) |
| Does the substrate generalise beyond JSON? | report-only | optional CSS prior probe at bootstrap.css |
| Are CSS SOTA gates cleared? | NO (defers to V1 H.W4) | CSS prior probe is a substrate-generality signal, not a CSS SOTA verdict |
| Is incremental parsing performance acceptable? | NO (defers to V1 I) | n/a — `Box<[T]>` sealing precludes; documented in INDEX.md deviation ledger |
| Is generated JSON LOC inside budget? | yes | `xtask lint-loc` / gate metadata; V1 nine-grammar scale defers to F.W3 |
| Is WASM lower path measured? | NO (defers to V2) | n/a — see §9.4 |
| Is the build PGO-tuned? | NO (out-of-the-box LTO release for both bbnf and competitors) | recorded as `pgo_mode: "none"`; J.W1 may re-run with PGO |

The skinny answers the leading question — JSON-line SOTA viability — with
honest threshold gates. The remaining questions defer to V1 tranches with
their own bench surfaces.

---

End of `restart/skinny/BENCH.md`.
