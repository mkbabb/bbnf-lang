# GN-4 — THE VALUE FABRIC

The tranche that raises four measured value lanes into one production surface. Where the
recon tree answered "how fast" at proof grade, GN-4 answers "what does the caller hold" at
production grade: a single `Value` enum with four proven lanes behind it — eager tape,
stop-early tape-free get, direct-to-struct fused sink, and the cross-language tape decode
that seeds the TypeScript backend. The stop-early skip engine, dormant since SK-V18, is made
first-class and live. The prototype-grade Value API is retired here.

Paths below are the greenfield tree of record — repo-root `crates/`, established by GN-1's
port of the recon organs (runtime tape, `bbnf-simd`, codegen emit spine, `bbnf-bench`
canon). Origin seams cite the proving source by `skinny/…` where a PROVE patch was measured;
those seams land in the greenfield during GN-1 and GN-4 productionizes atop them.

---

## §1 Charter

### Mission

Unify the value plane. Four lanes are already measured and adversarially certified (SPEC §0
rows 06, 07, 14, plus the extant eager tape). GN-4 productionizes each — public API, error
handling, oracle coverage, documentation — and binds them under one enum whose parity oracle
is the tranche's close gate. No new parsing invention: the mechanisms exist and won; GN-4 is
the surfacing and the unification.

### Precept bindings (sk-v23 research/07)

- **8 / 38 / 40 — substrate lands with its consumer in the same wave.** Every wave below
  names the consumer that reads its surface *inside* the wave: a public-API oracle, a cold
  paired bench, or a downstream typed test. No lane lands dark.
- **6 — no legacy; delete, never flag-and-keep.** The neutralized NEON scanner
  (`scan.rs:1-13`, "NEUTRALIZED on the live JSON path") is activated, not left dormant behind
  a plan flag. The hand schema (`xtask/src/real_typed_schema.rs`) is already deleted by
  GN-3.W3; W3 asserts its absence.
- **9 — no overfitting; no grammar-name gates.** `get` is generic over `Seg`; the typed
  lane keys on shape, not the literal rule name `value`. CI grep-gate enforced in the
  challenge wave.
- **14 / 15 — no god modules; directory-modules with children.** `path/`, `typed/`,
  `value/` are directory-modules owning their submodules and constants; the wasm export and
  the TS decoder are their own colocated homes.
- **16 / 54 — tests in `tests/`, never inline.** All parity oracles live under
  `crates/*/tests/` and the TS package's `test/`.
- **2 / 50 — no workarounds, no stubs.** The skip engine is real NEON; the serde bridge is a
  real `Deserializer`, not a shim.
- **4 — abrogate before patch.** The TS full-walk lane is measured at parity with native
  `JSON.parse` (0.9×); GN-4 abrogates it as a goal (documented non-goal) rather than patch a
  cold-JIT JS walk toward an unreachable 2×.
- **17 / 55 — docs ride the wave.** W5 ships the value-surface documentation with the enum
  it describes.
- **43-49 — the profiling standing law.** Every number is cold, paired, PMU cyc/B, N≥200,
  multi-window; ratios certified, absolutes directional; quiet-host certification at the
  challenge wave.
- **19 — a hard gate needs an artefact.** Every gate below is a bench table or a parity
  oracle, never a grep alone.

### Refutation guards (SPEC §0 laws that constrain this tranche)

These are binding negatives. A wave that trips one is a defect the challenge wave must catch.

- **Tape-navigation is NOT the get lane.** Row 06 measured eager `tape_pointer` (full parse
  + `JsonValue::pointer`) at **150×-1700×** the stop-early cost. W1's public `get` MUST be
  the tape-free stop-early path. The `JsonValue::pointer` verb over the eager tape
  (`json_templates/view.rs:330`) is a **contrast/oracle lane only** — never the recommended
  API, never the default `Value::get`. Routing `get` through a built tape is a wave-failing
  defect.
- **No digit-scan SIMD (row 02 NO_GO).** W2's SIMD key scanner classifies **key string
  bytes** (structural + quote/escape), never numeric fields. No Mula/Lemire digit-parse
  revival; the CPI-inversion that sank row 02 is out of scope here.
- **No block-scheduling pursuit (row 04 refuted).** W2's kernel is the movemask-narrow +
  compaction class (the GN-2.W4 lever), not multi-block software pipelining — the M-class
  OoO core already pipelines the single-block loop.
- **No shim perf claims (row 09 refuted).** W4's wasm-bindgen export earns its keep on
  cross-language reach and the measured 4.4× stop-early get — never on a portable_simd→NEON
  shim speed claim. There is no NEON shim on the W4 path.
- **Fold-over-cursor is dead by law.** W3's direct-to-struct is the fused single-pass sink,
  the ONLY typed lane. No fold-over-lazy-cursor materialization (2.3-4.9× behind sonic,
  refuted). The TS decoder's `nextSibling` walk (W4) is lazy tape navigation for the
  stop-early lane, distinct from the refuted eager Rust fold — and it is NOT the full-walk
  goal.
- **Warm benches are disinformation.** Cold per-parse, one out-of-band touch, N≥200, paired
  interleave. No warm/cached figure is quoted, ever.

### Supersessions

- The prototype-grade Value API (HANDOFF §2) is retired by the W5 unified surface.
- The bench-only lanes `json_path_bench` (06) and the `json_canon_bench` typed arms (07)
  are prototype residue: their **mechanisms** graduate to public API in W1/W3; the benches
  survive only as gates.
- The neutralized structural scanner (`scan.rs`, dormant since SK-V18) is superseded by W1's
  live skip engine.
- The hand-authored `DirectSchemaSet` (`xtask/src/real_typed_schema.rs`) is superseded by
  GN-3's derivation; W3 depends on that deletion having already compiled.

---

## §2 Interlocks

| Direction | Tranche.wave | Contract |
|---|---|---|
| depends-on | **GN-1.W** (the loom) | Ports runtime tape (with row 06 template changes), `bbnf-simd`, codegen emit spine, `bbnf-bench` canon into repo-root `crates/`. The four tape-plane accessors land live: `Tape::{source,offsets,flag_cursors,flag_values}` (`crates/runtime/src/tape/mod.rs` ~121-137). |
| depends-on | **GN-3.W3** (Wall 1) | Grammar-derived `DirectSchemaSet` replaces the hand schema; `real_typed_schema.rs` is deleted. W3 consumes the derived schema and asserts the hand file's absence. |
| depends-on | **GN-3.W1** (Wall 2, sum types) | `DirectTypeKind` sum types make `value = object\|array\|…` representable — the typed lane needs it for the `Value` enum's variant set (W5). |
| depends-on | **GN-2.W4** (movemask-stitch lever) | W2's SIMD key scanner reuses the narrow/compaction kernel rather than minting a second SIMD idiom. |
| soft (reuse) | **GN-2.W1** (prefilter `find_member`) | W2 may route key-byte classification through the wired `find_member` lane if it lands first; not a hard block. |
| feeds | **GN-6.W** (the shopwindow) | The playground greenfield runs wasm on the GN-4 seam: W4's `bbnf-wasm` tape export + the W5 value surface are the wasm boundary. |
| feeds | **GN-5.W4** (const-mask generalization) | The unified `Value` surface is the consumer API the fleet grammars materialize through; GN-5 generalizes the stop-set mask atop these lanes. |
| complements | **PT-3** (parse-that TS backbone 1.1.0) | W4's `ts/tape` decoder is a standalone monomorphic seed of the TS backend — it imports no parse-that combinators (its oracle is `JSON.parse`). PT-3 productionizes the parse-that combinator backbone the wider TS backend later composes atop this seed. Complementary, non-gating: neither is the other's precondition. |

**Execution position:** GN-4 opens after `{GN-2 ∥ GN-3}` close (SPEC §2 order). GN-3's
derived schema and sum types are therefore guaranteed present when W3/W5 open — no schema
workaround is permitted (precept 2).

**Owner gates:** none GN-4-specific. The §3 adjudication checkboxes are archival/letter
concerns (GN-0, PT-N). GN-4 proceeds on green gates alone.

---

## §3 Wave manifest

| id | name | scope (one line) | LOC (+new/−del) | gate type | hard cap |
|---|---|---|---|---|---|
| **W1** | The Stop-Early Public Path | Productionize row 06 `get` as the public tape-free path API: pointer + typed get + prefix parse; activate the NEON skip engine first-class. | +650 / −45 | born-verified (non-regression) | 30 min |
| **W2** | The Key Scanner | SIMD key scanner in `bbnf-simd`, consumed by the path walk, to close the deep-path descent loss. | +290 / −30 | **born-RED** | 30 min |
| **W3** | Direct-to-Struct Public | Public `parse_into::<T>()` + serde bridge over the fused single-pass sink; schema is GN-3-derived. | +410 / −20 | born-verified (non-regression) | 30 min |
| **W4** | The TS Seed | wasm-bindgen export of the four tape planes; ship the productionized `TapeDoc` decoder + stop-early get in TS; full-walk as documented non-goal. | +960 / −0 | born-verified + born-RED (wasm parity) | 40 min |
| **W5** | The Unified Surface | One `Value` enum, four-lane dispatch, value-surface docs; cross-lane parity oracle as the close gate. | +700 / −35 | **born-RED** (parity) | 30 min |
| **CW** | The Adversary | Non-author re-measure of every GN-4 claim; prototype-residue hunt; quiet-host cert. | +60 / −0 | audit | 25 min |

Totals: **+3070 / −130** net-new. Ledger reconciled in §5.

---

## §4 Per-wave specification

### W1 — The Stop-Early Public Path

**Objective.** Raise PROVE-06's measured stop-early `get` from a bench prototype to the
public path API. The mechanism — walk source bytes directly, resolve each pointer segment,
stop the instant the target is located, jump skipped subtrees with the re-activated NEON
scanner — is CONFIRMED (2/3 path-class win, byte-exact parity, resolves at 0.032% of the
document). W1 adds the caller-facing surface: RFC-6901 pointer parsing, typed accessors, and
production error handling.

**File-level plan** (colocation edict; `path/` is a new directory-module, grammar-side):

```
crates/runtime/src/grammars/json/
  path/
    mod.rs        get(&str, &[Seg]) -> Option<Located>; Located; public re-exports; the
                  crate-doc for the lane (tape-free contract stated explicitly)
    segment.rs    Seg { Key(&str), Idx(usize) }; parse_pointer(&str) -> Result<Vec<Seg>,
                  PointerError> — RFC-6901 (/statuses/0/user/screen_name, ~0/~1 unescape)
    walk.rs       object/array member walk; skip_value over the skip engine; escape-aware
                  key compare (this is W2's routing point)
    typed.rs      get_str / get_i64 / get_u64 / get_f64 / get_bool — decode Located to a
                  native value (reuses parse_that_regex::number::materialize_* + unescape)
  scan.rs         + skip_container (NEON jump-to-matching-bracket), + skip_tail (scalar),
                  + block_emit_mask factored out of neon::scan (DRY, parity-preserved);
                  DELETE the "NEUTRALIZED" gating comment at scan.rs:1-13 — the scanner is
                  now live on the get hot path
codegen/src/json_templates/view.rs   JsonValue::pointer(&[Seg]) — the CONTRAST/oracle lane
                  only (eager-tape navigation; NOT the public get)
codegen/src/runtime_generator.rs      JSON_MOD_RS: pub mod path; pub use path::{get,
                  Located, Seg}; (regen wiring, not a hand-patch)
crates/runtime/tests/json_path.rs     the public-API oracle (below)
crates/bbnf-bench/src/bin/json_path_bench.rs   the productionized cold paired + PMU harness
```

**Seams (origin, skinny source-of-truth).** New path get `skinny/…/grammars/json/path.rs`
(PROVE-06, +178). Skip engine additions `…/grammars/json/scan.rs:56-…` (`skip_container`,
`skip_tail`, `block_emit_mask` factor-out) over the dormant scanner `scan.rs:1-13`. Contrast
verb `codegen/src/json_templates/view.rs:330` (+18). Wiring `runtime_generator.rs:363-372`.
The pain it replaces: linear `JsonObject::get` (`view.rs:106-114`) and the re-decoding
`next_sibling_cursor` skip (`view.rs:355-382`).

**Patch landed.** `prototypes/PROVE-06/patch.diff`. Adaptation: the bench-only `path.rs`
promotes into the `path/` directory-module (segment/walk/typed split per colocation); the
string→`Seg` pointer parse (RFC 6901) and typed accessors are net-new productionization not
in the prototype; the `regen-json` wiring is unchanged.

**Consumer IN this wave.** `crates/runtime/tests/json_path.rs` — the public-API parity
oracle: for shallow/deep/missing/prefix pointers, `get` (tape-free) is byte-identical to
`sonic_rs::get_unchecked(...).as_raw_str()` and to the contrast `JsonValue::pointer` over the
eager tape. Plus the cold paired bench as the perf consumer.

**GATE (born-verified, non-regression).**
Command:
```
cargo run --release -p bbnf-bench --bin json_path_bench -- paired 200
```
Threshold: on `twitter.json`, the **shallow** (`statuses[0]`) and **missing**
(`statuses[0].nonexistent`) classes each have paired ratio (mine/sonic `get_from_str`)
**< 1.0 with the 95% CI upper bound < 1.0**; byte-exact parity asserted on all four paths
*before* any timing. Deep is permitted to lose here (W2 owns it).
Failure semantics: if either shallow or missing CI crosses 1.0 (the shallow lane is
load-fragile per PROVE-06 VERIFY — a tie at loadavg≈16), or any parity assert fails, the
wave is RED. Shallow's CI-exclusion is re-confirmed under quiet-host in CW.

**Oracle.** `tests/json_path.rs` (above). **Bench.** `json_path_bench` cold paired + PMU
cyc/get, N≥200, multi-window, per-block loadavg recorded.

---

### W2 — The Key Scanner

**Objective.** Close the one measured loss. PROVE-06 deep-descent (`statuses[0].user.
screen_name`) runs **2.0-2.1×** sonic get_from_str because the key walk is scalar
`match_string` + scalar byte compare while sonic uses a SIMD key scanner. W2 mints a SIMD
key scanner in `bbnf-simd` and routes the path walk through it.

**File-level plan:**

```
crates/bbnf-simd/src/key_scan/
  mod.rs        find_key_end(src, at) -> usize; compare_key(src, at, needle) -> KeyCmp —
                classify quote/escape/structural over a 16-byte NEON block; the
                movemask-narrow + compaction from GN-2.W4 (reused, not a second idiom)
  neon.rs       #[cfg(target_arch="aarch64")] the block kernel; scalar fallback sibling
crates/runtime/src/grammars/json/path/walk.rs   route escape-aware key compare through
                bbnf_simd::key_scan (replaces the scalar compare); unchanged for skip
crates/bbnf-bench/src/bin/json_path_bench.rs     deep-class re-measure (already present)
crates/bbnf-simd/tests/key_scan.rs               kernel parity oracle (scalar ≡ NEON)
```

**Seams.** The descent-bound loss is diagnosed in PROVE-06 REPORT §3 ("scalar `match_string`
+ scalar key compare trail sonic's SIMD key scanner"). The reuse target is GN-2.W4's
movemask-stitch kernel (SPEC §2 GN-2.W4). **Refutation guard:** this scans key *strings*,
never digit fields (row 02), and uses narrow/compaction, never block scheduling (row 04).

**Consumer IN this wave.** `crates/bbnf-simd/tests/key_scan.rs` proves scalar ≡ NEON on
adversarial keys (escapes, non-ASCII, empty, 63-byte). The deep-class bench IS the perf
consumer — the scanner lands wired into `walk.rs`, not as a dark kernel.

**GATE (born-RED).**
Command:
```
cargo run --release -p bbnf-bench --bin json_path_bench -- paired 200
```
Threshold: **deep** class paired ratio (mine/sonic get_from_str) **≤ 1.2 with 95% CI**, down
from the measured ~2.05; AND shallow + missing wins from W1 retained (both CI < 1.0); AND
byte-exact parity on all four paths.
Failure semantics: deep CI upper bound > 1.2, OR any W1 win lost to the scanner's overhead,
OR parity broken → RED. Born-RED because the current measured state is 2.05× — the wave
opens failing and must be driven green.

**Bench.** As W1 plus the kernel parity oracle; cold paired PMU, multi-window.

---

### W3 — Direct-to-Struct Public

**Objective.** Raise PROVE-07's measured fused single-pass typed lane to a public API with a
serde bridge, fed by GN-3's grammar-derived schema. The lane is CONFIRMED: byte-exact parity
vs serde AND sonic, faster than both typed deserializers on all three corpora (sonic-typed paired
**0.746-0.786×**, ≈1.27-1.34× faster; serde-typed **1.85-1.92× on citm**, honestly thinner ~1.3×
on twitter's projection). The fused sink is the ONLY typed lane —
fold-over-cursor is dead by law.

**File-level plan** (`typed/` directory-module, grammar-side):

```
crates/runtime/src/grammars/json/typed/
  mod.rs        parse_into::<T: FromTape>(&str) -> Result<T, TypedError> — the public entry
                over the generated single-pass filler (codegen json_typed_direct)
  serde.rs      the serde bridge: parse_into_serde::<T: Deserialize>(&str) -> Result<T, _> —
                a real Deserializer driving the sink callbacks (no fold-over-cursor)
  error.rs      TypedError { path, expected, found } — production error with pointer context
crates/runtime/tests/json_typed.rs   public-API parity oracle vs serde_json + sonic_rs
crates/bbnf-bench/src/bin/json_canon_bench.rs   the PROVE-07 typed lanes (already present)
```

**Seams.** Emitter `codegen/src/json_typed_direct.rs:10` (1245 LOC, working). Schema bridge
`lower/schema_direct.rs:11`. Generated fillers `bbnf-bench/generated_real_typed.rs`. Parity
harness `bbnf-bench/direct_struct.rs` (`assert_direct_strict_product_parity`). The bench lane
`bbnf-bench/src/bin/json_canon_bench.rs` (PROVE-07). The schema now comes from
GN-3.W3's `type_plane::derive` — NOT the deleted `xtask/src/real_typed_schema.rs`.

**Patch landed.** `prototypes/PROVE-07/patch.diff` supplies the measured bench arms
(`TypedFn`/`TYPED_WORKLOADS`, parity preflight, per-mode typed sub-blocks) verbatim. W3's
net-new is the public `parse_into` + serde bridge + error type; the emitter/harness are
unchanged (precept 9 — no re-invention).

**Consumer IN this wave.** `crates/runtime/tests/json_typed.rs` — `parse_into::<TwitterSearch>`
and `parse_into_serde::<CitmCatalog>` materialize byte-identical structs to
`serde_json::from_slice` and `sonic_rs::from_slice` (checksum parity). The canon bench is the
perf consumer.

**GATE (born-verified, non-regression + schema-source assertion).**
Command:
```
cargo run --release -p bbnf-bench --bin json_canon_bench -- paired 200
grep -rq real_typed_schema crates/ && echo RESIDUE || echo CLEAN
```
Threshold: parity preflight prints `direct==serde==sonic typed-struct OK` for
twitter/citm/canada *before* timing; `direct/sonic_typed` paired ratio **< 1.0 (≥1.0×
faster) on all three corpora**; `direct/serde_typed` **< 1.0 on all three**; the grep prints
`CLEAN` (the hand schema is gone — GN-3 dependency satisfied).
Failure semantics: parity mismatch, any corpus regressing to ≥1.0× vs sonic-typed, or
`RESIDUE` (hand schema still present) → RED. **The serde-2× bar is explicitly NOT a gate** —
it is honestly unmet on twitter (~1.3×, a thin-projection structural fact per PROVE-07 §3)
and reached only on citm (1.9×); W3 documents this, never claims it.

**Refutation guard.** The lane is the fused single-pass sink (disasm-verified straight-line
SWAR + jump-table, PROVE-07 §5). Any fold-over-cursor reintroduction is a wave-failing
defect.

---

### W4 — The TS Seed

**Objective.** Graduate PROVE-14's cross-language tape decoder to the TypeScript-backend
seed. The layout crosses byte-exactly (node-for-node parity across 320 cold children); the
stop-early get in TS clears 2× at **4.4×** vs `JSON.parse`; the full-walk lane is at parity
(~0.9×) and is abrogated as a goal. W4 ships the wasm-bindgen export of the four tape planes
and the productionized decoder.

**File-level plan** (wasm sub-crate + TS package, both colocated homes):

```
crates/bbnf-wasm/                     NEW cdylib sub-crate (wasm-bindgen), aarch64/wasm32
  Cargo.toml                          crate-type = ["cdylib"], wasm-bindgen dep
  src/lib.rs                          #[wasm_bindgen] parse(src: &str) -> TapeExport
  src/tape_export.rs                  TapeExport { offsets: Uint32Array, flag_cursors:
                                      Uint32Array, flag_values: Uint8Array, source:
                                      Uint8Array } — zero-copy views over the four planes
ts/tape/                              NEW TS package — the TS-backend seed
  src/tape.ts                         TapeDoc monomorphic decoder (4 ctor-only fields, single
                                      hidden class); classify, sparse-flag binary search,
                                      lazy nextSibling walk, pointer nav, byte-hash fast path
  src/load.ts                         planes -> aligned typed arrays (decodeTape)
  src/pointer.ts                      stop-early tapeGet (the 4.4× lane)
  test/tape-parity.test.ts            digest parity vs JSON.parse (the oracle)
  test/tape.bench.ts                  vitest bench() cold paired: full + stop-early
  README.md                           the full-walk NON-GOAL, with the measured reason
```

**Seams.** Plane accessors `crates/runtime/src/tape/mod.rs` `source()`/`offsets()`/
`flag_cursors()`/`flag_values()` (~121-137) — the export shape PROVE-14 already proved with
`tape_dump.rs:78`. Decoder origin `bbnf-bench/ts-tape/tape.mjs` (364), `load.mjs`,
`pointer.mjs`, `verify.mjs`, `bench.mjs` (PROVE-14, 703 LOC total). **V8 discipline (R4 §6):**
the `TapeDoc` fields are assigned only in the constructor (single hidden class); classify
switches on the source byte (no name gate); `dispatch` via `Int8Array` table where a
first-byte fan-out is needed; no post-ctor mutation. Same-map, monomorphic call sites.

**Patch landed.** `prototypes/PROVE-14/patch.diff` supplies `tape_dump.rs` (the export-shape
proof) and the `.mjs` decoder. Adaptation: `.mjs` → typed `.ts` under `ts/tape/src/`; the
Rust dump bin is superseded by the live `bbnf-wasm` export (the dumper survives only as a
cross-check fixture in the parity oracle). The full-walk lane is retained as a **measured
non-goal**, not deleted — its number is the evidence for the abrogation.

**Consumer IN this wave.** `ts/tape/test/tape-parity.test.ts` consumes the wasm export and
asserts digest parity; `test/tape.bench.ts` consumes it for the stop-early 4.4× measure.
The wasm export is read by real TS, not dark.

**GATE (born-verified on stop-early + parity; born-RED on wasm plane parity).**
Commands:
```
cargo build --release -p bbnf-wasm --target wasm32-unknown-unknown
cd ts/tape && npm test && npm run bench
```
Threshold: (a) `tape-parity.test.ts` — all six structural counts + codeUnits + 32-bit
fingerprint identical between the wasm-exported tape and `JSON.parse` (byte-exact,
node-for-node); (b) the wasm-exported planes are byte-identical to the `tape_dump` fixture
(the born-RED cross-check — proves the export layout did not drift); (c) `tape.bench.ts`
stop-early ratio (JSON.parse/tape) **≥ 2.0×** (measured 4.4×), cold paired, N≥200 fresh
children (the corpus carries 320 cold children — the achieved sample clears the standing-law
N≥200 floor).
Failure semantics: any parity count mismatch, wasm-plane drift vs the fixture, or stop-early
< 2.0× → RED. **Full-walk is NOT gated** — it is documented at ~0.9× parity in `README.md`
with the reason (cold native `JSON.parse` is a hand-optimized C++ parser; cold-JIT JS cannot
beat it on full materialization). Hiding the full-walk number instead of documenting it is a
CW-catchable defect.

**Refutation guards.** No shim perf claim (row 09): the wasm export's value is cross-language
reach + the 4.4× stop-early, both measured; there is no portable_simd→NEON shim on this path.
Tape-navigation-is-NOT-the-get-lane is reconciled here, not breached: that law (row 06) was
measured on the RUST eager full-parse `tape_pointer` lane (~1.26M cyc per get — build the whole
tape, then navigate it). W4's TS `tapeGet` operates on an ALREADY-CROSSED tape whose one-time
decode is the very product being amortized across many gets, and it measured **4.4× vs
`JSON.parse`+get** — a DIFFERENT lane (amortized stop-early over a crossed tape), not the refuted
build-then-navigate lane. The two do not contradict; the guard forbids routing a get through a
freshly-built Rust eager tape, which W4 does not do.

---

### W5 — The Unified Surface

**Objective.** Bind the four lanes under one enum with a cross-lane parity oracle as the
close gate. This is the tranche's keystone: the caller holds one `Value` type and selects a
lane by intent (materialize-all, get-one, decode-to-struct, cross-language), and every lane
agrees on the value it yields.

**File-level plan** (`value/` directory-module — the value-fabric home):

```
crates/runtime/src/value/
  mod.rs        Value<'i> — the one surface enum (Object/Array/String/Number/Bool/Null,
                variant set from GN-3.W1 sum types); the four-lane entry methods:
                  Value::from_tape(&Tape)        -> eager lazy view (lane 1, existing)
                  Value::get(src, &str)          -> stop-early tape-free (lane 2, W1)
                  Value::parse_into::<T>(src)     -> direct-to-struct (lane 3, W3)
                  (lane 4, TS decode, is the wasm/ts seam — documented, not a Rust method)
  lanes.rs      the intent->lane dispatch table + the doc contract for each lane's cost
                (tape-nav 150-1700x the get cost is stated here so no caller mis-routes)
  docs.rs       //! module docs: the four lanes, when to use each, the negative results
crates/runtime/tests/value_parity.rs   the cross-lane parity oracle (the close gate)
docs/tranches/sk-v24/value-fabric.md    the durable prose (feeds GN-6 ARCHITECTURE)
```

**Seams.** Lane 1 wraps the existing lazy view `grammars/json/view.rs` (`JsonRoot`,
`JsonValue`, `value_from_ref` at `value.rs:143-171`). Lane 2 is W1's `path::get`. Lane 3 is
W3's `typed::parse_into`. Lane 4 is W4's `ts/tape` decoder over the `bbnf-wasm` export. The
enum variant set draws on GN-3.W1's `DirectTypeKind` sum types.

**Consumer IN this wave.** `crates/runtime/tests/value_parity.rs` — the oracle materializes
the same logical values through all four lanes and asserts equivalence; it IS the close gate,
and it is the consumer. Doc examples in `value-fabric.md` are compile-tested (doctest).

**GATE (born-RED, parity).**
Commands:
```
cargo test -p runtime --test value_parity --release
cd ts/tape && node test/cross-lane.mjs        # the TS side of the four-way oracle
```
Threshold: on twitter/citm/canada, all four lanes agree node-for-node — (1) eager
`from_tape` walk digest ≡ (2) stop-early `get` on every leaf pointer ≡ (3) `parse_into::<T>`
field checksum ≡ (4) TS `TapeDoc` walk digest — **100% agreement, zero divergence**.
Failure semantics: any lane's digest/checksum diverges on any corpus → RED. Born-RED because
the unified enum and the four-way oracle are net-new and unproven until this wave drives them
green.

**Docs (precept 17/55).** `value-fabric.md` — standalone prose (no meta-language): the four
lanes, the cost contract (tape-nav is not the get lane), the honest non-goals (serde-2× on
thin projections, TS full-walk parity). Feeds GN-6's ARCHITECTURE + NEGATIVE-RESULTS.

---

### CW — The Adversary (the one challenge wave)

**Objective.** A non-author fleet adversarially re-measures every GN-4 claim on a
quiet-host, and hunts prototype residue. Advance-on-zero-REJECT (precept 32).

**Re-measure (fresh windows, quiet-host cert per precept 49):**
- **W1** — re-run `json_path_bench paired 200` on a quiet host. Does shallow's CI still
  exclude 1.0, or does the load-fragility (VERIFY §Attack 1: tie at loadavg 16) sink it under
  honest quiet conditions? Named counterexample: a shallow ratio 95% CI of [0.97, 1.03]
  fails the gate.
- **W2** — is the deep ≤1.2× a real SIMD-key-scanner win, or a two-arm window asymmetry
  (the classic self-deception, row 09's standing lesson)? Verify paired interleave, both
  arms one binary. Named counterexample: deep improves only in the unpaired harness.
- **W3** — audit the serde-2× honesty: is the doc claiming 2× anywhere it is only 1.3×?
  Confirm `real_typed_schema.rs` is deleted (grep `CLEAN`) and the schema is genuinely
  GN-3-derived. Named counterexample: a `DirectSchemaSet` literal still hand-authored under
  a renamed file.
- **W4** — re-run `tape.bench.ts partial` (N≥200 fresh children); does stop-early hold ≥2× at
  quiet-host (PROVE-14 VERIFY saw 4.44× at loadavg 8)? Confirm the full-walk non-goal is *documented*, not buried.
  Confirm the wasm planes are byte-identical to the fixture (no silent layout drift).
- **W5** — is the parity oracle a real four-way check or a stub that compares a lane to
  itself? Named counterexample: `value_parity.rs` asserting `get == get` instead of
  `get == from_tape == parse_into == ts_decode`.

**Prototype-residue hunt (the named-defect classes):**
- **Bench-only wiring surviving as the API** — did W1/W3 actually promote the mechanism to
  public `pub` surface with tests, or is the "public API" still only reachable through the
  bench bin?
- **Feature-flag limbo** — is the NEON skip engine live (the `scan.rs:1-13` "NEUTRALIZED"
  comment deleted, the scanner on the get hot path), or still gated behind a plan env var?
- **Dormant kernels** — does `bbnf-simd::key_scan` have a live consumer in `walk.rs`, or is
  it a dark kernel with only a test?
- **Hand-schema crutch** — grep-gate: zero grammar-name predicates in codegen; zero
  hand-authored `DirectSchemaSet`.

**GATE (audit).** CW closes on: every re-measured ratio reproduces in direction under
quiet-host cert; zero prototype-residue defects survive; the CI grep-gates
(`no grammar-name predicate`, `no real_typed_schema`) pass. Any surviving REJECT reopens the
owning wave under a new letter (precept 39 — a wave that cannot close is re-planned, never
absorbed). Failure semantics: a reproduced regression or an un-cured residue defect → the
tranche does not close.

---

## §5 LOC ledger

| wave | +new | −deleted | net | dominant cost |
|---|--:|--:|--:|---|
| W1 | 650 | 45 | +605 | `path/` dir-module (segment/walk/typed) + skip engine + RFC-6901 parse + oracle |
| W2 | 290 | 30 | +260 | `bbnf-simd::key_scan` NEON + scalar + parity oracle |
| W3 | 410 | 20 | +390 | `typed/` dir-module (parse_into + serde bridge + error) + oracle |
| W4 | 960 | 0 | +960 | `bbnf-wasm` cdylib + `ts/tape` TS package (decoder + tests + bench + non-goal doc) |
| W5 | 700 | 35 | +665 | `value/` dir-module (enum + lane dispatch + docs) + four-way parity oracle |
| CW | 60 | 0 | +60 | oracle hardening, quiet-host capture |
| **total** | **3070** | **130** | **+2940** | |

Deletions: the `scan.rs:1-13` neutralization comment + scalar key-compare path (W1/W2), the
prototype `.mjs` dump bin's role (W4 supersedes with wasm), the prototype-grade Value API
stubs (W5). The hand schema deletion is charged to GN-3.W3, not counted here.

---

## §6 Exit criteria

GN-4 closes when:

1. W1 public `get` retains the 2/3 path-class win (shallow + missing CI < 1.0) with
   byte-exact parity — tape-free, the contrast tape-nav verb demoted to oracle.
2. W2 deep-path ≤ 1.2× vs sonic get (from 2.05×), W1 wins retained, kernel parity proven.
3. W3 `parse_into` + serde bridge beat sonic-typed and serde-typed ≥1.0× on all three
   corpora, byte-exact parity, schema GN-3-derived (`CLEAN` grep), serde-2× honestly
   documented as unmet-on-thin-projection.
4. W4 wasm export crosses byte-exactly, TS stop-early ≥2× (measured 4.4×), full-walk
   documented as a measured non-goal.
5. W5 four-lane parity oracle green across twitter/citm/canada; the `Value` surface + docs
   shipped.
6. CW: every claim reproduces under quiet-host cert; zero prototype-residue defects; grep-gates
   green.

Every gate is a cold paired PMU bench table or a parity oracle (precept 19). No wave closes
on a document (precept 35) except W5's doc, which rides its enum (precept 17).

---

## §7 Risks with named counterexamples

- **R1 — Shallow load-fragility sinks W1 at quiet-host.** PROVE-06 VERIFY flagged shallow as
  the thinnest margin (~1.28×), tying at loadavg 16. Counterexample: quiet-host shallow CI
  [0.98, 1.02] — a non-win. Mitigation: W1's gate requires CI exclusion of 1.0; if quiet-host
  shallow ties, W1 reopens with the skip-engine hot-loop as the target (the shallow lane is
  one bulk container skip — its margin lives entirely in `skip_container`).
- **R2 — W2 key scanner is a paper win.** The deep loss is descent-bound; a SIMD key scanner
  that adds setup cost on short keys could regress the *other* classes while barely helping
  deep. Counterexample: deep 2.05→1.3× but shallow 0.78→0.95× (net worse). Mitigation: the
  gate binds W1 wins retained; W2 must be net-positive across all three classes or it is RED.
  Fall-back path: an N\*=3 crossover guard (short keys stay scalar) per the empirical spine.
- **R3 — W3 blocks on GN-3.** If GN-3.W3's schema derivation slips, W3 has no derived schema
  and the no-workaround law forbids reviving the hand file. Counterexample: GN-3 delivers sum
  types but not document-shape derivation. Mitigation: GN-4 opens only after {GN-2∥GN-3}
  close (SPEC §2 order) — the dependency is structurally satisfied; if GN-3 is genuinely
  incomplete, GN-4.W3 does not open (precept 7 — no silent deferral; the interlock is named).
- **R4 — W4 wasm plane drift.** A zero-copy `Uint32Array` view over the offset plane can
  silently mis-align (endianness, offset stride) and still "parse". Counterexample: the wasm
  planes decode to a *different* fingerprint than `tape_dump` yet `JSON.parse` parity passes
  by coincidence on ASCII-only leaves. Mitigation: the born-RED cross-check asserts wasm
  planes byte-identical to the `tape_dump` fixture, not merely JSON-parity.
- **R5 — W5 oracle stubs itself.** A four-way oracle that accidentally compares one lane to
  itself gives false green. Counterexample: `value_parity.rs` calling `get` for both the
  "eager" and "stop-early" digest. Mitigation: CW's named audit checks each lane's digest is
  sourced from a distinct entry method; the TS lane runs in a separate process.

---

*GN-4 — the value fabric. Four measured lanes, one surface, parity as the close gate. The
stop-early skip engine wakes; the typed sink goes public; the tape crosses to TypeScript; the
Value API is retired for the union.*
