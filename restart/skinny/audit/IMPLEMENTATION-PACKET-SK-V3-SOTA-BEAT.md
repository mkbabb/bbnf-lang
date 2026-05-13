# Implementation packet SK-V3 SOTA-BEAT

Date: 2026-05-12  
Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`  
Authority: `restart/skinny/audit/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`

## 0. Close condition

The expanded corpus close is satisfied on the M5 Max host (arm64) without
any `CollapsedStage` participation. The packet is complete only when both
the parse/tape plane and the direct-to-struct workload are above SOTA within
the skinny bounds:

```sh
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Required report:

- historical triad remains passing;
- expanded corpus no longer has outcome G rows;
- `twitter`, `random`, `unicode_mixed`, and `unicode_basic` no longer show a
  substrate/runtime G miss;
- direct-to-struct rows do not emit `N-direct`;
- `random` and Unicode-heavy rows no longer show `parse_value_at` as dominant
  samply leaf;
- `update-center` no longer shows sparse-flag capacity/allocation growth as a
  top cluster;
- string/Unicode and number materialization gates pass;
- native sidecar rows report yyjson, simdjson C++, and asmjson where the host
  hardware can run them, with strictness plane recorded — **these are
  comparator-only**; the SK-V3 close does not require BBNF-generated
  `CollapsedStage` code on x86_64.

The §8a primitive layer is part of the SK-V3 close, but primitive admission is
not itself a close verdict. As of HEAD `74406332` + `9eef728c`, the two-layer
Layer 1 vocabulary canon is recorded and `BYTE_CLASS_FROM_EQ_SET_64` has an
end-to-end scalar/aarch64/x86/checkasm skeleton path. The measured M5 Max
Wave 0/1 result leaves the parse/tape plane with G rows and the full gate at
`N-direct / NoGo`: the aarch64 Class A/B primitives are admitted, strict
checkasm is green, and the active 16-byte tiny-string parser route is disabled
because it regressed `twitter`; the direct workload is correctness-green but
throughput-red on 11 of 17 corpora against sonic-rs direct after the
UTF-validation and integer-classification redress. §8b
`CollapsedStage` remains a successor-tranche dispatch even after Zen 4 silicon
access lands.

## 1. Non-negotiables

| Rule | Gate |
|---|---|
| No new BBNF directive | `rg -n "@(simd|runtime|backend|shape|asm)" grammars restart/skinny` returns no new grammar surface. |
| No new BIR variant | BIR snapshot count remains the ARCH §7.2 20-variant shape. |
| No parallel substrate | Runtime retains one tape/event projection or direct sink. Mask streams are transient. |
| No JSON-specific generic crate code | Lock 14 lint stays green; grammar names appear only in generated/runtime grammar modules or bench fixture names. |
| Scalar reference per primitive | `cargo test -p bbnf-simd --release`; primitive parity tests run before throughput claims. |
| Profiles are first-class | every SOTA claim has samply/perf profile paths and c/B. |
| **Phase 4 (`CollapsedStage`) is NOT part of the SK-V3 close** | The SK-V3 close requires the expanded SOTA-BEAT gate on M5 Max (arm64); it does not require any x86_64 path. If Zen 4 silicon access does not materialise within the SK-V3 cap, Phase 4 (Wave 6b in this packet) is deferred to a successor tranche with its own plan document, **NOT** folded into a successor wave of SK-V3. This guards against the AW-V auto-derive failure-mode recurrence (`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md:187-188`). Per the V9.5 PSI excavation cohort (2026-05-13): codegen-emitted Rust automaton is the proximate cause of the prior 1000-commit failure; LLVM cannot compile-away the dispatch overhead the way it does for recursive descent. `CollapsedStage` as Rust codegen is structurally that same failure shape. The shape is preserved in the taxonomy (cost model can derive it) but its dispatch is gated by NASM authoring + Zen 4 silicon + per-grammar parity harness. |
| **Wave 3 substrate ships with same-wave consumer** | Per `LESSONS-LEARNED.md:17-26` (2026-04-29 canonical rule). Every wave exit cites a samply leaf at a consumer fn with ≥X% self-time; wave exit blocks if no consumer cite. |

## 2. Wave 0: authority and profiling preflight

Owner paths:

- `skinny/RESULTS.md`
- `skinny/profile/reprofile-2026-05-12/`
- `skinny/profile/wave2-{asm,pmu,capacity,prototype}/`
- `skinny/profile/native-sidecars/`
- `restart/skinny/BENCH.md`

Tasks:

1. Preserve the authority split: historical triad pass, current parse-G rows,
   and current `N-direct / NoGo` direct workload.
2. Add the three fresh profile rows to the local profile report or results notes.
3. Confirm release profile:

```sh
cargo build --release -v 2>&1 | grep -E -- '-C lto=(fat|true)|-C codegen-units=1'
```

### Preflight fix items (block all Wave 1 dispatch until cleared)

**P0.1 — adopt tape-capacity Plan D as production default.**

Status: **LANDED.** Plan D (`GrowOnly`) is the production default after Wave
0/1; keep the rejected-route ledger for A/B/C.

Wave 2 Agent 6 evidence (`profile/wave2-capacity/CAPACITY-REPORT.md`): plan D
(`Vec::with_capacity(256)` + geometric grow) wins +4.8% on `random`, +10.2% on
`github_events`, with 23–64% capacity reclamation across the four measured
corpora. The sampled heuristic over-reserves by 2.53× and is overfit to
`update-center`'s 4 KiB prefix; plan D lands at 1.87× — tightest of the four
probes. Pre-scan cost (1.4% self-time on plan A) disappears.

Files:

- `skinny/crates/runtime/src/tape/assembler.rs` — keep `CapacityPlan` enum
  during the rollout tranche, switch `json_structural_capacity_for` default arm
  to `CapacityPlan::GrowOnly`. Delete `json_structural_capacity` (sampled
  heuristic) and `sparse_flag_capacity` next tranche.
- `skinny/crates/runtime/src/grammars/json/parser.rs` — env selector stays
  available behind `cfg(feature = "wave2-probe")` for one tranche so the
  rejected-route table can be re-derived without rebuilding the probe binary.
- `skinny/crates/runtime/src/tape/mod.rs` — re-export `CapacityPlan`.

Rejected routes recorded in `skinny/REDRESS.md`: plan A (overfit prefix), plan
B (full scalar pre-scan costs 2.3× throughput), plan C (one-shot SIMD pre-scan
discards the position vector — re-examined after Wave 1 event-cursor lands).

**P0.2 — fix `escape_mask_64` NEON correctness bug.**

Status: **LANDED / STRICT GREEN.** `BBNF_SIMD_STRICT=1 cargo test -p
bbnf-simd --release --test checkasm_parity` returns zero divergences,
including the adversarial handoff sweep and the new aarch64 Class A/Class B
admission tests.

Wave 2 Agent 5 evidence (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md` §d):
NEON `escape_mask_64` boundary handoff to `scan_json_tail` does not match the
scalar tail's `escaped` flag semantics on random-noise inputs. Minimal
adversarial repro: xorshift seed `0xCAFEF00DBAADF00D`, iter 0, 128-byte
JSON-pool buffer; scalar emits position 126 (closing quote of a 1-char escaped
string at EOF), NEON does not. Root cause is state-handoff confusion between
`escape_mask_64`'s `new_carry` (does the next stripe start mid-escape?) and
`scan_json_tail`'s `escaped` arg (is the *current* byte under an escape?).

Files:

- `skinny/crates/bbnf-simd/src/lib.rs` — fix is either (a) consume the
  trailing 16-byte 4×chunk of every input under the SIMD branch so the tail
  never sees mid-escape state, or (b) translate `bs_carry → escaped` correctly
  by walking the residual mask.
- `skinny/crates/runtime/src/grammars/json/parser.rs` — re-verify any
  downstream consumer of `escape_mask_64` boundary semantics.

This previously blocked **all SOTA-BEAT bench claims**. It is now cleared, but
the parse plane still has G rows and the full gate remains `N-direct / NoGo`
for throughput reasons.

**P0.3 — confirm Lock 15 (`lto=fat`) already active.**
Wave 2 Agent 3 evidence (`profile/wave2-asm/PROFILE-REPORT.md` Appendix C):
`parse_value_at` is a single 7,304-byte hot function (RVA `0x2460..0x40e8`,
1,826 mnemonics) — the i-cache budget is already met under the current
workspace `[profile.release]` (`opt-level=3`, `lto=thin`, `codegen-units=1`,
`debug=true`). The `match_tiny_plain_string` scalar loop appears duplicated at
PCs `0x2734` (key) and `0x3158` (value) via inliner duplication; this is the
intended LTO-fat shape and is the precondition for the single-source Class A
fix below to land both call sites.

```sh
cargo build --release -v 2>&1 | grep -E -- '-C lto=(fat|true)|-C codegen-units=1'
```

Exit gate:

- profile artifacts exist for `random`, `unicode_escapes`, and `update-center`;
- `skinny/RESULTS.md` does not collapse the split into one green verdict;
- P0.1 plan-D adoption landed and recorded in REDRESS.md;
- P0.2 `BBNF_SIMD_STRICT=1` returns zero divergences on the adversarial repro;
- P0.3 LTO/CGU flags confirmed present in the release-build invocation.

## 3. Wave 1: typed event cursor over the tape projection

Owner paths:

- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/grammars/json/{parser.rs,generated.rs,view.rs,value.rs}`
- `skinny/crates/codegen/src/lower/rust.rs`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/bbnf-simd/src/aarch64/` (Class A + Class B NEON kernels)
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` (admission gate)

Current defect:

- `generated::attach_structural_index` is a no-op.
- generated parse still walks raw source bytes with `cursor`, `skip_ws`, and
  `parse_value_at`.
- `twitter`, `random`, `unicode_mixed`, and `unicode_basic` are current parse
  G rows; `random` and Unicode-heavy profiles remain `parse_value_at`-heavy.

### Two-pathology-class diagnosis (Wave 2 Agent 2)

`profile/wave2-asm/PROFILE-REPORT.md` partitions the five failing corpora into
two distinct pathology classes — no single kernel closes all five:

| Class | Corpora (3-of-5 / 2-of-5) | Dominant band | Source lines |
|---|---|---|---|
| **A: tiny_string scalar loop** | github_events, update-center, random | `match_tiny_plain_string scalar loop` (key 31–35.5%, value 17–24%) | `generated.rs:161–172` |
| **B: \uXXXX hex decode** | unicode_escapes, y_string_unicode | `parse_pair / colon / key escape recovery` (69.6% / 35.1%) + `\uXXXX hex decode` (22.2% / 13.9%) | `parser.rs:78–113` + unescape |

Both classes share the same root cause: scalar inner loops paid 8 bytes at a
time, inlined into `parse_value_at`, sharing i-cache with the structural hot
loop. The Class A loop is duplicated at PCs `0x2734` (key) and `0x3158`
(value) via LTO-fat inliner duplication; a single source-level fix lands both
call sites.

Implementation:

1. Introduce `runtime::tape::EventCursor`:

```rust
pub struct EventCursor<'t, 'i> {
    tape: &'t Tape<'i>,
    cursor: u32,
}

impl<'t, 'i> EventCursor<'t, 'i> {
    pub fn byte(&self) -> u8;
    pub fn offset(&self) -> usize;
    pub fn flags(&self) -> OffsetFlags;
    pub fn advance(&mut self);
    pub fn expect(&mut self, byte: u8) -> Result<usize, ParseError<'i>>;
}
```

2. Lower `Alt { mode: Dispatch }` on `OffsetTape` rules as `match
   event.byte()` rather than `match source[pos]`.
3. Move whitespace boundary handling into the event cursor: JSON whitespace is
   skipped while building the event stream, not by every parse function.
4. Keep string/number primitive source reads inside `parse-that/string` and
   `parse-that/number`; those are the only legal source-byte rescans.
5. Preserve `JsonRoot` and `DocumentView` public shape.

### Class A primitive — NEON `match_tiny_plain_string` 16-byte class check

Target site: `skinny/crates/runtime/src/grammars/json/generated.rs:161–172`.
Replaces the scalar 8-byte loop whose hot body is the `cmp #0x22` + `cmp
#0x5c` + `cmp #0x20` + `b.hs` cascade with the `rbit`/`clz` SWAR-tail at
`0x2754..`. Single source-level edit lands both inlined call sites
(`0x2734` key, `0x3158` value).

Kernel shape: load 16 bytes with `vld1q_u8`, build the three class masks
(`vceqq_u8` against `0x22`, `vceqq_u8` against `0x5c`, `vcltq_u8` against
`0x20`) under k-mask-style `vorrq_u8` fusion, compressed movemask via
`vshrn_n_u16<4>`, find-first via `tzcnt`-equivalent on the low 64-bit lane.
Tail handled by the existing SWAR path.

Measured Wave 0/1 status: admitted under strict checkasm, but not active in
Track 1/Track 2. Routing the 16-byte helper through the parser regressed
`twitter` by roughly 25% on both tracks, so the active route stays on the
8-byte scalar tiny recognizer until event-cursor/codegen extraction changes
the call shape.

### Class B fix — NEON TBL-driven `\uXXXX` hex decode

Target site: `unescape_json_string` (called from `parser.rs:78–113`).
Replaces the scalar `sub #0x30` + `sub #0x61` + `sub #0x57` + `csel` cluster
(× 4 nibbles) with `orr` combine — currently visible at PCs `0x2ad4` /
`0x2aa4` / `0x2b04` in the `y_string_unicode` asm dump.

Kernel shape: LUT-driven hex decode using `vqtbl1q_u8` — load the 4 nibble
bytes contiguously, single-shuffle through a 16-byte LUT mapping
`'0'..'9' | 'a'..'f' | 'A'..'F'` to nibble values (and `0xFF` sentinel for
invalid), then `vshl` + `vorr` to pack into a `u16`. 3 ops/nibble vs 11.

Targets: unicode_escapes, y_string_unicode. Original projection was a 2–3×
speedup on escape-heavy corpora. Wave 0/1 admits the primitive under strict
checkasm, but the expanded close still depends on the event-cursor /
`parse_value_at` work and a non-regressing parser route.

### Admission gate (mandatory before any bench claim)

Both kernels MUST pass differential parity through the checkasm harness:

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
```

Harness location: `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`. Wave
0/1 strict mode is green. Bench claims now require both green parity and a
non-regressing parser route; the 16-byte tiny-string route failed that second
condition on `twitter`.

Exit gate:

```sh
cargo test -p runtime --release json
cargo run -p xtask --release -- check-json
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o profile/reprofile-sk-v3/random.event_cursor.profile.json.gz \
  ./target/release/profile-lazy 50000 test_data/random.json
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o profile/reprofile-sk-v3/unicode_escapes.event_cursor.profile.json.gz \
  ./target/release/profile-lazy 30000 test_data/unicode_escapes.json
```

Pass condition: `parse_value_at` falls below 20% self-time on all five
failing corpora, or the wave records the exact remaining leaf that replaced
it. The next measured target is typed event-cursor consumption over the tape
projection; Class A/B primitive admission alone no longer counts as a close.

## 4. Wave 2: capacity and tape-builder policy

Owner paths:

- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/runtime/src/tape/offsets.rs`
- `skinny/crates/bbnf-bench/`

Status: **Plan D adoption landed in Wave 0/1 (P0.1).** This wave records the
probe ledger, the rejected-route table, and the post-event-cursor
re-examination of plan C.

Current defect (now characterised, no longer open as a blocker):

- The `update-center` "alloc-path bottleneck" framing is **partially refuted**
  (`profile/wave2-capacity/CAPACITY-REPORT.md` §2). Dominant cost is
  `parse_value_at` itself (97.6% self-time on plan A, 98.7% on plan D), not
  realloc. The sampled-plan pre-scan does cost 1.4% self-time and is removed
  under plan D.

Implementation (plan D landing tranche):

1. Adopt plan D (`Vec::with_capacity(256)` + geometric grow) as the production
   default. Probe table:

   | Plan | Strategy | Pre-scan | Initial capacity |
   |---|---|---|---|
   | A `sampled` | 4 KiB-prefix heuristic | sample 4 KiB | `(emitted × len × 5)/(sample × 4) + 8` |
   | B `exact` | full-source scalar count of `{}[],:"` | full source | `exact + 8` |
   | C `oneshot-simd` | re-run `bbnf_simd::scan_json_structurals` | full SIMD scan | `count + 8` |
   | D `grow-only` | start at 256, geometric `Vec::reserve` | none | 256 |

2. Cross-corpus throughput (30 000 iters, Mbps):

   | Corpus | A:sampled | B:exact | C:simd | D:grow | D vs A |
   |---|---:|---:|---:|---:|---:|
   | update-center | 19 746 | 8 010 | 10 884 | 16 429 | −16.8% |
   | random | 12 185 | 6 941 | 8 420 | **12 764** | **+4.8%** |
   | unicode_escapes | **15 559** | 7 038 | 11 545 | 13 024 | −16.3% |
   | github_events | 19 915 | 8 467 | 15 563 | **21 948** | **+10.2%** |

   Capacity reclamation (bytes/parse savings, D over A): 26%, 23%, **64%**,
   **62%**. Memory plane in Wave 5 weights this directly.

3. Keep the `CapacityPlan` enum + `BBNF_CAPACITY_PLAN` env selector behind
   `cfg(feature = "wave2-probe")` for one tranche so the rejected-route table
   can be re-derived without rebuilding the probe binary. Delete the sampled
   (`json_structural_capacity`) and sparse-flag (`sparse_flag_capacity`)
   helpers next tranche.

4. Emit capacity plan metadata in each bench row.

5. **Rejected as standalone** (recorded in `skinny/REDRESS.md`):
   - plan A (`sampled`) — overfits update-center 4 KiB prefix; loses 10.2% on
     github_events, 4.8% on random.
   - plan B (`exact` scalar pre-scan) — full-source pre-scan costs ~150
     µs/parse on update-center; 2.3× throughput hit.
   - plan C (`oneshot-simd`) — full SIMD pre-scan costs ~120 µs/parse and
     discards the position vector; **re-examined after Wave 1 lands the event
     cursor**, since plan C's pre-scan cost becomes free once the
     structural-index produced by `scan_json_parse_index` is consumed as an
     event stream.

Exit gate:

```sh
samply record --save-only --unstable-presymbolicate -r 1000 \
  -o profile/reprofile-sk-v3/update-center.capacity.profile.json.gz \
  ./target/release/profile-lazy 75000 test_data/update-center.json
cargo run -p xtask --release -- bench-json --corpus update-center
```

Pass condition: plan D landed as production default; sampled/sparse-flag
helpers are dead-code stubs scheduled for deletion; rejected routes A/B/C
recorded in REDRESS.md; cross-corpus throughput matches the table above
within ±2%. Plan C re-examination ticket filed against the post-Wave-1
re-baseline.

## 5. Wave 3: `bbnf-simd` kernel contract, host-aarch64 first

Owner paths:

- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/bbnf-simd/src/scalar/`
- `skinny/crates/bbnf-simd/tests/`

Implementation:

1. Define grammar-neutral primitives:

```rust
pub struct ByteClassPlan { /* alphabet, verifier, chunk policy */ }
pub struct KernelSet { /* scalar, aarch64, x86 dispatch table */ }
pub trait PrimitiveKernel {
    fn scan(&self, input: &[u8], out: &mut EventSink);
}
```

2. aarch64 kernels:
   - `vqtbl4q_u8` 4-table classifier;
   - pure-register movemask with `vshrn_n_u16` + `vsri` + `zip1`;
   - `vld1q_u8_x4` quad-load;
   - `vextq_u8` cross-chunk byte context;
   - optional `prfm` / `stnp` only after before/after measurement.
3. Scalar/SWAR remains parity reference and fallback.
4. Add `xtask primitive-checkasm` or equivalent primitive parity command.

Exit gate:

```sh
cargo test -p bbnf-simd --release
cargo test -p bbnf-simd --release --tests
cargo run -p xtask --release -- bench-json --structural-only
```

Pass condition: scalar and aarch64 kernels produce identical event streams on
every expanded corpus and JSONTestSuite string pack.

## 6. Wave 4: `parse-that` string, Unicode, and number closure

Owner paths:

- `skinny/crates/parse-that-regex/`
- `skinny/crates/parse-that/` if present; otherwise create the skinny-local
  primitive modules in the existing parser utility crate and document the V1
  move.
- `skinny/crates/runtime/src/grammars/json/view.rs`

Implementation:

1. `parse-that/string`:

```rust
pub enum StringMode { StrictJson, GrammarString, ByteString }
pub struct StringMatch {
    pub raw_start: usize,
    pub raw_end: usize,
    pub flags: StringFlags,
}
```

Flags: `HAS_ESC`, `HAS_CONTROL`, `HAS_NON_ASCII`, `NEEDS_DECODE`,
`UTF8_VALIDATED`.

2. `parse-that/unicode` owns:
   - hex unit read;
   - surrogate pair validation;
   - UTF-8 encode;
   - noncharacter acceptance per RFC 8259.
3. `parse-that/number` owns:
   - raw span scan;
   - digit-block SWAR baseline;
   - `materialize_f64`, `materialize_i64`, `materialize_u64`;
   - exact tests for `-0`, subnormals, overflow, `2^53`, ambiguous rounding,
     exponent boundaries.
4. `parse_bytes` validates UTF-8 once before any view is exposed.
5. View accessors return `Result`/`Cow` and never panic on invalid input.

Exit gate:

```sh
cargo run -p xtask --release -- check-conformance
cargo test -p runtime --release unicode
cargo test -p runtime --release number
cargo run -p xtask --release -- bench-json --workload unicode_string_float
```

Pass condition: JSONTestSuite string pack passes; full-traversal rows are
reported alongside parse-only rows.

## 7. Wave 5: workload gates and direct-to-struct proof

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/RESULTS.md`

Implementation:

Add these bench modes:

| Workload | Required comparator |
|---|---|
| `parse_only` | Track 1, Track 2, sonic-rs, simd-json, serde_json |
| `parse_full_traversal` | Track 1, sonic-rs Value-DOM, simdjson DOM, yyjson |
| `path_lookup` | Track 1 path, sonic-rs pointer, simdjson On-Demand pointer |
| `direct_to_struct` | Track 1 sink-only direct, Track 2 sink-only direct, retained-view parity oracle, sonic-rs serde struct; no `N-direct` rows |
| `unicode_string_float` | Track 1, sonic-rs, simd-json, yyjson sidecar |
| `memory` | peak RSS, offset/event counts, payload bytes |
| `cycles_per_byte` | samply/perf rows |

Exit gate:

```sh
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Pass condition: expanded corpus has no G rows, no `N-direct` direct workload
rows, and no correctness/schema rows fail. The first sink-only digest parser is
landed and correctness-green. Six rows now pass the direct slack; 11 still miss
sonic-rs direct. The remaining direct work is materialization quality inside
the sink (exact float plus Unicode/string primitives), not another retained-view
proof.

## 8. Wave 6: x86_64 strict SOTA path (split into 6a + 6b post-V9.5-excavation)

Per the V9.5 PSI excavation cohort (2026-05-13), this wave splits into two
sub-waves with distinct admission criteria. Wave 6a is unconditional once
x86_64 dev access exists; Wave 6b is deferred to a successor tranche.

### 8.0 Why the split

The asmjson reference (`10.93 GiB/s` Zen 4 AVX-512 DOM) is built from
**hand-written NASM** implementing a 9-state DPDA (per FSM-correctness
audit §d: not a pure FSM — carries `frames_buf[MAX_JSON_DEPTH=64]` +
`open_buf[64]` as a hardware-bounded explicit stack; the 9 states are the
finite-control fragment only). Its complete AVX-512 footprint is 10×
`vpcmpeqb` + 10× `kmovq` + 2× `vpcmpub` + 6× `korq` + 2× `vmovdqu8` + 18×
`tzcnt` per chunk. That is the **entire** instruction inventory of the
published SOTA number.

The user's load-bearing context (2026-05-13): "The automaton overhead that
we implemented directly in Rust not ASM had far too much overhead that the
LLVM optimizer could not compile away — though our recursive descent likely
compiled into an automaton directly, a direct approach with Rust just did
not work." This is the 1000-commit Era V failure mode the V9.5 excavation
surfaced. Codegen-emitted Rust DPDA is structurally that same failure
shape. The two routes that make `CollapsedStage` materially different from
PSI/DTA: (1) hand-written NASM kernel in `bbnf-simd/x86_64/*.asm` (the
asmjson route — major engineering commitment); (2) drop `CollapsedStage`
from this packet and consume the AVX-512 esoterica as primitives inside
`OffsetTape` recursive descent.

This packet adopts the second route for the SK-V3 close (Wave 6a) and
defers the first to a successor tranche (Wave 6b).

### 8a. Wave 6a — AVX-512 primitive admission (unconditional, consumed by OffsetTape)

§8a admits the **bbnf.asm Layer 1 vocabulary itself** (SOTA-BEAT-DESIGN §5.2
two-layer reusable vocabulary): a grammar-neutral primitive set vendored at
`skinny/crates/bbnf-simd/ext/x86/bbnf.asm` riding the BSD-2 `x86inc.asm` macro
substrate at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm`. The vocabulary is
consumed from **any** of `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`
via thin Rust FFI shims (SOTA-BEAT-DESIGN §5.3 grammar-neutrality). The
primitive layer ships independent of `CollapsedStage` admission; §8b
`CollapsedStage` admission, which **consumes** the same primitives plus a
per-grammar `.asm` wrapper, is gated separately. Hence §8a is the foundation
for both routes: OffsetTape on M5 Max NEON can call the admitted Class A /
Class B primitives today, but the measured full gate is still
`N-direct / NoGo`.
The same primitive vocabulary is reused unmodified when §8b lands.

Owner paths:
- `skinny/crates/bbnf-simd/ext/x86/bbnf.asm` (9 grammar-neutral primitives)
- `skinny/crates/bbnf-simd/ext/x86/x86inc.asm` (vendored BSD-2 macro substrate)
- `skinny/crates/bbnf-simd/src/x86_64/avx512_{gfni,kmask,vpclmul,bitalg,vnni}/`
- `skinny/crates/bbnf-simd/src/x86_64/avx_ifma/`
- `skinny/crates/bbnf-simd/src/x86_64/avx2/` (BMI2 + CLMUL128 fallback)

Tasks:

1. Land each Lock 16 AVX-512 primitive as a **standalone `bbnf-simd` kernel**
   with scalar reference + checkasm parity, **not as a component of
   `CollapsedStage`**:
   - `vgf2p8affineqb` (GFNI) — replaces 6× `vpcmpeqb` byte-class compare
   - `_kandn_mask64` / `_kxor_mask64` / `_kxnor_mask64` (k-mask family) —
     keeps masks in `k0..k7` across state hops; saves 4 store+load/chunk
   - `_mm512_clmulepi64_epi128` at 512-bit lane (VPCLMULQDQ) — 4× simdjson's
     128-bit prefix-XOR
   - `vpmadd52luq` / `vpmadd52huq` (AVX-IFMA) — Eisel-Lemire mantissa
   - `vpdpbusd` (VNNI) — digit-block byte×byte→i32 dot-product
   - `vpshufbitqmb` / `vpopcntb` (BITALG) — multi-class classify in 1 µop
2. Consume primitives from the existing `OffsetTape` lowering pattern on
   x86_64 dispatch hubs. **The existing structural-index-driven codegen
   template already has the access pattern**; the primitives accelerate
   the underlying scan and string/number primitives.
3. CPUID dispatch in `bbnf-simd::select_classifier()` selects per-host.
4. AVX-2 + BMI2 fallback (`_mm256_shuffle_epi8`, `_pdep_u64`, `_pext_u64`,
   `_mm_clmulepi64_si128`) for non-VBMI2 hosts.

Exit gate:

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --target x86_64-apple-darwin
cargo test -p bbnf-simd --release --target x86_64-unknown-linux-gnu  # CI
cargo run -p xtask --release -- bench-json --native-sidecars
```

Pass condition: each primitive has scalar reference + checkasm parity returning
0 divergences; x86_64 throughput row in `bench-json --native-sidecars` reports
improvement vs Wave 1+2 x86_64 baseline.

**No `CollapsedStage` admission required for Wave 6a.** The primitives are
useful for `OffsetTape` on x86_64 regardless of whether the FSM backend
ever ships.

### 8b. Wave 6b — `CollapsedStage` backend (DEFERRED to successor tranche)

**Status: DEFERRED to a successor tranche with its own plan document.**

Hard preconditions (all four must hold concurrently before any Wave 6b dispatch):

1. **Zen 4 silicon access** for empirical Phase 4 gate measurement
   (twitter T1 ≥ 7400 MiB/s, hot-leaf count = 1, c/B ≤ 0.45).
2. **Differential parity harness for FSM codegen** lands at
   `skinny/crates/runtime/tests/fsm_codegen_parity.rs` (analogue to
   `bbnf-simd/tests/checkasm_parity.rs`): runs cost-model-derived FSM
   bytecode/asm against a hand-written reference (asmjson on Zen 4) and
   returns 0 divergences across alignment sweep × random-input × ISA
   tier.
3. **`derive_backend_shape` firing matrix audit** records per-grammar
   shape selection across JSON, CSS L4, BBNF-self, Sheets, CSV, EBNF,
   BNF, math. If `CollapsedStage` fires only on JSON, the per-grammar
   god-module signature (Lock 14) routes to an explicit amendment OR
   the JSON-only scope is recorded as accepted in a new INDEX deviation
   ledger row.
4. **Implementation language declared**: if Wave 6b proceeds, the
   `CollapsedStage` kernel is hand-written NASM in
   `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/collapsed_stage.asm`
   compiled via the `nasm-rs` build script already scaffolded in Wave 3;
   **NOT Rust codegen-emitted**. This guards against the codegen-
   emitted-FSM concern (`/tmp/skv3-psi-diff-audit.md` §d). Rust-codegen
   `CollapsedStage` is a non-starter per the V9.5 excavation: the prior
   1000-commit DTA/PSI failure was specifically that LLVM cannot compile
   away the dispatch overhead of a Rust-implemented automaton, while
   recursive descent in Rust compiles into an implicit automaton.
5. **Primitive vocabulary complete on the target ISA**: the grammar-neutral
   primitive set in `skinny/crates/bbnf-simd/ext/x86/bbnf.asm` is complete
   and `checkasm_parity`-passing for the target ISA (`avx2` / `avx512` /
   `neon` as applicable; SOTA-BEAT-DESIGN §5.2 two-layer reusable
   vocabulary, §5.4 size budget). The 9 primitives are:
   `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`,
   `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`,
   `EOB_PAD_CLAMP`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`,
   `FRAME_POP_BOUNDED`. Each ships with a Rust scalar reference (the
   executable specification per SOTA-BEAT-DESIGN §5.2 Layer 3) and a
   `checkasm` differential test in `skinny/crates/bbnf-simd/tests/`.

The clause **"no Rust-codegen-emitted `CollapsedStage` will be admitted"**
stays in force. `CollapsedStage`'s lowering output is a triple
(`rust_caller_shim`, `per_grammar_asm_kernel_file`,
`per_grammar_data_section`). The dispatch spine is shared via
`FSM_DISPATCH_THREADED` from `ext/x86/bbnf.asm` (SOTA-BEAT-DESIGN §5.3
grammar-neutrality). The per-grammar variation lives entirely in the
`.data` section emitted by codegen (classifier LUT + state-transition
LUT). This makes per-grammar `CollapsedStage` authoring tractable:
~150 LOC of grammar-specific `.asm` wrapper per grammar × ISA pair,
calling the shared macros (SOTA-BEAT-DESIGN §5.4 size budget).

If any precondition is unmet at SK-V3 cap, Wave 6b is dropped from this
packet and rescheduled in a successor tranche. The successor tranche
ships its own plan document with explicit precondition resolution.

### 8b.1. Diagnostic — `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`

Wired into `derive_backend_shape` output (cf. precondition 3 firing matrix
audit).

| Field | Value |
|---|---|
| **Trigger** | The cost model selects `CollapsedStage` for a rule, but either (a) the primitive vocabulary in `ext/x86/bbnf.asm` is incomplete on the target ISA, **or** (b) no per-grammar `.asm` author has been declared for that grammar × ISA pair. |
| **Recovery** | Fall back to `OffsetTape` (recursive descent over the structural index) on that rule. |
| **Surface** | Silent at runtime; surfaces in the compile-time diagnostics report. |
| **Severity** | warning (not error) — the grammar still compiles. |

Citation: SOTA-BEAT-DESIGN §5.1 admissibility predicate (two conjuncts:
primitive vocabulary green ∧ per-grammar `.asm` committed); §5.2 two-layer
reusable vocabulary; §5.3 grammar-neutrality; §5.4 size budget.

### Successor-tranche step 1 — adopt asmjson architecture only where admitted

- 9-state DPDA finite-control fragment (object-key / object-colon / object-value / array-value /
  string / number / literal / pre-comma / pre-close);
- bounded explicit frame stack for object/array nesting;
- PC-as-state dispatch via `r10` register holding the next state's code
  address, `jmp r10` per transition (replaces the LLVM switch jump table);
- `tzcnt`-driven seek over the structural-position bitmap;
- msac-style EOB pad on tail (16-byte `0x00` pad after the source buffer so
  the SIMD loop never branches on length);
- instruction inventory starts with asmjson's published Zen 4 DOM path, then
  adds only checkasm-green strict primitives consumed by the grammar kernel.

This is **not** part of the SK-V3 M5 Max close. It is a successor-tranche
`CollapsedStage` authoring path for x86_64 hardware where the grammar, NASM
author, target silicon, and parity harness are all present. Otherwise the cost
model emits `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` and falls back to `OffsetTape`.

### Successor-tranche step 2 — stack esoterica strictly on top

The following esoterica are admissible because they each replace a specific
asmjson primitive with a strictly fewer-µop equivalent:

| Esoterica | Replaces in asmjson | µop count |
|---|---|---|
| **GFNI `vgf2p8affineqb`** | 6× `vpcmpeqb` byte-class classify | 6 → 1 |
| **k-mask family (`kandn`, `kxor`, `kxnor`)** | 4 store+load round-trips/chunk via keeping masks in `k0..k7` | −4 mem ops |
| **AVX-512 VPCLMULQDQ @ 512-bit lane** | 4× simdjson `_mm_clmulepi64_si128` prefix-XOR | 4 → 1 |
| **AVX-IFMA `vpmadd52luq`** | Eisel-Lemire mantissa MAC (asmjson punts to scalar) | scalar → 1 vec µop |
| **VNNI `vpdpbusd`** | digit-block multiply-accumulate for number bodies | 4 ops → 1 |
| **BITALG `vpshufbitqmb`** | per-state classification as data lookup | branch → table |

These are the **only** AVX-512 features beyond the asmjson inventory that we
admit; each must pass `bbnf-checkasm` parity before any bench claim, same
admission gate as Wave 1.

### Successor-tranche step 3 — floor + collapsed-stage backend

- AVX2/BMI2 floor (hosts without AVX-512BW): `_mm256_shuffle_epi8`,
  `_mm_clmulepi64_si128`, `_pdep_u64` / `_pext_u64` compaction probes.
- AVX-512BW strict path: mask-register byte classes, ternary mask fusion,
  strict quote/escape/control validation.
- VBMI/VBMI2 where present: byte-shuffle classifier, compress-store or
  compacted event emission.
- Collapsed-stage backend: state machine over mask stream, emits typed
  events or direct sink, selected by cost model and CPUID, not by directive.

### Projection

Projected ~14 GiB/s on Zen 4 is a **successor-tranche target**, not an SK-V3
close condition. It is retained as a falsifiable x86_64 planning number: a
strict `CollapsedStage` implementation must beat asmjson's 10.93 GiB/s anchor
on equivalent hardware and strictness plane before any SOTA-BEAT claim is
accepted. The M5 Max SK-V3 close remains: no expanded parse G rows and no
`N-direct` rows.

Exit gate:

```sh
cargo test -p bbnf-simd --release --target x86_64-apple-darwin
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
cargo run -p xtask --release -- bench-json --native-sidecars
```

Pass condition for the successor-tranche x86 route: x86 rows report strict
parse-only and full-traversal results against simdjson C++, yyjson, and asmjson
on equivalent hardware. asmjson rows are not accepted unless strictness and
output plane match. A `CollapsedStage` SOTA-BEAT row beats the asmjson
10.93 GiB/s anchor by >=1.20x on Zen 4 (target 14 GiB/s, gate threshold
13.1 GiB/s). Failure does not invalidate `OffsetTape`/`SinkOnly`; it only
falsifies the collapsed-stage x86 path for that grammar/ISA pair.

## 9. Commit/wave discipline

Each wave produces:

- before/after bench rows;
- profile paths;
- rejected-route note when an attempted alternative loses;
- doc update to `skinny/REDRESS.md` and `skinny/RESULTS.md`.

No wave closes on "future phase will fix it." A miss becomes a named blocker,
a measured rejected route, or the next concrete wave input.

## 10. Final report shape

Final SK-V3 close report goes in:

```text
restart/skinny/audit/HANDOFF-SK-V3-SOTA-BEAT.md
```

Required sections:

1. expanded-corpus outcome table;
2. comparator table: sonic-rs, simd-json, yyjson, simdjson C++, asmjson where
   runnable;
3. workload table, including direct sink outcome rows and any `N-direct`
   failures;
4. hot-leaf table before/after;
5. c/B table before/after;
6. rejected-route ledger (must include: plan A sampled, plan B exact, plan C
   one-shot SIMD, Fix 2 jump-table dispatch, Fix 5 computed-goto);
7. exact changes to fold back into global V1;
8. decision: SOTA-BEAT / SOTA-PARITY / NO-GO.

### Required citations (Wave 2 cohort artefacts)

The final report MUST cite the following artefacts inline alongside every
claim derived from them:

- `skinny/profile/wave2-asm/PROFILE-REPORT.md` — per-corpus top-PC table,
  source-band attribution, two-pathology-class diagnosis (Class A
  tiny_string_loop / Class B hex_decode).
- `skinny/profile/wave2-pmu/` — PMU counter rows for branch/i-cache/L1d/L2
  pressure on the failing corpora.
- `skinny/profile/wave2-capacity/CAPACITY-REPORT.md` — capacity-plan probe
  ledger, plan D adoption evidence, rejected routes A/B/C.
- `skinny/profile/wave2-prototype/` — Wave 1 prototype profile artefacts
  (post-event-cursor re-baseline).
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md` — checkasm parity harness
  description, `escape_mask_64` adversarial repro, admission-gate procedure.
- `skinny/profile/native-sidecars/PROFILE-REPORT.md` — yyjson + simdjson C++ +
  asmjson cross-comparator throughput, hot-leaf attribution, four-corpus
  skinny-already-wins ledger (citm, canada, mesh, unicode_mixed).
