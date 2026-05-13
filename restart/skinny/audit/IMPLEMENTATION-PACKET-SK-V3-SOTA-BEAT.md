# Implementation packet SK-V3 SOTA-BEAT

Date: 2026-05-12  
Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`  
Authority: `restart/skinny/audit/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md`

## 0. Close condition

The packet is complete only when the expanded skinny gate is above SOTA within
the skinny bounds:

```sh
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Required report:

- historical triad remains passing;
- expanded corpus no longer has outcome G rows;
- `random` and `unicode_escapes` no longer show `parse_value_at` as dominant
  samply leaf;
- `update-center` no longer shows sparse-flag capacity/allocation growth as a
  top cluster;
- string/Unicode and number materialization gates pass;
- native sidecar rows report yyjson, simdjson C++, and asmjson where the host
  hardware can run them, with strictness plane recorded.

## 1. Non-negotiables

| Rule | Gate |
|---|---|
| No new BBNF directive | `rg -n "@(simd|runtime|backend|shape|asm)" grammars restart/skinny` returns no new grammar surface. |
| No new BIR variant | BIR snapshot count remains the ARCH §7.2 20-variant shape. |
| No parallel substrate | Runtime retains one tape/event projection or direct sink. Mask streams are transient. |
| No JSON-specific generic crate code | Lock 14 lint stays green; grammar names appear only in generated/runtime grammar modules or bench fixture names. |
| Scalar reference per primitive | `cargo test -p bbnf-simd --release`; primitive parity tests run before throughput claims. |
| Profiles are first-class | every SOTA claim has samply/perf profile paths and c/B. |

## 2. Wave 0: authority and profiling preflight

Owner paths:

- `skinny/RESULTS.md`
- `skinny/profile/reprofile-2026-05-12/`
- `skinny/profile/wave2-{asm,pmu,capacity,prototype}/`
- `skinny/profile/native-sidecars/`
- `restart/skinny/BENCH.md`

Tasks:

1. Preserve the authority split: triad pass, expanded G / NoGo.
2. Add the three fresh profile rows to the local profile report or results notes.
3. Confirm release profile:

```sh
cargo build --release -v 2>&1 | grep -E -- '-C lto=(fat|true)|-C codegen-units=1'
```

### Preflight fix items (block all Wave 1 dispatch until cleared)

**P0.1 — adopt tape-capacity Plan D as production default.**
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

This blocks **all SOTA-BEAT bench claims**: corpus parity passes (17/17) only
because real-world JSON never lands a single backslash immediately before a
stripe boundary inside a string with no following ASCII, but synthetic and
adversarial inputs do. Fix gate is `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd
--release --test checkasm_parity` returning zero divergences.

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
- `random` and `unicode_escapes` profiles are dominated by `parse_value_at`.

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

### Class A fix — NEON `match_tiny_plain_string` 16-byte class check

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

Targets: github_events, update-center, random. Expected ~50–60% throughput
gain on bulk object-shaped corpora.

### Class B fix — NEON TBL-driven `\uXXXX` hex decode

Target site: `unescape_json_string` (called from `parser.rs:78–113`).
Replaces the scalar `sub #0x30` + `sub #0x61` + `sub #0x57` + `csel` cluster
(× 4 nibbles) with `orr` combine — currently visible at PCs `0x2ad4` /
`0x2aa4` / `0x2b04` in the `y_string_unicode` asm dump.

Kernel shape: LUT-driven hex decode using `vqtbl1q_u8` — load the 4 nibble
bytes contiguously, single-shuffle through a 16-byte LUT mapping
`'0'..'9' | 'a'..'f' | 'A'..'F'` to nibble values (and `0xFF` sentinel for
invalid), then `vshl` + `vorr` to pack into a `u16`. 3 ops/nibble vs 11.

Targets: unicode_escapes, y_string_unicode. Expected 2–3× speedup on
escape-heavy corpora; closes the smallest absolute gap on a NO-GO corpus
(unicode_escapes 587 MiB/s vs simdjson 672 MiB/s per
`profile/native-sidecars/PROFILE-REPORT.md` §e).

### Admission gate (mandatory before any bench claim)

Both kernels MUST pass differential parity through the checkasm harness:

```sh
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
```

Harness location: `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`. Adds
the two new kernels as additional `check_parity_at` closures alongside the
existing `classify_chunk` and `escape_mask_64` cells. Bench claims published
before the harness is green for the new kernels are blocked.

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
it. Class A corpora cross 96.6% of sonic-rs. Class B corpora close to within
5% of simdjson C++ on `unicode_escapes`.

## 4. Wave 2: capacity and tape-builder policy

Owner paths:

- `skinny/crates/runtime/src/tape/assembler.rs`
- `skinny/crates/runtime/src/tape/offsets.rs`
- `skinny/crates/bbnf-bench/`

Status: **Plan D adoption staged in Wave 0 (P0.1).** This wave records the
probe ledger, the rejected-route table, and the post-Wave-1 re-examination of
plan C.

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
| `direct_to_struct` | Track 1 direct view, Track 2 direct view, sonic-rs serde struct |
| `unicode_string_float` | Track 1, sonic-rs, simd-json, yyjson sidecar |
| `memory` | peak RSS, offset/event counts, payload bytes |
| `cycles_per_byte` | samply/perf rows |

Exit gate:

```sh
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Pass condition: expanded corpus has no G rows and no correctness/schema rows
fail.

## 8. Wave 6: x86_64 strict SOTA path

Owner paths:

- `skinny/crates/bbnf-simd/src/x86_64/`
- `skinny/crates/runtime/src/backends/collapsed_stage/`
- `skinny/crates/bbnf-bench/native/` or sidecar harness

Framing: **asmjson does not use esoterica.** Its 10.93 GiB/s Zen 4 AVX-512 DOM
anchor is built entirely from a 9-state FSM + PC-as-state via `r10` indirect
jump + `tzcnt`-driven seek + msac-style EOB-pad. The complete AVX-512
footprint is 6× `vpcmpeqb` + 10× `kmovq` + 2× `vpcmpub` + 6× `korq` + 2×
`vmovdqu8` + 18× `tzcnt` per chunk. That is the **entire** instruction
inventory of the published SOTA number. The implication for SK-V3: we adopt
that architecture verbatim and then stack esoterica strictly on top to
outclass it.

### Implementation step 1 — adopt asmjson architecture verbatim

- 9-state FSM (object-key / object-colon / object-value / array-value /
  string / number / literal / pre-comma / pre-close);
- PC-as-state dispatch via `r10` register holding the next state's code
  address, `jmp r10` per transition (replaces the LLVM switch jump table);
- `tzcnt`-driven seek over the structural-position bitmap;
- msac-style EOB pad on tail (16-byte `0x00` pad after the source buffer so
  the SIMD loop never branches on length);
- exact instruction inventory matches asmjson's published Zen 4 DOM path.

### Implementation step 2 — stack esoterica strictly on top

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

### Implementation step 3 — floor + collapsed-stage backend

- AVX2/BMI2 floor (hosts without AVX-512BW): `_mm256_shuffle_epi8`,
  `_mm_clmulepi64_si128`, `_pdep_u64` / `_pext_u64` compaction probes.
- AVX-512BW strict path: mask-register byte classes, ternary mask fusion,
  strict quote/escape/control validation.
- VBMI/VBMI2 where present: byte-shuffle classifier, compress-store or
  compacted event emission.
- Collapsed-stage backend: state machine over mask stream, emits typed
  events or direct sink, selected by cost model and CPUID, not by directive.

### Projection

Projected ~14 GiB/s on Zen 4 = **1.28× asmjson** (10.93 GiB/s anchor).
Esoterica budget is 1 GFNI µop replacing 6 `vpcmpeqb` + 4 memory round-trips
saved via k-mask resident masks + 1 VPCLMULQDQ replacing 4 `pclmulqdq` lanes
on the 512-bit prefix-XOR; the combined floor lift is ≈28% per chunk and the
critical-path latency saving is the lever closing on 14 GiB/s.

Exit gate:

```sh
cargo test -p bbnf-simd --release --target x86_64-apple-darwin
BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity
cargo run -p xtask --release -- bench-json --native-sidecars
```

Pass condition: x86 rows report strict parse-only and full-traversal results
against simdjson C++, yyjson, and asmjson on equivalent hardware. asmjson rows
are not accepted unless strictness and output plane match. SK-V3 row beats
the asmjson 10.93 GiB/s anchor by ≥1.20× on Zen 4 (target 14 GiB/s, gate
threshold 13.1 GiB/s).

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
3. workload table;
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
