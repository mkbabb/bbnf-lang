# Implementation Packet SK-V4 asmjson-beat

Date: 2026-05-13.

Workspace: `/Users/mkbabb/Programming/bbnf-lang/skinny`.

Authority:

- `restart/skinny/audit/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md`
- `skinny/RESULTS.md`
- `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md`
- `restart/ARCHITECTURE.md` §7.3 and §9.2

## 0. Close Condition

SK-V4 is complete only when the strict skinny gate beats the SOTA comparators
inside skinny bounds. Do not close on partial primitive admission, future x86
hopes, or a parse-only win.

Required local M5 Max pass:

```bash
cd /Users/mkbabb/Programming/bbnf-lang/skinny
cargo run -p xtask --release -- check-conformance
cargo run -p xtask --release -- bench-json
cargo run -p xtask --release -- gate-json
```

Required report state:

- historical triad still passes;
- expanded parse matrix has no outcome-G rows;
- direct-to-struct emits no `N-direct`;
- direct Track 1 calls generated runtime/codegen `SinkOnly`, not a
  `bbnf-bench` private parser;
- `parse_value_at` no longer hides all parse cost without a PC-level profile;
- JSON string, Unicode, and number materialization are exact and
  correctness-green;
- sidecar rows for sonic-rs, simdjson C++, yyjson, and asmjson are recorded
  when runnable on the host, with strictness and output plane named.

Required x86 successor pass, when equivalent hardware is available:

- AVX2/AVX-512 primitive rows have scalar/checkasm parity;
- per-grammar `CollapsedStage` rows exist only when a NASM author and grammar
  parity harness exist;
- strict `CollapsedStage` beats asmjson's 10.93 GiB/s anchor by >=1.20x on
  equivalent Zen 4-class hardware before any "asmjson-beat" claim.

## 1. Non-Negotiables

| Rule | Enforcement |
|---|---|
| No new BBNF directives | `rg -n "@(simd|runtime|backend|shape|asm|sink|direct)" grammars restart/skinny` has no new directive surface. |
| No hidden metadata backend selector | No `backend_shape =` grammar metadata key. `LayoutFacts.backend_shape` is cost-model-derived. |
| No new BIR variant | Use existing `Alt { Dispatch }`, `TapeEmit`, `DirectBuild`, `CallHost`, and surrounding nodes. |
| No parallel substrate | Mask streams are transient. Retained APIs seal `OffsetTape`/`EventTape`; direct-only APIs use `SinkOnly`. |
| No JSON code in generic primitive crates | `bbnf-simd` and `parse-that` expose grammar-neutral primitives only. |
| Scalar reference per primitive | Every SIMD/ASM primitive has scalar executable spec and checkasm parity before use. |
| Same-wave consumer | A primitive or ASM wrapper lands only with the generated/runtime consumer that exercises it. |
| Profiles first | Every SOTA claim cites profile path, c/B or Mbps, and affected corpus rows. |

## 2. Wave 0: Authority Re-Assay

Owner paths:

- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/profile/reassay-skv4-2026-05-13/`
- `restart/skinny/BENCH.md`

Tasks:

1. Re-run the full gate and direct workload.
2. Re-run fresh Samply profiles for the four current parse-G rows and four
   representative direct rows.
3. Add no-inline/PC-level `parse_value_at` attribution mode so parse profiles
   do not collapse into one fused symbol.
4. Confirm current Plan D grow-only capacity and strict checkasm remain green.

Suggested commands:

```bash
cargo build --release -p bbnf-bench --bin profile_direct
cargo build --release -p xtask --bin profile-lazy
cargo run -p xtask --release -- primitive-checkasm
cargo run -p xtask --release -- gate-json --advisory
```

Exit gate:

- `skinny/RESULTS.md` still records the measured split honestly;
- profile report names current parse-G and direct misses;
- any changed failure set updates `BENCH.md` and `REDRESS.md` in the same wave.

## 3. Wave 1: Generated `SinkOnly`

Owner paths:

- `skinny/crates/passes/`
- `skinny/crates/codegen/src/lower/`
- `skinny/crates/codegen/src/json_templates/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Current defect:

The timed direct Track 1 path is still bench-owned. It proves the sink idea,
but it does not prove the BBNF compiler can generate direct typed emission.

Implementation:

1. Add runtime grammar API entrypoints:
   - `parse_direct_digest(input: &str) -> Result<JsonDirectDigest, Error>`
   - later typed struct sinks use the same generated machinery.
2. Lower existing BIR `DirectBuild` to direct field writes when
   `LayoutFacts.backend_shape == SinkOnly`.
3. Keep retained view walk only as an untimed parity oracle.
4. Make `bbnf-bench` call generated runtime Track 1 and independent hand-coded
   Track 2 over the same runtime sink/event traits.
5. Delete or demote any private bench parser from the Track 1 path.

Exit gate:

- direct Track 1 symbol paths include generated runtime/codegen modules;
- direct correctness still `track1=track2=serde`;
- at least one currently red direct row crosses the sonic-rs 1.10 time slack,
  or the profile names the exact primitive blocker.

## 4. Wave 2: Exact Direct Materializers

Owner paths:

- `skinny/crates/parse-that-regex/src/`
- `skinny/crates/bbnf-simd/src/scalar/`
- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Current evidence:

- Direct `twitter`, `random`, and `unicode_mixed` are string-bound.
- Direct `numbers` is exact-number-bound.
- Direct `raw.parse::<f64>()` was tried and rejected on parity.

Implementation:

1. Number primitive:
   - exact raw-span classifier;
   - integer fast path for `i64`/`u64`;
   - exact f64 materialization matching serde/sonic on `-0`, subnormals,
     overflow, exponent boundaries, and `2^53`;
   - no lossy shortcut.
2. String primitive:
   - borrowed ASCII/no-escape path;
   - Unicode and escape path with scalar spec;
   - noncharacter scalar values accepted per RFC 8259;
   - invalid UTF-8 rejected at scan boundary, not view access.
3. Direct sink integration:
   - direct digest never decodes strings that the output does not consume;
   - when it does consume, it uses the same exact primitive as retained views.

Exit gate:

- `numbers`, `canada`, `mesh`, `unicode_mixed`, `unicode_escapes`,
  `unicode_basic`, and `y_string_unicode` direct rows improve or name exact
  residual blockers;
- JSONTestSuite string and number packs pass;
- no view accessor panics on invalid input.

## 5. Wave 3: Parse Hot-Hub Attribution And EventCursor Lowering

Owner paths:

- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/lower/rust.rs`
- `skinny/crates/codegen/src/json_templates/`

Current evidence:

Fresh profiles collapse current parse misses into `parse_value_at` at
~97-100% self-time. That is insufficient to prescribe a kernel.

Implementation:

1. Add no-inline diagnostic build or profile mode for:
   - dispatch byte read;
   - whitespace boundary;
   - container open/close;
   - string primitive entry;
   - number primitive entry;
   - literal verification;
   - tape emit / cursor advance.
2. If source-byte dispatch is the top residual cost, introduce generated
   `EventCursor` consumption for `OffsetTape`:
   - `event.byte()`
   - `event.offset()`
   - `event.flags()`
   - `event.advance()`
   - `event.expect(byte)`
3. Do not reintroduce a sidecar prepass. The cursor consumes the retained tape
   projection.

Exit gate:

- `parse_value_at` no longer hides the current parse-G rows without PC-level
  explanation;
- if `EventCursor` lands, at least one parse-G row crosses S or the report
  names the replacement leaf;
- Track 1 and Track 2 remain close, proving codegen still rides the substrate.

## 6. Wave 4: Same-Wave SIMD/ASM Primitive Consumption

Owner paths:

- `skinny/crates/bbnf-simd/src/scalar/`
- `skinny/crates/bbnf-simd/src/aarch64/`
- `skinny/crates/bbnf-simd/src/x86_64/`
- `skinny/crates/bbnf-simd/ext/x86/bbnf.asm`
- `skinny/crates/bbnf-simd/tests/`

Implementation:

1. Admit only primitives whose scalar spec and checkasm pass.
2. Consume them in the same wave through generated parse/direct paths.
3. On M5 Max, prioritize:
   - byte-class from table/equality set;
   - next-set-bit / movemask;
   - branchless `\uXXXX` hex decode;
   - digit-block classification/materialization;
   - plain-string block scan only when Wave 3 proves the call shape is right.
4. On x86, complete Layer 1 macro bodies without claiming collapsed-stage
   throughput.

Exit gate:

- `cargo run -p xtask --release -- primitive-checkasm` passes;
- changed primitive appears in a current hot path and moves a measured row;
- failed or regressed primitive is recorded in `skinny/REDRESS.md`.

## 7. Wave 5: Strict Workload Matrix

Owner paths:

- `skinny/crates/bbnf-bench/`
- `skinny/RESULTS.md`
- `skinny/profile/`
- `restart/skinny/BENCH.md`

Workloads:

- `parse_only`
- `parse_full_traversal`
- `path_lookup`
- `direct_to_struct`
- `unicode_string_float`
- `memory`
- `cycles_per_byte`

Requirements:

1. Every row has Mbps, ns/iter, c/B when possible, memory, arena counters, and
   strictness plane.
2. Sidecars are named by API:
   - sonic-rs `Value` / typed direct;
   - simdjson C++ DOM / On Demand where applicable;
   - yyjson inlined DOM;
   - asmjson SWAR/AVX-512 and strict/permissive mode.
3. Parse-only wins cannot hide direct or full-traversal failures.

Exit gate:

- no parse G rows;
- no `N-direct`;
- no correctness/schema/SIMD parity failures;
- `skinny/RESULTS.md` becomes the final SK-V4 gate authority.

## 8. Wave 6: x86 `CollapsedStage` Successor Route

This wave is conditional on equivalent x86_64 hardware. It is not required for
the M5 Max SK-V4 local close.

Owner paths:

- `skinny/crates/bbnf-simd/ext/x86/bbnf.asm`
- `skinny/crates/bbnf-simd/src/x86_64/`
- generated grammar `.data` tables
- per-grammar NASM wrappers

Implementation:

1. Complete Layer 1 macro bodies:
   - `BYTE_CLASS_FROM_TABLE_64`
   - `BYTE_CLASS_FROM_EQ_SET_64`
   - `BITMAP_PREFIX_XOR_64`
   - `BITMAP_NEXT_SET_BIT`
   - `BULK_EMIT_COMPRESSED`
   - `EOB_PAD_CLAMP`
   - `FSM_DISPATCH_THREADED`
   - `FRAME_PUSH_BOUNDED`
   - `FRAME_POP_BOUNDED`
2. Generate grammar classifier and transition `.data` tables from Grammar IR.
3. Hand-author `json_collapsed.asm` only if the cost model selects
   `CollapsedStage` and the author/parity harness exists.
4. Start from asmjson's minimal instruction shape:
   - AVX-512BW equality masks;
   - k-mask OR/reduction;
   - `kmovq`;
   - `tzcnt`;
   - EOB padding;
   - bounded frame stack.
5. Add esoterica only when proven:
   - k-mask arithmetic;
   - VPCLMULQDQ-512;
   - VBMI/VBMI2;
   - BITALG;
   - VNNI digit blocks;
   - IFMA mantissa helpers;
   - GFNI only for generated/proven affine class encodings.

Exit gate:

- asmjson strictness and output plane match;
- strict x86 row beats asmjson 10.93 GiB/s by >=1.20x on equivalent hardware;
- if any precondition is missing, emit `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` and
  leave `OffsetTape` as the valid shape.

## 9. Final Report

Final SK-V4 report lands at:

```text
restart/skinny/audit/HANDOFF-SK-V4-ASMJSON-BEAT.md
```

Required sections:

1. full 17-row parse matrix before/after;
2. full direct-to-struct matrix before/after;
3. sidecar comparator table;
4. strictness/output-plane table;
5. hot-leaf and c/B table;
6. primitive admission table;
7. rejected-route ledger;
8. exact skinny/global spec changes;
9. final decision: SOTA-BEAT / SOTA-PARITY / NO-GO.

No wave closes on "future phase will fix it." A miss becomes a named blocker,
a rejected route with evidence, or the next concrete wave input.
