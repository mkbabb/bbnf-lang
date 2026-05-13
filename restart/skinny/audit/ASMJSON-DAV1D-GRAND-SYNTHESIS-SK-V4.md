# SK-V4 asmjson / dav1d Grand Synthesis

Date: 2026-05-13.

Authority inputs:

- `skinny/RESULTS.md` current full gate.
- `skinny/REDRESS.md` accepted/rejected implementation ledger.
- `skinny/profile/reassay-skv4-2026-05-13/PROFILE-REPORT.md`.
- `skinny/profile/native-sidecars/PROFILE-REPORT.md`.
- `restart/skinny/audit/V9.5-PSI-EXCAVATION/*.md`.
- `restart/ARCHITECTURE.md` §7.3 and §9.2.
- `restart/MASTER-PLAN.md` §13.
- Primary external anchors:
  - asmjson docs: <https://docs.rs/asmjson/latest/asmjson/>
  - simdjson On Demand: <https://simdjson.org/api/0.8.0/md_doc_ondemand.html>
  - simdjson tape: <https://simdjson.org/api/2.0.0/md_doc_tape.html>
  - sonic-rs README: <https://github.com/cloudwego/sonic-rs>
  - yyjson README: <https://github.com/ibireme/yyjson>
  - FFmpeg checkasm: <https://www.ffmpeg.org/doxygen/7.1/checkasm_8h.html>
  - FFmpeg/VLC/dav1d assembly process: <https://archive.fosdem.org/2023/schedule/event/om_vlc/attachments/slides/5695/export/events/attachments/om_vlc/slides/5695/FFmpeg_VLC_js.pdf>

## 1. Current Measured Verdict

The current skinny is not SOTA-BEAT-ready.

`skinny/RESULTS.md` records:

- parse/tape hard G rows: `twitter`, `random`, `unicode_mixed`,
  `unicode_basic`;
- direct-to-struct correctness green;
- direct-to-struct throughput green on 6 of 17 rows and red on 11 of 17;
- overall verdict: `N-direct / NoGo`.

The historical triad pass remains real evidence: lazy-offset tape plus local
hot-path redress validated the substrate direction for `twitter`,
`citm_catalog`, and `canada`. It is not the close condition anymore. The
expanded corpus plus direct workload is the current close condition.

## 2. What We Have Done Hitherto

### 2.1 Implementation work

1. Built a runnable `skinny/` Cargo workspace with runtime, codegen, grammar,
   passes, `bbnf-simd`, `parse-that-regex`, fixtures, xtask, and bench crates.
2. Implemented generated JSON parsing and a hand-coded Track 2 parser against
   the same substrate.
3. Converted bench reporting to Mbps and persisted `skinny/RESULTS.md`.
4. Replaced eager `TapeToken` JSON hot-path materialization with lazy-offset
   tape: `Box<[u32]>` offsets plus sparse flags and zero JSON payload arena
   writes.
5. Removed parallel parser sidecars: parse consumes and seals one tape/event
   projection.
6. Implemented direct-to-struct digest workload and later removed the retained
   view walk from timed BBNF direct rows.
7. Enforced gate behavior: `xtask gate-json` hard-fails on `N-direct / NoGo`;
   `--advisory` is explicit.
8. Added `bbnf-simd` scalar/aarch64/x86 module scaffolding and strict
   checkasm-style primitive tests.
9. Landed Layer 0 `x86inc.asm` / `x86util.asm` substrate and Layer 1
   `bbnf.asm` vocabulary contracts.
10. Implemented `BYTE_CLASS_FROM_EQ_SET_64` end-to-end as scalar,
    aarch64, x86 shim/asm, and checkasm test.
11. Added string/Unicode aarch64 primitive work and caught an
    `escape_mask_64` boundary bug through strict parity.
12. Added `profile_direct` and `profile-lazy` binaries for Samply.
13. Profiled skinny, yyjson, simdjson C++, sonic-rs, serde_json, RapidJSON,
    and asmjson sidecars where runnable on M5 Max.
14. Rejected invalid or losing routes: function-pointer dispatch table,
    12-byte token width churn, pair-token fusion, structural-index sidecar
    parser prepass, sidecar eventcursor prototype, sampled/exact/one-shot
    capacity prescans, generic SWAR whitespace skipper, separator elision,
    direct `raw.parse::<f64>()`, and active 16-byte tiny-string routing in the
    current parser shape.

### 2.2 Spec work

1. Added Locks 15 and 16: build-profile/i-cache discipline and SIMD/ASM
   admissibility.
2. Generalized the substrate as a five-shape `BackendShape` taxonomy:
   `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`.
3. Clarified that structural projection is the tape, not a retained sidecar.
4. Clarified that direct builders do not bypass the substrate event stream;
   retained direct views and `SinkOnly` are two materializations of the same
   accepted events.
5. Added `G-fusion-quality`, `N-direct`, and related bench outcome classes.
6. Added UTF-8/noncharacter correctness diagnostics.
7. Added `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` as the safe fallback diagnostic.
8. Demoted asmjson-class collapsed stage from SK-V3 close to a successor
   x86_64 authoring path.

## 3. Validated Original Plan Items

### Lazy tape / structural projection union

Validated. Lazy-offset tape reduced materialization bytes and made the original
triad pass. The structural projection is the retained tape when document APIs
need a queryable view.

### Codegen overhead separability

Mostly validated for the current generated JSON. Track 1 and Track 2 move
together on many red rows. The remaining misses are substrate consumption,
direct materialization, and primitive quality, not a gross generated-vs-hand
gap.

### Expanded corpus requirement

Validated strongly. The original triad would have hidden the current hard rows
and the direct blocker.

### Direct-to-struct as a required workload

Validated. Parse-only wins cannot ratify the V1 direct-to-struct premise.
Current direct rows are correctness-green and still `N-direct / NoGo`.

### Lock 15 inlining/i-cache discipline

Validated. yyjson shows one huge hot leaf without explicit SIMD; skinny's own
hot hub is under the i-cache budget. The next work is not "more functions";
it is better fused generated shape and exact primitives inside that hot hub.

### Lock 16 primitive admission discipline

Validated. The `escape_mask_64` bug shows why scalar reference and checkasm
are non-negotiable. Primitive admission without a same-wave consumer is not a
SOTA claim.

## 4. Invalidated Or Narrowed Items

### Eager token stream as SOTA-beat substrate

Invalidated for JSON-class SOTA. It hit a measured ceiling and lost to
lazy-materialization competitors. Eager tape remains valid for recovery,
layout, parse-time host decode, and overlapping first sets.

### Cost model as aspirational

Invalidated. Materialization shape, capacity policy, primitive selection,
generated direct sink, and exact scalar materialization are load-bearing.

### Dispatch table as the missing win

Invalidated. The first row was an accidental duplicate; the real
function-pointer table regressed. LLVM-owned `match`/jump-table lowering stays
canonical outside admitted `CollapsedStage` NASM.

### Eventcursor sidecar prototype

Invalidated. A mask/LUT producer bolted in front of unchanged `parse_value_at`
regressed and grew the hot hub. Event cursor must be the lowering boundary,
not a parallel prepass.

### Primitive parity as sufficient

Invalidated. A primitive can be correct and still not help if the call shape,
inlining, or workload route is wrong.

### asmjson as an M5 Max close condition

Invalidated. asmjson's headline is AVX-512BW x86_64; on M5 Max the runnable
path is SWAR. The M5 close is against sonic-rs/simdjson/yyjson sidecars on
this host; asmjson-class `CollapsedStage` is the x86 successor route.

## 5. Competitor Lessons

### asmjson

What to copy:

- collapsed classification + state + output writes when grammar facts and ISA
  support admit it;
- 64-byte class masks, direct-threaded dispatch, `tzcnt` next-event seeking,
  EOB padding, flat DOM or direct SAX sink;
- SWAR fallback as a real performance floor.

What not to copy:

- permissive strictness plane: asmjson accepts control characters as
  whitespace and does not fully scan strings for unescaped controls per its own
  docs;
- JSON constants in generic primitive crates;
- x86 AVX-512 assumptions on arm64;
- the idea that a hand-written JSON DPDA is the same as grammar-generic
  codegen.

Generic lift: `CollapsedStage` is a deterministic pushdown automaton lowering,
not a JSON FSM. It needs grammar-derived byte classes, state transition tables,
frame stack facts, and a hand-authored ISA wrapper.

### simdjson

What to copy:

- On Demand's iterator model: parse values as they are used, skip unused work;
- structural-index/tape separation where useful;
- runtime dispatch by target CPU;
- distinct stage profiling.

What to avoid:

- treating stage1 as universally dominant. Escape and numeric corpora invert
  the stage ratio; our bench must classify by workload, not average.

Generic lift: `EventCursor` over accepted events is the abstraction. It can
feed retained `OffsetTape`/`EventTape` or direct `SinkOnly`.

### sonic-rs

What to copy:

- direct parse into struct-shaped outputs as a first-class benchmark;
- exact float/number compatibility;
- SIMD algorithms borrowed and adapted from multiple SOTA parsers.

What to avoid:

- a JSON-only direct path in our generic crates. Our generated `SinkOnly`
  must come from `DirectBuild` and `LayoutFacts.backend_shape`.

Generic lift: exact materializers belong in `parse-that/number`,
`parse-that/string`, and `parse-that/unicode`, not in JSON bench code.

### yyjson

What to copy:

- one i-cache-resident scalar hot driver;
- aggressive inlining and branch predictor friendliness;
- single-pass forward scan with low allocation pressure.

What to avoid:

- assuming SIMD is the only path to SOTA. yyjson proves scalar fusion can beat
  SIMD if the generated shape is better.

Generic lift: Lock 15 is as important as Lock 16.

### dav1d / FFmpeg / VLC

What to copy:

- macro-layer separation: Layer 0 ABI/ISA substrate, Layer 1 project
  primitives, Layer 2 per-codec or per-grammar composition;
- checkasm admission before performance claims;
- per-ISA files with stable naming and identical scalar specs;
- large handwritten ASM is tractable only with a reviewable vocabulary.

What to avoid:

- anonymous magic asm or primitives without a consumer.

Generic lift: `bbnf-simd` Layer 1 is a grammar-neutral vocabulary; grammar
tables live in generated `.data`, not in the primitive crate.

## 6. General Architecture Correction

The substrate boundary is:

```text
bytes
  -> scan / mask producer
  -> typed event cursor
  -> { OffsetTape | EventTape | SinkOnly | CollapsedStage | EagerTape fallback }
```

This is not a new directive and not a second substrate.

- `OffsetTape`: retained offsets; payloads lazy.
- `EventTape`: retained event cells when recovery/layout/payload side facts
  must survive.
- `SinkOnly`: direct typed writes; no retained document identity.
- `CollapsedStage`: hand-authored per-grammar DPDA kernel on admitted ISA.
- `EagerTape`: source-byte recursive descent fallback for recovery, layout,
  parse-time host decode, or first-set overlap.

The current implementation debt is precise:

1. Track 1 direct must move out of `bbnf-bench` into generated runtime/codegen
   `SinkOnly`.
2. Direct rows must use exact generated string/Unicode/number materializers.
3. Parse rows need no-inline/PC-level `parse_value_at` attribution before more
   primitives are prescribed.
4. `OffsetTape` generated bodies must consume an `EventCursor` when the cost
   model selects that shape.
5. x86 `CollapsedStage` must remain separate until NASM author, silicon, and
   checkasm are all present.

## 7. Path To Beat asmjson And Friends

The path has two lanes.

### Host lane: M5 Max arm64

Goal: no parse G rows, no `N-direct`, beat sonic-rs/simdjson/yyjson on the
strict workload matrix where those comparators run on this host.

1. Generate `SinkOnly` direct entrypoints from BIR `DirectBuild`.
2. Implement exact direct string/Unicode and number materializers.
3. Add no-inline/PC-level `parse_value_at` profile mode.
4. Route `OffsetTape` hot alts through `EventCursor` only where the profile
   proves source-byte dispatch is the current cost.
5. Land only same-wave consumed NEON primitives: byte class, next-set-bit,
   hex decode, exact digit blocks.
6. Re-run the full 17-row parse and direct matrix plus sidecar comparators.

### x86 lane: asmjson-class successor

Goal: beat asmjson on equivalent Zen 4/Ice Lake-class hardware on strict parse
and full traversal, with permissive rows split out.

1. Complete Layer 1 `bbnf.asm` bodies for the nine grammar-neutral macros.
2. Add scalar reference and checkasm for each macro.
3. Generate grammar `.data` tables from Grammar IR.
4. Hand-author per-grammar NASM wrappers only when the cost model selects
   `CollapsedStage`.
5. Use asmjson's minimal architecture first: AVX-512BW equality masks,
   k-mask reductions, `kmovq`, `tzcnt`, EOB padding, bounded frame stack.
6. Add strict esoterica only when checkasm-green and consumed: k-mask
   arithmetic, VPCLMULQDQ-512, VBMI/VBMI2, BITALG, VNNI digit blocks, IFMA
   mantissa helpers, GFNI only for proven affine class encodings.

Projected 14 GiB/s remains a target, not a claim. The gate is measurement
against asmjson on equivalent hardware and strictness.

## 8. Spec Patches Made In This Pass

- `SOTA-BEAT-DESIGN.md`: replaced stale `StructuralIndex` and metadata
  collapsed-stage selection with five-shape cost-model-derived lowering.
- `IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`: demoted asmjson-beat projection
  to x86 successor-tranche; SK-V3 close remains M5 Max expanded gate.
- `BENCH.md`: corrected `alternate_capacity_plan` after grow-only Plan D.
- `INDEX.md`: demoted two-pathology labels to diagnostics and corrected Plan D.
- `ARCHITECTURE.md`: clarified direct builders over event stream and added
  `BBNF-COLLAPSEDSTAGE-NOT-VIABLE`.
- `MASTER-PLAN.md`: made H.W5 primitive admission consumed by retained/direct
  shapes; per-grammar `CollapsedStage` remains separate.
- `HANDOFF.md`: marked sidecar comparator posture separately from current gate.
- `COMPILER.md`: removed `CursorDispatch`/`set_len(0)` stale primitive wording.

## 9. Decision

Current state: **SK-V4 AMENDMENT REQUIRED, IMPLEMENTATION PACKET READY**.

The next implementation agent should follow
`restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md`.
Completion is not "primitive landed" or "profile improved"; completion is:

- `skinny/RESULTS.md` has no parse G rows;
- direct-to-struct has no `N-direct` rows;
- strict sidecar comparisons beat sonic-rs, simdjson, and yyjson on M5 Max;
- x86 asmjson-beat rows are produced only on equivalent x86 hardware with
  strictness and output plane matched.
