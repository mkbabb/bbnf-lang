# Grand synthesis SOTA-BEAT SK-V3

Date: 2026-05-12  
Status: live authority for expanded-gate redress  
Receiver: `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md`

## 1. Measured authority

`skinny/RESULTS.md` now carries a split verdict:

| Tier | Verdict | Meaning |
|---|---|---|
| Historical triad: twitter / citm_catalog / canada | Passes | Lazy-offset tape plus local hot-path specialization validates the tape/direct union for JSON-class corpora. |
| Expanded SOTA-BEAT corpus | **G / NoGo** | `github_events`, `update_center`, `random`, `unicode_escapes`, and `y_string_unicode` miss the S anchor. |

The triad pass is real substrate evidence. It is not the dispatch verdict for
SOTA-BEAT. The expanded gate is the binding implementation target.

Fresh reprofile pass, all run with `samply record --save-only
--unstable-presymbolicate -r 1000`:

| Corpus | Command summary | Throughput | Profile signal |
|---|---:|---:|---|
| `random.json` | `profile-lazy 50000 test_data/random.json` | 12609 Mbps | `runtime::generated_json::generated::parse_value_at` dominates. |
| `unicode_escapes.json` | `profile-lazy 30000 test_data/unicode_escapes.json` | 16754 Mbps | `parse_value_at` dominates; parse-only miss is cursor/dispatch, not decode. |
| `update-center.json` | `profile-lazy 75000 test_data/update-center.json` | 18987 Mbps | Spread across parse entry, sparse-flag capacity, and allocation growth. |

Profile artifacts:

- `skinny/profile/reprofile-2026-05-12/random.profile.json.gz`
- `skinny/profile/reprofile-2026-05-12/unicode_escapes.profile.json.gz`
- `skinny/profile/reprofile-2026-05-12/update-center.profile.json.gz`

## 2. What has landed

The skinny has already established these facts:

| Item | Status | Evidence |
|---|---|---|
| Eager 16-byte tape token as SOTA-BEAT substrate | Invalidated | Eager tape plateaued around the prior outcome-G ceiling. |
| Lazy offset tape | Validated for triad | Triad reaches C/A/A with zero JSON payload writes. |
| Codegen versus hand-coded substrate | Mostly validated | Track 1 and Track 2 stay close on the triad; current expanded misses are shared shape/cursor work. |
| Scanner floor | Validated | Structural-only canada remains above the 40000 Mbps NEON floor. |
| Function-pointer dispatch table | Rejected | Real function-pointer table regressed; first probe was a duplicate. |
| 12-byte token / width churn | Rejected | Mixed result; no gate close. |
| Pair-token fusion | Rejected | Token-count win did not produce throughput win. |
| NEON no-escape matcher, separator elision, generic SWAR whitespace | Rejected | Recorded in `skinny/REDRESS.md`; no improved expanded gate. |
| Host-call dispatch overhead | Validated | Dispatch is sub-ns to ~1 ns/call in the measured probe. |
| Parse-time eager string decode | MASKING | Gross-time penalties are large enough that V1 JSON must keep decode lazy. |

## 3. Research pass synthesis

Six research agents and six challenge/profile agents converged on one shape:

```text
byte input
  -> mask stream
  -> typed event cursor
  -> { OffsetTape | EventTape | SinkOnly | CollapsedStage }
  -> DocumentView / direct typed output
```

The mask stream is transient. If retained, it is the tape projection. There is
no second structural sidecar.

### asmjson

asmjson is the x86_64 AVX-512 reference target. It wins by fusing byte
classification, parser state, and output writes. Its docs report AVX-512BW
assembly paths, 10.93 GiB/s DOM on synthetic string arrays, and a SAX sink that
avoids tape allocation. The strictness note matters: asmjson is experimental
and must be compared in strict and permissive planes separately.

Transferrable items:

- mask-held parser state;
- direct-threaded or table-driven dispatch when it removes branches;
- direct DOM/sink emission from the classifier;
- SWAR fallback as a real baseline, not a slow correctness path;
- full-traversal benchmark rows because lazy parsers can undercount parse-only work.

Source: <https://docs.rs/asmjson/latest/asmjson/>

### yyjson

yyjson proves that scalar C can beat SIMD-heavy DOM when the hot parser is tiny,
branch-friendly, and compiler-optimized. It advertises strict RFC 8259 behavior,
accurate number reads, and no explicit SIMD. Local profiles make the same point:
i-cache residency and force-inlining can be as important as instruction width.

Transferrable items:

- hot parse driver size budget;
- generated `#[inline(always)]` only for mined hot rules;
- single-pass scalar number scan before any f64 materialization;
- pointer-free or pointer-light iteration over retained document cells.

Source: <https://github.com/ibireme/yyjson>

### simdjson / simd-json

simdjson remains the structural-index reference. DOM uses a tape; On-Demand keeps
a forward-only cursor over structural indexes. The On-Demand design keeps a
single index and parses values when accessed. That maps cleanly to BBNF's typed
event cursor, provided the cursor is over the retained tape projection rather
than a parallel sidecar.

Transferrable items:

- stage split as a cost-model option, not a universal substrate;
- cursor advance over offsets instead of raw byte rescans;
- UTF-8 validation at scan boundary;
- strict separation between parse-only and full traversal rows.

Sources: <https://simdjson.github.io/simdjson/md_doc_ondemand_design.html>,
<https://simdjson.github.io/simdjson/md_doc_tape.html>

### sonic-rs

sonic-rs is the Rust in-process anchor. Its useful lesson is direct
materialization and lazy raw slice access, not a retained simdjson-style tape.
V1 should match that laziness for string and number scalars while retaining a
queryable tape only when the API requires it.

Transferrable items:

- raw-slice lazy strings and numbers;
- whole hot path fused through release profile discipline;
- pointer/path lookup rows separate from parse rows;
- host-call string decode stays lazy.

Source: <https://github.com/cloudwego/sonic-rs>

### DAV1D / FFmpeg / VLC ASM discipline

The DAV1D/FFmpeg lesson is process, not pixel math. Their assembly survives
because each kernel has a scalar reference, a target feature table, and a
checkasm-style parity/bench harness. The FOSDEM 2023 VLC/FFmpeg slides record
dav1d's cross-ISA assembly posture, including ARM64, x86_64, AVX2, AVX-512
IceLake, NEON, fuzzing, and check discipline.

Transferrable items:

- scalar reference per primitive;
- cold CPUID dispatch to a `KernelSet`;
- handwritten ASM only where Rust intrinsics are missing or inferior by measured
  proof;
- `xtask primitive-checkasm` as the admission gate;
- per-target files with identical public primitive names.

Sources: <https://archive.fosdem.org/2023/schedule/event/om_vlc/attachments/slides/5695/export/events/attachments/om_vlc/slides/5695/FFmpeg_VLC_js.pdf>,
<https://ffmpeg.org/doxygen/8.0/checkasm_8h_source.html>

## 4. Validated original research items

| Original item | Verdict | Fold-back |
|---|---|---|
| Tape and direct-to-struct are one substrate | Validated with clarification | Lock 1 now says structural projection IS tape when retained. |
| Lazy scalar materialization | Validated | String and number decode stay view-time unless workload gate asks for full traversal. |
| Dual-track bench | Validated | Track 1/Track 2 split isolated codegen overhead from substrate shape. |
| Expanded corpora | Validated | The triad would have produced a false green; expanded rows found the real blockers. |
| Build-profile discipline | Validated | LTO/hot-leaf behavior remains a hard gate. |
| SIMD primitive crate | Validated as boundary | `bbnf-simd` is the primitive owner, but scanner speed alone is insufficient. |
| Host-call probe split | Validated | Registry dispatch cost and eager-decode work are separate phenomena. |

## 5. Invalidated or narrowed items

| Item | Verdict | Replacement |
|---|---|---|
| Eager token tape can beat sonic/simdjson by local tuning | Invalidated | Offset/event tape plus typed event cursor. |
| Sidecar structural-index typed parser prepass | Invalidated as a sidecar | Consume the retained tape projection directly. |
| Single canonical plan without cost-model probes | Invalidated for SOTA-BEAT | Materialization/primitive/capacity alternatives must be measured. |
| JSON triad is enough | Invalidated | Expanded gate is binding. |
| Parse-only Unicode rows prove string performance | Invalidated | Need parse-only, full traversal, path lookup, and materialize-all string rows. |
| ASM as JSON-specific magic | Rejected | Only grammar-neutral primitives enter `bbnf-simd` / `parse-that`. |

## 6. Generality discipline

The refined design generalizes by facts, not by grammar names:

| Fact | JSON example | Other grammar receiver |
|---|---|---|
| Byte-class alphabet | `{ } [ ] : , "`, digits, literals | CSS token starts, BBNF terminals, Sheets formula starts |
| Chunk-spanning token | strings | CSS identifiers/strings, regex literals, comments |
| Numeric primitive | JSON number span | CSS `<number>`, Sheets numeric literals, TOML/INI numbers |
| Dispatch hub | `value` alt | CSS at-rules, BBNF expression starts, Sheets functions |
| Retained document need | JSON path/value view | CSS visitor, BBNF AST, Sheets workbook model |
| Direct-only sink | serde struct row | typed extraction, ETL rows, validation-only grammars |
| Recovery/layout side facts | absent in JSON | CSS layout, error recovery, editor parse |

No new BBNF directive is introduced. No new BIR variant is introduced. The
existing `Alt`, `SimdScan`, `TapeEmit`, and `DirectBuild` variants lower
differently based on `LayoutFacts.backend_shape`.

## 7. Spec fold-back

Updated surfaces:

| Surface | Change |
|---|---|
| `restart/skinny/BENCH.md` | Expanded corpus is binding; native yyjson/asmjson sidecar planes added; workload split added. |
| `restart/skinny/SUBSTRATE.md` | Lazy-offset triad pass preserved; typed event cursor over tape projection becomes canonical lowering target. |
| `restart/skinny/COMPILER.md` | Cost-model omission reclassified as MASKING for SOTA-BEAT; plan probes required. |
| `restart/skinny/INDEX.md` | SIMD layer necessary but not sufficient; expanded G / NoGo remains visible. |
| `restart/skinny/WORKSPACE.md` | `bbnf-simd` is scanner and byte-primitive boundary; expanded gate blocks dispatch. |
| `restart/ARCHITECTURE.md` | `BackendShape` becomes materialization-plan enum: `EagerTape`, `OffsetTape`, `EventTape`, `SinkOnly`, `CollapsedStage`. |
| `restart/MASTER-PLAN.md` | Tranche H becomes typed-event/SIMD/primitive close, host-aarch64 first and x86 strict plane second. |
| `restart/locks/14-LOCKS.md` | Lock 1 clarifies structural projection as tape; Lock 16 corpus wording uses the expanded suite. |
| `restart/corpora/SOTA.md` | yyjson and asmjson added as native reference planes. |

## 8. Decision

The current skinny is not done. It is useful because it found the real gap:
typed event consumption and primitive/capacity planning, rather than another
tape-width perturbation. Dispatch the SK-V3 implementation packet. The close
criterion is the expanded SOTA-BEAT gate, not the historical triad.
