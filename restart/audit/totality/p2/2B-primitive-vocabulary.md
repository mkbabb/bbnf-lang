---
agent: 2B
pass: T-P2-research
cycle: V1
generated_at: 2026-05-28T06:36:05Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 26
techniques_grounded: 10
techniques_refuted: 6
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions:
    - SK-V15-2B-aarch64-first-admission-contract
    - SK-V15-2B-layer0-layer1-separation-reverified
    - SK-V15-2B-source-present-unwired-refutation
locks_amendment_candidates: 4
sk_cycle: SK-V15
t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z
implementation_floor: PASS-IMPL-V1-CSS-CONTRIVANCE-JSON-HONEST
host_close_route: Apple-M5-Max-aarch64
stale_sk_v14_material_reused: none
---

# T-P2 2B - Primitive Vocabulary Research

## Executive Summary

SK-V15 can keep a reusable primitive vocabulary, but only as an
aarch64-first admission system, not as a source inventory. T-P1 entered T-P2
as `CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z`; therefore this dossier treats
the current source tree as evidence to re-audit, not as a locked SK-V14
inheritance.

The defensible vocabulary is two-layered. Layer 0 is vendored x86 assembly
macro infrastructure only; it is diagnostic for SK-V15 because Apple M5 Max /
aarch64 is the close route. Layer 1 is bbnf's grammar-neutral primitive
contract vocabulary, but source presence is not admission. The minimum
admission cell is scalar oracle, checkasm-style differential, hardware gate,
same-wave consumer, and row movement. Current HEAD supports six scalar-backed
primitive families with checkasm-style tests, one true aarch64 NEON eq-set body,
and several aarch64 scalar delegates. The FSM and frame-stack macro contracts
remain source-only and are refuted for SK-V15 admission unless deleted or
rebuilt with oracle, parity, consumer, and row movement.

## Technique Grounding Table

| spec claim / divergence | primary source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| Layer 0 is vendored macro infrastructure, not parser logic. | `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5-6`, `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:49-53`; `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:24-28` | grounded | `x86inc.asm` / `x86util.asm` are build-time ABI and macro aids. They do not carry JSON/CSS/Sheets grammar policy and cannot close M5 Max rows. |
| Layer 1 is a bbnf-authored contract vocabulary that depends one way on Layer 0. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1-12`, `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:47-60` | grounded | `bbnf.asm` includes Layer 0 and states per-grammar data lives outside the macro library. This preserves Layer 0 / Layer 1 separation. |
| The historical nine Layer 1 macro names are current source inventory. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:30-44` | grounded as inventory only | The nine names are not nine admitted primitives. SK-V15 admission is per primitive and host-gated. |
| Scalar oracle is mandatory. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:9-12`; scalar refs at `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:1-10`, `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38`, `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1-14`, `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1-13`, `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1-13`, `skinny/crates/bbnf-simd/src/scalar/eob_pad_clamp.rs:1-19` | grounded | These six scalar references are executable specifications. No SIMD/ASM body may be admitted without matching them or a row-specific oracle. |
| checkasm-style differential is mandatory and must be strict for admission. | FFmpeg checkasm source `https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html` lines 1028-1046, 1101-1119, 1128-1130; local harness `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-20`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:200-227`; `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:114-125` | grounded | The local harness copies the process shape: scalar vs candidate, buffer mutation checks, signal guard, alignment sweep, strict mode. SK-V15 should reject non-strict parity as admission evidence. |
| Hardware gate must be host-specific and aarch64-primary. | `restart/skinny/tranches/sk-v15/SPEC.md:133-145`; `restart/skinny/tranches/sk-v15/SPEC.md:119-122` | grounded | Apple M5 Max/aarch64 is the close route; x86 and AVX-512 are planning signals only. |
| Current aarch64 eq-set is a real NEON primitive body. | `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:31-72`; Arm ACLE NEON intrinsics reference `https://arm-software.github.io/acle/neon_intrinsics/advsimd.html` (`vceqq_u8` entry around HTML line 8031, `vorrq_u8` around 19328, `vld1q_u8` around 24834) | grounded | The body loads four 16-byte stripes, fans `vceqq_u8`, OR-reduces with `vorrq_u8`, and packs a 64-bit mask. It still needs strict SK-V15 gate consumption and row movement for close. |
| Current aarch64 table / prefix-xor / next-bit / bulk-emit / eob-pad paths are NEON wins. | Delegates at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/eob_pad_clamp.rs:1-6` | refuted | These are scalar delegates, not SIMD admissions. They may be valid fallback states, but they cannot be cited as aarch64 SIMD row movement. |
| Same-wave consumer is required for every source-present primitive. | `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:319-324`; `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:39-41` | grounded | A primitive cannot land as an orphan kernel. Its consumer must be hot path or gate-consumer in the same wave. |
| Row movement is required, not only source + tests. | `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:53-67`; `skinny/RESULTS.md:112-135`; overfit audit `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:7-35` | grounded | The CSS W8R broadcast shows why row-local movement and equality matter. Reused timing or non-typed comparator evidence rejects. |
| `scan_dispatch` currently consumes table classify + bulk emit as a structural-index path. | `skinny/crates/bbnf-simd/src/lib.rs:106-124`, `skinny/crates/bbnf-simd/src/lib.rs:228-243` | partial | There is a same-crate consumer, but `StructuralIndex::from_positions(..., ScanBackend::Scalar)` keeps the current public backend classification scalar at `skinny/crates/bbnf-simd/src/lib.rs:123`. This is not a SIMD admission row by itself. |
| `find_ascii_set_member64` consumes eq-set classification. | `skinny/crates/bbnf-simd/src/lib.rs:209-226`, `skinny/crates/bbnf-simd/src/lib.rs:282-291` | partial | The consumer exists and dispatches to the aarch64 NEON body on aarch64, but SK-V15 admission still needs strict checkasm/parity plus measured row movement on a named workload. |
| `EOB_PAD_CLAMP` has a runtime consumer. | `skinny/crates/runtime/src/grammars/json/scan.rs:118`; `skinny/crates/bbnf-simd/src/lib.rs:274-277` | partial | It is wired as a tail-block helper, but the current aarch64 implementation delegates to scalar, so it should be classified as scalar-delegated unless a measured NEON body lands. |
| Simdjson quote-mask / prefix-XOR supports a grammar-neutral toggle-region primitive. | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", arXiv `https://arxiv.org/abs/1902.08318`; `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:145-181` | grounded as abstract primitive | Prefix XOR is not JSON-only; it transfers to any grammar with quote-like toggle regions. It still needs an aarch64 implementation or scalar-delegate status for SK-V15. |
| Interleaved-vector / LD4 research may be admitted by citation alone. | Validark, "Use interleaved vectors for parsing on ARM" `https://validark.dev/posts/interleaved-vectors-on-arm/` (named technique: LD4/interleaved vectors for parser movemask, unmovemask, and elementwise shifts) | refuted | The post grounds the technique, not bbnf admission. A bbnf LD4 primitive requires scalar oracle, strict checkasm, feature gate, same-wave consumer, and row movement. |
| FSM dispatch and frame-stack macros are admissible because they are listed in `bbnf.asm`. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:317-368`, `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:370-473`; no scalar/checkasm hits in `skinny/crates/bbnf-simd/src/scalar/` or `skinny/crates/bbnf-simd/tests/` for `fsm_dispatch_threaded`, `frame_push_bounded`, `frame_pop_bounded` | refuted | These are source-only contracts. SK-V15 should delete, scalar-block, or rebuild them; they cannot remain as admitted vocabulary. |
| Source-present but unwired primitives can close as inventory-demoted. | `restart/skinny/tranches/sk-v15/SPEC.md:119-122`; `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:89-107` | refuted | SK-V15's accepted statuses are gate-consumed wired, scalar-delegated, deleted, strict-checkasm admitted with consumer, or blocked. Inventory-only is not a close state. |

## Architectural Assertions Defended

### A1 - Layer 0 / Layer 1 Separation Is Defensible

Layer 0 is the vendored x86 macro layer. Its license and origin file says the
directory contains `x86inc.asm` and `x86util.asm` copied verbatim for build-time
use (`skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5-6`) and explains that
vendoring avoids a runtime FFmpeg/x264 dependency
(`skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:49-53`). `x86inc.asm` itself
describes calling-convention abstraction and DSP helper macros
(`skinny/crates/bbnf-simd/ext/x86/x86inc.asm:24-28`).

Layer 1 is separate: `bbnf.asm` declares bbnf primitive contracts, not bodies,
and says scalar references are the executable specification
(`skinny/crates/bbnf-simd/ext/x86/bbnf.asm:9-12`). It includes the vendored
Layer 0 files only one way (`skinny/crates/bbnf-simd/ext/x86/bbnf.asm:47-48`)
and keeps per-grammar LUT/FSM data outside the macro library
(`skinny/crates/bbnf-simd/ext/x86/bbnf.asm:55-60`). This is the right
architecture boundary. The correction is that Layer 0 is x86-only diagnostic
for SK-V15 and must not be mistaken for the M5 Max close path.

### A2 - The Admissible Unit Is A Five-Cell Primitive Manifest

SK-V15 needs each primitive row to carry:

| cell | required evidence |
|---|---|
| Scalar oracle | `src/scalar/<primitive>.rs` or another executable oracle. |
| Differential | strict checkasm/parity test, not record-only mode. |
| Hardware gate | explicit aarch64 feature or scalar-delegate state; x86 feature gates are diagnostic only. |
| Same-wave consumer | hot path or gate consumer in the same wave, per `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:319-324`. |
| Row movement | named `skinny/RESULTS.md` row or gate row with unique measurement/equality; no broadcast tuple. |

This is a process lift from FFmpeg checkasm and the in-tree harness, not a
claim that video-domain kernels transfer. FFmpeg's source registers reference
functions and reports failures (`https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html`
lines 1043-1046, 1101-1119), while the bbnf harness states scalar-vs-candidate
comparison, mutation checks, alignment sweep, signal guard, and strict mode
(`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-20`,
`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:200-227`).

### A3 - Current HEAD Supports Six Scalar-Backed Primitive Families

The current scalar-backed inventory is:

| primitive family | scalar oracle | current consumer / status |
|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` | `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:1-10` | Used by `scan_dispatch` at `skinny/crates/bbnf-simd/src/lib.rs:106-115`; aarch64 path delegates to scalar at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`. |
| `BYTE_CLASS_FROM_EQ_SET_64` | `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38` | Used by `find_ascii_set_member64` at `skinny/crates/bbnf-simd/src/lib.rs:209-226`; true aarch64 NEON body at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:31-72`. |
| `BITMAP_PREFIX_XOR_64` | `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1-14` | Public wrapper at `skinny/crates/bbnf-simd/src/lib.rs:169-172`; aarch64 scalar delegate at `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`. |
| `BITMAP_NEXT_SET_BIT` | `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1-13` | Primitive wrapper at `skinny/crates/bbnf-simd/src/lib.rs:264-267`; aarch64 scalar delegate at `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`. |
| `BULK_EMIT_POSITIONS_64` | `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1-13` | Used by `compact_mask` at `skinny/crates/bbnf-simd/src/lib.rs:228-243`; aarch64 scalar delegate at `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`. |
| `EOB_PAD_CLAMP` | `skinny/crates/bbnf-simd/src/scalar/eob_pad_clamp.rs:1-19` | Runtime JSON scan consumer at `skinny/crates/runtime/src/grammars/json/scan.rs:118`; aarch64 scalar delegate at `skinny/crates/bbnf-simd/src/aarch64/eob_pad_clamp.rs:1-6`. |

This inventory is useful, but it is not equivalent to six SIMD admits. For
SK-V15 reporting, the row status should distinguish `strict-checkasm admitted`,
`wired`, `scalar-delegated`, `deleted`, and `blocked`.

### A4 - aarch64-First Means x86/AVX-512 Does Not Close Rows

The SK-V15 SPEC makes Apple M5 Max/aarch64 the only admission host and demotes
x86/AVX-512 to diagnostics (`restart/skinny/tranches/sk-v15/SPEC.md:133-137`).
Therefore `byte_class_from_eq_set_64.asm` is valuable evidence for Layer 1's
contract shape (`skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.asm:49-90`),
but it cannot close SK-V15 rows. The corresponding M5 Max claim must use the
aarch64 NEON body, a scalar delegate, or an intrinsic-block/redress status.

### A5 - Deep SIMD Is Admissible Only With Process

Validark's interleaved-vector post is relevant aarch64 technique grounding:
it names LD4/interleaved vectors for parser movemask, unmovemask, and
elementwise-shift shapes (`https://validark.dev/posts/interleaved-vectors-on-arm/`).
It does not admit a bbnf primitive. The admissible bbnf shape is: create a
scalar oracle, add strict checkasm, gate on aarch64 features, wire a same-wave
JSON plus non-JSON consumer where the primitive is generic, and show row
movement without broadcast. This keeps citation-only SIMD out of SK-V15.

## Architectural Assertions Refuted

| assertion | refutation |
|---|---|
| The nine `bbnf.asm` macro names are nine admissible SK-V15 primitives. | Refuted. `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, and `FRAME_POP_BOUNDED` are listed in `bbnf.asm` (`skinny/crates/bbnf-simd/ext/x86/bbnf.asm:317-473`) but lack current scalar oracle, checkasm file, aarch64 body, and same-wave consumer evidence. |
| aarch64 files imply aarch64 SIMD implementation. | Refuted. Five current aarch64 primitive files are direct scalar delegates (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/eob_pad_clamp.rs:1-6`). They are fallback states, not NEON row movement. |
| x86 AVX-512 source can close SK-V15 primitive claims. | Refuted. SK-V15 says x86 and AVX-512 are diagnostic only (`restart/skinny/tranches/sk-v15/SPEC.md:133-137`). |
| A primitive with parity but no row movement should be admitted. | Refuted. P3-C requires same-wave consumer and row movement formulas (`restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:53-67`). Source/test-only primitives are paper close. |
| A repeated throughput tuple can admit multiple primitive or parser rows. | Refuted by CSS W8R. The overfit audit identifies one CSS timing tuple broadcast across 24 conceptual rows (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:7-35`), and SK-V15 requires `broadcast_group_id` and row-local measurement fields (`restart/skinny/tranches/sk-v15/SPEC.md:100-122`). |
| Retained frame/open stacks are safe primitive vocabulary by default. | Refuted for SK-V15 unless folded into the existing substrate with proof. SPEC rejects retained cursor/list and sidecar-style vocabulary (`restart/skinny/tranches/sk-v15/SPEC.md:147-153`). |

## Open Research Questions

| id | UNKNOWN | verify_action |
|---|---|---|
| OQ-2B-01 | Which six scalar-backed primitives are actually consumed by admitted JSON rows after W0 establishes `SK-V15-open`? | Add a W2 primitive-status report consumed by the gate: primitive id, call path, row ids, strict parity command, and row-movement artifact. |
| OQ-2B-02 | Does the true aarch64 eq-set body move any M5 Max row, or does dispatch overhead erase it? | Measure one named JSON row and one non-JSON FIRST-set/trivia consumer in the same wave that wires the primitive; compare against scalar delegate. |
| OQ-2B-03 | Should `BITMAP_PREFIX_XOR_64` become an aarch64 PMULL primitive or remain scalar-delegated? | Prototype only after scalar/checkasm is strict and a same-wave string-region consumer exists; admit only with M5 Max row movement. |
| OQ-2B-04 | Can LD4/interleaved classify be a grammar-neutral Layer 1 primitive? | Build an abstract `byte_class_interleaved_64` oracle and exercise JSON plus CSS/Sheets/BBNF-self receiver before any admission claim. |
| OQ-2B-05 | Are FSM/frame-stack macros deletions or future CollapsedStage rebuild inputs? | W2 should classify them as deleted or blocked unless W8/W9 names a same-wave CollapsedStage consumer with scalar oracle and gate proof. |

## LOCKS-AMENDMENTS-CANDIDATE

| id | target lock / surface | candidate amendment |
|---|---|---|
| LAC-2B-V1-01 | Lock 16 / SPEC W2 | Add a mandatory primitive manifest schema: primitive id, abstract primitive, source path, scalar oracle, strict checkasm command, aarch64 hardware gate, fallback state, same-wave consumer, row ids, row movement, and disposition. |
| LAC-2B-V1-02 | Lock 16 | Define `scalar-delegated` as an admissible fallback state only when the scalar oracle is wired and the report explicitly states no SIMD row movement is claimed. |
| LAC-2B-V1-03 | Lock 14 / Lock 16 | Forbid `bbnf.asm` source inventory from counting as primitive admission. Macro contracts without oracle/checkasm/consumer must be reported as `source-present-unwired` and then deleted, blocked, or rebuilt. |
| LAC-2B-V1-04 | SPEC telemetry | Require every primitive-influenced result row to carry `lock16_status` and `checkasm_or_parity_status`; missing or producer-only values reject close. |
