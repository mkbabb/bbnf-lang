---
agent: 2B
pass: T-P2-research
cycle: V2
generated_at: 2026-05-28T06:36:05Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 26
techniques_grounded: 10
techniques_refuted: 7
prior_cycle_dispositions_folded:
  accepted:
    - CH4-V1-06-positive-control-preserve-citation-only-PMULL-CSSC-LD4-SVE2-rejection
    - CH6-V1-05-preserve-2B-primitive-manifest-template
  rejected: []
  revised:
    - CH4-V1-01-add-LOC-risk-wave-owner-hard-cap-fields-to-primitive-manifest
    - CH6-V1-row-shape-add-transfer-reason-admission-gate-verification-action-close-status
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

Each V2 row carries the CH6 standalone suffix. SIMD/ASM primitive rows also
carry the scalar/checkasm/hardware/consumer/row-movement manifest inline; for
non-SIMD process rows those fields are intentionally `n/a-non-simd`.

| spec claim / divergence | primary source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| Layer 0 is vendored macro infrastructure, not parser logic. | `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5-6`, `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:49-53`; `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:24-28` | grounded | `x86inc.asm` / `x86util.asm` are build-time ABI and macro aids. They do not carry JSON/CSS/Sheets grammar policy and cannot close M5 Max rows. V2: transfer_reason=separate vendored ABI macros from bbnf primitive contracts; admission_gate=no M5 Max/aarch64 close evidence may cite Layer 0; verification_action=Lock 16 report keeps Layer 0 diagnostic-only; close_status=diagnostic-only; loc_estimate=0-20 audit/report LOC; risk_class=low; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| Layer 1 is a bbnf-authored contract vocabulary that depends one way on Layer 0. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1-12`, `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:47-60` | grounded | `bbnf.asm` includes Layer 0 and states per-grammar data lives outside the macro library. This preserves Layer 0 / Layer 1 separation. V2: transfer_reason=retain grammar-neutral primitive contract vocabulary without admitting source inventory; admission_gate=each contract must pass the primitive manifest before close; verification_action=W2 manifest enumerates contract-to-oracle/parity/consumer state; close_status=admissible-after-gate; loc_estimate=20-60 report/gate LOC; risk_class=medium; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| The historical nine Layer 1 macro names are current source inventory. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:30-44` | grounded as inventory only | The nine names are not nine admitted primitives. SK-V15 admission is per primitive and host-gated. V2: transfer_reason=inventory prevents accidental deletion but cannot certify runtime behavior; admission_gate=source name alone is rejected unless scalar oracle, strict parity, aarch64 gate, same-wave consumer, and row movement exist; verification_action=W2 marks each macro `admitted`, `scalar-delegated`, `source-present-unwired`, `blocked`, or `deleted`; close_status=source-present-unwired; loc_estimate=20-40 report LOC; risk_class=medium; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| Scalar oracle is mandatory. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:9-12`; scalar refs at `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:1-10`, `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38`, `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1-14`, `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1-13`, `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1-13`, `skinny/crates/bbnf-simd/src/scalar/eob_pad_clamp.rs:1-19` | grounded | These six scalar references are executable specifications. No SIMD/ASM body may be admitted without matching them or a row-specific oracle. V2: transfer_reason=make primitive semantics executable before architecture-specific transfer; admission_gate=all SIMD/ASM candidates name an oracle file or are blocked; verification_action=strict parity/checkasm compares candidate output and mutation behavior against oracle; close_status=admissible-after-gate; loc_estimate=0-80 per missing oracle; risk_class=medium; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes if scoped per primitive. |
| checkasm-style differential is mandatory and must be strict for admission. | FFmpeg checkasm source `https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html` lines 1028-1046, 1101-1119, 1128-1130; local harness `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-20`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:200-227`; `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:114-125` | grounded | The local harness copies the process shape: scalar vs candidate, buffer mutation checks, signal guard, alignment sweep, strict mode. SK-V15 should reject non-strict parity as admission evidence. V2: transfer_reason=transfer process discipline, not media-kernel semantics; admission_gate=strict mode must be green for the named primitive and host path; verification_action=run the primitive-specific strict parity/checkasm command and attach report row; close_status=admissible-after-gate; loc_estimate=20-120 per new primitive test; risk_class=medium; wave_owner=W2/W7 primitive implementation owner; hard_cap_fit=yes when bounded to one primitive. |
| Hardware gate must be host-specific and aarch64-primary. | `restart/skinny/tranches/sk-v15/SPEC.md:133-145`; `restart/skinny/tranches/sk-v15/SPEC.md:119-122` | grounded | Apple M5 Max/aarch64 is the close route; x86 and AVX-512 are planning signals only. V2: transfer_reason=bind primitive admission to the SK-V15 close host; admission_gate=M5 Max/aarch64 execution or scalar-delegate disclosure, never x86/AVX-512 close evidence; verification_action=W2 report records host feature gate and fallback state per primitive; close_status=admissible-after-gate; loc_estimate=10-40 report/gate LOC; risk_class=low; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| Current aarch64 eq-set is a real NEON primitive body. | `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:31-72`; Arm ACLE NEON intrinsics reference `https://arm-software.github.io/acle/neon_intrinsics/advsimd.html` (`vceqq_u8` entry around HTML line 8031, `vorrq_u8` around 19328, `vld1q_u8` around 24834) | grounded | The body loads four 16-byte stripes, fans `vceqq_u8`, OR-reduces with `vorrq_u8`, and packs a 64-bit mask. It still needs strict SK-V15 gate consumption and row movement for close. V2: transfer_reason=eq-set is a grammar-neutral byte-class primitive used by JSON and non-JSON receivers; admission_gate=strict parity plus M5 Max row movement on a named consumer; verification_action=run `checkasm_parity` strict for eq-set and measure `find_ascii_set_member64` plus one non-JSON FIRST/trivia row; close_status=partial-blocked until row movement lands; loc_estimate=20-80 test/report LOC, 0 implementation LOC if body unchanged; risk_class=medium; wave_owner=W2/W7 primitive implementation owner; hard_cap_fit=yes; scalar_reference=`src/scalar/byte_class_from_eq_set_64.rs`; parity_or_checkasm=`skinny/crates/bbnf-simd/tests/checkasm_parity.rs` strict eq-set case; hardware_gate=Apple-M5-Max-aarch64 NEON; same_wave_consumer=`find_ascii_set_member64` plus non-JSON receiver; row_movement_target=named JSON/non-JSON row in `skinny/RESULTS.md`. |
| Current aarch64 table / prefix-xor / next-bit / bulk-emit / eob-pad paths are NEON wins. | Delegates at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/eob_pad_clamp.rs:1-6` | refuted | These are scalar delegates, not SIMD admissions. They may be valid fallback states, but they cannot be cited as aarch64 SIMD row movement. V2: transfer_reason=preserve fallback correctness while preventing false NEON claims; admission_gate=SIMD admission requires a real aarch64 body plus strict parity and row movement; verification_action=W2 manifest labels these rows scalar-delegated unless rebuilt; close_status=scalar-delegated; loc_estimate=0-30 report LOC now, 80-250 per future NEON body; risk_class=medium for rebuild, low for delegate disclosure; wave_owner=W2 now, W7/W8 if rebuilt; hard_cap_fit=yes only per primitive; scalar_reference=matching `src/scalar/<primitive>.rs`; parity_or_checkasm=strict parity required before promotion; hardware_gate=scalar delegate on M5 Max/aarch64; same_wave_consumer=current wrappers or runtime JSON tail for eob-pad; row_movement_target=none claimed while scalar-delegated. |
| Same-wave consumer is required for every source-present primitive. | `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:319-324`; `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:39-41` | grounded | A primitive cannot land as an orphan kernel. Its consumer must be hot path or gate-consumer in the same wave. V2: transfer_reason=prevent orphan kernel admission; admission_gate=consumer row id or gate-consumer must be named before close; verification_action=W2 primitive-status report joins primitive id to consumer path and benchmark row; close_status=admissible-after-gate; loc_estimate=20-60 report LOC plus consumer-specific wiring; risk_class=medium; wave_owner=W2/W7 primitive owner; hard_cap_fit=yes when one consumer per primitive is named. |
| Row movement is required, not only source + tests. | `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:53-67`; `skinny/RESULTS.md:112-135`; overfit audit `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:7-35` | grounded | The CSS W8R broadcast shows why row-local movement and equality matter. Reused timing or non-typed comparator evidence rejects. V2: transfer_reason=separate correctness parity from performance/equality close; admission_gate=unique row-local comparator, equality, timing, and broadcast-group disclosure; verification_action=attach one `skinny/RESULTS.md` row or gate row per primitive-influenced claim; close_status=admissible-after-gate; loc_estimate=20-80 measurement/report LOC; risk_class=high for timing-sensitive rows; wave_owner=W2/W7 gate owner; hard_cap_fit=yes if no broadcast reuse. |
| `scan_dispatch` currently consumes table classify + bulk emit as a structural-index path. | `skinny/crates/bbnf-simd/src/lib.rs:106-124`, `skinny/crates/bbnf-simd/src/lib.rs:228-243` | partial | There is a same-crate consumer, but `StructuralIndex::from_positions(..., ScanBackend::Scalar)` keeps the current public backend classification scalar at `skinny/crates/bbnf-simd/src/lib.rs:123`. This is not a SIMD admission row by itself. V2: transfer_reason=identify existing consumer without overclaiming backend movement; admission_gate=table/bulk emit need strict parity, aarch64 implementation state, and structural-index row movement; verification_action=W2 reports current scalar backend classification and blocks SIMD close; close_status=partial-blocked; loc_estimate=40-160 to wire/report, 80-250 per future NEON body; risk_class=high; wave_owner=W2/W7 primitive owner; hard_cap_fit=conditional, only if scoped to one structural-index row; scalar_reference=`byte_class_from_table_64.rs` and `bulk_emit_positions_64.rs`; parity_or_checkasm=strict parity for table classify and bulk emit; hardware_gate=M5 Max/aarch64 scalar delegate unless rebuilt; same_wave_consumer=`scan_dispatch`; row_movement_target=structural-index row in `skinny/RESULTS.md` with backend label fixed. |
| `find_ascii_set_member64` consumes eq-set classification. | `skinny/crates/bbnf-simd/src/lib.rs:209-226`, `skinny/crates/bbnf-simd/src/lib.rs:282-291` | partial | The consumer exists and dispatches to the aarch64 NEON body on aarch64, but SK-V15 admission still needs strict checkasm/parity plus measured row movement on a named workload. V2: transfer_reason=existing eq-set consumer can close only with row-local evidence; admission_gate=strict eq-set parity plus M5 Max row movement for the consumer; verification_action=run parity and measure `find_ascii_set_member64` against scalar delegate with equality; close_status=partial-blocked; loc_estimate=20-80 report/test LOC; risk_class=medium; wave_owner=W2/W7 primitive owner; hard_cap_fit=yes; scalar_reference=`byte_class_from_eq_set_64.rs`; parity_or_checkasm=strict eq-set checkasm/parity; hardware_gate=Apple-M5-Max-aarch64 NEON; same_wave_consumer=`find_ascii_set_member64`; row_movement_target=named ASCII-set row plus non-JSON receiver row. |
| `EOB_PAD_CLAMP` has a runtime consumer. | `skinny/crates/runtime/src/grammars/json/scan.rs:118`; `skinny/crates/bbnf-simd/src/lib.rs:274-277` | partial | It is wired as a tail-block helper, but the current aarch64 implementation delegates to scalar, so it should be classified as scalar-delegated unless a measured NEON body lands. V2: transfer_reason=tail safety primitive is real but not a SIMD win; admission_gate=scalar-delegate disclosure is enough for fallback, SIMD close requires new body/parity/row movement; verification_action=W2 records JSON scan consumer and scalar-delegated status; close_status=scalar-delegated; loc_estimate=10-40 report LOC, 60-160 if future NEON body; risk_class=low now, medium if rebuilt; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes; scalar_reference=`eob_pad_clamp.rs`; parity_or_checkasm=strict eob-pad parity before promotion; hardware_gate=M5 Max/aarch64 scalar delegate; same_wave_consumer=`runtime/src/grammars/json/scan.rs`; row_movement_target=none claimed while scalar-delegated. |
| Simdjson quote-mask / prefix-XOR supports a grammar-neutral toggle-region primitive. | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", arXiv `https://arxiv.org/abs/1902.08318`; `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:145-181` | grounded as abstract primitive | Prefix XOR is not JSON-only; it transfers to any grammar with quote-like toggle regions. It still needs an aarch64 implementation or scalar-delegate status for SK-V15. V2: transfer_reason=toggle-region abstraction transfers beyond JSON when receiver semantics are named; admission_gate=scalar oracle, strict parity, M5 Max implementation/delegate state, same-wave string-region consumer, and row movement; verification_action=W2 marks current aarch64 path scalar-delegated and blocks PMULL promotion until rebuilt; close_status=scalar-delegated; loc_estimate=30-80 report LOC now, 120-300 for future PMULL/NEON implementation; risk_class=high if rebuilt; wave_owner=W2 now, W7/W8 if rebuilt; hard_cap_fit=conditional; scalar_reference=`bitmap_prefix_xor_64.rs`; parity_or_checkasm=strict prefix-XOR parity; hardware_gate=M5 Max/aarch64 scalar delegate unless future PMULL body lands; same_wave_consumer=future string-region consumer; row_movement_target=none claimed while scalar-delegated. |
| Interleaved-vector / LD4 research may be admitted by citation alone. | Validark, "Use interleaved vectors for parsing on ARM" `https://validark.dev/posts/interleaved-vectors-on-arm/` (named technique: LD4/interleaved vectors for parser movemask, unmovemask, and elementwise shifts) | refuted | The post grounds the technique, not bbnf admission. A bbnf LD4 primitive requires scalar oracle, strict checkasm, feature gate, same-wave consumer, and row movement. V2: transfer_reason=retain LD4 as research input while rejecting citation-only admission; admission_gate=new bbnf oracle, strict parity, M5 Max feature gate, same-wave JSON plus non-JSON consumer, and row movement; verification_action=leave LD4 blocked until prototype and row evidence exist; close_status=refuted; loc_estimate=160-400 implementation/test/report LOC; risk_class=high; wave_owner=future W7/W8 SIMD owner; hard_cap_fit=no unless split to one primitive and one consumer; scalar_reference=not present; parity_or_checkasm=not present; hardware_gate=Apple-M5-Max-aarch64 only; same_wave_consumer=not named; row_movement_target=not present. |
| PMULL/CSSC/SVE2 architecture citations may promote primitive rows without consumer movement. | `restart/audit/totality/p2/2E-host-arch-esoterica.md:75-82`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:111-122`, `restart/skinny/tranches/sk-v15/SPEC.md:133-145`, `restart/skinny/tranches/sk-v15/SPEC.md:480` | refuted | PMULL and CSSC can be host-gated research candidates, while SVE2 MATCH/NMATCH is not a NEON route on this host. None admits 2B vocabulary by citation, ISA bit, or checkasm-only evidence. V2: transfer_reason=preserve the 2E host-architecture guard inside the primitive vocabulary; admission_gate=PMULL/CSSC require scalar oracle, emitted-asm proof, strict parity, same-wave consumer with scalar cost removed, and row movement; SVE2 requires a future SVE2 host and separate scalable-vector dispatch plan; verification_action=W2 keeps PMULL/CSSC scalar-delegated or blocked and keeps SVE2 refuted for SK-V15; close_status=refuted; loc_estimate=0-40 report LOC now, 120-300 per future PMULL/CSSC body; risk_class=high; wave_owner=W2 guard owner, future W7/W8 only with named consumer; hard_cap_fit=yes as refutation, conditional/no as implementation; scalar_reference=prefix-XOR/next-set/bulk-emit scalar oracles if reopened; parity_or_checkasm=not sufficient without row movement; hardware_gate=Apple-M5-Max PMULL/CSSC bits for candidates, SVE2 absent for MATCH/NMATCH; same_wave_consumer=none by default; row_movement_target=none claimed. |
| FSM dispatch and frame-stack macros are admissible because they are listed in `bbnf.asm`. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:317-368`, `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:370-473`; no scalar/checkasm hits in `skinny/crates/bbnf-simd/src/scalar/` or `skinny/crates/bbnf-simd/tests/` for `fsm_dispatch_threaded`, `frame_push_bounded`, `frame_pop_bounded` | refuted | These are source-only contracts. SK-V15 should delete, scalar-block, or rebuild them; they cannot remain as admitted vocabulary. V2: transfer_reason=prevent macro-list source inventory from becoming admission; admission_gate=rebuild requires oracle, strict parity, M5 Max gate, same-wave CollapsedStage consumer, and row movement; verification_action=W2 classifies each as deleted, blocked, or source-present-unwired; close_status=source-present-unwired; loc_estimate=20-60 report LOC now, 200-500 if rebuilt; risk_class=high; wave_owner=W2 now, W8/W9 if rebuilt; hard_cap_fit=no for bulk rebuild; scalar_reference=not present; parity_or_checkasm=not present; hardware_gate=not present; same_wave_consumer=not present; row_movement_target=not present. |
| Source-present but unwired primitives can close as inventory-demoted. | `restart/skinny/tranches/sk-v15/SPEC.md:119-122`; `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:89-107` | refuted | SK-V15's accepted statuses are gate-consumed wired, scalar-delegated, deleted, strict-checkasm admitted with consumer, or blocked. Inventory-only is not a close state. V2: transfer_reason=make source inventory an audit input only; admission_gate=unwired source must move to deleted, blocked, scalar-delegated, or strict-checkasm admitted with consumer; verification_action=W2 primitive-status report rejects `inventory-demoted` close; close_status=refuted; loc_estimate=10-40 report LOC; risk_class=medium; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |

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

### A2 - The Admissible Unit Is A Costed Primitive Manifest

SK-V15 needs each primitive row to carry the original five admission cells plus
the V2 cost fields:

| cell | required evidence |
|---|---|
| Scalar oracle | `src/scalar/<primitive>.rs` or another executable oracle. |
| Differential | strict checkasm/parity test, not record-only mode. |
| Hardware gate | explicit aarch64 feature or scalar-delegate state; x86 feature gates are diagnostic only. |
| Same-wave consumer | hot path or gate consumer in the same wave, per `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:319-324`. |
| Row movement | named `skinny/RESULTS.md` row or gate row with unique measurement/equality; no broadcast tuple. |
| LOC estimate | bounded adoption/report/test LOC for the primitive or macro-family route. |
| Risk class | low / medium / high / critical if misfiled. |
| Wave owner | W2 status owner by default; W7/W8/W9 only when implementation/consumer work is named. |
| Hard-cap fit | whether the route fits the SK-V15 hard cap as a scoped primitive, not a bulk rewrite. |
| Close status | one of `admissible-after-gate`, `diagnostic-only`, `partial-blocked`, `source-present-unwired`, `scalar-delegated`, `blocked`, or `refuted`. |

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

### A3a - V2 Primitive / Macro-Family Route Manifest

| route | scalar_reference | parity_or_checkasm | hardware_gate | same_wave_consumer | row_movement_target | transfer_reason | admission_gate | verification_action | close_status | loc_estimate | risk_class | wave_owner | hard_cap_fit |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` | `src/scalar/byte_class_from_table_64.rs` | strict table-classify parity/checkasm required before promotion | M5 Max/aarch64 scalar delegate today | `scan_dispatch` structural-index path | blocked until backend label and row-local movement are fixed | generic byte-class table classify feeds structural indexing | scalar oracle plus strict parity plus named structural-index consumer and row movement | W2 report current scalar delegate, then W7 only if a real NEON/table body lands | scalar-delegated | 20-60 report LOC now, 80-220 future body/test LOC | medium | W2 now, W7 if rebuilt | yes now; conditional if rebuilt |
| `BYTE_CLASS_FROM_EQ_SET_64` | `src/scalar/byte_class_from_eq_set_64.rs` | strict eq-set parity/checkasm | M5 Max/aarch64 NEON body present | `find_ascii_set_member64`, plus required non-JSON receiver | named ASCII-set row and non-JSON FIRST/trivia row | grammar-neutral equality-set classify already has a real NEON body | strict parity, aarch64 execution, same-wave consumer, and row movement | measure consumer against scalar delegate with equality and no broadcast reuse | partial-blocked | 20-80 test/report LOC | medium | W2/W7 primitive owner | yes |
| `BITMAP_PREFIX_XOR_64` | `src/scalar/bitmap_prefix_xor_64.rs` | strict prefix-XOR parity/checkasm | M5 Max/aarch64 scalar delegate today; PMULL blocked without consumer | future string-region/toggle consumer | none claimed while scalar-delegated | quote/toggle-region primitive is grammar-neutral but not a current SIMD win | PMULL or NEON body requires emitted asm, strict parity, same-wave consumer, and row movement | keep scalar-delegated; reopen only with consumer removing scalar cost | scalar-delegated | 30-80 report LOC now, 120-300 future PMULL/body LOC | high | W2 now, W7/W8 if reopened | conditional/no without consumer |
| `BITMAP_NEXT_SET_BIT` | `src/scalar/bitmap_next_set_bit.rs` | strict next-set parity/checkasm | M5 Max/aarch64 scalar delegate today; CSSC blocked without consumer | bitmap scan/compact consumer not yet named for close | none claimed while scalar-delegated | next-bit can support compact/emit loops but source presence is not movement | CSSC route requires emitted asm, strict parity, same-wave consumer, and row-local movement | W2 records scalar-delegated state; W7 only with named compact row | scalar-delegated | 20-60 report LOC now, 90-180 future CSSC/body LOC | high if promoted from ISA alone | W2 now, future compact owner | conditional/no from ISA alone |
| `BULK_EMIT_POSITIONS_64` | `src/scalar/bulk_emit_positions_64.rs` | strict bulk-emit parity/checkasm | M5 Max/aarch64 scalar delegate today | `compact_mask` / `scan_dispatch` | blocked until structural-index row movement exists | bulk position emission is a reusable mask-to-index primitive | strict parity plus consumer row and movement, not wrapper presence | W2 marks delegate; future W7 must isolate bulk-emit movement | scalar-delegated | 20-60 report LOC now, 80-220 future body/test LOC | medium/high | W2 now, W7 if rebuilt | conditional |
| `EOB_PAD_CLAMP` | `src/scalar/eob_pad_clamp.rs` | strict eob-pad parity/checkasm before SIMD promotion | M5 Max/aarch64 scalar delegate today | JSON scan tail helper | none claimed while scalar-delegated | tail clamp is a correctness helper with a real runtime consumer | fallback admission only states scalar-delegated; SIMD close needs new body and movement | W2 records runtime consumer and no SIMD movement claim | scalar-delegated | 10-40 report LOC now, 60-160 future body/test LOC | low now, medium if rebuilt | W2 primitive owner | yes |
| x86 Layer 0 macro infrastructure | n/a | n/a | x86 diagnostic only; no M5 Max close | none for SK-V15 close | none | preserve build-time macro provenance without admitting x86 close evidence | cannot close SK-V15 rows; only informs contract shape | W2 report marks diagnostic-only | diagnostic-only | 0-20 report LOC | low | W2 primitive-vocabulary owner | yes |
| `FSM_DISPATCH_THREADED` / frame-stack macro-family | not present | not present | not present for M5 Max | not present; future CollapsedStage only if named | not present | source inventory exists but no oracle/parity/consumer path | delete, block, or rebuild with full primitive manifest | W2 classifies source-present-unwired; W8/W9 may reopen only with CollapsedStage consumer | source-present-unwired | 20-60 report LOC now, 200-500 rebuild LOC | high | W2 now, W8/W9 if rebuilt | no for bulk rebuild |
| LD4 / interleaved-vector candidate | not present | not present | Apple-M5-Max-aarch64 only if implemented | not named | not present | aarch64 research may inform a future byte-class/interleave primitive | citation alone rejected; needs oracle, strict parity, consumer, and movement | remain blocked/refuted until same-wave consumer is profiled hot | refuted | 160-400 implementation/test/report LOC | high | future W7/W8 SIMD owner | no unless narrowed |
| PMULL/CSSC/SVE2 citation candidates | prefix-XOR / next-set / bulk-emit scalar oracles if reopened; SVE2 none for SK-V15 | checkasm/ISA alone explicitly insufficient | PMULL/CSSC host-gated candidates; SVE2 MATCH absent and not NEON | none by default | none | preserve host-architecture research while blocking paper-close promotion | PMULL/CSSC require emitted asm, strict parity, same-wave consumer with scalar cost removed, and row movement; SVE2 needs future host/dispatch family | W2 keeps PMULL/CSSC scalar-delegated or blocked and SVE2 refuted | refuted | 0-40 report LOC now, 120-300 per future body | high | W2 guard owner, future W7/W8 only with consumer | yes as refutation; conditional/no as implementation |

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

| assertion | refutation | V2 close fields |
|---|---|---|
| The nine `bbnf.asm` macro names are nine admissible SK-V15 primitives. | Refuted. `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, and `FRAME_POP_BOUNDED` are listed in `bbnf.asm` (`skinny/crates/bbnf-simd/ext/x86/bbnf.asm:317-473`) but lack current scalar oracle, checkasm file, aarch64 body, and same-wave consumer evidence. | transfer_reason=source inventory audit only; admission_gate=full primitive manifest or deletion/block; verification_action=W2 source-present-unwired classification; close_status=source-present-unwired; loc_estimate=20-60 report LOC; risk_class=high; wave_owner=W2; hard_cap_fit=no for bulk rebuild. |
| aarch64 files imply aarch64 SIMD implementation. | Refuted. Five current aarch64 primitive files are direct scalar delegates (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/eob_pad_clamp.rs:1-6`). They are fallback states, not NEON row movement. | transfer_reason=preserve fallback state without SIMD overclaim; admission_gate=real aarch64 body plus strict parity and row movement; verification_action=W2 marks scalar delegates; close_status=scalar-delegated; loc_estimate=0-30 report LOC; risk_class=medium; wave_owner=W2; hard_cap_fit=yes as disclosure. |
| x86 AVX-512 source can close SK-V15 primitive claims. | Refuted. SK-V15 says x86 and AVX-512 are diagnostic only (`restart/skinny/tranches/sk-v15/SPEC.md:133-137`). | transfer_reason=keep x86 as diagnostic contract evidence only; admission_gate=M5 Max/aarch64 evidence required; verification_action=W2 rejects x86 close citations; close_status=diagnostic-only; loc_estimate=0-20 report LOC; risk_class=low; wave_owner=W2; hard_cap_fit=yes. |
| A primitive with parity but no row movement should be admitted. | Refuted. P3-C requires same-wave consumer and row movement formulas (`restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:53-67`). Source/test-only primitives are paper close. | transfer_reason=separate unit parity from workload close; admission_gate=strict parity plus consumer plus row movement; verification_action=attach row-local result before admission; close_status=refuted; loc_estimate=20-80 report/measurement LOC; risk_class=high; wave_owner=W2/W7; hard_cap_fit=yes if scoped. |
| A repeated throughput tuple can admit multiple primitive or parser rows. | Refuted by CSS W8R. The overfit audit identifies one CSS timing tuple broadcast across 24 conceptual rows (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:7-35`), and SK-V15 requires `broadcast_group_id` and row-local measurement fields (`restart/skinny/tranches/sk-v15/SPEC.md:100-122`). | transfer_reason=prevent broadcast timing close; admission_gate=unique row-local equality/timing and broadcast disclosure; verification_action=W2/W7 require per-row measurement artifact; close_status=refuted; loc_estimate=20-80 measurement/report LOC; risk_class=high; wave_owner=W2/W7; hard_cap_fit=yes if no broadcast reuse. |
| Retained frame/open stacks are safe primitive vocabulary by default. | Refuted for SK-V15 unless folded into the existing substrate with proof. SPEC rejects retained cursor/list and sidecar-style vocabulary (`restart/skinny/tranches/sk-v15/SPEC.md:147-153`). | transfer_reason=avoid sidecar/retained-stack substrate expansion; admission_gate=new Alpha/P1/SPEC contract or fold into existing substrate with proof; verification_action=W2 blocks frame/open stack macro reuse; close_status=blocked; loc_estimate=0-60 report LOC now, 200-500 if rebuilt; risk_class=high; wave_owner=W2 now, W8/W9 if reopened; hard_cap_fit=no for retained sidecar route. |

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
| LAC-2B-V2-01 | Lock 16 / SPEC W2 | Add a mandatory primitive manifest schema: primitive id, abstract primitive, source path, scalar oracle, strict checkasm command, aarch64 hardware gate, fallback state, same-wave consumer, row ids, row movement, transfer reason, admission gate, verification action, close status, LOC estimate, risk class, wave owner, and hard-cap fit. |
| LAC-2B-V2-02 | Lock 16 | Define `scalar-delegated` as an admissible fallback state only when the scalar oracle is wired and the report explicitly states no SIMD row movement is claimed. |
| LAC-2B-V2-03 | Lock 14 / Lock 16 | Forbid `bbnf.asm` source inventory from counting as primitive admission. Macro contracts without oracle/checkasm/consumer must be reported as `source-present-unwired` and then deleted, blocked, or rebuilt. |
| LAC-2B-V2-04 | SPEC telemetry | Require every primitive-influenced result row to carry `lock16_status` and `checkasm_or_parity_status`; missing or producer-only values reject close. |
