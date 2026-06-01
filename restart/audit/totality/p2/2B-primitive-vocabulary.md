---
agent: 2B
pass: T-P2-research
cycle: V3-SKV18-totality
generated_at: 2026-06-01T00:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 31
techniques_grounded: 13
techniques_refuted: 7
prior_cycle_dispositions_folded:
  accepted:
    - V2-full-grounding-table-and-A1..A5-architectural-assertions-carried-verbatim
    - CH4-V1-06-positive-control-preserve-citation-only-PMULL-CSSC-LD4-SVE2-rejection
    - CH6-V1-05-preserve-2B-primitive-manifest-template
  rejected: []
  revised:
    - SK-cycle-lens-SK-V15->SK-V18 (host-close route unchanged Apple-M5-Max-aarch64)
    - "CH2-V3-01: retained V2 eq-set rows (:74 Tech-Grounding, :160 A3a manifest) now carry the JSON-consumer-SUPERSEDED annotation — find_ascii_set_member64 has no live runtime caller, JSON rides byte_class_from_table_64, the eq-set kernel's only live consumer is CSS count_top_level_commas; structural neutrality retained (V3)."
  v3_sk_v18_additions:
    - SK-V18-2B-css_balanced_component_scan-named-primitive-grounding
    - SK-V18-2B-eq-set-member-scan-inner-sub-kernel-grounding
    - SK-V18-2B-find_component_delim-NEON-retarget-13-byte-two-fan-OR-reduce-grounding
    - SK-V18-2B-Lock16-admissibility-(citation+abstract-name+scalar-ref+checkasm-parity+same-wave-consumer)
    - SK-V18-2B-neutrality-proof-demotion-grounding (balanced_component_scan -> css_balanced_component_scan)
locks_amendment_candidates: 4
sk_cycle: SK-V18
sk_v18_lens: ONE-grammar-driven-generator-JSON+CSS+Sheets-from-.bbnf, preserves->SOTA, aarch64-PRIMARY-x86-SECONDARY-deleted-in-skinny
t_p1_entry_state: V5-SKV18-totality 1E divergence_count {impl:2, unimpl:11, exceeds:2, unknown:3}; G6=WIRE (find_component_delim 94.1% scalar)
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

## Technique Grounding Table — SK-V15 HISTORICAL (non-SK-V18-cost)

Each V2 row carries the CH6 standalone suffix. SIMD/ASM primitive rows also
carry the scalar/checkasm/hardware/consumer/row-movement manifest inline; for
non-SIMD process rows those fields are intentionally `n/a-non-simd`.

*Every `wave_owner=W#` value in this table (and in the A3a manifest below) is the
RETIRED SK-V15 ledger, NOT a live SK-V18 cost cell. The live SK-V18-cost owners
(`G#`/`G5/G6`/`G2`) are in the SK-V18 Cost Manifest and SK-V18 grounding tables
below; the SK-V15→SK-V18 wave map is in that Cost Manifest's re-key paragraph. Do
NOT copy a `W#` owner from a row here as a live SK-V18 cost.*

| spec claim / divergence | primary source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| Layer 0 is vendored macro infrastructure, not parser logic. | `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:5-6`, `skinny/crates/bbnf-simd/ext/x86/LICENSE-VENDOR:49-53`; `skinny/crates/bbnf-simd/ext/x86/x86inc.asm:24-28` | grounded | `x86inc.asm` / `x86util.asm` are build-time ABI and macro aids. They do not carry JSON/CSS/Sheets grammar policy and cannot close M5 Max rows. V2: transfer_reason=separate vendored ABI macros from bbnf primitive contracts; admission_gate=no M5 Max/aarch64 close evidence may cite Layer 0; verification_action=Lock 16 report keeps Layer 0 diagnostic-only; close_status=diagnostic-only; loc_estimate=0-20 audit/report LOC; risk_class=low; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| Layer 1 is a bbnf-authored contract vocabulary that depends one way on Layer 0. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:1-12`, `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:47-60` | grounded | `bbnf.asm` includes Layer 0 and states per-grammar data lives outside the macro library. This preserves Layer 0 / Layer 1 separation. V2: transfer_reason=retain grammar-neutral primitive contract vocabulary without admitting source inventory; admission_gate=each contract must pass the primitive manifest before close; verification_action=W2 manifest enumerates contract-to-oracle/parity/consumer state; close_status=admissible-after-gate; loc_estimate=20-60 report/gate LOC; risk_class=medium; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| The historical nine Layer 1 macro names are current source inventory. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:30-44` | grounded as inventory only | The nine names are not nine admitted primitives. SK-V15 admission is per primitive and host-gated. V2: transfer_reason=inventory prevents accidental deletion but cannot certify runtime behavior; admission_gate=source name alone is rejected unless scalar oracle, strict parity, aarch64 gate, same-wave consumer, and row movement exist; verification_action=W2 marks each macro `admitted`, `scalar-delegated`, `source-present-unwired`, `blocked`, or `deleted`; close_status=source-present-unwired; loc_estimate=20-40 report LOC; risk_class=medium; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| Scalar oracle is mandatory. | `skinny/crates/bbnf-simd/ext/x86/bbnf.asm:9-12`; scalar refs at `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:1-10`, `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38`, `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1-14`, `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1-13`, `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1-13`, `skinny/crates/bbnf-simd/src/scalar/eob_pad_clamp.rs:1-19` | grounded | These six scalar references are executable specifications. No SIMD/ASM body may be admitted without matching them or a row-specific oracle. V2: transfer_reason=make primitive semantics executable before architecture-specific transfer; admission_gate=all SIMD/ASM candidates name an oracle file or are blocked; verification_action=strict parity/checkasm compares candidate output and mutation behavior against oracle; close_status=admissible-after-gate; loc_estimate=0-80 per missing oracle; risk_class=medium; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes if scoped per primitive. |
| checkasm-style differential is mandatory and must be strict for admission. | FFmpeg checkasm source `https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html` lines 1028-1046, 1101-1119, 1128-1130; local harness `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-20`, `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:200-227`; `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:114-125` | grounded | The local harness copies the process shape: scalar vs candidate, buffer mutation checks, signal guard, alignment sweep, strict mode. SK-V15 should reject non-strict parity as admission evidence. V2: transfer_reason=transfer process discipline, not media-kernel semantics; admission_gate=strict mode must be green for the named primitive and host path; verification_action=run the primitive-specific strict parity/checkasm command and attach report row; close_status=admissible-after-gate; loc_estimate=20-120 per new primitive test; risk_class=medium; wave_owner=W2/W7 primitive implementation owner; hard_cap_fit=yes when bounded to one primitive. |
| Hardware gate must be host-specific and aarch64-primary. | `restart/skinny/tranches/sk-v15/SPEC.md:133-145`; `restart/skinny/tranches/sk-v15/SPEC.md:119-122` | grounded | Apple M5 Max/aarch64 is the close route; x86 and AVX-512 are planning signals only. V2: transfer_reason=bind primitive admission to the SK-V15 close host; admission_gate=M5 Max/aarch64 execution or scalar-delegate disclosure, never x86/AVX-512 close evidence; verification_action=W2 report records host feature gate and fallback state per primitive; close_status=admissible-after-gate; loc_estimate=10-40 report/gate LOC; risk_class=low; wave_owner=W2 primitive-vocabulary owner; hard_cap_fit=yes. |
| Current aarch64 eq-set is a real NEON primitive body. | `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:31-72`; Arm ACLE NEON intrinsics reference `https://arm-software.github.io/acle/neon_intrinsics/advsimd.html` (`vceqq_u8` entry around HTML line 8031, `vorrq_u8` around 19328, `vld1q_u8` around 24834) | grounded | The body loads four 16-byte stripes, fans `vceqq_u8`, OR-reduces with `vorrq_u8`, and packs a 64-bit mask. It still needs strict SK-V15 gate consumption and row movement for close. V2: transfer_reason=eq-set is a grammar-neutral byte-class primitive (JSON-consumer framing SUPERSEDED — `find_ascii_set_member64` has NO live runtime caller; the JSON aarch64 path rides the DIFFERENT `byte_class_from_table_64`/TBL classifier via `neon::scan`→`classify_tbl4`, never the eq-set kernel; the eq-set kernel's only live production consumer is CSS `count_top_level_commas` at `runtime_simd.rs:44,56,199`; see the live SK-V18 row at `:267`. Structural neutrality stands by caller-supplied byte set + grammar-naming-free kernel; the empirical dual-consumer claim is REFUTED); admission_gate=strict parity plus M5 Max row movement on a named consumer; verification_action=run `checkasm_parity` strict for eq-set and measure the CSS `count_top_level_commas` consumer (the `find_ascii_set_member64`/non-JSON FIRST framing is superseded — no such live consumer exists); close_status=partial-blocked until row movement lands; loc_estimate=20-80 test/report LOC, 0 implementation LOC if body unchanged; risk_class=medium; wave_owner=W2/W7 primitive implementation owner; hard_cap_fit=yes; scalar_reference=`src/scalar/byte_class_from_eq_set_64.rs`; parity_or_checkasm=`skinny/crates/bbnf-simd/tests/checkasm_parity.rs` strict eq-set case; hardware_gate=Apple-M5-Max-aarch64 NEON; same_wave_consumer=CSS `count_top_level_commas` (the SUPERSEDED `find_ascii_set_member64` cell had no live caller); row_movement_target=named CSS row in `skinny/RESULTS.md` (SUPERSEDED: not a JSON row — the JSON path does not ride this kernel). |
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

### A3a - V2 Primitive / Macro-Family Route Manifest — SK-V15 HISTORICAL (non-SK-V18-cost)

*The `wave_owner` column below carries RETIRED SK-V15 `W#` values, NOT live SK-V18
cost cells. The live SK-V18-cost owners are in the SK-V18 Cost Manifest below. Do
NOT copy a `W#` owner from a row here as a live SK-V18 cost.*

| route | scalar_reference | parity_or_checkasm | hardware_gate | same_wave_consumer | row_movement_target | transfer_reason | admission_gate | verification_action | close_status | loc_estimate | risk_class | wave_owner | hard_cap_fit |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` | `src/scalar/byte_class_from_table_64.rs` | strict table-classify parity/checkasm required before promotion | M5 Max/aarch64 scalar delegate today | `scan_dispatch` structural-index path | blocked until backend label and row-local movement are fixed | generic byte-class table classify feeds structural indexing | scalar oracle plus strict parity plus named structural-index consumer and row movement | W2 report current scalar delegate, then W7 only if a real NEON/table body lands | scalar-delegated | 20-60 report LOC now, 80-220 future body/test LOC | medium | W2 now, W7 if rebuilt | yes now; conditional if rebuilt |
| `BYTE_CLASS_FROM_EQ_SET_64` | `src/scalar/byte_class_from_eq_set_64.rs` | strict eq-set parity/checkasm | M5 Max/aarch64 NEON body present | CSS `count_top_level_commas` (SUPERSEDED: the `find_ascii_set_member64` cell had NO live runtime caller; the JSON path rides the DIFFERENT `byte_class_from_table_64`/TBL classifier, not the eq-set kernel — see the live SK-V18 row at `:267`) | named CSS row (SUPERSEDED: not a JSON/non-JSON FIRST row — JSON does not ride this kernel) | grammar-neutral equality-set classify already has a real NEON body; structural neutrality stands by caller-supplied byte set, the empirical JSON-consumer claim is REFUTED | strict parity, aarch64 execution, same-wave consumer, and row movement | measure the CSS `count_top_level_commas` consumer against scalar delegate with equality and no broadcast reuse | partial-blocked | 20-80 test/report LOC | medium | W2/W7 primitive owner | yes |
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
| OQ-2B-05 | Are FSM/frame-stack macros deletions or future CollapsedStage rebuild inputs? | RECONCILED in SKV18-A5: DELETE-only by default. The balanced-nesting need is already met by the recursive scalar shell (native call-stack nesting, transient) + the transient eq-set skip, with NO frame stack. A `FRAME_PUSH/POP_BOUNDED` macro is a RETAINED stack (the refuted sidecar shape, line 188); any future CollapsedStage rebuild must carry the Lock-1 transient-per-call-FSM-state proof INLINE (per 2D LAC-2D-V3-04), never a retained frame array — else INADMISSIBLE. |

## LOCKS-AMENDMENTS-CANDIDATE

| id | target lock / surface | candidate amendment |
|---|---|---|
| LAC-2B-V2-01 | Lock 16 / SPEC W2 | Add a mandatory primitive manifest schema: primitive id, abstract primitive, source path, scalar oracle, strict checkasm command, aarch64 hardware gate, fallback state, same-wave consumer, row ids, row movement, transfer reason, admission gate, verification action, close status, LOC estimate, risk class, wave owner, and hard-cap fit. |
| LAC-2B-V2-02 | Lock 16 | Define `scalar-delegated` as an admissible fallback state only when the scalar oracle is wired and the report explicitly states no SIMD row movement is claimed. |
| LAC-2B-V2-03 | Lock 14 / Lock 16 | Forbid `bbnf.asm` source inventory from counting as primitive admission. Macro contracts without oracle/checkasm/consumer must be reported as `source-present-unwired` and then deleted, blocked, or rebuilt. |
| LAC-2B-V2-04 | SPEC telemetry | Require every primitive-influenced result row to carry `lock16_status` and `checkasm_or_parity_status`; missing or producer-only values reject close. |

---

# SK-V18 V3 EXTENSION — §6 Named-Primitive Grounding (CSS Balanced Scan + Eq-Set Inner Kernel + find_component_delim NEON Retarget)

This extension EXTENDS the V2 dossier (carried verbatim above); it does not re-derive
the converged Layer 0 / Layer 1 / admission-discipline content. It absorbs the certified
SK-V18 GENERALIZATION — ONE grammar-driven generator emitting JSON+CSS+Sheets from `.bbnf`,
preserving >SOTA, aarch64-PRIMARY (Apple M5 Max; x86 SECONDARY/deleted in skinny). The SK-V18
S-P1 profile (`SYNTHESIS-PROFILE.md §3`) returns **G6=WIRE** on a single CSS scalar hot path
(`find_component_delim` 79.5% + `consume_balanced_at` 14.6% = **94.1%** of parser self-time).
The S-P2 §6 finding (`SYNTHESIS-RESEARCH.md §4`) names the **`css_balanced_component_scan`**
primitive (R-B PRIMARY §6) and its **inner alphabet-scan sub-kernel** (the `bbnf-simd` eq-set
member scan), and the R-F candidate (`SYNTHESIS-RESEARCH.md §1`/`§5-risk-6`) names the
**find_component_delim NEON retarget** — the ≤13-byte significant-set scan exceeding the
8-byte eq-set cap, salvaging the **two-fan OR-reduce** from the dead `find_css_significant`.

This extension grounds those three named §6 primitives against PRIMARY LITERATURE under the
Lock-16 admissibility contract: **published citation + abstract-primitive name + scalar
reference + checkasm-parity plan + same-wave consumer**. aarch64 NEON/dotprod only. An
undocumented hand-tuned intrinsic loop is INADMISSIBLE.

## SK-V18 Executive Summary

All three SK-V18 §6 primitives are GROUNDED as admissible under Lock 16 — every one carries a
real, verified primary citation, an abstract-primitive name, an on-disk scalar reference, a
checkasm-parity plan, and a same-wave consumer. The **eq-set member scan inner sub-kernel**
(`byte_class_from_eq_set_64`) is the strongest: it has a real aarch64 NEON body
(`vceqq_u8` fan + `vorrq_u8` OR-reduce + `vaddv_u8` movemask spill), a scalar oracle, a
checkasm harness, and a live consumer (`find_ascii_set_member64`); its movemask emulation is
grounded in Lemire's `neonmovemask_addv` ADDV-reduce technique and the "Lemire + Mula … AArch64
movemask spill" the in-tree comment names verbatim (`byte_class_from_eq_set_64.rs:63`-`64`). The
**find_component_delim NEON retarget** is grounded with a SURPRISE: the two-fan OR-reduce it
needs ALREADY EXISTS, fully written, in the dead `find_css_significant`
(`runtime_simd.rs:169-216`) — splitting the ≤13-byte significant set (≤4 caller delimiters +
9-byte fixed family) into `set_a[8]` + `set_b` and OR-reducing two eq-set fans; it is a
RETARGET (wire the recursive shell + neutrality demotion), NOT a new kernel. The
**css_balanced_component_scan SHELL** is grounded as a balanced-delimiter recognizer
(simdjson skip-region discipline) but is REFUTED as a *neutral* name absent a non-CSS caller:
the FORCED demotion to `css_balanced_component_scan` is grounded. No new amendment candidates
beyond the V2 four — the SK-V18 (a)-(d) named-primitive gate and neutrality-proof obligation
are already 1E LAC candidates; 2B confirms they are the correct admission discipline.

## SK-V18 Technique Grounding Table

Each SK-V18 row carries the Lock-16 cell inline: scalar_reference / checkasm-parity /
hardware_gate / same_wave_consumer.

| spec claim / T-P1 divergence id | published primary source (verified) | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| The eq-set member scan (`byte_class_from_eq_set_64`) is an admissible grammar-NEUTRAL inner alphabet-scan sub-kernel (S-P2 §4 R-B inner kernel; D-1E-V5-11). | Langdale & Lemire, "Parsing Gigabytes of JSON per Second", *The VLDB Journal* 28(6), 2019 (arXiv `https://arxiv.org/abs/1902.08318`) — vectorized character-class classification by broadcast-compare; Arm ACLE NEON intrinsics reference `https://arm-software.github.io/acle/neon_intrinsics/advsimd.html` (`vceqq_u8`=CMEQ, `vorrq_u8`=ORR, `vld1q_u8`=LD1, `vandq_u8`=AND, `vdupq_n_u8`=DUP all confirmed present) | **grounded** | Real NEON body at `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:34-72`: loads four `uint8x16_t` stripes (`vld1q_u8`), fans `vceqq_u8(s_i, vdupq_n_u8(member))` per set member, OR-reduces with `vorrq_u8`, packs via the `vaddv`-based movemask. This is the **abstract-primitive name `BYTE_CLASS_FROM_EQ_SET_64`**. scalar_reference=`src/scalar/byte_class_from_eq_set_64.rs:25-43` (`set.contains(&src[i])` bit loop); checkasm-parity=`tests/checkasm_parity.rs` strict eq-set case; hardware_gate=Apple-M5-Max-aarch64 NEON (body live, x86 AVX-512 arm is SECONDARY/deleted in skinny per P1); same_wave_consumer=the CSS `count_top_level_commas` path (the ONLY live production consumer) AND the SK-V18 G6 retarget shell (`find_ascii_set_member64` `lib.rs:208-226` is a wrapper with NO non-test/non-bench runtime caller — do NOT cite it as a live JSON consumer; the JSON `scan_dispatch` path rides `byte_class_from_table_64`, a DIFFERENT primitive). Inner kernel is STRUCTURALLY neutral (caller-supplied byte set ≤8; set is caller DATA, kernel names no grammar) but CSS-only by live consumer per Lock 14's bbnf-simd clause. |
| The NEON movemask spill (pack 64-lane compare into a 64-bit mask) is a grounded primitive, not a hand-tuned loop. | Daniel Lemire, `neonmovemask_addv` (vaddv-based movemask emulation), simdjson AArch64 path — confirmed via the in-tree citation comment and Lemire's published ARM movemask technique (`https://lemire.me/blog/2017/07/10/pruning-spaces-faster-on-arm-processors-with-vector-table-lookups/` lineage; the `vaddv_u8`/ADDV horizontal-reduce is the standard ACLE intrinsic) | **grounded** | `movemask_u8x16` at `byte_class_from_eq_set_64.rs:80-89`: AND with the `[1,2,4,…,128,1,2,…,128]` power-of-two pattern (`vandq_u8`), then `vaddv_u8(vget_low_u8)` + `vaddv_u8(vget_high_u8)` horizontal-reduce per half, concatenate. This is the published ARM movemask emulation (no native `pmovmskb` on AArch64). scalar_reference=implicit in the eq-set scalar oracle's bit loop; checkasm-parity=covered by the eq-set strict case (the 64-bit mask is the compared output); hardware_gate=aarch64 ADDV; same_wave_consumer=the eq-set body itself. The in-tree comment's "Lemire + Mula … AArch64 movemask spill" attribution is VERIFIED real, not confabulated. |
| `find_component_delim` (the 94.1% CSS hot leaf) is a valid G6=WIRE NEON-retarget target, not a RETIRE. | SK-V18 S-P1 profile `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md:88-103` (parser-share table: `find_component_delim` 79.5%, `consume_balanced_at` 14.6%, scalar-scan share 4121/4379=94.1%); dav1d/checkasm differential discipline FFmpeg `https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html` (`check_func`/`report`/buffer-clobber/`--bench` confirmed) | **grounded** | Live hot leaf at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:657-680` (`find_component_delim`) + `:693-713` (`consume_balanced_at`): byte-at-a-time scan, `delimiters.contains(&byte)` stop, recursing through `()[]{}` and skipping strings/comments. 94.1% ≫ ~8% wire threshold → WIRE is the grounded action; deleting a kernel covering a 94%-share path is wrong. hardware_gate=aarch64 NEON only; checkasm-parity=`neon_significant_skip_matches_scalar` guard retargeted to the recursive shell over the REAL 71KB-495KB corpora; same_wave_consumer=the P3-COLLAPSED single CSS scan (the retarget is sequencing-dependent on P3, then the emitted singular call site). |
| The ≤13-byte significant-set NEON scan exceeding the 8-byte eq-set cap is admissible via a two-fan OR-reduce (S-P2 §5-risk-6). | Langdale & Lemire 2019 (arXiv 1902.08318) skip-region / classify discipline + Arm ACLE (`vorrq_u8` OR-reduce confirmed); the cap rationale: NEON 16-lane compare admits ≤8 broadcast members per fan before the movemask packing exceeds register-economy, so two fans OR-reduce | **grounded — already implemented in the dead kernel** | The two-fan OR-reduce ALREADY EXISTS, fully written, at `skinny/crates/runtime/src/runtime_simd.rs:169-216` (`find_css_significant`): splits the ≤13-byte set into `set_a := fixed[..8]` (8) + `set_b := fixed[8] ⧺ delimiters[..≤4]` (≤5), then `mask = byte_class_from_eq_set_64(block, set_a) \| byte_class_from_eq_set_64(block, set_b)`, `trailing_zeros` to the first significant byte, scalar tail < 64 bytes. abstract-primitive name=`find_css_significant` (two-fan significant-set scan). scalar_reference=the scalar tail loop (`fixed.contains \|\| delimiters.contains`) + the eq-set scalar oracle; checkasm-parity=the eq-set strict case per fan; hardware_gate=aarch64 NEON; same_wave_consumer=**currently ZERO live callers** (R7: only `#[cfg(test)]` in `runtime/src/lib.rs:574`) — RETARGET this OR-reduce machinery onto the live recursive shell. |
| The dead `find_css_significant`/`find_comment_close` NEON kernels cover the dominant hot path as-written. | SK-V18 S-P1 profile R7 caveat `SYNTHESIS-PROFILE.md:105-112`; live caller census `skinny/crates/runtime/src/lib.rs:500-501,574,598,608` (test-only) | **refuted — must RETARGET, not wire as-is** | `find_css_significant` is a FLAT stop-at-delimiter skip; the hot `find_component_delim`+`consume_balanced_at` machine RECURSES through nested `()[]{}` and skips strings/comments. The NEON does NOT cover the dominant hot path as written. The OR-reduce *machinery* (set-split + two-fan classify) is the salvage; the recursive SHELL stays scalar (handles nesting/strings/error positions), and the vector skip stops AT `([{'"/` so the shell still drives recursion. RETARGET is the honest action; `find_comment_close` may RETIRE if balanced-consume retargeting proves unsafe. |
| `balanced_component_scan` is admissible as a grammar-NEUTRAL named primitive. | SK-V18 SYNTHESIS-RESEARCH §4 R-B (`restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md:231-237`); 1E neutrality-proof obligation `restart/audit/totality/p1/1E-locks-evidence.md:149` (LAC-1E-V5-03) | **refuted as neutral; grounded as `css_balanced_component_scan` (FORCED demotion)** | The balanced-recognizer SHELL is exercised ONLY by CSS in this campaign. The inner alphabet-scan sub-kernel (eq-set member scan) IS neutral (caller-supplied byte set), but the SHELL must be PROVEN neutral by ≥1 non-CSS invocation (JSON `{}`/`[]` nesting OR Sheets `paren_expr` balancing invoking the SAME primitive) — ELSE it demotes to the honestly CSS-scoped `css_balanced_component_scan`. 1E-V5-U2 grounds the demotion as FORCED: the offered non-CSS dischargers are parse-with-emit DESCENTS structurally incompatible with the byte-SKIP shell, so no current non-CSS caller exists. A neutrally-named single-grammar primitive is an overfit-in-waiting. |
| Every SK-V18 §6 primitive must carry the Lock-16 admission cell (citation + abstract name + scalar ref + checkasm-parity + same-wave consumer). | Lock 16 `restart/locks/LOCKS.md:453-491,:622`; FFmpeg checkasm `https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html`; SK-V18 (a)-(d) gate `SYNTHESIS-RESEARCH.md:257-266` | **grounded** | The minimum admission cell from V2-A2 stands; SK-V18 ADDS the (a)-(d) named-primitive gate: (a) grammar-INVOKED-by-name, (b) emitted-output-VARIES-under-invoking-rule-mutation, (c) `verbatim_blob_present==false`, (d) PROFILE-PROVEN-NARROW-LEAF (single profiled hot leaf; surrounding skeleton walk-derived). An undocumented hand-tuned intrinsic loop fails (c)+(d) and is INADMISSIBLE. All three SK-V18 primitives PASS the Lock-16 cell; the css_balanced_component_scan SHELL passes only under the demoted name. |

## SK-V18 Architectural Assertions Defended

### SKV18-A1 — The eq-set inner sub-kernel is the load-bearing, fully-admissible primitive

The eq-set member scan is the ONE SK-V18 §6 primitive that already discharges the full Lock-16
cell on the close host. Its NEON body
(`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:34-72`) is a real
broadcast-compare fan (`vceqq_u8`) + OR-reduce (`vorrq_u8`) over four 16-byte stripes
(`vld1q_u8`), packed with the published Lemire ARM movemask emulation (`vaddv_u8` ADDV
horizontal-reduce, `vandq_u8` against the power-of-two pattern). The classify-by-broadcast-compare
shape is the simdjson character-class technique (Langdale & Lemire 2019, The VLDB Journal 28(6)),
applied to a *caller-supplied* ≤8-byte set rather than a fixed JSON structural alphabet — which
is exactly what makes it grammar-NEUTRAL. The neutrality proof is STRUCTURAL: the set is caller
DATA, the kernel names no grammar. It is NOT an empirical dual-consumer claim — the kernel's ONLY
live production consumer is the CSS `count_top_level_commas` path (`runtime_simd.rs`); the
`find_ascii_set_member64` wrapper (`lib.rs:208-226`) has NO non-test, non-bench runtime caller,
and the JSON `scan_dispatch` path rides a DIFFERENT primitive (`byte_class_from_table_64`, the
256-LUT TBL classifier), not the eq-set kernel. So the kernel is neutral by construction but
CSS-only by live consumer — disclosed per Lock 14's bbnf-simd clause. It already has a scalar
oracle and a checkasm-style strict harness. SK-V18 admission needs only the same-wave G6 retarget
consumer + row movement.

NOTE — the cited neutrality-evidence file `skinny/crates/runtime/src/runtime_simd.rs:6`-`7`
carries an INACCURATE source comment ("the same kernel JSON's `scan_structurals` rides"). That
claim is empirically FALSE and is the refuted dual-consumer claim above: on the aarch64 close
host, JSON `scan_structurals` (`json/scan.rs:22`) returns `neon::scan(input)` (`:25`) →
`bbnf_simd::aarch64::classify_tbl4` (`:214`,`:219`,`:228`) — the TBL/`byte_class_from_table_64`
family, a DIFFERENT primitive than the eq-set kernel; `scan_structurals_scalar` (`:29`) is the
`#[allow(unreachable_code)]` non-aarch64 fallback, NOT the close-host route. Either way the eq-set
kernel is never on the JSON structural-scan path, and the JSON `scan_dispatch` path rides the TBL
`byte_class_from_table_64` family. The abstract
neutrality (caller-supplied byte set, kernel names no grammar) stands, but the file's
`scan_structurals`-rides comment is a same-wave G6 source-fix obligation, so a future consumer
reading the cited file does not re-adopt the refuted claim.

### SKV18-A2 — The find_component_delim NEON retarget is a SALVAGE, not a new kernel

The single sharpest SK-V18 grounding: the ≤13-byte two-fan OR-reduce that R-F/§5-risk-6 calls
for **already exists, fully written**, in the dead `find_css_significant`
(`skinny/crates/runtime/src/runtime_simd.rs:169-216`). The kernel splits the significant set —
≤4 caller `delimiters` (grammar-derived stop bytes) + a 9-byte `fixed` recognizer family
(`'"/([{` + closers) — into `set_a[8] = fixed[..8]` and `set_b = fixed[8] ⧺ delimiters`, runs
two eq-set fans, and OR-reduces (`mask_a | mask_b`), then `trailing_zeros` to the first
significant byte with a scalar tail for the final < 64-byte window. Both sets are caller data;
the kernel names no grammar. The 8-byte cap is the NEON register-economy bound on broadcast
members per fan (each fan is the SKV18-A1 eq-set primitive); two fans is the grounded way to
cover ≤16 members. The R-F move is therefore a **retarget** — wire this OR-reduce skip into the
recursive `find_component_delim` SHELL (the shell keeps nesting/string/comment recursion and
error positions; the vector skip stops AT `([{'"/` so it never swallows a structural byte the
shell must dispatch) — landed WITH its same-wave generated call site, NOT an orphan kernel. The
salvage is grounded against the dead kernel's own source + the eq-set citations.

The retarget is also LEDGER-grounded (REDRESS): the route has a PRODUCTION PRECEDENT and bounding
priors that govern the deferred-to-H1 net-win. REDRESS 144 (`skinny/REDRESS.md:4418`-`4438`,
`G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT) wired the same `find_ascii_set_member64` kernel class into
production CSS `Scanner::scan_block` delimiter search and MOVED the Track 1 CSS row (444.2 vs 434.1
Mbps, Criterion +109.87%, strict cssparser/lightningcss green) — so "the kernel already exists /
retarget the admitted route" is ledger-precedented, not asserted. The CAUTIONARY priors the H1
net-win must clear: REDRESS 96/97/98 (`:2795`-`2940`, `G-W3-UNION-SUBSTRATE` retired — two
correctness-green SIMD-structural-cursor-into-the-retained-JSON-parse-loop implementations
UNIFORMLY regressed every must-improve M5-Max row, the `:2928`-`2933` finding that the wide-issue
scalar delimiter path is cheaper than streaming a SIMD cursor) and REDRESS 126 (`:3766`-`3805`,
the same `find_ascii_set_member64(…, b"{};")` microbench passed 4.7× but was closed
`ROUTE-PRODUCTION-SPLIT` WITHOUT production wiring — a microbench/checkasm PASS is NOT a
production-row move). G6's inert-run net-win therefore re-opens a question the ledger answered
NEGATIVELY for the JSON streamed-cursor case and POSITIVELY for the CSS delimiter case — it is not
a fresh unknown, and the transient-same-loop-skip (G6) is the ADMISSIBLE side of the same line
REDRESS 50/51/53 (`:715`-`882`,`:807`-`813`) draws against the retained-cursor side.

### SKV18-A3 — The css_balanced_component_scan SHELL is the §6 primary finding, admitted only demoted

The 94.1% scalar hot path is a flat balanced-delimiter recognizer whose delimiter alphabet
(`{}:;`) and structural-byte dispatch (`'"/([{`) are EMERGENT from the rule shapes, modeled by
no `SinkOnlyExpr` node. A naive grammar-walk lowering produces the combinator-shaped recursive
descent (lightningcss's own architecture) that categorically regresses >SOTA — so the scan must
land as a grammar-parameterized NAMED PRIMITIVE taking grammar-DERIVED byte-set ARGS, invoked
from the emitted scan, with a per-primitive (a)-(d) mutate-falsifier (mutate the invoking
`.bbnf` rule → emitted ARG byte sets change). This SHELL is the SAME seam as the G6 NEON
retarget (one seam for G2+G6). Its INNER sub-kernel (SKV18-A1 eq-set scan) is genuinely
neutral; its outer balanced-recognizer SHELL is NOT proven neutral and is FORCE-DEMOTED to
`css_balanced_component_scan` (SKV18-A3 refutation below). The grounding is: the SHELL is a
real, profile-attributed, narrow §6 primitive (passes (a)-(d)); the NEUTRALITY of its name is
the refuted claim.

### SKV18-A4 — aarch64 is PRIMARY; x86 esoterica is SECONDARY and deleted in skinny

The SK-V18 close host is Apple M5 Max / aarch64 ONLY (P1 deletes the 28-file x86 surface
crate-wide). The eq-set body's `#[cfg(all(target_arch="x86_64", target_feature="avx512bw"))]`
AVX-512 arm (`lib.rs:285-289`) is SECONDARY: it is the strict additive lift of the same fan
(`vpcmpeqb` + `korq`-reduce, per the scalar oracle's asmjson citation comment) but it is a
prune target in skinny and can close NO row on M5 Max. The aarch64 NEON path is the sole close
route; x86/AVX-512/SVE2 literature is architecture pressure that cannot admit 2B vocabulary by
citation or ISA bit (carried from V2 refutations).

### SKV18-A5 — The FSM/frame-stack rebuild route is DELETE-only, NOT a retained-frame-stack reintroduction

The V2 A3a manifest (row `FSM_DISPATCH_THREADED` / frame-stack macro-family) and OQ-2B-05 name a
rebuild vehicle "a same-wave CollapsedStage consumer with scalar oracle and gate proof" whose
macro contract is a `FRAME_PUSH_BOUNDED`/`FRAME_POP_BOUNDED` stack. That is in TENSION with the V2
refuted-assertions table (line 188): "retained frame/open stacks are NOT safe primitive vocabulary
by default" (SPEC rejects retained cursor/list/sidecar substrate, `sk-v15/SPEC.md:147-153`; SK-V18
Lock 1 one-substrate `sk-v18/SPEC.md:397`-`402`). A `FRAME_PUSH/POP_BOUNDED` macro IS a retained
stack — the rebuild route silently reintroduces the very coupling the dossier blocks. RECONCILED
for SK-V18: the FSM/frame-stack rebuild route is **DELETE-only**. The balanced-nesting need is
ALREADY covered by the recursive scalar shell (`find_component_delim`/`consume_balanced_at`) +
the transient eq-set skip (SKV18-A2 / 2F PTG-2F-10) with NO frame stack — the scalar shell holds
the nesting state on the native call stack (transient, per-call), never a retained frame array.
If a future CollapsedStage wave ever reopens the route, any FSM/frame-stack rebuild MUST carry the
Lock-1 proof INLINE: a per-call FSM state threaded transiently (the `ParserState +
CollapsedStagePlan` carry, no retained side stream — see 2D LAC-2D-V3-04), NEVER a retained
`FRAME_PUSH/POP_BOUNDED` array. As written without that proof, the rebuild route is a latent
sidecar reintroduction and is INADMISSIBLE; the default disposition is DELETE.

## SK-V18 Architectural Assertions Refuted

| assertion | refutation | grounding |
|---|---|---|
| `balanced_component_scan` may keep a neutral name because its inner kernel is neutral. | **Refuted.** The inner eq-set sub-kernel is neutral (caller byte set), but the balanced-recognizer SHELL is exercised by CSS ONLY in this campaign and no non-CSS caller (JSON `{}`/`[]`, Sheets `paren_expr`) invokes the SAME byte-SKIP shell — the offered dischargers are parse-with-emit descents structurally incompatible with the skip shell (1E-V5-U2). FORCE-DEMOTE to `css_balanced_component_scan`. | `SYNTHESIS-RESEARCH.md:231-237`; `1E-locks-evidence.md:136` (1E-V5-U2), `:149` (LAC-1E-V5-03). |
| The dead `find_css_significant`/`find_comment_close` NEON kernels can be WIRED as-is to cover the 94.1% hot path. | **Refuted.** They are FLAT stop-at-delimiter skips; the hot machine RECURSES through nested `()[]{}` and skips strings/comments. RETARGET the OR-reduce skip onto the recursive shell (shell stays scalar for nesting/error positions); retire `find_comment_close` only if balanced-consume retargeting proves unsafe. | `SYNTHESIS-PROFILE.md:105-112`; live caller census `runtime/src/lib.rs:574` (`#[cfg(test)]`). |
| The eq-set 8-byte cap is a hard limit, so the ≤13-byte CSS significant set cannot be NEON-classified. | **Refuted.** The 8-byte cap is per-FAN; two fans OR-reduce to cover ≤16 members. Already implemented at `runtime_simd.rs:189-201`. The cap is NEON register-economy on broadcast members, not an algorithmic wall. | `runtime_simd.rs:169-216`; Arm ACLE `vorrq_u8`. |
| x86 AVX-512 `vpcmpeqb`/`korq` evidence can close the SK-V18 eq-set row on the M5 Max. | **Refuted.** x86 is SECONDARY and crate-deleted in skinny (P1). Only the aarch64 NEON body or a scalar delegate closes; AVX-512 is a strict-additive lift but a prune target. | `1E-locks-evidence.md:96,150` (LAC-1E-V5-04, aarch64-ONLY); `lib.rs:285-289`. |

## SK-V18 Open Research Questions

| id | UNKNOWN | verify_action |
|---|---|---|
| OQ-2B-SKV18-01 | Does the retargeted two-fan OR-reduce skip move the 94.1% CSS row, or does the inert-run length bound the realized speedup below dispatch overhead? **LEDGER-FRAMED:** this is NOT a fresh unknown — REDRESS 96/97/98 (`skinny/REDRESS.md:2795`-`2940`,`:2928`-`2933`) answered the analogous JSON streamed-cursor case NEGATIVELY on this exact host (the wide-issue scalar loop beat the SIMD cursor), and REDRESS 144 (`:4418`-`4438`) answered the CSS delimiter case POSITIVELY (+109.87%). The G6 inert-run net-win must clear the negative prior, AND a checkasm/microbench PASS is explicitly NOT a row move (REDRESS 126 `:3766`-`3805` `ROUTE-PRODUCTION-SPLIT`). | Wire the retarget into the P3-collapsed single CSS scan, measure `track1_rich/lcss` on `css_canon_bench` cold corpus-in-timer against the S-P1 ratio under a QUIET re-capture (S-P1 ran loadavg 4.35; absolute Mbps is DIRECTIONAL). Realized speedup is bounded by inert-run length — a post-wire MEASUREMENT that must beat the REDRESS-98 scalar-cheaper finding, not merely a microbench PASS. |
| OQ-2B-SKV18-02 | Can the `css_balanced_component_scan` SHELL ever re-promote to a neutral name via a future non-CSS caller (JSON/Sheets balanced nesting)? | T-P3 binds whether the neutrality-proof obligation permits re-promotion on a future non-CSS caller, or whether the byte-SKIP-vs-parse-with-emit structural incompatibility makes the CSS-scoped name permanent. Re-grep `rg balanced_component_scan skinny/crates/codegen` after G2 lands. |
| OQ-2B-SKV18-03 | Does the retarget honor the (d) PROFILE-PROVEN-NARROW-LEAF bound — is the NEON skip strictly the inert-run-skip leaf, with the recursive shell remaining walk/scalar-derived? | Machine-check the primitive LOC vs the profiled hot-leaf extent: the NEON skip must cover ONLY the inert-run-skip inner loop, never the balanced-recursion shell or error-position handling, else it is an over-large relabeled blob = REJECT. |

## SK-V18 Cost Manifest (wave re-key + Lock-16 v+1 columns)

The V2 A3a manifest (lines 147-158) carries SK-V15 `W#` wave owners (W2/W7/W8/W9) verbatim and
those describe the retired SK-V15 ledger. For the SK-V18 close, every SK-V18-scope primitive row
is RE-KEYED to the LIVE SK-V18 §8 wave ledger `P4/P5/G1/G2/G3/G4/G5-G6/PROVE/H1`
(`restart/skinny/tranches/sk-v18/SPEC.md:436`-`444`; G2/G5-G6 = `≤450 hand LOC`, `≤90 min wall`,
`30-45 min/redress`), and carries the Lock-16 v+1 `rollback path` + `abrogate threshold` columns:

| SK-V18 primitive | wave_owner (LIVE §8) | loc_estimate | risk_class | rollback path | abrogate threshold | final disposition |
|---|---|---|---|---|---|---|
| eq-set member scan (SKV18-A1, `byte_class_from_eq_set_64`) | G5/G6 | ≈ 0 (body live, +test/report only) | LOW (already admissible) | leave the retarget unwired; the kernel stays a CSS `count_top_level_commas` sub-leaf | if no SK-V18 grammar exercises the retarget, keep CSS-only (no orphan promotion) | wired (G5/G6 retarget consumer) |
| find_component_delim NEON retarget (SKV18-A2, salvage two-fan OR-reduce) | G5/G6 | ≤150 (retarget onto recursive shell + checkasm ext) | MED-HIGH | revert to scalar `find_component_delim` (no shipped output depends on the SIMD skip) | if the SIMD skip cannot net-beat the M5-Max scalar loop (REDRESS 96/98 risk), ABROGATE the wire and keep scalar (G6 outcome stays `C`) | retarget-not-author |
| `css_balanced_component_scan` SHELL (SKV18-A3, force-demoted) | G2 | ≤200 (shell + grammar-derived arg derivation) | MED-HIGH | G2 keeps the scalar balanced shell | if arg-derivation under-delivers a neutral primitive, ABROGATE to the CSS-scoped name permanently (never re-promote without a non-CSS caller) | grounded-as-demoted |
| FSM/frame-stack rebuild route (A3a / OQ-2B-05) | none (DELETE-default) | 0 (no rebuild authored) | n/a | n/a (source stays unwired) | if reopened, the FSM/frame-stack rebuild is INADMISSIBLE unless it carries the Lock-1 transient-per-call-FSM-state proof — a retained `FRAME_PUSH/POP_BOUNDED` array is a refuted sidecar | DELETE-only (see SKV18-A5 reconcile) |

The V2 `W#` wave keys in A3a stand only as the SK-V15 historical record; no SK-V18 close cost is
budgeted against a `W#` wave.

## SK-V18 LOCKS-AMENDMENTS-CANDIDATE

**No NEW 2B amendment candidates beyond the V2 four.** The SK-V18 §6 named-primitive (a)-(d)
gate and the `css_balanced_component_scan` neutrality-proof obligation are ALREADY 1E candidates
(`1E-locks-evidence.md`: LAC-1E-V5-01 binds the (a)-(d) gate into Lock 14/16; LAC-1E-V5-03 binds
the neutrality-proof demotion; LAC-1E-V5-04 sharpens the aarch64-ONLY x86-deletion standing).
2B's role here is to CONFIRM, against verified primary literature, that those three 1E candidates
are the correct admission discipline for the three SK-V18 §6 primitives grounded above — every
primitive carries a real citation + abstract name + scalar reference + checkasm-parity + same-wave
consumer, so the Lock-16 cell as amended by LAC-1E-V5-01/03/04 admits all three (the SHELL only
under its demoted name). The V2 LAC-2B-V2-01..04 (primitive-manifest schema, `scalar-delegated`
fallback definition, `bbnf.asm` inventory prohibition, per-row `lock16_status` telemetry) carry
forward unchanged and are sufficient for the SK-V18 primitive vocabulary.

| id | target lock / surface | candidate amendment | disposition |
|---|---|---|---|
| (no new candidate) | Lock 14 / Lock 16 | The SK-V18 §6 (a)-(d) gate + neutrality-proof demotion + aarch64-ONLY standing are 1E-owned (LAC-1E-V5-01/03/04). 2B confirms they correctly admit the three grounded SK-V18 primitives. | EXPLICIT no-new-2B-candidate row; defer to 1E candidates. |
