---
agent: 2E
pass: T-P2-research
cycle: V2
generated_at: 2026-05-28T14:20:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 19
techniques_grounded: 8
techniques_refuted: 4
prior_cycle_dispositions_folded:
  accepted: [CH6-V1-05-preserve-2E-primitive-manifest-template]
  rejected: []
  revised: [CH4-V1-01-add-LOC-risk-wave-owner-hard-cap-to-host-primitives, CH6-V1-05-preserve-inline-scalar-checkasm-hardware-consumer-row-movement-gates]
  first_cycle_additions: [SKV15-2E-A64-PRIMARY, SKV15-2E-CSSC-HOST-GATE, SKV15-2E-SVE2-REFUTE]
locks_amendment_candidates: 3
sk_cycle: SK-V15
t_p1_entry_state: CLEAN-FINAL-G1-AUTO-PINNED-NOT-NORMAL-3Z
implementation_floor: PASS-IMPL-V1-CSS-CONTRIVANCE-JSON-HONEST
host_close_route: Apple-M5-Max-aarch64
stale_sk_v14_material_reused: none
---

## Executive Summary

SK-V15 2E keeps aarch64 as the only close route. The local host probe
(`sysctl`) reports `machdep.cpu.brand_string=Apple M5 Max` with
`FEAT_AES=1`, `FEAT_PMULL=1`, `FEAT_DotProd=1`, `FEAT_I8MM=1`,
`FEAT_CSSC=1`, `FEAT_SHA3=1`, `FEAT_SME=1`, and no
`hw.optional.arm.FEAT_SVE2` key. That supports hardware-gating rows, not
admission: every primitive still needs scalar oracle, strict checkasm or
parity, row-local equality, row-local timing, and a same-wave consumer per
SK-V15 SPEC lines 76-78 and 143-145.

Defended: NEON/AdvSIMD TBL-classify remains the aarch64 baseline; DotProd
and I8MM are real but conditional numeric primitives; PMULL and CSSC are
real hardware, but REDRESS blocks production promotion from ISA/checkasm
alone; PMU validation belongs in W0/W11 evidence. Refuted: a NEON
`svmatch_u8` route, AVX-512 as close evidence, CSSC/PMULL replay without a
new consumer, and CSS broadcast rows as architecture proof.

## Source Registry

| ID | Primary source | Used for |
|---|---|---|
| SRC-A64-ACLE | Arm C Language Extensions, feature macros and intrinsics gates, 2026Q1, <https://arm-software.github.io/acle/main/acle.html> | `__ARM_FEATURE_PMULL`, `__ARM_FEATURE_DOTPROD`, `__ARM_FEATURE_MATMUL_INT8`, `__ARM_FEATURE_CSSC`, `__ARM_FEATURE_SHA3` style gates. |
| SRC-A64-NEON | Arm Neon Intrinsics Reference, 2026Q1, <https://arm-software.github.io/acle/neon_intrinsics/advsimd.html> | `vqtbl4q_u8`, `vld1q_u8_x4`, `vld4q_u8`, `vmull_p64`, `vdotq_u32`, `vusdotq_s32`, `veor3q_u8`, `vbcaxq_u8`. |
| SRC-A64-DOTPROD | Arm, "Exploring the Arm dot product instructions", <https://community.arm.com/arm-community-blogs/b/tools-software-ides-blog/posts/exploring-the-arm-dot-product-instructions> | Named technique source for UDOT/SDOT digit/vector dot-product usage. |
| SRC-A64-SVMATCH | Arm, "Multi-token search strings using the SVMATCH instruction", <https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/multi-token-search-strings-svmatch-instruction> | SVE2 MATCH/NMATCH is scalable-vector, not NEON. |
| SRC-A64-SVE-BASICS | Arm Learn, SVE basics, <https://learn.arm.com/learning-paths/servers-and-cloud-computing/sve/sve_basics/> | Separates SVE/SVE2 scalable vectors from fixed-width NEON/AdvSIMD. |
| SRC-A64-PMU | Arm Learn, "Counter access options" in "How to use the Arm Performance Monitoring Unit and System Counter", <https://learn.arm.com/learning-paths/servers-and-cloud-computing/arm_pmu/access_options/> | PMU validation discipline: cycles/instructions are measurement evidence, not inferred from throughput. |
| SRC-SIMDJSON | Langdale and Lemire, "Parsing Gigabytes of JSON per Second", VLDBJ 2019 / arXiv:1902.08318, <https://arxiv.org/abs/1902.08318> | Published structural classification, quote masking, and SIMD parser primitive lineage. |
| SRC-LANGDALE-PCLMUL | Geoff Langdale, "Code Fragment: Finding quote pairs with carry-less multiply (PCLMULQDQ)", 2019, <https://branchfree.org/2019/03/06/code-fragment-finding-quote-pairs-with-carry-less-multiply-pclmulqdq/> | Named PMULL/PCLMUL prefix-XOR lineage. |
| SRC-LANGDALE-NEON | Geoff Langdale, "Fitting My Head Through The ARM Holes...", 2019, <https://branchfree.org/2019/04/01/fitting-my-head-through-the-arm-holes-or-two-sequences-to-substitute-for-the-missing-pmovmskb-instruction-on-arm-neon/> | NEON movemask substitute and fixed-width NEON process constraints. |
| SRC-INTEL-INTRIN | Intel Intrinsics Guide, <https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html> | x86 secondary contrast: AVX2/AVX-512/VBMI2/GFNI/VPCLMUL/VNNI are diagnostic only for SK-V15. |
| SRC-LOCAL-CLASSIFY | `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:17`, `:29`, `:31`, `:82` | Current NEON TBL/LD1x4 classifier body. |
| SRC-LOCAL-DIGITMAC | `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:10`, `:25`, `:39`, `:51`, `:63` | Current DotProd proof body and scalar fallback. |
| SRC-LOCAL-SCALAR-DELEGATES | `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1` | PMULL/CSSC/bulk emit are currently scalar delegates on aarch64. |
| SRC-LOCAL-LOCK16 | `skinny/crates/bbnf-simd/src/lib.rs:245`, `:251`, `:260`, `:265`, `:270`, `:282` | Local primitive facade and Lock 16 executable-spec posture. |
| SRC-LOCAL-CHECKASM | `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1`, `:3`, `:13`, `:16`, `:30` | Strict differential admission pattern and current non-strict caveat. |
| SRC-LOCAL-PMU | `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs:1`, `:5`, `:6`, `:62`, `:63`, `:113` | Apple PMU capture harness and cold-loop sanity parse. |
| SRC-SPEC | `restart/skinny/tranches/sk-v15/SPEC.md:51`, `:64`, `:76`, `:100`, `:119`, `:135`, `:143`, `:287`, `:295`, `:480` | SK-V15 host, telemetry, Lock 16, and PMULL/CSSC non-negotiables. |
| SRC-RESULTS | `skinny/RESULTS.md:112`, `:128`, `:135` | Current CSS rows still show one repeated W8R tuple and Apple M5 Max/aarch64 host metadata. |
| SRC-OVERFIT | `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:17`, `:24`, `:76`, `:100`, `:120` | PASS-IMPL floor: JSON honest, CSS broadcast/brace-counter contrived, target-cpu=native required. |

## Technique Grounding Table

Every V2 row below carries the anti-paper-close manifest required by CH4/CH6.
Apple M5 Max/aarch64 remains the only close route; x86, SVE2-on-non-SVE2
hardware, and ISA/checkasm-only PMULL/CSSC rows are not close evidence.

| spec claim / primitive | published source cited | state | abstract primitive | V2 admission and cost manifest |
|---|---|---|---|---|
| NEON/AdvSIMD TBL low-6 byte classify | SRC-A64-NEON, SRC-SIMDJSON, SRC-LOCAL-CLASSIFY | grounded | `ByteClassLookup64Tbl` | transfer_reason=local TBL classifier is the aarch64 baseline for byte-class lookup and maps directly to Lock 16 primitive status; admission_gate=Apple M5 Max/aarch64 host transcript, strict byte-class equality, cold same-plane row timing, and no CSS broadcast group; verification_action=run strict `checkasm_byte_class_from_table_64`/`checkasm_parity`, inspect `vld1q_u8`/`vqtbl4q_u8`, then attach W11 JSON 51/51 row-local equality/timing; close_status=admissible-after-gate; scalar_reference=scalar table lookup oracle; parity_or_checkasm=strict checkasm plus same-workload parity, not logged-only; hardware_gate=`target_arch=aarch64` with AdvSIMD baseline on Apple M5 Max; same_wave_consumer=W2 Lock 16 status plus W11 JSON close rows; row_movement_target=byte-class hot leaf moves only if same-plane W11 equality and timing beat scalar; loc_estimate=40-80 LOC to harden strict checkasm/status plumbing; risk_class=medium; wave_owner=W2 primitive owner with W11 evidence owner; hard_cap_fit=yes, bounded to existing facade and classifier body. |
| LD1x4 table load / LD4 classify candidate | SRC-A64-NEON, SRC-LOCAL-CLASSIFY | partial | `Interleave4Classify` | transfer_reason=existing `vld1q_u8_x4` table load suggests a possible interleave/deinterleave family but does not itself prove parser row movement; admission_gate=profiled four-stream SoA consumer plus scalar deinterleave oracle, alignment sweep, strict equality, and cold row timing on Apple M5 Max/aarch64; verification_action=write dedicated LD4/deinterleave checkasm only after W5/W6 or string/UTF-8 profiling names the consumer; close_status=partial-blocked; scalar_reference=scalar deinterleave/classify oracle; parity_or_checkasm=dedicated LD4 strict checkasm plus alignment and tail cases; hardware_gate=AdvSIMD on Apple M5 Max/aarch64, no PMULL/CSSC/DotProd gate; same_wave_consumer=none in SK-V15 V1, future W5/W6 typed CSS or string/UTF-8 only if profiled hot; row_movement_target=blocked until a four-stream consumer moves a named row; loc_estimate=120-220 LOC if reopened; risk_class=high; wave_owner=W5/W6 future consumer owner, not W2 default; hard_cap_fit=conditional, only if consumer is named in the same wave. |
| PMULL prefix-XOR lineage | SRC-A64-ACLE, SRC-A64-NEON, SRC-LANGDALE-PCLMUL, SRC-SIMDJSON, SRC-LOCAL-SCALAR-DELEGATES, SRC-SPEC | partial / architecture-pressure | `PrefixXor64Pmull` | transfer_reason=PMULL can express carry-less prefix-XOR lineage for quote-mask style work, but local aarch64 currently delegates prefix-XOR to scalar; admission_gate=Apple M5 Max/aarch64 with FEAT_PMULL, emitted `pmull` proof, scalar prefix-XOR oracle, strict checkasm, same-wave consumer that removes scalar consume cost, and row-local JSON/CSS equality/timing; verification_action=keep scalar-delegated until a wave supplies asm transcript, strict parity, and measured row movement; close_status=scalar-delegated; scalar_reference=current scalar `bitmap_prefix_xor_64` oracle; parity_or_checkasm=strict prefix-XOR checkasm plus row-local parser parity; hardware_gate=`hw.optional.arm.FEAT_PMULL=1` and compile `+pmull` or equivalent emitted-asm proof; same_wave_consumer=none by default, required before reopening; row_movement_target=quote-mask/prefix-XOR row only if same-wave scalar bottleneck is deleted or bypassed; loc_estimate=120-240 LOC for intrinsic body, dispatch, asm probe, and tests; risk_class=high; wave_owner=W2 blocks/status owner until W11 or a named parser wave supplies consumer; hard_cap_fit=no for close from ISA/checkasm alone, conditional with consumer. |
| CSSC next-set-bit / count primitives | SRC-A64-ACLE, SRC-A64-NEON, SRC-LOCAL-SCALAR-DELEGATES, SRC-SPEC | partial / architecture-pressure | `NextSetBitCssc` | transfer_reason=CSSC may lower next-set/count operations, but current aarch64 next-set and bulk-emit paths are scalar delegates; admission_gate=Apple M5 Max/aarch64 with FEAT_CSSC, emitted CSSC asm, scalar next-set/bulk-emit oracle, strict checkasm, same-wave compact/bulk-emit consumer, and row-local timing; verification_action=W2 records scalar-delegated/source-present status until a consumer removes the scalar bottleneck and proves movement; close_status=scalar-delegated; scalar_reference=current scalar `bitmap_next_set_bit` and `bulk_emit_positions_64` oracles; parity_or_checkasm=strict next-set/count/bulk-emit checkasm plus parser equality; hardware_gate=`hw.optional.arm.FEAT_CSSC=1` and compile `+cssc` or equivalent emitted-asm proof; same_wave_consumer=none yet, required compact/bulk-emit row before production; row_movement_target=bitmap scan/bulk emit only after cold row-local movement; loc_estimate=90-180 LOC for intrinsic body, dispatch, asm probe, and tests; risk_class=high; wave_owner=W2 status owner with future compact/bulk-emit consumer owner; hard_cap_fit=no from ISA/checkasm alone, conditional with same-wave consumer. |
| DotProd UDOT/SDOT digit MAC | SRC-A64-ACLE, SRC-A64-NEON, SRC-A64-DOTPROD, SRC-LOCAL-DIGITMAC | partial | `DigitMac4Udot` | transfer_reason=local DotProd proof can accelerate four-digit decode if profiling names numeric decode as hot; admission_gate=Apple M5 Max/aarch64 with FEAT_DotProd, target-feature fallback, exhaustive valid/invalid checkasm, same-wave numeric consumer, and W0/W11 hot-leaf evidence; verification_action=run all 0000-9999 plus invalid-byte cases, inspect `udot`, then compare row-local numeric parse equality/timing; close_status=source-present-unwired; scalar_reference=`parse_4_digits` scalar fallback; parity_or_checkasm=exhaustive digit checkasm plus invalid-byte parity; hardware_gate=`hw.optional.arm.FEAT_DotProd=1` and Rust `target_feature="dotprod"` fallback/reject path; same_wave_consumer=none until W0/W11 names numeric decode hot and W7 selects it; row_movement_target=numeric hot leaf only, never proof-only helper code; loc_estimate=80-160 LOC for exhaustive tests, dispatch polish, and consumer wiring; risk_class=medium; wave_owner=W7 cost owner after W0/W11 profiling; hard_cap_fit=conditional, fits only when profiling names a hot numeric row. |
| I8MM mixed-sign dot product | SRC-A64-ACLE, SRC-A64-NEON | partial | `DigitMac8I8mm` / `PackedByteDotI8mm` | transfer_reason=I8MM is a possible packed-byte dot product family but has no local body or named parser consumer; admission_gate=Apple M5 Max/aarch64 with FEAT_I8MM, new scalar oracle, dedicated checkasm, emitted matrix instruction proof, and same-wave multi-digit consumer; verification_action=do not admit until a consumer and scalar oracle are written, then run strict parity and row timing; close_status=partial-blocked; scalar_reference=new scalar packed-byte/multi-digit oracle required; parity_or_checkasm=new dedicated I8MM checkasm required; hardware_gate=`hw.optional.arm.FEAT_I8MM=1` and compile `+i8mm` or ACLE `__ARM_FEATURE_MATMUL_INT8` proof; same_wave_consumer=none in SK-V15 V1, future CSS numeric/token or Sheets math decode only; row_movement_target=blocked until a packed multi-digit row is profiled hot; loc_estimate=180-320 LOC for oracle, intrinsic body, dispatch, tests, and consumer; risk_class=high; wave_owner=future W7/W5/W6/Sheets consumer owner; hard_cap_fit=no without named consumer. |
| SHA3 ternary bitwise EOR3 / BCAX | SRC-A64-ACLE, SRC-A64-NEON | partial | `TernaryXor3Eor3` / `BicXor3Bcax` | transfer_reason=SHA3 ternary bitwise instructions may lower grammar-neutral 3-input mask expressions if the Decision Engine emits one; admission_gate=Apple M5 Max/aarch64 with FEAT_SHA3, scalar boolean oracle, emitted `eor3`/`bcax`, strict checkasm, and a same-wave mask-expression consumer; verification_action=W7/W8 must first name the expression, then W2 adds oracle/checkasm and W11 measures row movement; close_status=partial-blocked; scalar_reference=new exact 3-input boolean oracle required; parity_or_checkasm=dedicated EOR3/BCAX checkasm plus parser equality; hardware_gate=`hw.optional.arm.FEAT_SHA3=1` and compile `+sha3` emitted-asm proof; same_wave_consumer=none until Decision Engine emits a grammar-neutral 3-input mask; row_movement_target=blocked until W7/W8 names and W11 moves that mask row; loc_estimate=80-160 LOC after expression exists; risk_class=medium; wave_owner=W7/W8 decision owner plus W2 primitive owner; hard_cap_fit=conditional, blocked without expression. |
| PMU validation on Apple M5 Max | SRC-A64-PMU, SRC-LOCAL-PMU, SRC-SPEC | grounded | `PmuColdParseEvidence` | transfer_reason=PMU cycles/instructions are required to validate row-local hot leaves after equality on the close host; admission_gate=Apple M5 Max/aarch64 host transcript, row-local command, input, equality result, timing result, and no broadcast group; verification_action=run `xctrace_probe` or successor with sanity parse and attach command transcript to W0/W11 evidence; close_status=admissible-after-gate; scalar_reference=not a SIMD oracle, equality comparator must precede counters; parity_or_checkasm=row-local equality plus PMU transcript, no checkasm-only admission; hardware_gate=Apple M5 Max/aarch64 only; same_wave_consumer=W0 telemetry lock and W11 close handoff; row_movement_target=measured parser row after equality, not architectural inference; loc_estimate=60-120 LOC for transcript/status hardening; risk_class=medium; wave_owner=W0/W11 evidence owner; hard_cap_fit=yes for evidence capture, not for admitting wrong-plane CSS rows. |
| x86 AVX-512/VBMI2/GFNI/VPCLMUL/VNNI contrast | SRC-INTEL-INTRIN | grounded as secondary only | `X86FutureBackendContrast` | transfer_reason=x86 intrinsics inform future backend vocabulary but cannot close SK-V15; admission_gate=none for SK-V15 close because SPEC lines 135-136 require Apple M5 Max/aarch64; verification_action=keep as diagnostic source inventory only and do not attach to W11 close evidence; close_status=diagnostic-only; scalar_reference=future x86 backend would require its own scalar oracle; parity_or_checkasm=future strict x86 checkasm required, not part of SK-V15; hardware_gate=CPUID features are future-backend gates only; same_wave_consumer=none for SK-V15; row_movement_target=none for SK-V15, future backend row only; loc_estimate=0 LOC in SK-V15; risk_class=low if diagnostic-only, critical if promoted; wave_owner=none-SK-V15; hard_cap_fit=no, outside Apple M5 Max/aarch64 close route. |
| NEON `svmatch_u8` port | SRC-A64-SVMATCH, SRC-A64-SVE-BASICS, local host probe | refuted | `MatchSetSve2`, not NEON | transfer_reason=prompt-level route is invalid because MATCH/NMATCH is SVE/SVE2, not NEON/AdvSIMD; admission_gate=blocked on this host because local M5 Max probe has no `hw.optional.arm.FEAT_SVE2` key and NEON cannot supply `svmatch_u8`; verification_action=do not write NEON `svmatch_u8` checkasm, and require a future SVE2 host plus scalable-vector dispatch family before any new row; close_status=refuted; scalar_reference=N/A for SK-V15 refuted route; parity_or_checkasm=no NEON checkasm row permitted; hardware_gate=SVE2 MATCH/NMATCH host required, absent here; same_wave_consumer=none for SK-V15; row_movement_target=remove from SK-V15 candidates and use TBL/equality NEON patterns instead; loc_estimate=0 LOC for SK-V15; risk_class=critical if misfiled as NEON, low when refuted; wave_owner=W2 guard owner; hard_cap_fit=yes as deletion/refutation, no as implementation. |

## Architectural Assertions Defended

1. **aarch64 is the close route.** SK-V15 SPEC requires all JSON rows on
   native Apple M5 Max/aarch64 and x86 only as diagnostic signal
   (`SPEC.md:51-53`, `:135-136`). 2E therefore treats Intel AVX-512 as
   vocabulary contrast, never admission evidence.

2. **Instruction availability is a gate, not admission.** SPEC requires
   strict parity/checkasm and cold per-parse evidence (`SPEC.md:76-78`,
   `:143-145`). The local source confirms this split: primitive facades
   exist in `lib.rs:251-282`, while the aarch64 PMULL/CSSC-adjacent bodies
   are scalar delegates in `bitmap_prefix_xor_64.rs:1-4`,
   `bitmap_next_set_bit.rs:1-4`, and `bulk_emit_positions_64.rs:1-4`.

3. **TBL-classify is the aarch64 baseline primitive.** It is grounded by
   Arm NEON intrinsics and simdjson's structural-classification lineage,
   and the local implementation uses `vqtbl4q_u8` over a caller-supplied
   low-6-bit table (`classify_tbl4.rs:17-31`). Its SK-V15 task is Lock 16
   status plus row-local consumers, not new theory.

4. **PMU validation is required after equality, not before it.** The
   existing `xctrace_probe` harness reads Apple cycles/instructions
   (`xctrace_probe.rs:5-8`, `:62-63`) and performs a sanity parse before
   the loop (`:113-120`). That is fit for W0/W11 telemetry, but cannot
   rescue CSS rows whose measurement tuple is broadcast across conceptual
   rows (`skinny/RESULTS.md:112-135`).

5. **CSSC is host-gated on this M5 Max, but still blocked by REDRESS.**
   The local host exposes `FEAT_CSSC=1`; nevertheless SPEC line 480 says
   no PMULL or CSSC production hot-body promotion from checkasm/ISA alone.
   W2 should report CSSC source-present status honestly, not turn it into
   close evidence.

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| "NEON `svmatch_u8` can be ported directly." | Arm's MATCH/NMATCH material is SVE/SVE2, while NEON/AdvSIMD is fixed-width. The local M5 Max probe lacks an SVE2 sysctl key. | Remove NEON `svmatch_u8` from SK-V15 candidates; use TBL/equality NEON patterns instead. |
| "PMULL or CSSC should land because M5 Max exposes the feature." | Local aarch64 files delegate PMULL/CSSC-adjacent primitives to scalar, and SPEC line 480 blocks PMULL/CSSC production hot-body promotion from ISA/checkasm alone. | Any reopen needs a materially different same-wave consumer with scalar cost removed and row-local movement. |
| "AVX-512 esoterica can help close SK-V15." | SPEC lines 135-136 make Apple M5 Max/aarch64 the only admission host and x86 diagnostic. | Keep x86 rows as future-backend vocabulary only. |
| "PMU counters can validate the CSS W8R rows." | PMU counters validate a measured run, but PASS-IMPL and RESULTS show CSS rows reuse one aggregate tuple; SK-V15 SPEC lines 54-58 and 96-98 demote that evidence. | PMU evidence must be row-local and paired with typed same-workload equality before CSS admission. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Whether Apple clang/rustc on this host accepts explicit `+cssc`, `+i8mm`, `+sha3`, and `+pmull` in the same release profile used by W0/W11. | W2 should add an emitted-asm probe per feature and record accepted flags plus `otool -tV`/`cargo asm` output. |
| Whether any JSON, CSS typed-value, Sheets, or BBNF-self row has a numeric hot leaf large enough for DotProd/I8MM. | W0/W11 PMU attribution must name a hot symbol before W7 can ask the cost model to choose `DigitMac4Udot` or I8MM. |
| Whether LD4 deinterleave beats four independent loads on M5 Max for parser input. | Only test after W5/W6 or later profiling names a four-stream consumer; otherwise mark LD4 source-backed/no-consumer. |
| Whether ternary bitwise (`EOR3`/`BCAX`) matches a generated grammar-neutral mask expression. | W7/W8 should emit a decision report showing a 3-input boolean expression; then add scalar oracle/checkasm and row-local timing. |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | amendment text | rationale |
|---|---|---|
| LOCK16-A64-HOST-GATE | Lock 16 should report each aarch64 primitive as `wired`, `scalar-delegated`, `source-present-blocked`, `strict-checkasm-admitted`, or `deleted`, and include the exact hardware gate plus emitted-asm proof for non-baseline features. | Prevents M5 Max feature bits from being mistaken for primitive admission. |
| LOCK16-PMU-ROW-LOCAL | PMU counters may support close only when attached to a row-local command, input, equality result, timing result, host feature transcript, and no broadcast group. | Keeps W0/W11 PMU validation from laundering CSS broadcast evidence. |
| LOCK16-SVE2-SEPARATION | SVE/SVE2 primitives must not be filed under NEON/AdvSIMD. A future `svmatch_u8` row requires an SVE2 host gate and separate scalable-vector dispatch plan. | Refutes the prompt-level stale phrase "NEON `svmatch_u8` port" before it becomes implementation scope. |
