---
agent: 2E
pass: T-P2-research
cycle: V1
generated_at: 2026-05-28T06:37:30Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 19
techniques_grounded: 8
techniques_refuted: 4
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
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

| spec claim / primitive | published source cited | state | abstract primitive | hardware gate | scalar oracle / checkasm admission plan | same-wave consumer |
|---|---|---|---|---|---|---|
| NEON/AdvSIMD TBL low-6 byte classify | SRC-A64-NEON, SRC-SIMDJSON, SRC-LOCAL-CLASSIFY | grounded | `ByteClassLookup64Tbl` | `target_arch=aarch64`; FEAT_AdvSIMD baseline on the close host; host metadata must be Apple M5 Max/aarch64 per SRC-SPEC | Scalar table lookup remains executable oracle; `checkasm_byte_class_from_table_64` and `checkasm_parity` must be strict, not logged-only; local body uses `vld1q_u8`, `vqtbl4q_u8` at `classify_tbl4.rs:29-31` | W2 Lock 16 primitive status, then W11 JSON 51/51 cold same-plane guard. |
| LD1x4 table load / LD4 classify candidate | SRC-A64-NEON, SRC-LOCAL-CLASSIFY | partial | `Interleave4Classify` | FEAT_AdvSIMD; no CSSC/PMULL/DotProd gate | Existing `vld1q_u8_x4` at `classify_tbl4.rs:17-18` is table load, not row movement. Any `vld4q_u8` route needs scalar deinterleave oracle, alignment sweep, and a profiled SoA consumer before admission. | No SK-V15 V1 production consumer. Candidate only after W5/W6 typed CSS or string/UTF-8 profiling names four-stream deinterleave hot. |
| PMULL prefix-XOR lineage | SRC-A64-ACLE, SRC-A64-NEON, SRC-LANGDALE-PCLMUL, SRC-SIMDJSON, SRC-LOCAL-SCALAR-DELEGATES, SRC-SPEC | partial / architecture-pressure | `PrefixXor64Pmull` | `hw.optional.arm.FEAT_PMULL=1` on local M5 Max; compile gate must prove FEAT_PMULL emitted (`+pmull` or toolchain-equivalent, plus asm inspection) | Current aarch64 file delegates to scalar at `bitmap_prefix_xor_64.rs:1-4`. A reopen requires scalar prefix-XOR oracle, strict checkasm, assembly probe showing `pmull`, and row-local JSON/CSS equality/timing. | No default W2/W11 consumer. Only admissible if the same wave deletes or bypasses the scalar consume cost source and proves row movement; SPEC line 480 blocks ISA/checkasm-only promotion. |
| CSSC next-set-bit / count primitives | SRC-A64-ACLE, SRC-A64-NEON, SRC-LOCAL-SCALAR-DELEGATES, SRC-SPEC | partial / architecture-pressure | `NextSetBitCssc` | `hw.optional.arm.FEAT_CSSC=1` on local M5 Max; compile gate must use `+cssc` or equivalent and emitted-asm proof | Current aarch64 `bitmap_next_set_bit` delegates to scalar at `bitmap_next_set_bit.rs:1-4`; `bulk_emit_positions_64` also delegates at `bulk_emit_positions_64.rs:1-4`. Strict checkasm is necessary but insufficient. | W2 may classify status as scalar-delegated/blocked; no production consumer until a same-wave compact/bulk-emit row removes the scalar bottleneck and beats cold row-local measurement. |
| DotProd UDOT/SDOT digit MAC | SRC-A64-ACLE, SRC-A64-NEON, SRC-A64-DOTPROD, SRC-LOCAL-DIGITMAC | partial | `DigitMac4Udot` | `hw.optional.arm.FEAT_DotProd=1`; Rust `target_feature="dotprod"`; compile must reject/fallback without dotprod | Scalar oracle is the `parse_4_digits` fallback at `digit_mac.rs:15-21`; DotProd body is gated at `digit_mac.rs:25` and emits `udot` at `:39-45`. Needs checkasm over all 0000-9999 plus invalid-byte cases and row-local numeric hot-leaf evidence. | W7 cost model may select it only after W0/W11 PMU/profiling names numeric decode hot; no SK-V15 close row can land from proof-only helper code. |
| I8MM mixed-sign dot product | SRC-A64-ACLE, SRC-A64-NEON | partial | `DigitMac8I8mm` / `PackedByteDotI8mm` | `hw.optional.arm.FEAT_I8MM=1`; compile gate `+i8mm` or ACLE `__ARM_FEATURE_MATMUL_INT8` equivalent | Needs a new scalar oracle and dedicated checkasm; current local `digit_mac.rs` uses DotProd (`sdot`/`udot`), not I8MM matrix multiply. | No same-wave consumer in SK-V15 V1. Future only if CSS numeric/token classification or Sheets math decode exposes a hot multi-digit packed multiply leaf. |
| SHA3 ternary bitwise EOR3 / BCAX | SRC-A64-ACLE, SRC-A64-NEON | partial | `TernaryXor3Eor3` / `BicXor3Bcax` | `hw.optional.arm.FEAT_SHA3=1`; compile gate `+sha3`; emitted asm must show `eor3` or `bcax` | Needs scalar boolean oracle for the exact 3-input algebra. Current dossier does not find a same-wave hot fan-in proving XOR3 or `a XOR (b AND NOT c)` beats existing masks. | W7/W8 only if the Decision Engine names a grammar-neutral 3-input mask expression and W11 row timing moves; otherwise W2 should mark source-absent/blocked, not admitted. |
| PMU validation on Apple M5 Max | SRC-A64-PMU, SRC-LOCAL-PMU, SRC-SPEC | grounded | `PmuColdParseEvidence` | Host must be Apple M5 Max/aarch64; local harness reads `ri_instructions` and `ri_cycles` fields at `xctrace_probe.rs:62-63` | PMU evidence is not a scalar oracle; it validates row-local hot leaves after equality. Harness has sanity parse at `xctrace_probe.rs:113-120` and cold-loop comments at `:15-18`; pair with strict equality and command transcript. | W0 telemetry lock and W11 close handoff; PMU counters cannot admit CSS rows with broadcast timing or wrong output plane. |
| x86 AVX-512/VBMI2/GFNI/VPCLMUL/VNNI contrast | SRC-INTEL-INTRIN | grounded as secondary only | `X86FutureBackendContrast` | CPUID target features (`avx2`, `avx512*`, `vpclmulqdq`, `gfni`, `avx512vnni`, `vbmi2`) | Any x86 backend would need its own scalar oracle/checkasm and same-workload rows, but SK-V15 SPEC lines 135-136 make x86 diagnostic only. | No SK-V15 admission consumer; may inform future backend vocabulary after aarch64 closes. |
| NEON `svmatch_u8` port | SRC-A64-SVMATCH, SRC-A64-SVE-BASICS, local host probe | refuted | `MatchSetSve2`, not NEON | Requires SVE2 MATCH/NMATCH, not AdvSIMD. Local M5 Max probe has no `hw.optional.arm.FEAT_SVE2` key. | Do not write a NEON `svmatch_u8` checkasm row. If a future SVE2 host appears, it needs a separate scalable-vector oracle and dispatch family. | None for SK-V15. Existing NEON equivalent remains TBL/equality classification, not `svmatch_u8`. |

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
