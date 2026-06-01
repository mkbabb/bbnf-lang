---
agent: 2E
pass: T-P2-research
cycle: V6-SKV18-totality
generated_at: 2026-06-01T19:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 24
techniques_grounded: 11
techniques_refuted: 4
locks_amendment_candidates: 3
sk_cycle: SK-V18
prior_cycle_dispositions_folded:
  accepted:
    - V2 Source Registry (SRC-A64-* / SRC-SIMDJSON / SRC-LANGDALE-* / SRC-INTEL-INTRIN) carried forward, re-anchored on disk
    - V2 NEON/AdvSIMD TBL-classify grounded; PMULL/CSSC/DotProd/I8MM/SHA3 host-gated-not-admitted; svmatch refuted
  revised:
    - V2 close host SK-V15 -> SK-V18; x86 SECONDARY-totality-only, DELETED in skinny (P1)
    - byte_class_from_eq_set_64 movemask convention re-examined (shift-add vaddv vs canonical SHRN)
    - "CH3-V3-S2: Movemask64Shrn (:107) + Interleave4Classify (:113) now carry the inline REDRESS 96/97/98 + 126 net-win fence — kernel-internal pack/load swaps, NOT retained-cursor revivals; no Mbps/× promotable off the kernel row (V3)."
    - "CH4-V3-03: the Lemire-2026 eq-fan-as-deployable-route attribution carries the (comments) qualifier into the Executive Summary + Assertion 3 + the G6 grounding row — the post's BODY endorses the TBL classifier, the eq-fan is a commenter route; binding grounding is simdjson/Langdale-Lemire + the on-disk kernel (V3)."
  first_cycle_additions:
    - SKV18-2E-G6-RETARGET-GROUND (find_css_significant two-fan OR-reduce; ≤13-byte significant set)
    - SKV18-2E-SHRN-MOVEMASK (vshrn_n_u16<4> canonical-movemask upgrade, Kutenin Arm-Neon-bittwiddling)
    - SKV18-2E-LD4-INTERLEAVE (Salter/Validark interleaved-vectors ARM, grounds Interleave4Classify)
    - SKV18-2E-LEMIRE-2026-MATCH (the deployable NEON eq-fan vs the SVE2-MATCH "fastest" the host lacks)
host_close_route: Apple-M5-Max-aarch64 (skinny PRIMARY); x86 totality-SECONDARY-only, skinny-DELETED
lock16_admissibility: published-citation + abstract-primitive-name + scalar-reference + checkasm-parity + same-wave-consumer + hardware-gate
---

# Totality T-P2 2E Host-Arch ASM/SIMD Esoterica (V6 — SK-V18 Generalization Lens)

## Executive Summary

V6 absorbs the certified SK-V18 GENERALIZATION (ONE grammar-driven generator
emitting JSON+CSS+Sheets, aarch64 PRIMARY) and the S-P1 G6=WIRE verdict:
`find_component_delim` + `consume_balanced_at` = 94.1% of CSS parser self-time,
the scalar delimiter/balanced-skip machine the R-F retarget targets. The G6
NEON kernel ALREADY EXISTS on disk — `find_css_significant`
(`runtime_simd.rs:169`) splits the ≤13-byte significant family
(`([{'"/` + delimiters) into two ≤8-byte eq-set fans OR-reduced through the
shared `byte_class_from_eq_set_64_neon` classifier (`byte_class_from_eq_set_64.rs:33`,
four `vceqq_u8`/`vorrq_u8` stripes). It is DEAD at admission (R7: only
`#[cfg(test)] mod tests` callers, `lib.rs:574`) and was written for a flatter
function than the recursive hot leaf — so G6 is a RETARGET onto the live
balanced-consume shell, not a wire-as-is. Newly grounded, SK-V18-specific: the
canonical SHRN-by-4 movemask (Kutenin) the kernel's shift-add `vaddv_u8` path
does not yet use; LD4 interleaved classify (Salter/Validark); and Lemire's
2026 finding that SVE2 `match` is "fastest" — the post's BODY endorses the
Langdale/Lemire TABLE-driven (TBL/shuffle) classifier as the conventional NEON
route, and the `vceqq_u8` eq-fan as the deployable route appears in the COMMENT
thread, not the author's benchmark text. The eq-fan's binding grounding is the
simdjson/Langdale-Lemire vectorized-classification lineage + the on-disk two-fan
kernel, NOT a Lemire-2026 body endorsement. The post REFUTES (again) the
NEON-`svmatch` route on this SVE2-absent host. The local
M5 Max probe: PMULL/DotProd/I8MM/CSSC/SHA3/BF16/SME2 present, FEAT_SVE2 ABSENT.
x86 esoterica are totality-SECONDARY only — skinny DELETES the x86 tree (P1).

## Source Registry

V6 carries the V2 registry forward (re-anchored on disk this pass) and ADDS the
four SK-V18-specific sources (SRC-KUTENIN-NEON, SRC-VALIDARK-LD4,
SRC-LEMIRE-2026-MATCH, SRC-LOCAL-G6) plus the re-anchored local citations.

| ID | Primary source | Used for |
|---|---|---|
| SRC-A64-ACLE | Arm C Language Extensions, feature macros + intrinsic gates, <https://arm-software.github.io/acle/main/acle.html> | `__ARM_FEATURE_PMULL/DOTPROD/MATMUL_INT8/CSSC/SHA3` style hardware gates. |
| SRC-A64-NEON | Arm Neon Intrinsics Reference, <https://arm-software.github.io/acle/neon_intrinsics/advsimd.html> | `vqtbl4q_u8`, `vld1q_u8`, `vceqq_u8`, `vorrq_u8`, `vshrn_n_u16`, `vld4q_u8`, `vmull_p64`, `vdotq_u32`, `veor3q_u8`, `vbcaxq_u8`. |
| SRC-A64-DOTPROD | Arm, "Exploring the Arm dot product instructions", <https://community.arm.com/arm-community-blogs/b/tools-software-ides-blog/posts/exploring-the-arm-dot-product-instructions> | UDOT/SDOT digit/vector dot-product named technique. |
| SRC-A64-SVMATCH | Arm, "Multi-token search strings using the SVMATCH instruction", <https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/multi-token-search-strings-svmatch-instruction> | SVE2 MATCH/NMATCH is scalable-vector, NOT NEON. |
| SRC-A64-SVE-BASICS | Arm Learn, SVE basics, <https://learn.arm.com/learning-paths/servers-and-cloud-computing/sve/sve_basics/> | Separates SVE/SVE2 scalable vectors from fixed-width NEON. |
| SRC-A64-PMU | Arm Learn, "How to use the Arm Performance Monitoring Unit and System Counter", <https://learn.arm.com/learning-paths/servers-and-cloud-computing/arm_pmu/access_options/> | PMU cycles/instructions are measurement evidence, not inferred. |
| SRC-SIMDJSON | Langdale & Lemire, "Parsing Gigabytes of JSON per Second", VLDBJ 2019 / arXiv:1902.08318, <https://arxiv.org/abs/1902.08318> | Structural classification, quote masking, SIMD parser primitive lineage; the vectorized-classification + movemask shape. |
| SRC-LANGDALE-PCLMUL | Geoff Langdale, "Code Fragment: Finding quote pairs with carry-less multiply (PCLMULQDQ)", 2019, <https://branchfree.org/2019/03/06/code-fragment-finding-quote-pairs-with-carry-less-multiply-pclmulqdq/> | PMULL/PCLMUL prefix-XOR lineage. |
| SRC-LANGDALE-NEON | Geoff Langdale, "Fitting My Head Through The ARM Holes…", 2019, <https://branchfree.org/2019/04/01/fitting-my-head-through-the-arm-holes-or-two-sequences-to-substitute-for-the-missing-pmovmskb-instruction-on-arm-neon/> | NEON movemask substitute; fixed-width NEON constraint vs PMOVMSKB. |
| **SRC-KUTENIN-NEON** | Danila Kutenin, "Bit twiddling with Arm Neon: beating SSE movemasks, counting bits and more", Arm Community / servers-and-cloud-computing blog, <https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon> | **NEW**: the canonical `shrn`/`vshrn_n_u16(<…>, 4)` SHRN-by-4 movemask (2 instrs); grounds the SK-V18 movemask upgrade. **(Kutenin-reported / lineage-only, NOT a promotable bbnf row figure)**: the post reports "10-15 percent improvements on a `strlen` distribution extracted from the SPEC CPU 2017 benchmark" replacing the prior `addp`-based movemask with `shrn` (verified verbatim against the post this pass); `memchr` is named as a related beneficiary without a separate figure, and `memcmp` is NOT tied to that figure. The figure is the technique's published lineage on libc string kernels — it does NOT transfer to a bbnf throughput claim and must clear the G6 net-win + REDRESS 96/97/98/126 fence (see SECTION-A row + Assertion 4) before any Mbps/× is asserted. |
| **SRC-VALIDARK-LD4** | Niles Salter (@Validark), "Use interleaved vectors for parsing on ARM", 2024-09-03, <https://validark.dev/posts/interleaved-vectors-on-arm/> (CC BY-NC-SA 4.0) | **NEW**: `ld4`/`vld4q_u8` interleaved-load classify + cheaper movemask/unmovemask/element-shift; grounds the `Interleave4Classify` partial. |
| **SRC-LEMIRE-2026-MATCH** | Daniel Lemire, "The fastest way to match characters on ARM processors?", 2026-04-19, <https://lemire.me/blog/2026/04/19/the-fastest-way-to-match-characters-on-arm-processors/> | **NEW**: SVE2 `match` is "the fastest", referencing Langdale/Lemire 2019 table classifier; the deployable NEON route is the `vceqq_u8` eq-fan (comments). Grounds the G6 eq-fan AND re-refutes NEON-svmatch on SVE2-absent hosts. |
| SRC-A64-SHA3 | LLVM D96381 "[AArch64] Adding SHA3 Intrinsics support" <https://reviews.llvm.org/D96381> + GCC aarch64 EOR3/BCAX patterns <https://www.mail-archive.com/gcc-patches@gcc.gnu.org/msg376505.html> | `veor3`/`vbcax` ternary-bitwise ACLE intrinsics exist and are codegen-reachable (EOR3 = `(x^y^z)`, BCAX = `(x ^ (y & ~z))`). |
| SRC-INTEL-INTRIN | Intel Intrinsics Guide, <https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html> | x86 SECONDARY-totality contrast only: AVX2/AVX-512/VBMI2/GFNI/VPCLMUL/VNNI/BITALG. |
| **SRC-LOCAL-G6** | `skinny/crates/runtime/src/runtime_simd.rs:169` (`find_css_significant`), `:180`-`204` (two-fan OR-reduce salvage), `:199` (`byte_class_from_eq_set_64 \| byte_class_from_eq_set_64`); dead-caller at `runtime/src/lib.rs:574` under `#[cfg(test)] mod tests` | **NEW**: the on-disk G6 retarget machinery + R7 dead-caller proof. |
| SRC-LOCAL-CLASSIFY | `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33`-`73` (4×`vceqq_u8`/`vorrq_u8` fan + shift-add movemask `:79`-`87`); `classify_tbl4.rs:17`-`31` (TBL low-6 classify) | The eq-set classifier body the G6 fans call + the TBL baseline. |
| SRC-LOCAL-MOVEMASK | `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:5` (`vshrn_n_u16::<4>` canonical SHRN movemask) vs `byte_class_from_eq_set_64.rs:79`-`87` (shift-add `vaddv_u8` movemask) | The SK-V18 movemask-divergence: project ALREADY has SHRN in `movemask.rs`; the eq-set body still uses the slower shift-add path. |
| SRC-LOCAL-SCALAR-DELEGATES | `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`, `bitmap_next_set_bit.rs:1`, `bulk_emit_positions_64.rs:1` | PMULL/CSSC/bulk-emit are scalar delegates on aarch64. |
| SRC-LOCAL-DIGITMAC | `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:10`-`63` | DotProd UDOT proof body + scalar fallback. |
| SRC-LOCAL-COMMENT | `skinny/crates/bbnf-simd/src/aarch64/comment_body_mask_64.rs:33`-`60` | L5 comment-digraph mask (4×`vceqq_u8` eq-fan + carry); the `find_comment_close` consumer. |
| SRC-LOCAL-CHECKASM | `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`, `checkasm_ascii_set_member_find_64.rs` | The strict differential the G6 retarget extends to the recursive shell. |
| SRC-LOCAL-LOCK16 | `skinny/crates/bbnf-simd/src/lib.rs:5` (`pub mod x86_64`), `:209` (`find_ascii_set_member64`) | Local primitive facade; x86 STILL LIVE (P1 deletes). |
| SRC-HOST-PROBE | local `sysctl machdep.cpu.brand_string` = `Apple M5 Max`; `hw.optional.arm.{FEAT_PMULL,FEAT_DotProd,FEAT_I8MM,FEAT_CSSC,FEAT_SHA3,FEAT_BF16,FEAT_SME2,FEAT_SME2p1}=1`; `hw.optional.arm.FEAT_SVE2` = **unknown oid (ABSENT)** | Hardware-gating rows; the SVE2 absence that refutes NEON-svmatch. |
| SRC-SPEC-SKV18 | `restart/skinny/tranches/sk-v18/SPEC.md:51`-`52`,`:130` (aarch64-only / x86-delete), `§8` G6 (`:155`-`165` retarget; `:25`-`35` two-fan / `([{'"/` skip-stop; `bbnf-simd/src/lib.rs:209` named), `:439` G2 band, `:442` G5/G6 band, `:480`-`482` gate refresh | SK-V18 G6 retarget contract + aarch64-only standing + Lock-16 admission. |
| SRC-PROFILE-SKV18 | `restart/skinny/tranches/sk-v18/research/p1/SYNTHESIS-PROFILE.md §3` (94.1% scalar-scan; `find_component_delim` 79.5%; G6=WIRE; R7 dead-kernel caveat) | The profile-anchored hot leaf that justifies the G6 retarget (no orphan kernel). |
| SRC-1E | `restart/audit/totality/p1/1E-locks-evidence.md` D-1E-V5-04 (x86 28 files), 1E-V5-U2 (neutrality demotion), LAC-1E-V5-03/04 | The T-P1 divergence ids this pass grounds against literature. |

## Technique Grounding Table

Every row carries the Lock-16 admissibility manifest: published citation +
abstract-primitive name + scalar reference + checkasm-parity plan +
same-wave consumer + hardware gate. aarch64 (M5 Max) is the ONLY skinny close
route; x86 rows are totality-SECONDARY contrast, DELETED in skinny.

**The table is split into two QUARANTINED sections so a downstream consumer cannot
aggregate the citation count across wired and no-consumer rows. SECTION A =
WIRED-IN-SK-V18 (a same-wave consumer on the SK-V18 close path). SECTION B =
HOST-PRESENT-NO-CONSUMER (citation-grounded, `same_wave_consumer = NONE`, NOT on the
SK-V18 close path — host-present technique rows, not SK-V18-grounded primitives).
A "grounded" state in SECTION B means the TECHNIQUE/citation is real, NOT that the
primitive is admitted for SK-V18.**

### SECTION A — WIRED-IN-SK-V18 (same-wave consumer on the close path)

| spec claim / T-P1 divergence id | published source cited | state | abstract primitive | bbnf-specific note (admission manifest) |
|---|---|---|---|---|
| **G6 two-fan eq-set significant skip** (SRC-PROFILE-SKV18 §3 94.1%; 1E-V5-U2) | SRC-LOCAL-G6, SRC-LOCAL-CLASSIFY, SRC-SIMDJSON, SRC-LEMIRE-2026-MATCH | **grounded** | `SignificantSetSkipTwoFan` | The on-disk `find_css_significant` (`runtime_simd.rs:169`) splits the ≤13-byte significant family into two ≤8 eq-set fans OR-reduced (`:180`-`204`), each fan the `byte_class_from_eq_set_64_neon` 4×`vceqq_u8`/`vorrq_u8` classifier. Grounded by simdjson vectorized-classification lineage (the BINDING grounding) AND Lemire-2026 (whose BODY endorses the TBL/shuffle classifier as the conventional NEON route; the `vceqq_u8` eq-fan as the deployable route is a COMMENTER suggestion, not the post's benchmark — SVE2 `match` being host-absent). Scalar ref = `significant_ref` (`lib.rs:506`) + the `find_component_delim` inner loop; checkasm = extend `checkasm_byte_class_from_eq_set_64` + retarget `neon_significant_skip_matches_scalar` to the recursive shell over the 71KB–495KB corpora; same-wave consumer = the P3-collapsed SINGLE generated CSS scan call site (G2+G6 one seam); hardware gate = `target_arch=aarch64` AdvSIMD on M5 Max. Set is CALLER DATA — kernel names no grammar. RETARGET (not wire-as-is): the dead kernel was written flat, the hot leaf recurses. |
| **SHRN-by-4 canonical movemask upgrade** (SRC-LOCAL-MOVEMASK divergence) | SRC-KUTENIN-NEON, SRC-LANGDALE-NEON, SRC-A64-NEON | **grounded** | `Movemask64Shrn` | SK-V18-SPECIFIC FINDING: `byte_class_from_eq_set_64.rs:79`-`87` packs its 4 stripes with the SLOW shift-add `vaddv_u8` (`vand` + horizontal add per half), while the project's own `movemask.rs:5` ALREADY uses the canonical `vshrn_n_u16::<4>` SHRN-by-4 (2-instr) movemask Kutenin grounds (10–15% SPEC CPU 2017 on `memchr`/`strlen`). The G6 retarget should pack via the SHRN path the project already ships. Scalar ref = bit-identical mask; checkasm = the existing `checkasm_byte_class_from_eq_set_64` differential catches any divergence; same-wave consumer = the G6 fan; hardware gate = aarch64 AdvSIMD. ADMISSION: this is a kernel-internal swap inside the singular `bbnf-simd` primitive, NOT a per-grammar emission — fits Lock-16 (the emitter calls the primitive, the kernel changes once). **LEDGER FENCE (REDRESS 96/97/98 + 126): this is a kernel-internal pack-convention swap, NOT a retained-cursor revival; ANY "10–15% movemask" / Mbps SPEEDUP claim still rides the G6 inert-run net-win that must FIRST clear the REDRESS 96/97/98 `G-W3-UNION-SUBSTRATE` finding (M5 Max scalar-cheaper-than-a-streamed-SIMD-cursor) — a checkasm bit-identity PASS is NOT a row move (REDRESS 126 `ROUTE-PRODUCTION-SPLIT`). The Kutenin SPEC-CPU figure is the technique's published lineage, NOT a promotable bbnf row figure off this kernel row alone.** |
### SECTION B — HOST-PRESENT-NO-CONSUMER (citation-grounded, `same_wave_consumer = NONE`, NOT on the SK-V18 close path)

| spec claim / T-P1 divergence id | published source cited | state | abstract primitive | bbnf-specific note (admission manifest) |
|---|---|---|---|---|
| **NEON/AdvSIMD TBL low-6 byte classify** (carried V2) | SRC-A64-NEON, SRC-SIMDJSON, SRC-LOCAL-CLASSIFY | grounded (technique); no G6 consumer | `ByteClassLookup64Tbl` | `vqtbl4q_u8` over a caller-supplied low-6 table (`classify_tbl4.rs:17`-`31`) is the aarch64 classify baseline (4 calls cover 256 values, the documented vqtbl4 idiom). NOT the G6 path (the G6 significant set is small enough for the eq-fan, cheaper than a full TBL build); reserved for any future ≥16-class classify. Scalar ref = scalar table lookup; checkasm = `checkasm_byte_class_from_table_64`; same-wave consumer = none in G6 (eq-fan wins for ≤13 bytes); hardware gate = aarch64 AdvSIMD. |
| **LD4 interleaved classify candidate** (carried V2, now URL-grounded) | SRC-VALIDARK-LD4, SRC-A64-NEON, SRC-LOCAL-CLASSIFY | partial | `Interleave4Classify` | Salter/Validark "Use interleaved vectors for parsing on ARM" (2024) grounds `vld4q_u8` deinterleaved loads as a cheaper movemask/unmovemask/element-shift substrate than 4 independent `vld1q_u8`. The current eq-set body uses 4×`vld1q_u8` (`byte_class_from_eq_set_64.rs:39`-`43`). LD4 is a POSSIBLE upgrade but unproven for THIS 64-byte single-pass skip (its win is in shift/unmovemask-heavy tokenizers, not a pure membership-OR scan). Scalar ref = scalar deinterleave; checkasm = dedicated LD4 differential; same-wave consumer = NONE — blocked until a measured win on the G6 shell; hardware gate = aarch64 AdvSIMD. PARTIAL: source-backed, no profile-proven consumer in G6. **LEDGER FENCE (REDRESS 96/97/98 + 126): an LD4 deinterleave is a kernel-internal load-convention swap, NOT a retained-cursor revival; ANY speedup CLAIM still rides the G6 inert-run net-win that must clear the REDRESS 96/97/98 scalar-cheaper-than-SIMD-cursor finding — a checkasm bit-identity PASS is NOT a row move (REDRESS 126). No Mbps/× figure is promotable off this partial row.** |
| **PMULL prefix-XOR lineage** (carried V2; 1E SRC-LOCAL-SCALAR-DELEGATES) | SRC-A64-ACLE, SRC-A64-NEON, SRC-LANGDALE-PCLMUL, SRC-SIMDJSON | partial / scalar-delegated | `PrefixXor64Pmull` | PMULL expresses carry-less prefix-XOR (quote-mask), but aarch64 delegates prefix-XOR to scalar (`bitmap_prefix_xor_64.rs:1`). Host gate FEAT_PMULL=1 (probe). NOT on the G6 critical path (CSS significant skip needs no prefix-XOR; the JSON product path is scan-free). Scalar ref = `bitmap_prefix_xor_64`; checkasm = strict prefix-XOR differential; same-wave consumer = NONE — required before reopening; hardware gate = `FEAT_PMULL=1` + emitted `pmull` proof. PARTIAL: real hardware, no SK-V18 consumer. |
| **CSSC next-set-bit / count** (carried V2; 1E SRC-LOCAL-SCALAR-DELEGATES) | SRC-A64-ACLE, SRC-A64-NEON, SRC-LOCAL-SCALAR-DELEGATES | partial / scalar-delegated | `NextSetBitCssc` | CSSC may lower next-set/count; current aarch64 next-set + bulk-emit are scalar delegates (`bitmap_next_set_bit.rs:1`, `bulk_emit_positions_64.rs:1`). Host FEAT_CSSC=1 (probe). The G6 mask consumer uses `mask.trailing_zeros()` (`runtime_simd.rs:201`) — a scalar CTZ already; CSSC could lower it but the gain is sub-leaf. Scalar ref = current delegates; checkasm = next-set/count differential; same-wave consumer = NONE; hardware gate = `FEAT_CSSC=1` + emitted `cssc` proof. PARTIAL: host-gated, no consumer. |
| **DotProd UDOT/SDOT digit MAC** (carried V2) | SRC-A64-ACLE, SRC-A64-NEON, SRC-A64-DOTPROD, SRC-LOCAL-DIGITMAC | partial / source-present-unwired | `DigitMac4Udot` | Local UDOT proof (`digit_mac.rs:10`-`63`) accelerates 4-digit decode IF profiled hot. S-P1 §2 shows JSON product path is the SinkOnly digest with `materialize_u64` at 0.53% — NOT a DotProd-sized hot leaf. Host FEAT_DotProd=1. Scalar ref = `parse_4_digits`; checkasm = exhaustive 0000–9999 + invalid-byte; same-wave consumer = NONE until a numeric hot leaf is named (Sheets math decode is the only candidate, PROVE-deferred); hardware gate = `FEAT_DotProd=1`. PARTIAL: proof present, no SK-V18 hot consumer. |
| **I8MM mixed-sign packed-byte dot** (carried V2) | SRC-A64-ACLE, SRC-A64-NEON | partial-blocked | `PackedByteDotI8mm` | No local body, no named parser consumer. Host FEAT_I8MM=1. Scalar ref = new oracle required; checkasm = new differential; same-wave consumer = NONE in SK-V18 (Sheets math only, deferred); hardware gate = `FEAT_I8MM=1`. PARTIAL-BLOCKED: no oracle, no consumer. |
| **SHA3 EOR3/BCAX ternary mask** (carried V2, intrinsic-grounded) | SRC-A64-SHA3, SRC-A64-ACLE, SRC-A64-NEON | partial-blocked | `TernaryXor3Eor3` / `BicXor3Bcax` | `veor3`/`vbcax` intrinsics confirmed real (LLVM D96381 + GCC aarch64 patterns); EOR3 = `(x^y^z)`, BCAX = `(x ^ (y & ~z))`. Host FEAT_SHA3=1. The G6 two-fan OR-reduce (`fan_a | fan_b`, `runtime_simd.rs:199`) is a 2-input OR — NOT a 3-input ternary; EOR3/BCAX apply only IF a future Decision-Engine emits a grammar-neutral 3-input mask (S-P2 §6 candidate, unrealized). Scalar ref = exact 3-input boolean oracle; checkasm = EOR3/BCAX differential; same-wave consumer = NONE until the engine emits one; hardware gate = `FEAT_SHA3=1`. PARTIAL-BLOCKED: expression does not exist. |
### SECTION C — PROCESS / CONTRAST / REFUTED (not a primitive admission count)

| spec claim / T-P1 divergence id | published source cited | state | abstract primitive | bbnf-specific note (admission manifest) |
|---|---|---|---|---|
| **PMU validation on M5 Max** (carried V2) | SRC-A64-PMU, SRC-PROFILE-SKV18 | grounded (process) | `PmuColdParseEvidence` | The S-P1 profile ran under loadavg 4.35 (DIRECTIONAL not re-locked); the H1 QUIET re-capture (`host_loadavg < 1.0`) is the binding re-lock, and the G6 speedup figure is DEFERRED from G6 to H1's corpus-in-timer symmetric harness (SPEC `:482`). Grounded: PMU cycles/instructions validate the row AFTER equality, on the close host, row-local (no broadcast). Scalar ref = N/A (equality comparator precedes counters); same-wave consumer = H1 close; hardware gate = M5 Max only. |
| **x86 AVX-512/VBMI2/GFNI/VPCLMUL/VNNI/BITALG contrast** (carried V2; 1E D-1E-V5-04) | SRC-INTEL-INTRIN | grounded-as-SECONDARY-totality-only | `X86FutureBackendContrast` | x86 informs the totality-SECONDARY >SOTA path (a future x86 backend) but CANNOT close any skinny row — SK-V18 P1 DELETES the whole x86 tree (`lib.rs:5` `pub mod x86_64`, 28 files; SPEC `:130`). GFNI/VPCLMUL would be the x86 analogue of the PMULL prefix-XOR + the eq-fan, but on a DIFFERENT, hardware-gated, DELETED-in-skinny plane. Diagnostic-only; never M5 Max close. |
| **NEON `svmatch_u8` port** (carried V2; re-refuted) | SRC-A64-SVMATCH, SRC-A64-SVE-BASICS, SRC-LEMIRE-2026-MATCH, SRC-HOST-PROBE | **refuted** | `MatchSetSve2` (NOT NEON) | MATCH/NMATCH is SVE2, not NEON/AdvSIMD; local M5 Max probe has `FEAT_SVE2` = unknown oid (ABSENT) despite SME2/SME2p1 present. Lemire-2026 confirms SVE2 `match` is the literature "fastest" — but it is host-absent, so the deployable route is the `vceqq_u8` eq-fan the kernel already uses. REFUTED on this host; no NEON `svmatch` row. |

## Per-primitive admission cost manifest (CH4 Lock-16 v+1 columns)

Row-local `loc_estimate` / `risk_class` / `rollback path` / `abrogate threshold` per
primitive (wave band: SPEC §8 G5/G6 `≤450 LOC` `:442`; kernel-internal swaps ≈ 0):

| primitive (table row) | section | wave_owner | loc_estimate | risk_class | rollback path | abrogate threshold |
|---|---|---|---|---|---|---|
| `SignificantSetSkipTwoFan` (G6 eq-set skip) | A | G5/G6 | ≤150 (retarget onto recursive shell + checkasm ext) | MED-HIGH | revert to scalar `find_component_delim` (no shipped output depends on the SIMD skip) | if the skip cannot net-beat the M5-Max scalar loop (REDRESS 96/98), ABROGATE the wire, keep scalar (G6 outcome `C`) |
| `Movemask64Shrn` (SHRN-by-4 swap) | A | G5/G6 | ≈ 10 (kernel-internal pack swap) | LOW (bit-identity checkasm-gated) | revert to the shift-add `vaddv_u8` pack | if SHRN is not bit-identical or not net-faster on M5 Max, keep the shift-add pack |
| `ByteClassLookup64Tbl` (TBL classify) | B | none (no G6 consumer) | 0 (baseline exists, unwired in G6) | n/a | N/A (not wired) | DO NOT wire unless a ≥16-class classify hot leaf is profiled (eq-fan wins for ≤13 bytes) |
| `Interleave4Classify` (LD4) | B | none | ≤120 if ever built | MED-HIGH | N/A (no consumer) | DEFER permanently unless H1 names a load-bound residual hot leaf; no orphan kernel |
| `PrefixXor64Pmull` (PMULL) | B | none | ≤200 if reopened | HIGH | scalar `bitmap_prefix_xor_64` retained | DO NOT author — no SK-V18 consumer (CSS skip needs no prefix-XOR; JSON scan-free) |
| `NextSetBitCssc` (CSSC) | B | none | ≤90 if reopened | HIGH | scalar `bitmap_next_set_bit` + `mask.trailing_zeros()` retained | gain is sub-leaf; DO NOT author without a named consumer |
| `DigitMac4Udot` (DotProd) | B | none (Sheets-math, PROVE-deferred) | ≤120 if reopened | MED | scalar `parse_4_digits` retained | no numeric hot leaf in JSON §2 / CSS §3; defer to PROVE/SK-V19 |
| `PackedByteDotI8mm` (I8MM) | B | none | ≤180 (new oracle + body) | HIGH | N/A (no oracle, no consumer) | DO NOT author — no oracle, no consumer in SK-V18 |
| `TernaryXor3Eor3`/`BicXor3Bcax` (SHA3) | B | none | ≤80 if the engine emits a 3-input mask | MED | N/A (the 3-input expression does not exist) | DO NOT author until the Decision Engine emits a grammar-neutral 3-input mask |
| `MatchSetSve2` (svmatch) | C | none (REFUTED) | n/a | n/a | n/a (host lacks FEAT_SVE2) | REFUTED on this host; a future SVE2 host needs a separate scalable-vector dispatch family |
| `X86FutureBackendContrast` | C | none (DELETED in skinny, P1) | n/a (deletion target) | n/a | n/a | x86 is a P1 deletion target; closes no row, totality-SECONDARY only |

## Architectural Assertions Defended

1. **The G6 NEON retarget kernel already exists and is profile-justified.** S-P1
   §3 attributes 94.1% of CSS parser self-time to `find_component_delim` +
   `consume_balanced_at`; `find_css_significant` (`runtime_simd.rs:169`) is the
   pre-built two-fan eq-set skip for exactly that significant family. There is NO
   orphan kernel — the kernel exists, the hot leaf is measured, and G6 is a
   RETARGET onto the live recursive shell (the dead kernel was written flat).
   Grounded by simdjson's vectorized-classification lineage and Lemire-2026's
   NEON eq-fan as the deployable membership route. LEDGER (REDRESS): the retarget
   is ledger-grounded, not a fresh route. REDRESS 144 (`skinny/REDRESS.md:4418`-`4438`,
   `G-W12-SIMD-ASM-PRODUCTION` PASS-ADMIT) is the PRODUCTION PRECEDENT — the same
   `find_ascii_set_member64` kernel class WAS wired into production CSS
   `Scanner::scan_block` delimiter search and MOVED the Track 1 row (444.2 vs 434.1
   Mbps, +109.87%, strict cssparser/lightningcss green). The deferred-to-H1 net-win
   must clear the CAUTIONARY priors: REDRESS 96/97/98 (`:2795`-`2940`,`:2928`-`2933`,
   `G-W3-UNION-SUBSTRATE` retired — a SIMD structural cursor STREAMED through the
   retained JSON parse loop uniformly regressed every M5-Max row, the wide-issue
   scalar loop being cheaper) and REDRESS 126 (`:3766`-`3805`, `ROUTE-PRODUCTION-SPLIT`
   — a microbench/checkasm PASS is NOT a production-row move). The inert-run net-win
   (OQ below) thus re-opens a question the ledger answered NEGATIVELY for the JSON
   streamed-cursor case and POSITIVELY for CSS — not a new unknown.

2. **The two-fan OR-reduce is the correct architecture for a >8-byte set.** The
   significant family `([{'"/` (the `fixed[9]`) plus ≤4 caller delimiters spans
   ≤13 bytes, exceeding the 8-byte `byte_class_from_eq_set_64` cap. The split into
   two ≤8 fans OR-reduced (`runtime_simd.rs:180`-`204`) is the published shape
   (Langdale/Lemire fanned `vceqq_u8` membership) and is the salvage point SPEC §8
   names (`find_css_significant:180-204`). The set stays CALLER DATA — the kernel
   names no grammar (Lock 14), so the demoted-to-`css_balanced_component_scan`
   SHELL invokes a genuinely neutral inner kernel.

3. **The deployable ARM character-match is the eq-fan, not SVE2 MATCH.** Lemire's
   2026 post argues SVE2 `match` is fastest, but the M5 Max host lacks FEAT_SVE2
   (SME2/SME2p1 present, SVE2 absent). The 4×`vceqq_u8`/`vorrq_u8` fan in
   `byte_class_from_eq_set_64_neon` is the deployable NEON membership route where
   SVE2 is unavailable. ATTRIBUTION (CH4-V3-03): the post's BODY endorses the
   Langdale/Lemire TABLE-driven (TBL/shuffle) classifier as the conventional NEON
   route; the eq-fan as the deployable route is a COMMENTER suggestion, not the
   author's benchmark. The eq-fan's binding grounding is the simdjson/Langdale-Lemire
   vectorized-classification lineage + the on-disk two-fan kernel — so the existing
   kernel is on the correct, hardware-realistic architecture independent of the
   Lemire-2026 comment thread.

4. **The movemask pack is upgradeable to the project's own canonical SHRN.** The
   eq-set body packs via shift-add `vaddv_u8` (`byte_class_from_eq_set_64.rs:79`),
   but `movemask.rs:5` already ships the canonical `vshrn_n_u16::<4>` SHRN-by-4
   (2-instr) movemask the project already authored. The grounding is Kutenin's
   2-instr canonical movemask technique; its "10–15% SPEC CPU 2017 / `strlen`"
   figure is **Kutenin-reported / lineage-only, NOT a promotable bbnf row figure**
   (it is a libc-string-kernel number, fenced behind the G6 net-win + REDRESS
   96/97/98/126 gate — see the SECTION-A row's LEDGER FENCE and the SRC-KUTENIN-NEON
   Source Registry qualifier). The G6 retarget should pack via the SHRN — a
   kernel-internal, single-site swap, Lock-16-admissible because the emitter calls
   the primitive.

5. **Instruction availability is a gate, not admission; aarch64-ONLY in skinny.**
   PMULL/CSSC/DotProd/I8MM/SHA3 are all host-present (probe) but scalar-delegated
   or unwired, each requiring scalar oracle + strict checkasm + same-wave consumer
   + emitted-asm proof. x86 is DELETED in skinny (P1) and is totality-SECONDARY
   contrast only — never an M5 Max close path.

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| "NEON `svmatch_u8` can be ported directly for the G6 set scan." | MATCH/NMATCH is SVE2; M5 Max probe lacks FEAT_SVE2 (SME2/SME2p1 present, SVE2 absent). Lemire-2026 names SVE2 `match` fastest but it is host-absent. | Use the existing `vceqq_u8` two-fan eq-set scan (already on disk); no NEON `svmatch` row; a future SVE2 host would need a separate scalable-vector dispatch family. |
| "Wire `find_css_significant` as-is — it's the CSS NEON kernel." | R7: the kernel is DEAD at admission (only `#[cfg(test)] mod tests` caller, `lib.rs:574`) and was written for a FLAT stop-at-delimiter skip; the hot `find_component_delim`+`consume_balanced_at` machine recurses through `()[]{}` and skips strings/comments. | G6 is a RETARGET onto the live recursive shell + a generated call-site swap landed WITH its consumer (P3-collapsed singular site), not a wire-as-is; checkasm must be over the REAL corpora, not the flat micro-case. |
| "x86 AVX-512/GFNI/VPCLMUL esoterica can help close SK-V18." | SK-V18 P1 DELETES the entire x86 tree (`lib.rs:5`, 28 files; SPEC `:130`); aarch64 is the only skinny admission host. | x86 rows are totality-SECONDARY future-backend vocabulary on a different, deleted-in-skinny plane; never M5 Max close evidence. |
| "PMU counters can validate the G6 speedup at the G6 wave." | The S-P1 capture ran under loadavg 4.35 (DIRECTIONAL); SPEC `:482` DEFERS the G6 speedup figure to H1's QUIET (`host_loadavg < 1.0`) corpus-in-timer symmetric re-capture. G6 reports only the checkasm PASS/FAIL pre-H1. | No absolute Mbps/speedup CLAIM from G6; the H1 quiet re-capture on `css_canon_bench` is the binding re-lock. PMU evidence must be row-local + equality-paired. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Whether the SHRN-by-4 movemask swap in `byte_class_from_eq_set_64_neon` is bit-identical and net-faster on M5 Max for the G6 two-fan skip. | G6 swaps the shift-add pack for `vshrn_n_u16::<4>` (per `movemask.rs:5`), proves bit-identity via the existing `checkasm_byte_class_from_eq_set_64`, and measures on `css_canon_bench` at H1 (quiet). |
| Whether LD4/`vld4q_u8` deinterleaved loads beat 4×`vld1q_u8` for the pure membership-OR skip (vs Validark's shift-heavy tokenizer win). | Test ONLY after the G6 baseline retarget lands and H1 names a residual load-bound hot leaf; else mark `Interleave4Classify` source-backed/no-consumer. |
| Whether Apple clang/rustc accepts explicit `+cssc`/`+i8mm`/`+sha3`/`+pmull` in the W0/W11 release profile, and whether any of PMULL/CSSC/DotProd has a SK-V18 hot consumer. | Add a per-feature emitted-asm probe (`otool -tV`/`cargo asm`); S-P1 attribution must name a hot symbol before any of these can be admitted (none named in JSON §2 or CSS §3). |
| Whether the inert-run length on the real corpora is long enough for the 64-byte vector skip to net-win over the scalar `find_component_delim` inner loop. **LEDGER-FRAMED:** REDRESS 96/97/98 (`skinny/REDRESS.md:2795`-`2940`,`:2928`-`2933`) answered the analogous JSON streamed-cursor case NEGATIVELY on this exact host; REDRESS 144 (`:4418`-`4438`) answered the CSS delimiter case POSITIVELY. This is not a fresh unknown — the net-win must beat the REDRESS-98 scalar-cheaper finding, and a checkasm PASS is NOT a row move (REDRESS 126 `:3766`-`3805`). | A MEASUREMENT (not correctness) — confirm post-wire on `css_canon_bench` at H1; realized speedup is bounded by inert-run length (SPEC §5-risk-6) and must clear the REDRESS-98 M5-Max scalar-cheaper-than-SIMD-cursor prior. |

## LOCKS-AMENDMENTS-CANDIDATE

SCOPE 2E candidates are confined to the host-arch / SIMD-admission surface
(Lock 16, and Lock 14 where the kernel's grammar-neutrality is at stake). The
V2 candidates (`LOCK16-A64-HOST-GATE`, `LOCK16-PMU-ROW-LOCAL`,
`LOCK16-SVE2-SEPARATION`) FOLD FORWARD into the SK-V18-aligned set below; the 1E
candidates LAC-1E-V5-03 (neutrality demotion) and LAC-1E-V5-04 (aarch64-only)
are the cross-inventory siblings these co-bind to (no duplication — 2E supplies
the SIMD-admission specificity).

| candidate | type | target locks | proposed candidate text | wave hint | supporting path:line evidence |
|---|---|---|---|---|---|
| LAC-2E-V6-01 | refinement | L16 | Bind the host-gate-vs-admission split AND the SK-V18 aarch64-ONLY standing: every aarch64 primitive reports `wired` / `scalar-delegated` / `source-present-blocked` / `strict-checkasm-admitted` / `deleted`, with its exact `hw.optional.arm.FEAT_*` gate + emitted-asm proof for non-baseline features; the whole x86 surface (`src/x86_64/`+`ext/x86/`+`diagnostic-x86` gate, `bbnf-simd/src/lib.rs:5`) is a DELETION target (P1), and x86/AVX-512/GFNI literature is totality-SECONDARY pressure that closes NO skinny row. (Folds V2 `LOCK16-A64-HOST-GATE` + 1E LAC-1E-V5-04; the M5 Max probe shows FEAT_PMULL/DotProd/I8MM/CSSC/SHA3=1 but FEAT_SVE2 ABSENT.) | P1 (x86 delete) ∧ G5/G6 (admission reporting) | SRC-HOST-PROBE; `bbnf-simd/src/lib.rs:5`; SPEC `:51`-`52`,`:130`; 1E D-1E-V5-04. |
| LAC-2E-V6-02 | addition | L16, L14 | Bind the G6 retarget-not-wire + neutral-inner-kernel discipline: a profile-justified NEON kernel admitted under the §6 escape MUST (i) retarget the LIVE generated hot-leaf shell (NOT a dead/flat sibling — R7), proven by a non-`#[cfg(test)]` caller census over the P3-collapsed singular call site; (ii) carry its byte-set as CALLER DATA so the inner eq-set kernel is grammar-neutral even when the recognizer SHELL is CSS-scoped (`css_balanced_component_scan`); (iii) pass the checkasm differential + the `neon_significant_skip_matches_scalar` guard retargeted to the recursive shell over the REAL 71KB–495KB corpora; (iv) DEFER any speedup CLAIM to the H1 quiet corpus-in-timer re-capture. The two-fan OR-reduce for a >8-byte set is the admissible shape (`find_css_significant:180-204`); the skip MUST stop at `([{'"/` so the scalar shell still handles recursion/strings/error-positions. | G5/G6 (one seam with G2) | SRC-LOCAL-G6 (`runtime_simd.rs:169`,`:180`-`204`; dead-caller `lib.rs:574`); SRC-PROFILE-SKV18 §3; SPEC `§8` (`:25`-`35`,`:155`-`165`,`:480`-`482`); SRC-LEMIRE-2026-MATCH. |
| LAC-2E-V6-03 | refinement | L16 | Bind the singular-movemask-convention rule: a SIMD primitive that packs lane-comparison vectors to a 64-bit mask MUST use the project's ONE canonical movemask pack (`vshrn_n_u16::<4>` SHRN-by-4, `aarch64/movemask.rs:5`) — not a per-kernel shift-add `vaddv_u8` re-roll (`byte_class_from_eq_set_64.rs:79`-`87`); divergent pack conventions are an in-kernel KISS/DRY violation and forfeit the Kutenin-grounded 2-instr movemask. The swap is bit-identity-gated by the existing `checkasm_byte_class_from_eq_set_64` differential. **STRUCTURAL CO-GATE (a name-grep is NECESSARY-NOT-SUFFICIENT against a renamed/aliased second pack):** bind the structural mask-convention co-gate symbol `bbnf_simd_single_mask_convention` (co-defined with 2F LAC-2F-V3-01) — the `bbnf-simd` analog of `runtime_target_rows_collapsed` — asserting that every 64-byte→64-bit pack in the crate DELEGATES to the one canonical `movemask::movemask_u8x16`, counting DISTINCT non-delegating horizontal-pack call-sites (`vshrn_n_u16`/`vaddv_u8` used inline rather than via the canonical pack), alias-immune (counts pack IMPLEMENTATIONS, not symbol names), so a vendored/renamed second nibble-LUT classifier planting a second pack convention is caught. **Wave-owner / enforcement wave: G2 entry** (the guard becomes an enforced xtask/CI check at G2 entry, mirroring 2D's P3/G3 naming for the relocated-seam co-gate). | G2 entry (structural co-gate) ∧ G5/G6 (kernel-internal swap) | SRC-LOCAL-MOVEMASK (`movemask.rs:5` vs `byte_class_from_eq_set_64.rs:79`-`87`); SRC-KUTENIN-NEON; SRC-LANGDALE-NEON; co-binds 2F LAC-2F-V3-01. |

### No-candidates axes scanned (explicit)

- New SIMD width / scalable-vector (SVE2) admission: NONE — SVE2 is host-ABSENT
  (probe); the deployable plane is fixed-width NEON; a future SVE2 host needs a
  separate dispatch family, not a Lock-16 amendment now.
- New x86 admission row: NONE — x86 is a P1 DELETION target; no row admits it.
- 6th movemask/classify convention: NONE — the project has exactly one canonical
  SHRN movemask (`movemask.rs:5`) and one eq-set / one TBL classifier; the
  divergent shift-add pack is a defect to converge (LAC-2E-V6-03), not a new axis.
