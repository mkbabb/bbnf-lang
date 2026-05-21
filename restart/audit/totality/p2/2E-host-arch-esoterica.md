---
agent: 2E
pass: T-P2-research
cycle: V1
generated_at: 2026-05-21T08:38:35Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 18
techniques_grounded: 11
techniques_refuted: 5
prior_cycle_dispositions_folded:
  accepted: []
  rejected: []
  revised: []
  first_cycle_additions: [A64-TBL, A64-PMULL, A64-CSSC-CTZ, A64-DOTPROD, A64-EOR3-BCAX, A64-LD4, A64-SVE2-MATCH, A64-ASCII-RUN-SKIP, A64-CACHE-HINTS, X86-AVX512-BACKGROUND, L16-HARDWARE-GATE-MANIFEST]
locks_amendment_candidates: 4
---

## Executive Summary

aarch64 remains the only SK-V13 production host lane. The literature and
local source agree on the usable vocabulary: NEON TBL/TBX and LD1/LD4
table/deinterleave primitives are generally available on A64; PMULL is gated
through FEAT_PMULL/`+aes`; CSSC CTZ is gated through `+cssc`;
UDOT/SDOT is gated through `+dotprod`; EOR3/BCAX is gated through SHA3; and
`svmatch_u8` is SVE2, not a NEON primitive. Local bbnf evidence sharply
narrows those choices. TBL is already production-real. The W4 delimiter
candidate is the strongest near-term ASM route because it is parity-green and
4.72x faster in a caller-level microbench, but it is not admitted until a CSS
production consumer moves a row. PMULL prefix-XOR and CSSC CTZ bulk emission
are real instructions but prior consumed implementations regressed JSON rows,
so they are only eligible behind a fresh material differential such as
SIMD-first union C. UDOT, EOR3/BCAX, LD4, cache hints, and x86 AVX-512 remain
background/inventory until a same-wave consumer and row movement exist.

## Source Registry

| ID | Source | Use in this dossier |
|---|---|---|
| SRC-A64-ACLE | Arm C Language Extensions, feature macros: `__ARM_FEATURE_AES`, `__ARM_FEATURE_SHA3`, `__ARM_FEATURE_DOTPROD`, `__ARM_FEATURE_CSSC` ([ACLE](https://arm-software.github.io/acle/main/acle.html)) | Hardware gates for PMULL, SHA3 ternary logic, DotProd, CSSC. |
| SRC-A64-NEON | Arm Neon Intrinsics Reference ([NEON intrinsics](https://arm-software.github.io/acle/neon_intrinsics/advsimd.html)) | Intrinsic-to-instruction mapping for `vqtbl4q_u8`, `vqtbx4q_u8`, `vld4q_u8`, `vld1q_u8_x4`, `vmull_p64`, `vmull_high_p64`, `veor3q_u8`, `vbcaxq_u8`. |
| SRC-A64-SVE2-MATCH | Arm Learning Path SVE2 MATCH examples ([SVE2 MATCH](https://learn.arm.com/learning-paths/servers-and-cloud-computing/sve2-match/sve2-match-search/)) | Refutes treating `svmatch_u8` as a NEON primitive; it is an SVE2 path. |
| SRC-X86-INTEL | Intel Intrinsics Guide, official instruction-set registry ([Intel Intrinsics Guide](https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html)) | x86 background gates for AVX2, AVX-512 VBMI2, GFNI, VPCLMUL, VNNI, BITALG, IFMA. |
| SRC-SCOPE | `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:10-180` | SK-V13 aarch64 inventory, W4 production split, union A/B/C candidates, untapped ISA surface. |
| SRC-REDRESS | `skinny/REDRESS.md:2508-2618`, `skinny/REDRESS.md:2795-2938`, `skinny/REDRESS.md:3603-3820`, `skinny/REDRESS.md:3860-3872` | Prior measured rejections and SK-V12 close dispositions. |
| SRC-BBNF-A64 | `skinny/crates/bbnf-simd/src/aarch64/*.rs` | Local primitive bodies and scalar delegates. |
| SRC-BBNF-DISPATCH | `skinny/crates/bbnf-simd/src/dispatch.rs:49-87`, `skinny/crates/bbnf-simd/src/lib.rs:169-272` | Current production dispatch and primitive facade. |
| SRC-BBNF-CHECKASM | `skinny/crates/bbnf-simd/tests/checkasm_*.rs` | Scalar references, parity harnesses, and W4 caller microbench. |
| SRC-BBNF-X86 | `skinny/crates/bbnf-simd/src/x86_64/avx512_*`, `skinny/crates/bbnf-simd/src/x86_64/avx_ifma` | Local x86 background modules and target-feature gates. |

## Technique Grounding Table

| spec claim / T-P1 divergence id | published source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| A64-TBL low-6 byte classification | SRC-A64-NEON maps `vqtbl4q_u8` to A64 `TBL`; local `classify_tbl4.rs:17-32` loads four 16-byte tables and calls `vqtbl4q_u8`; `dispatch.rs:89-112` selects it for admissible alphabets. | grounded | This is the one clearly production-real aarch64 classifier route. It remains grammar-neutral only while the alphabet is caller-provided and collision-checked; hardcoded JSON punctuation would re-open Lock 14. |
| A64-TBX fallback lookup | SRC-A64-NEON maps `vqtbx4q_u8` to A64 `TBX`; SRC-SCOPE marks TBX as unused refinement. | partial | TBX is useful only if a row needs preserve-old-value semantics on out-of-range indices, e.g. unicode or CSS escape fallback. Current TBL paths already have scalar fallback and row evidence does not name TBX. Not S-P3-eligible alone. |
| A64-LD1x4 / LD4 deinterleave classify | SRC-A64-NEON maps `vld1q_u8_x4` to `LD1 {Vt.16B - Vt4.16B}` and `vld4q_u8` to interleaved `LD4`; local `classify_tbl4.rs:17-18` uses `vld1q_u8_x4`. | partial | `LD1x4` is already a table-load convenience, not a row-moving primitive. `LD4` deinterleave needs a profiled 4-window consumer; SK-V13 scoping says it is out of scope until UTF-8/string deinterleave is hot. |
| A64-PMULL prefix-XOR | SRC-A64-ACLE says AES support identifies FEAT_AES and FEAT_PMULL; SRC-A64-NEON maps `vmull_p64`/`vmull_high_p64` to `PMULL`/`PMULL2`; REDRESS 88 emitted `pmull.1q` with `+cssc,+aes` but regressed JSON rows. | partial / prior implementation refuted | PMULL exists and is correctable, but the default hot `bitmap_prefix_xor_64` body is historically rejected. Eligible only as a fresh material differential, preferably union C, with prefix-XOR scalar retained unless the consumer row proves non-regression. |
| A64-CSSC CTZ next-bit / bulk emit | SRC-A64-ACLE defines `__ARM_FEATURE_CSSC` for common short sequence compression including CTZ; REDRESS 89 emitted `ctz` with `+cssc` and passed checkasm but regressed guard rows. | partial / prior implementation refuted | CSSC CTZ is not blocked as an instruction; the prior bulk consumer shape is blocked. A new CTZ attempt must move a CSS/JSON row and avoid the `compact_mask` scalar-delegate regression pattern. |
| A64-DOTPROD UDOT/SDOT digit MAC | SRC-A64-ACLE defines `__ARM_FEATURE_DOTPROD`; local `digit_mac.rs:25-49` and `digit_mac.rs:51-71` use `#[target_feature(enable = "dotprod")]` and inline `udot`/`sdot`. | partial | Local code is proof-only. It can target number-heavy JSON rows only if the parser actually consumes digit blocks through a same-wave number parser route; otherwise it remains an orphan despite valid hardware gating. |
| A64-EOR3/BCAX ternary bitwise | SRC-A64-ACLE defines SHA3 support; SRC-A64-NEON maps `veor3q_u8` to `EOR3` and `vbcaxq_u8` to `BCAX`; SRC-SCOPE says no local 3-input fold exists. | partial | Good fit for quote/escape/control fusion only if a P1/P2 profile names a 3-input boolean fan-in. Current local string scan uses two-input OR/equality chains and no EOR3/BCAX body. Not eligible as "tap SHA3 surface" alone. |
| A64-SVE2 `svmatch_u8` | SRC-A64-SVE2-MATCH demonstrates `svmatch_u8` under SVE2 with scalable vectors; SRC-SCOPE says SK-V13 is NEON-only on fixed 128-bit M5 Max. | refuted for SK-V13 NEON port | The prompt phrase "NEON `svmatch_u8` port" is technically wrong. A future SVE2 track may use MATCH; SK-V13 aarch64/M5 Max should not scope it as a NEON primitive. |
| A64-ASCII run-skip | SRC-BBNF-CHECKASM `checkasm_ascii_set_member_find_64.rs:20-40` compares scalar byte-walk to `byte_class_from_eq_set_64`; `:137-190` emits the microbench artifact; REDRESS 126 records 18.51 ns vs 3.92 ns, 4.718x. | grounded as micro-proven; not admitted | This is the best SK-V13 production-wiring candidate. It requires a generated CSS scan-block caller, strict lightningcss equality, Lock 14 parent authorization, and Criterion row movement before it becomes a production SIMD admission. |
| A64-cache hints / STNP | Local `cache_hints.rs:4-28` uses `prfm` and `stnp`; SRC-SCOPE and REDRESS 126 demote it as no production caller. | refuted as standalone admission | Prefetch/store hints are placement-sensitive. They must be deleted or wired behind a measured caller; a support-only hint module is not a Lock 16 close. |
| A64-byte_context vext boundary shifts | SRC-A64-NEON maps vector extract via the NEON table; local `byte_context.rs:4-10` uses `vextq_u8`; SRC-SCOPE marks no production caller. | partial | Valid helper for cross-chunk string special scans, but support-only today. It needs a row-local string/unicode consumer or deletion evidence. |
| X86-AVX512 background | SRC-X86-INTEL lists AVX-512 family features; local modules gate VBMI2, GFNI, BITALG, VNNI, VPCLMUL, IFMA bodies by `target_feature` and most bodies are `unimplemented!`. | grounded as background / not SK-V13 close route | x86 is secondary only. It can inform primitive vocabulary and totality architecture, but cannot satisfy the M5 Max close bar or aarch64 Lock 16 admission. |

## Hardware Gates

| abstract primitive | primary hardware gate | local gate / proof | admissibility state |
|---|---|---|---|
| `ByteClassLookup64` via TBL | `target_arch = "aarch64"` with Advanced SIMD; ACLE NEON `vqtbl4q_u8` | `dispatch.rs:89-112`; `classify_tbl4.rs:17-32`; checkasm classifier tests | Production-real, but grammar policy must stay caller-supplied. |
| `AsciiSetRunSkip64` | A64 NEON TBL/equality/movemask path through `byte_class_from_eq_set_64` | `checkasm_ascii_set_member_find_64.rs:20-40`, `:137-190`; REDRESS 126 | Micro-proven; needs CSS production consumer. |
| `PrefixXor64Pmull` | `target_arch = "aarch64"` + FEAT_PMULL exposed through `+aes` / `__ARM_FEATURE_AES` | REDRESS 88 disasm proof and measured reject; local aarch64 prefix-XOR now scalar delegates at `bitmap_prefix_xor_64.rs:1-4` | Reopen only with material differential and row gate. |
| `NextSetBitCssc` | `target_arch = "aarch64"` + `+cssc` / `__ARM_FEATURE_CSSC` | REDRESS 89 disasm proof and measured reject; local next-bit now scalar delegates at `bitmap_next_set_bit.rs:1-4` | Reopen only with a different consumer, not the rejected bulk path. |
| `BulkEmitPositions64` | Usually paired with CTZ or vector compaction; no standalone ISA gate | Local aarch64 bulk emit delegates to scalar at `bulk_emit_positions_64.rs:1-4`; facade consumed by `lib.rs:208-223` | Not an ASM admission; wire or delete under D5. |
| `DigitMac4/16` | `+dotprod` / `__ARM_FEATURE_DOTPROD` | `digit_mac.rs:25-71` uses `udot`/`sdot`; no parser consumer | Proof-only until a number parser consumes it. |
| `TernaryBoolean3` | `+sha3` / `__ARM_FEATURE_SHA3`; ACLE NEON `EOR3` and `BCAX` | No local aarch64 body; scoping says no hot-leaf attribution | Not selectable until a row-local 3-input fold exists. |
| `SVE2MatchSet` | `__ARM_FEATURE_SVE2`; scalable vector SVE2 MATCH | No local SVE code; SK-V13 fixed to NEON/M5 Max | Out of scope for SK-V13 production. |
| `CacheHintPrefetchStore` | A64 instruction availability for `PRFM`/`STNP`; placement is uarch/workload-sensitive | `cache_hints.rs:4-28`; no consumer | Delete or wire with measured caller; no standalone close. |
| x86 AVX-512 classify/carry/digit/float | x86_64 plus per-feature gates: `avx512vbmi2`, `gfni`, `avx512bitalg`, `vpclmulqdq`, `avx512vnni`, `avxifma` | Local x86 modules declare gates and scalar refs, but most bodies are unimplemented | Totality background; not M5 Max admission. |

## Architectural Assertions Defended

1. **Lock 16 must be a manifest, not a prose allowlist.** ACLE and local source
   show that each primitive has a distinct hardware gate. The admissible unit is
   `{abstract primitive, feature gate, scalar reference, checkasm parity,
   same-wave consumer, measured row}`. A single "aarch64 SIMD supported" claim
   is too coarse.

2. **TBL is the current production classifier spine.** The local dispatcher
   selects `NeonTbl4` only when the low-6 alphabet table is admissible, and the
   classifier body uses `vqtbl4q_u8` over caller-provided tables. This is the
   grammar-neutral transfer pattern to preserve for CSS/Sheets/BBNF-self.

3. **W4's ASCII run-skip is a real candidate, but still pre-production.** It has
   a caller-level scalar reference, parity, adversarial seeds, frozen CSS fixture
   coverage, and a 4.718x microbench. REDRESS correctly refuses production
   admission because CSS scan-block wiring and lightningcss equality are not yet
   landed.

4. **REDRESS 88/89 should remain route-specific, not category-wide.** PMULL and
   CSSC are real architecture features, but the consumed prefix-XOR and CTZ bulk
   implementations regressed rows. USER PIN D4 can reopen the categories only
   by naming a material differential and measuring a row-moving consumer.

5. **x86 AVX-512 belongs in totality vocabulary but not in SK-V13 closure.**
   The local x86 modules are useful for future backend-shape vocabulary and
   checkasm process symmetry, but SK-V13's admission silicon is aarch64/M5 Max.

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| "Instruction availability implies a primitive should land." | PMULL and CSSC implementations passed correctness/disasm checks but failed JSON row gates in REDRESS 88/89. | Every ISA row needs row-local measured consumption, not just a scalar/checkasm cell. |
| "`svmatch_u8` is a NEON primitive." | Arm documents `svmatch_u8` in the SVE2 MATCH path, not NEON. | Do not scope SVE2 MATCH as an SK-V13 NEON/M5 Max route. |
| "Final orphan count zero means the source tree has no future SIMD cleanup." | REDRESS 126 demotes five source-present primitives with evidence; scoping says several still delegate to scalar or have no caller. | SK-V13 D5 still needs wire-or-delete disposition if the active pin treats source-present support modules as orphans. |
| "PMULL prefix-XOR should replace scalar carry by default." | REDRESS 88 measured hard JSON regressions, including unicode and number rows, despite visible `pmull.1q`. | PMULL can appear only inside a new consumer shape, e.g. SIMD-first union C, and must preserve scalar fallback. |
| "Cache hints are harmless support code." | Local `cache_hints.rs` has `prfm`/`stnp` but no measured placement or caller. | Treat as support-only inventory; delete or wire with a hot caller. |

## Bbnf-Specific Transfer Notes

| primitive family | transfer condition | non-transfer condition |
|---|---|---|
| Byte-class set membership | Grammar supplies byte alphabet/table; checkasm covers collisions, duplicates, high-bit bytes, tails, and source immutability; same-wave consumer is a generated scan-block. | Generic crate hardcodes JSON/CSS byte policy or the consumer is only a synthetic microbench. |
| Prefix/carry masks | Consumer is a string/escape/structural state machine whose row profile names carry propagation as hot; scalar reference handles cross-window carry. | Default replacement of scalar `prefix_xor_64` in the production dispatcher. |
| Next-bit extraction / compaction | Consumer writes positions or events in a way that avoids the REDRESS 89 bulk regression; row gate watches JSON guard floors and CSS parity. | `compact_mask` support primitive replacement without production row movement. |
| Digit dot product | Parser consumes fixed-width digit chunks and strict equality covers invalid/non-digit fallback; row gate targets number-heavy corpora. | Proof-only `parse_4_digits` helper with no generated number parser caller. |
| Ternary bitwise | Profile identifies 3-input boolean fan-in, and EOR3/BCAX replaces a measured chain in string or digest code. | "Tap SHA3 surface" without a named boolean expression and hot-leaf consumer. |
| SVE2 MATCH | Future scalable-vector target with SVE2 feature gate and SVE checkasm. | SK-V13 NEON/M5 Max route. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Does the M5 Max native target advertise CSSC to Rust without explicit `-C target-feature=+cssc`? | Capture `rustc --print cfg -C target-cpu=native` and disasm for any CSSC candidate before S-P3 wave-scoping. REDRESS 89 says native cfg did not advertise CSSC then. |
| Which CSS generated scan-block function can consume `AsciiSetRunSkip64` without changing comment/string semantics? | Inspect CSS generated/template loops and build a caller-level scalar reference over the same fact stream before production wiring. |
| Is Union-C's PMULL+CSSC path actually a material differential from REDRESS 88/89, or just a composition of rejected bodies? | S-P3 must name the new consumer shape and microbench it in isolation before any source redress. |
| Do number-heavy rows still profile digit MAC as hot after SK-V13 P1? | Use fresh P1 TSVs only; if digit parsing is not a hot leaf, keep UDOT inventory-only. |
| Are any x86 modules accidentally compiled or tested in the aarch64 close path? | `cargo metadata` and cfg audit should prove x86 modules are background only under SK-V13. |

## LOCKS-AMENDMENTS-CANDIDATE

| Candidate | Type | Lock(s) | Proposed amendment candidate | Supporting evidence |
|---|---|---|---|---|
| LAC-2E-01 | addition | Lock 16 | Add a hardware-gate manifest requirement: every intrinsic or `asm!` use maps to abstract primitive, ACLE/Intel source, target feature, scalar reference, checkasm test, same-wave consumer, and measured row or deletion disposition. | T-P1 1E marks Lock 16 traceability UNKNOWN; local `digit_mac.rs`, `cache_hints.rs`, scalar delegates, and x86 modules show heterogeneous gates. |
| LAC-2E-02 | refinement | Lock 16 / SK-V13 D5 | Define `inventory_demoted_with_evidence` as a historical disposition, not a permanent zero-orphan source state. SK-V13 close should require each source-present primitive to be production-wired, scalar-delegated with explicit non-ASM status, or deleted with REDRESS evidence. | SRC-SCOPE rows 16-23; REDRESS 126 final orphan wording; local bitmap/bulk files delegate to scalar. |
| LAC-2E-03 | refinement | Lock 16 / host-arch allowlist | State that `svmatch_u8` is SVE2-only and cannot satisfy a NEON/M5 Max primitive gate. | Arm SVE2 MATCH source and absence of local SVE code. |
| LAC-2E-04 | refinement | Lock 1 / Lock 16 | For PMULL/CSSC reopen attempts, require material-differential text that distinguishes new union/consumer shape from REDRESS 88/89 and REDRESS 96/97/98, plus a micro-prove-first artifact before S-P3 wave-scoping. | REDRESS 88/89 body-fill failures; REDRESS 96/97/98 union failures; SK-V13 scoping Union-C risk table. |
