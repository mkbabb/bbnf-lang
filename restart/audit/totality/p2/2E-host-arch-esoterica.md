---
agent: 2E
pass: T-P2-research
cycle: V7
v6_baseline: V6 (atomic write-only commit)
v7_fold_origin: T-P2 V1 hardening V2 — dossier-2E fold packet (CH1 BLK-02 URL refresh, CH6 F7 esoterica labels, CH7 §3.4 audit-state column normalisation)
generated_at: 2026-05-23T00:00:00Z
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
primary_sources_cited: 28
counted_source_ids:
  - SRC-A64-ACLE
  - SRC-A64-NEON
  - SRC-A64-ARM-ARM
  - SRC-A64-SVE2-MATCH
  - SRC-A64-CSSC-SPEC
  - SRC-X86-INTEL-INTRIN
  - SRC-X86-INTEL-CLMUL
  - SRC-X86-INTEL-SDM
  - SRC-WIKICHIP-GFNI
  - SRC-LEMIRE-SIMDJSON-PAPER
  - SRC-LANGDALE-PCLMUL-PREFIX-BLOG
  - SRC-LEMIRE-VBMI2-BLOG
  - SRC-LANGDALE-MOVMASK-NEON
  - SRC-MULA-AVX512-VBMI
  - SRC-MULA-GFNI-BIT-MANIPULATION
  - SRC-VALIDARK-COMPRESS
  - SRC-DOWNS-INTERLEAVED-LOADS
  - SRC-ARM-DOTPROD-BLOG
  - SRC-INTEL-CLMUL-WP
  - SRC-S-P2-V3-P2C
  - SRC-S-P2-V3-CONSOLIDATED
  - SRC-SCOPE
  - SRC-REDRESS
  - SRC-BBNF-A64
  - SRC-BBNF-DISPATCH
  - SRC-BBNF-CHECKASM
  - SRC-BBNF-X86
  - SRC-V2-FOLD
techniques_grounded: 14
techniques_refuted: 6
shared_fold_authority: restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
v3_fold_authority: restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
v4_fold_authority: restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md
prior_cycle_dispositions_folded:
  accepted: [V4-CH1, V4-CH2, V4-CH3, V4-CH4, V4-CH5, V4-CH6, V5-confirm-all, V6-baseline]
  rejected: []
  revised: []
  first_cycle_additions: []
  v6_fold_additions:
    - PER-ENTRY-PUBLISHED-CITATION
    - S-P2-V3-CANDIDATE-CROSSREF
    - PMULL-VPCLMUL-LINEAGE
    - LD4-INTERLEAVED-CLASSIFY-CITATION
    - SVE2-MATCH-NEON-PORT-REFUTATION
    - X86-AVX512-SECONDARY-EXPANSION
  v7_fold_additions:
    - CH1-BLK-02-MULA-LEMIRE-URL-REFRESH
      # 2 URLs refreshed clean: SRC-MULA-AVX512-VBMI (→ -remove-spaces slug per
      # 0x80.pl/notesen.html index), SRC-MULA-GFNI-BIT-MANIPULATION (→
      # articles/avx512-galois-field-for-bit-shuffling.html per
      # 0x80.pl/articles/index.html). 3 URLs URL-stale-fallback (no
      # recoverable slug at fold time): SRC-MULA-MOVMASK-NEON replaced by
      # SRC-LANGDALE-MOVMASK-NEON (Langdale branchfree 2019 "Fitting My
      # Head Through The ARM Holes" — canonical SHRN-based NEON pseudo-
      # movemask reference); SRC-LEMIRE-PCLMUL-PREFIX-BLOG replaced by
      # SRC-LANGDALE-PCLMUL-PREFIX-BLOG (Langdale branchfree 2019 "Code
      # Fragment: Finding quote pairs with carry-less multiply (PCLMULQDQ)");
      # SRC-LEMIRE-VBMI2-BLOG re-pinned to lemire.me 2022-04-28 "Removing
      # characters from strings faster with AVX-512" (the actual published
      # Lemire VPCOMPRESSB post). PMULL prefix-XOR abstract primitive
      # remains independently grounded by SRC-INTEL-CLMUL-WP + SRC-LEMIRE-
      # SIMDJSON-PAPER §3.3 per CH1 BLK-02 backstop ground.
    - CH6-F7-NOT-S-P3-ELIGIBLE-AT-V1-LABELS
      # Explicit `not_S-P3-eligible_at_V1` labels added to BCAX, LD4,
      # CRC32C, cache hints, ASCII run-skip per CH6 F7 fold instruction.
      # BCAX paragraph tightened to qualify "higher relevance than EOR3"
      # as conditional on a measured AND-NOT-XOR hot fan-in (not standalone
      # shape-superiority). "Other esoterica" block carries explicit per-
      # entry state annotation distinguishing source_backed grounding from
      # S-P3 admission eligibility.
    - CH7-AUDIT-STATE-COLUMN-NORMALISATION
      # Surface the audit_state cell as a distinct column in both aarch64
      # PRIMARY (13 entries) and x86 SECONDARY (9 entries) tables, using
      # the 4-state vocabulary (grounded / refuted / partial /
      # architecture_pressure) consistent with 2A/2B/2C/2D/2F. V6 narrative
      # state column retained alongside for detail (e.g. "source-backed /
      # prior implementation measured-rejected"). architecture_pressure
      # applied to PMULL prefix-XOR + CSSC CTZ (consumer-rejected per
      # REDRESS-88/-89 with abstract-primitive lineage intact).
locks_amendment_candidates: 4
---

## Executive Summary

This V6 cycle re-grounds 2E against the S-P2 V3 LOCKED candidate pool
(5 active arch esoterica: C-P2C-2 PMULL/CSSC structural union, C-P2C-3
UDOT digit MAC, C-P2C-4 TBL/TBX escape decode, C-P2C-5 string-special
64-byte context, C-P2C-8 parse-attribution profile rebuild gate). Each
esoterica entry carries an explicit published citation, an abstract-
primitive name, and the exact hardware gate (ACLE feature macro,
`target_feature` flag, or CPU-ID bit). aarch64 remains primary per the
M5 Max user pin; x86 is secondary background for cross-arch primitive
vocabulary (P2-C §1 #9; P2-F generalization).

The V4/V5 architectural conclusions hold: PMULL/CSSC are real, but the
REDRESS-88 (PMULL prefix-XOR hot body) and REDRESS-89 (CSSC CTZ next-bit
bulk) consumers are pre-blocked under the V6 fold — only a *SIMD-first
union consumer* that DELETES the scalar consume step opens C-P2C-2. The
V6 fold adds: (i) the published PMULL/VPCLMUL lineage (Intel CLMUL
Whitepaper 2014; Langdale + Lemire VLDBJ 2019 §3.3; Langdale 2019
branchfree.org PCLMULQDQ quote-pair code fragment — the V2 fold withdrew
a V6 reference to a Lemire 2016 prefix-XOR blog URL that 404s at V2 fold
time, leaning instead on Whitepaper + paper + Langdale branchfree), (ii)
the published LD4-interleaved classify primitive (`vld4q_u8`
decomposition per Arm Neon Intrinsics + Downs's interleaved-load blog),
(iii) the CSSC v8.9-A / v9.4-A spec anchor (Arm Architecture Reference
Manual §C7.2), (iv) the explicit refutation of "NEON `svmatch_u8` port"
(SVE2-only per Arm ARM §C2.2), (v) per-entry x86 AVX-512 secondary
expansion (VBMI2/GFNI/VPCLMUL/k-mask/AVX-IFMA/VNNI/BITALG) each tied to
Intel Intrinsics Guide + Mula/Langdale/Lemire/Wikichip primary
technique citations.

Refutation remains first-class: "instruction availability implies a
primitive should land", "`svmatch_u8` is a NEON primitive", "PMULL should
replace scalar carry by default", "cache hints are harmless support",
"final orphan count zero means no future SIMD cleanup", and a new V6
refutation "x86 AVX-512 GFNI/VBMI2 are cross-arch portable primitives"
all stand.

## Source Registry

Counted primary evidence rows are exactly the IDs in `counted_source_ids`
(28 sources). External hardware references are Arm ACLE, Arm Neon
Intrinsics Reference, Arm Architecture Reference Manual (ARM ARM), Arm
SVE2 documentation, Intel Intrinsics Guide, Intel CLMUL Whitepaper,
Intel SDM, and Wikichip GFNI page. Named-technique posts are Langdale +
Lemire's simdjson paper + Langdale's PCLMULQDQ quote-pairs branchfree
post + Lemire's 2022 VBMI2 / VPCOMPRESSB blog, Mula's AVX-512 VBMI +
GFNI bit-shuffling articles, Langdale's NEON pseudo-movemask branchfree
post, Validark's compress-store work, Downs's interleaved-loads blog,
and Arm's official Dot Product technical blog. **V2 fold note:** five
named-technique URLs returned 404 at V2 fold time; the two Lemire-
authored URLs (PCLMUL prefix-XOR + VBMI2) had no equivalent slug
findable on `lemire.me/blog/` at the cited title — for PCLMUL prefix-XOR
the canonical named-technique citation is replaced with Langdale's 2019
branchfree.org "Code Fragment" post, and for VBMI2 with Lemire's 2022
"Removing characters" post that demonstrably uses VPCOMPRESSB. One Mula
URL (NEON pseudo-movemask) had no replacement slug findable on
`0x80.pl/notesen.html` and was replaced with Langdale's 2019
branchfree.org "Fitting My Head Through The ARM Holes" post — the
canonical SHRN-based NEON pseudo-movemask reference and the actual
sequence bbnf-simd emits. Two Mula URLs (AVX-512 VBMI + GFNI bit-
shuffling) refresh cleanly per `0x80.pl/notesen.html` and
`0x80.pl/articles/index.html`. The PMULL/PCLMUL abstract primitive
itself remains independently grounded by `SRC-INTEL-CLMUL-WP` (Intel
CLMUL Whitepaper 323640) + `SRC-LEMIRE-SIMDJSON-PAPER` §3.3 (Langdale +
Lemire VLDBJ 2019) regardless of blog URL state. Local bbnf evidence is `SRC-BBNF-A64`,
`SRC-BBNF-DISPATCH`, `SRC-BBNF-CHECKASM`, `SRC-BBNF-X86`, `SRC-SCOPE`,
`SRC-REDRESS`; the S-P2 V3 P2-C dossier + consolidated are
`SRC-S-P2-V3-P2C` and `SRC-S-P2-V3-CONSOLIDATED`. FFmpeg, dav1d,
simdjson/sonic-rs/yyjson, egg/OR-Tools/RE2/Rust regex/fast_float,
Sneller, parse-that remain inherited support under `SRC-V2-FOLD`; they
are not counted as separate 2E registry rows.

| ID | Source | Use in this dossier |
|---|---|---|
| SRC-A64-ACLE | Arm C Language Extensions, feature macros: `__ARM_FEATURE_AES`, `__ARM_FEATURE_SHA3`, `__ARM_FEATURE_DOTPROD`, `__ARM_FEATURE_CSSC`, `__ARM_FEATURE_PMULL`, `__ARM_FEATURE_CRYPTO` ([ACLE 2026Q1](https://arm-software.github.io/acle/main/acle.html)) | Hardware gates for PMULL, SHA3 ternary logic, DotProd, CSSC, AES/PMULL crypto extension. |
| SRC-A64-NEON | Arm Neon Intrinsics Reference 2026Q1 ([NEON intrinsics](https://arm-software.github.io/acle/neon_intrinsics/advsimd.html)) | Intrinsic-to-instruction mapping for `vqtbl1q_u8`/`vqtbl4q_u8`/`vqtbx4q_u8`, `vld1q_u8_x4`/`vld4q_u8`, `vmull_p64`/`vmull_high_p64`, `veor3q_u8`/`vbcaxq_u8`, `vdotq_u32`/`vusdotq_s32`, `vextq_u8`. |
| SRC-A64-ARM-ARM | Arm Architecture Reference Manual for A-profile architecture (ARM ARM DDI 0487; §C7.2 CSSC, §C2.2 SVE2 MATCH, §C7 PMULL) | Authoritative ISA-level definition of CSSC (CTZ/CNT/ABS/SMAX/SMIN), SVE2 MATCH/NMATCH, PMULL/PMULL2 polynomial multiply 64x64→128. |
| SRC-A64-SVE2-MATCH | Arm Learning Path SVE2 MATCH example ([SVE2 MATCH](https://learn.arm.com/learning-paths/servers-and-cloud-computing/sve2-match/sve2-match-search/)) + ARM ARM §C2.2 MATCH/NMATCH | Refutes scoping `svmatch_u8` as a NEON primitive; MATCH is SVE2-only, requires scalable vectors. |
| SRC-A64-CSSC-SPEC | Arm Architecture Reference Manual §C7.2 + ACLE Common Short Sequence Compression (`__ARM_FEATURE_CSSC`) — CTZ, CNT, ABS, SMAX, SMIN, UMAX, UMIN; introduced in Armv8.9-A / Armv9.4-A | Authority for CSSC native CTZ on GP register; SK-V14 P2-C §1 #8 verifies CSSC requires explicit `+cssc` + asm-emission probe on the SK-V7 host. |
| SRC-X86-INTEL-INTRIN | Intel Intrinsics Guide, official instruction-set registry ([Intel Intrinsics Guide](https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html)) | x86 background: AVX2, AVX-512 family (F/CD/BW/DQ/VL), VBMI2, GFNI, VPCLMULQDQ, VPOPCNTDQ/BITALG, VNNI, AVX-IFMA. |
| SRC-X86-INTEL-CLMUL | Intel "Carry-Less Multiplication Instruction and its Usage for Computing the GCM Mode" Whitepaper (Gueron + Kounavis, Intel WP 323640) | PMULL/PCLMUL lineage; carry-less multiply algebra for prefix-XOR over packed structural masks. |
| SRC-X86-INTEL-SDM | Intel 64 and IA-32 Architectures Software Developer's Manual (SDM), Volume 2 (Instruction Set Reference), Vol 1 §14 (AVX-512) | Authoritative x86 ISA reference for AVX-512 k-mask arithmetic (KAND/KOR/KXNOR/KMOV), VPTERNLOGD, VPCOMPRESSB. |
| SRC-WIKICHIP-GFNI | Wikichip GFNI article ([Galois Field New Instructions](https://en.wikichip.org/wiki/x86/gfni)) | GFNI background + uarch availability table (Ice Lake client/server, Zen 4, etc.); confirms cross-arch portability gap. |
| SRC-LEMIRE-SIMDJSON-PAPER | Langdale + Lemire, "Parsing Gigabytes of JSON per Second", VLDBJ 2019 ([arXiv:1902.08318](https://arxiv.org/abs/1902.08318)) | Canonical structural-scan + prefix-XOR + escape-mask primitives; the foundational technique citation for C-P2C-2 and C-P2C-5. |
| SRC-LANGDALE-PCLMUL-PREFIX-BLOG | Geoff Langdale (simdjson co-author), "Code Fragment: Finding quote pairs with carry-less multiply (PCLMULQDQ)", 2019 ([branchfree.org/2019/03/06](https://branchfree.org/2019/03/06/code-fragment-finding-quote-pairs-with-carry-less-multiply-pclmulqdq/)) | Named-technique primary for PCLMUL/PMULL prefix-XOR; the algebra `M ⊗ ALL_ONES` projecting prefix-XOR in one polynomial multiply. **V2-fold note:** the V1 cite to a Lemire 2016 PCLMUL prefix-XOR blog (`lemire.me/blog/2016/05/23/...`) returned 404 at V2 fold time; no equivalent slug surfaces in title-search of `lemire.me/blog/` — the technique itself remains grounded by `SRC-INTEL-CLMUL-WP` (Intel CLMUL Whitepaper 323640) + `SRC-LEMIRE-SIMDJSON-PAPER` §3.3 (Langdale + Lemire VLDBJ 2019) independently of any blog URL. Branchfree (Langdale) carries the named-technique code fragment for the same primitive. |
| SRC-LEMIRE-VBMI2-BLOG | Daniel Lemire, "Removing characters from strings faster with AVX-512", 2022 ([lemire.me/blog/2022/04/28](https://lemire.me/blog/2022/04/28/removing-characters-from-strings-faster-with-avx-512/)) | Named-technique primary for `VPCOMPRESSB` byte-level compress-store; the AVX-512 analogue of bulk-emit-positions. **V2-fold note:** the V1 cite (`lemire.me/blog/2019/06/19/avx-512-vpcompressb/`) returned 404 at V2 fold time; the canonical Lemire VPCOMPRESSB post is the 2022 "Removing characters" article which explicitly uses `_mm512_mask_compressstoreu_epi8` over Ice Lake / Tiger Lake / Rocket Lake / Alder Lake VBMI2. |
| SRC-LANGDALE-MOVMASK-NEON | Geoff Langdale, "Fitting My Head Through The ARM Holes or: Two Sequences to Substitute for the Missing PMOVMSKB Instruction on ARM NEON", 2019 ([branchfree.org/2019/04/01](https://branchfree.org/2019/04/01/fitting-my-head-through-the-arm-holes-or-two-sequences-to-substitute-for-the-missing-pmovmskb-instruction-on-arm-neon/)) | Named-technique primary for the SHRN/SRI / SHRN-by-4 movemask emulation pattern at `bbnf-simd/src/aarch64/movemask.rs:1-25`. **V2-fold note:** the V1 cite to Mula's `0x80.pl/articles/simd-pmovmskb.html` returned 404 at V2 fold time; no replacement slug for that page surfaces in `0x80.pl/notesen.html`. Langdale's branchfree.org 2019 article is the canonical named-technique reference for the SHRN-based NEON pseudo-movemask sequence; the underlying SHRN-by-4 trick (Sequence #2 in that post) is what bbnf-simd emits. |
| SRC-MULA-AVX512-VBMI | Wojciech Mula, "AVX512VBMI — remove spaces from text" ([0x80.pl/notesen/2019-01-05-avx512vbmi-remove-spaces.html](http://0x80.pl/notesen/2019-01-05-avx512vbmi-remove-spaces.html)) | `VPERMB` / `VPERMI2B` byte-table-lookup analogue of NEON TBL/TBX; informs cross-arch primitive vocabulary. **V2-fold note:** V1 cite (`0x80.pl/notesen/2019-01-05-avx512vbmi.html`) returned 404 at V2 fold time; corrected slug per `0x80.pl/notesen.html` index appends `-remove-spaces` to the date stem. |
| SRC-MULA-GFNI-BIT-MANIPULATION | Wojciech Mula, "Use AVX512 Galois field affine transformation for bit shuffling" ([0x80.pl/articles/avx512-galois-field-for-bit-shuffling.html](http://0x80.pl/articles/avx512-galois-field-for-bit-shuffling.html)) | GFNI as single-instruction bit-shuffler / byte-classifier kernel; the simdjson SK-V12-cited usage anchor. **V2-fold note:** V1 cite (`0x80.pl/notesen/2022-10-18-avx512vbmi2-gfni-conversions.html`) returned 404 at V2 fold time; corrected to Mula's published GFNI bit-shuffling article in the `articles/` directory (per `0x80.pl/articles/index.html`), which uses `VGF2P8AFFINEQB` for arbitrary bit-shuffling within bytes and bit-matrix transposition — the same primitive abstraction. |
| SRC-VALIDARK-COMPRESS | Validark / Niels Möller analyses on AVX-512 byte-compress / store-compress for parser bulk-emit ([validark blog](https://validark.dev/posts/)) | Named-technique post for VBMI2 `VPCOMPRESSB` as the bulk-emit-positions analogue; the x86 secondary route for C-P2C-2's emit step. |
| SRC-DOWNS-INTERLEAVED-LOADS | Travis Downs, "Notes on interleaved loads" ([travisdowns.github.io](https://travisdowns.github.io/blog/2019/08/26/vector-inc.html)) — and ARM SDM `LD4` 4-way de-interleave entry | LD4-interleaved classify primitive: 4-way de-interleave load for SoA byte-stream classification. |
| SRC-ARM-DOTPROD-BLOG | Arm "Exploring the Arm dot product instructions", developer blog ([community.arm.com](https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/exploring-the-arm-dot-product-instructions)) | Named-technique post for UDOT/SDOT/UDOT-by-element for digit MAC and 4-lane decimal-digit fusion (C-P2C-3). |
| SRC-INTEL-CLMUL-WP | Intel "Intel Carry-Less Multiplication Instruction and its Usage for Computing the GCM Mode" Whitepaper, Gueron + Kounavis 2014 (323640) | x86 lineage of PMULL — PCLMULQDQ then VPCLMULQDQ 256/512-bit; the algebra-justification primary source for prefix-XOR via polynomial multiply. |
| SRC-S-P2-V3-P2C | `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md:1-164` — SK-V14 S-P2 P2-C V1 LOCKED dossier | 5 active arch esoterica candidates post-V2 demotion: C-P2C-2 (Union-C PMULL+CSSC), C-P2C-3 (UDOT digit MAC), C-P2C-4 (TBL/TBX escape decode), C-P2C-5 (string-special 64-byte context), C-P2C-8 (parse-attribution rebuild gate). |
| SRC-S-P2-V3-CONSOLIDATED | `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md` — S-P2 V3 §3Z COHORT LOCK | LOCKED state of candidate pool; binding cross-track reference for T-P2 grounding. |
| SRC-SCOPE | `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:10-180` | SK-V13 aarch64 inventory, W4 production split, union A/B/C candidates, untapped ISA surface. |
| SRC-REDRESS | `skinny/REDRESS.md:2508-2618` (Items 88-90), `:2795-2938` (Items 96-98 union variants), `:3603-3820` (Item 119 direct fixpoint matrix), `:3860-3872` (Item 126 orphan inventory) | Prior measured rejections; PMULL prefix-XOR + CSSC CTZ bulk pre-blocks; union substrate variant failures. |
| SRC-BBNF-A64 | `skinny/crates/bbnf-simd/src/aarch64/*.rs` | Local primitive bodies and scalar delegates: `classify_tbl4.rs:1-104`, `bitmap_prefix_xor_64.rs:1-5` (scalar delegate), `bitmap_next_set_bit.rs:1-5` (scalar delegate), `bulk_emit_positions_64.rs:1-5` (scalar delegate), `digit_mac.rs:1-71`, `movemask.rs:1-25`, `unescape_uxxxx.rs:1-214`, `byte_context.rs`, `cache_hints.rs:4-28`. |
| SRC-BBNF-DISPATCH | `skinny/crates/bbnf-simd/src/dispatch.rs:49-87`, `skinny/crates/bbnf-simd/src/lib.rs:169-272` | Current production dispatch + primitive facade; `NeonTbl4` selection at `dispatch.rs:89-112`. |
| SRC-BBNF-CHECKASM | `skinny/crates/bbnf-simd/tests/checkasm_*.rs` | Scalar references, parity harnesses, and W4 caller microbench (REDRESS 126 4.718x). |
| SRC-BBNF-X86 | `skinny/crates/bbnf-simd/src/x86_64/avx512_*`, `.../avx_ifma`, `.../vbmi2`, `.../gfni`, `.../vpclmul` | Local x86 background modules with `target_feature` gates; most bodies `unimplemented!`. |
| SRC-V2-FOLD | `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` + V3 + V4 addenda | Binding provenance register, Lock 14 transfer contract, Lock 1 substrate-kind contract, per-technique admission ledger, source-present orphan state enum, union/PMULL/CSSC material-differential checklist, executable hardware ledger, REDRESS-slice ownership table. |

## V6 Shared Provenance Register

This dossier inherits the V2 pinned-source register from `SRC-V2-FOLD`,
the V3 counted-source convention, and the V4 executable ledger delta.
Hardware references used directly here remain the stable Arm and Intel
architecture documents above. Moving source trees and local imports are
not used as admission authority unless pinned per V2/V3/V4 addenda.

V6 fold note: per the dispatch instruction "REDRESS pre-block 88 PMULL
prefix-XOR hot body, 89 CSSC CTZ next-bit bulk (do NOT re-open)" — the
V6 grounding of C-P2C-2 cites the published PMULL/PCLMUL lineage as
the *abstract primitive justification*, not as a license to replay the
REDRESS-88/-89 consumers. The Intel CLMUL Whitepaper 323640 + Langdale +
Lemire VLDBJ 2019 §3.3 algebra is the algorithm-correctness anchor for
*what the primitive computes*; the SK-V7 W10/W10b measured rejection of
the consumer remains binding. **V2 fold note:** the V6 reference to a
Lemire 2016 PCLMUL prefix-XOR blog URL is withdrawn (404 at V2 fold
time); the Whitepaper + simdjson VLDBJ paper carry the primitive
independently of any blog citation.

## Technique Grounding Table

Per dispatch §2 each esoterica entry carries `{ published citation,
abstract primitive name, hardware gate }`.

### aarch64 PRIMARY (M5 Max target)

**V2 fold note (audit-state column normalisation per CH7 §4.2.1):** the
per-row `audit_state` cell normalises to the 4-state vocabulary used
across 2A/2B/2C/2D/2F (`grounded` / `refuted` / `partial` /
`architecture_pressure`) — distinct from the longer `V6 narrative state`
cell which retains the detailed admission shape (`source-backed /
dispatchable local primitive`, `source-backed / prior implementation
measured-rejected`, etc.). The `architecture_pressure` value applies to
rows where the abstract primitive is grounded by published architecture
references but the consumer has been measured-rejected (PMULL prefix-XOR
hot body REDRESS-88; CSSC CTZ next-bit bulk REDRESS-89).

| spec claim / T-P1 divergence id | published source cited | audit_state | V6 narrative state | abstract primitive | hardware gate | bbnf-specific note |
|---|---|---|---|---|---|---|
| **A64-TBL low-6 byte classification** | `SRC-A64-NEON` maps `vqtbl4q_u8` to A64 `TBL` over 4×16-byte tables; `SRC-LEMIRE-SIMDJSON-PAPER` §3.1 cites NEON `TBL` as the dispatchable classifier; local `classify_tbl4.rs:17-32` loads four 16-byte tables and calls `vqtbl4q_u8`. | grounded | source-backed / dispatchable local primitive | `ByteClassLookup64TBL` | `target_arch = "aarch64"` + Advanced SIMD (always available on M-series); ACLE no-feature-gate needed (FEAT_AdvSIMD baseline). | Cleanest aarch64 classifier; remains grammar-neutral while alphabet is caller-provided and collision-checked (Lock 14 holds). Production-row gate: same-wave consumer + named generated grammar policy. Cross-ref **C-P2C-5** support primitive. |
| **A64-TBX fallback lookup** | `SRC-A64-NEON` maps `vqtbx4q_u8` to A64 `TBX` (preserves destination on out-of-range index); `SRC-SCOPE` marks TBX as unused refinement. | partial | partial | `ByteClassLookup64TBX` (accumulator-preserving) | `target_arch = "aarch64"` + Advanced SIMD baseline. | TBX useful only if row needs preserve-old-value semantics for out-of-range indices (unicode or CSS escape fallback). Cross-ref **C-P2C-4 TBX variant**: only admits when CSS escaped identifier / variable-length escape demands it; no SK-V14 V1 row evidence (PRUNE-2 successor wave). |
| **A64-LD1x4 / LD4 deinterleave classify** | `SRC-A64-NEON` maps `vld1q_u8_x4` to `LD1 {Vt.16B - Vt4.16B}` (4-register adjacent load) and `vld4q_u8` to `LD4 {Vt.16B,Vt2.16B,Vt3.16B,Vt4.16B}` (4-way de-interleave); `SRC-DOWNS-INTERLEAVED-LOADS` explains de-interleave-load as SoA byte-stream classification primitive. Local `classify_tbl4.rs:17-18` uses `vld1q_u8_x4`. | partial | partial / source-backed / **`not_S-P3-eligible_at_V1`** | `Interleave4Classify` (LD4 SoA byte-stream split) | `target_arch = "aarch64"` + Advanced SIMD (FEAT_AdvSIMD); no further feature gate. | `LD1x4` already a table-load convenience, not a row-moving primitive. `LD4` deinterleave needs a profiled 4-window consumer; SK-V13 scoping says it is out of scope until UTF-8/string deinterleave is hot. **Not currently a C-P2C candidate** but reserved for future string/escape SoA paths. **`not_S-P3-eligible_at_V1`** per CH6 F7 fold: eligible only post-F-V2-P1ABC-RERECORD when SoA / UTF-8 / string-deinterleave consumer becomes measured hot leaf. |
| **A64-PMULL prefix-XOR (CLMUL / simdjson lineage)** | `SRC-A64-ACLE` (`__ARM_FEATURE_AES` exposes FEAT_AES + FEAT_PMULL); `SRC-A64-NEON` maps `vmull_p64`/`vmull_high_p64` to `PMULL`/`PMULL2`; `SRC-A64-ARM-ARM` §C7 defines polynomial multiply 64×64→128; `SRC-INTEL-CLMUL-WP` is the x86 lineage (PCLMULQDQ) and the *load-bearing primary citation* for the algebra `M ⊗ ALL_ONES` (the V1 Lemire 2016 PCLMUL prefix-XOR blog URL returned 404 at V2 fold time and the primitive remains independently grounded by the Whitepaper + simdjson VLDBJ); `SRC-LANGDALE-PCLMUL-PREFIX-BLOG` is the named-technique secondary code-fragment reference for prefix-XOR via PCLMUL/PMULL; `SRC-LEMIRE-SIMDJSON-PAPER` §3.3 uses this technique for structural scanning. **REDRESS 88** emitted `pmull.1q` with `+cssc,+aes` but regressed JSON rows. | architecture_pressure | source-backed / prior implementation **measured-rejected** | `PrefixXor64Pmull` (carry-less multiply algebra for parallel prefix-XOR) | `target_arch = "aarch64"` + FEAT_PMULL via `+aes` / `__ARM_FEATURE_AES` (M-series ships PMULL natively). | PMULL exists, but default hot `bitmap_prefix_xor_64` body is historically rejected. Cross-ref **C-P2C-2** (Union-C PMULL+CSSC structural union): the PMULL grounding is the abstract-primitive justification (Intel CLMUL Whitepaper algebra + simdjson VLDBJ §3.3 construction), not a re-open license. A reopen must satisfy V2 material-differential checklist: name row consumer, cite REDRESS 88/89/96-98, delete or bypass scalar cost source intentionally, pass scalar/checkasm/microbench before S-P3. |
| **A64-CSSC CTZ next-bit / bulk emit** | `SRC-A64-ACLE` defines `__ARM_FEATURE_CSSC`; `SRC-A64-CSSC-SPEC` (Arm ARM §C7.2 + ACLE CSSC: CTZ/CNT/ABS/SMAX/SMIN/UMAX/UMIN); introduced in Armv8.9-A / Armv9.4-A. **REDRESS 89** emitted `ctz` with `+cssc` and passed checkasm but regressed 7 rows ≥2%. | architecture_pressure | source-backed / prior implementation **measured-rejected** | `NextSetBitCssc` (native GP-register CTZ for bulk-emit-positions) | `target_arch = "aarch64"` + FEAT_CSSC via `+cssc` / `__ARM_FEATURE_CSSC`. SK-V7 W10b probe: `-C target-cpu=native` did *not* auto-advertise CSSC on the SK-V7 host; M5 Max may auto-advertise under a newer toolchain — verify via emitted-asm probe (`otool -tV` / `cargo asm`), not `-cpu=native` inference alone. | CSSC CTZ instruction is not blocked; the prior bulk consumer shape is blocked. Cross-ref **C-P2C-2** Union-C: CTZ admissible only inside a measured union consumer that also deletes the scalar consume step (REDRESS 89 falsified the standalone CTZ bulk consumer). Cannot replay the `compact_mask` scalar-delegate regression pattern. |
| **A64-DOTPROD UDOT/SDOT digit MAC** | `SRC-A64-ACLE` defines `__ARM_FEATURE_DOTPROD`; `SRC-A64-NEON` maps `vdotq_u32` to `UDOT Vd.4S, Vn.16B, Vm.16B`; `SRC-ARM-DOTPROD-BLOG` named-technique primary for UDOT/SDOT digit fusion; local `digit_mac.rs:25-49` uses `#[target_feature(enable = "dotprod")]` and inline `udot {acc:v}.4s, {digits:v}.16b, {weights:v}.16b` over `[100,10,1,0]`. | partial | source-backed / conditional | `DigitMac4Udot` (4-digit decimal MAC: one UDOT folds 4 ASCII digits → u32) | `target_arch = "aarch64"` + FEAT_DotProd via `+dotprod` / `__ARM_FEATURE_DOTPROD`. M-series advertises via `sysctl hw.optional.arm.FEAT_DotProd`; M5 Max post-FEAT_DotProd by generation. | Local code is proof-only. Cross-ref **C-P2C-3** `udot_digit_span_x4`: re-evaluation gate is **F-V2-P1ABC-RERECORD** (parse-attribution rebuild) naming a numeric leaf rank-1 on a number-heavy corpus (`canada`/`numbers`/`mesh`). Until then UDOT remains source-present conditional inventory despite valid hardware gating. |
| **A64-EOR3 ternary bitwise** | `SRC-A64-ACLE` defines `__ARM_FEATURE_SHA3`; `SRC-A64-NEON` maps `veor3q_u8` to `EOR3 Vd.16B, Vn.16B, Vm.16B, Va.16B`; `SRC-SCOPE` says no local 3-input fold exists. | partial | source-backed / conditional | `TernaryXor3Eor3` (three-input XOR fold in one cycle) | `target_arch = "aarch64"` + FEAT_SHA3 via `+sha3` / `__ARM_FEATURE_SHA3` (M-series advertises `hw.optional.armv8_2_sha3`). | Good fit for quote/escape/control fusion only if fresh profile names a 3-input boolean fan-in. Cross-ref **C-P2C-6 (demoted)**: SK-V14 scanner uses ANDs/ANDNs (`scan.rs:225-265`), not XOR triples. **`NOT-S-P3-ELIGIBLE` at SK-V14 V1**; re-evaluate post-F-V2-P1ABC-RERECORD only if rebuild surfaces a three-input XOR expression. |
| **A64-BCAX bitwise clear-AND-XOR** | `SRC-A64-NEON` maps `vbcaxq_u8` to `BCAX Vd.16B, Vn.16B, Vm.16B, Va.16B` (computes `Vn XOR (Vm AND NOT Va)`); `SRC-A64-ARM-ARM` §C7 entry under FEAT_SHA3. | partial | source-backed / conditional / **`not_S-P3-eligible_at_V1`** | `BicXor3Bcax` (three-input `a XOR (b AND NOT c)` in one cycle) | `target_arch = "aarch64"` + FEAT_SHA3 via `+sha3` / `__ARM_FEATURE_SHA3`. | Direct hardware *shape match* for the SK-V14 scanner's `punctuation & !string_body`, `quotes & !escaped` mask algebra at `scan.rs:225-265`. **Would be higher V6 relevance than EOR3 _if_ the AND-NOT-XOR algebra is named as a measured hot fan-in** — the existing scanner uses AND-NOT chains rather than XOR triples, but standalone shape-superiority over EOR3 is not by itself shortlist-promoting. **NOT in current C-P2C-6**; V6 surfaces BCAX as a *candidate extension* worth re-evaluating once F-V2-P1ABC-RERECORD names a measured 3-input AND-NOT-XOR fan-in. **`not_S-P3-eligible_at_V1`** per CH6 F7 fold: no V1 row evidence; eligible only post-F-V2-P1ABC-RERECORD measured naming. |
| **A64-SVE2 `svmatch_u8` (REFUTED for SK-V13/V14 NEON)** | `SRC-A64-SVE2-MATCH` + `SRC-A64-ARM-ARM` §C2.2 — `svmatch_u8` is SVE2 MATCH with scalable vectors; `SRC-SCOPE` says SK-V13/V14 is NEON-only on fixed 128-bit M-series (M1-M4 no SVE/SVE2/SME; M5 family introduces SME but SVE2 availability requires M5-generation verification). | refuted | **refuted for SK-V13/V14 NEON port** | `Match16ByteSetSve2` (SVE2 MATCH for set-membership over scalable vector) | `target_arch = "aarch64"` + FEAT_SVE2 via `+sve2` / `__ARM_FEATURE_SVE2`. Not available on M1-M4; M5 generation requires verification before any scope-in. | Prompt phrase "NEON `svmatch_u8` port" is technically wrong. A future SVE2 track may use MATCH; SK-V13/V14 aarch64/M5 Max should not scope it as a NEON primitive. *NEON port via `vqtbl1q_u8` + comparison* is the alternative path: build set table once with `vld1q_u8` over the 16-byte set, then `vceqq_u8` against each input byte and OR-reduce — but this is just the standard NEON byte-set primitive, not a "port of MATCH". |
| **A64-ASCII run-skip** | `SRC-BBNF-CHECKASM` `checkasm_ascii_set_member_find_64.rs:20-40` compares scalar byte-walk to `byte_class_from_eq_set_64`; `:137-190` emits microbench artifact; REDRESS 126 records 18.51 ns vs 3.92 ns, 4.718x. | partial | micro_proven / not admitted / **`not_S-P3-eligible_at_V1`** | `AsciiSetRunSkip64` (64-byte byte-set first-member-find) | `target_arch = "aarch64"` + Advanced SIMD baseline; no specialized feature flag. | Best SK-V13 production-wiring candidate on microbench shape. Cross-ref **C-P2C-1 (demoted V2)**: SK-V14 V1 PRUNE-2 falsifies CSS L4 row movement (23/24 CSS L4 rows are fixture-lookup short-circuits); re-eligible only after CSS L4 plane is rebuilt (S-P3 PRUNE-2 successor wave). **`not_S-P3-eligible_at_V1`** per CH6 F7 fold: PRUNE-2 demotion binding; eligible only post-CSS-L4-plane-rebuild successor wave. |
| **A64-vextq_u8 byte-context boundary shifts** | `SRC-A64-NEON` maps `vextq_u8` to A64 `EXT` (cross-vector byte extract); local `byte_context.rs:4-10` uses `vextq_u8`. | partial | partial | `ByteContextExt` (cross-chunk one-byte neighbor propagate) | `target_arch = "aarch64"` + Advanced SIMD baseline. | Valid helper for cross-chunk string special scans, but support-only today. Cross-ref **C-P2C-5** `string_special_64_context`: V6 surfaces `vextq_u8` as the cross-chunk primitive folded into the 64-byte oracle; standalone admission still blocked (REDRESS 126 orphan). |
| **A64-cache hints / PRFM / STNP** | Local `cache_hints.rs:4-28` uses `prfm` and `stnp`; `SRC-A64-ARM-ARM` §C5 prefetch/non-temporal store entries; REDRESS 126 demotes module (no production caller). | refuted | **refuted as standalone admission** / **`not_S-P3-eligible_at_V1`** | `CacheHintPrefetchStore` (PRFM/STNP placement hints) | `target_arch = "aarch64"` baseline (no feature gate). | Prefetch/store hints are placement-sensitive. Must be deleted or wired behind a measured caller; support-only hint module is not a Lock 16 close. **Excluded from C-P2C active pool**; close-hygiene only. **`not_S-P3-eligible_at_V1`** per CH6 F7 fold: REDRESS 126 demotion binding; eligible only if a measured hot caller appears in a future profile *and* hint placement is benchmarked as net-positive. |
| **A64-CRC32C** | `SRC-A64-ACLE` defines `__ARM_FEATURE_CRC32`; `SRC-A64-NEON` maps `__crc32cb`/`__crc32ch`/`__crc32cw`/`__crc32cd` to A64 `CRC32C{B,H,W,X}`. M-series advertises FEAT_CRC32. | partial | source-backed / not in current candidate set / **`not_S-P3-eligible_at_V1`** | `Crc32CHash` (hardware-accelerated CRC32C for digest folds) | `target_arch = "aarch64"` + FEAT_CRC32 via `+crc` / `__ARM_FEATURE_CRC32`. | Not in active C-P2C set; surfaced as a *cross-arch primitive* (x86 has SSE 4.2 `CRC32`). Relevant only if a future digest/fact-stream consumer wants a hardware-accelerated hash; current `JsonDirectDigest` is FxHash-style scalar mix. **`not_S-P3-eligible_at_V1`** per CH6 F7 fold: no measured consumer; eligible only if future profile names digest mix as hot and CRC32C beats current FxHash scalar mix in measured throughput. |

### x86 SECONDARY (cross-arch primitive vocabulary, P2-F generalization context)

Per dispatch §2 "x86 SECONDARY". Each entry: Intel Intrinsics Guide
citation + CPU feature flag. **No x86 instruction is a SK-V14 candidate
per S-P2 V3 P2-C §5.4**; x86 grounding is for cross-arch primitive
vocabulary only.

| spec claim | published source cited | audit_state | V6 narrative state | abstract primitive | hardware gate (Intel CPU-ID + Rust `target_feature`) | cross-arch / bbnf note |
|---|---|---|---|---|---|---|
| **x86 AVX2 PSHUFB byte-table** | `SRC-X86-INTEL-INTRIN` `_mm256_shuffle_epi8` → `VPSHUFB ymm, ymm, ymm`; `SRC-LEMIRE-SIMDJSON-PAPER` §3.1 uses VPSHUFB as the dispatchable classifier. | grounded | source-backed | `ByteClassLookup64VPSHUFB` (x86 analogue of NEON TBL) | CPU-ID AVX2 (Haswell+); Rust `target_feature = "avx2"`. | Cross-arch peer to `ByteClassLookup64TBL`; primitive abstracts to "16-entry byte table lookup over a 16-byte input chunk". |
| **x86 AVX2 PMOVMSKB native movemask** | `SRC-X86-INTEL-INTRIN` `_mm256_movemask_epi8` → `VPMOVMSKB`. | grounded | source-backed | `Movemask32Avx2` (32-byte→32-bit mask, native) | CPU-ID AVX2; Rust `target_feature = "avx2"`. | Cross-arch peer to NEON SHRN/SRI emulation at `bbnf-simd/src/aarch64/movemask.rs:1-25` (`SRC-LANGDALE-MOVMASK-NEON`); the x86 path has a *single instruction*, NEON requires a 5-instruction sequence. |
| **x86 AVX-512 VPCLMULQDQ wide PMULL** | `SRC-X86-INTEL-INTRIN` `_mm256_clmulepi64_epi128` / `_mm512_clmulepi64_epi128` → `VPCLMULQDQ ymm/zmm`; `SRC-INTEL-CLMUL-WP` lineage; `SRC-LANGDALE-PCLMUL-PREFIX-BLOG`. | grounded | source-backed background | `PrefixXor256Vpclmul` / `PrefixXor512Vpclmul` (wider PMULL for prefix-XOR over packed structural masks) | CPU-ID VPCLMULQDQ + AVX-512F/VL (Ice Lake+, Zen 4+); Rust `target_feature = "vpclmulqdq,avx512f"`. | Cross-arch *wider* peer to aarch64 PMULL prefix-XOR (`A64-PMULL prefix-XOR`); same abstract primitive `PrefixXorPmull`, parameterized by lane width (128/256/512). |
| **x86 AVX-512 GFNI byte affine transform** | `SRC-X86-INTEL-INTRIN` `_mm512_gf2p8affine_epi64_epi8` → `VGF2P8AFFINEQB zmm,zmm,zmm,imm8`; `SRC-WIKICHIP-GFNI` uarch table (Ice Lake / Zen 4); `SRC-MULA-GFNI-BIT-MANIPULATION` technique. | grounded | source-backed background | `ByteAffineGfniClassify` (single-op byte-classifier via GF(2^8) affine map) | CPU-ID GFNI + AVX-512BW/F/VL (Ice Lake+, Zen 4+); Rust `target_feature = "gfni,avx512bw"`. | Cross-arch peer to NEON TBL; semantically *richer* (full affine map vs. table lookup). **No aarch64 equivalent** — the simdjson SK-V12 GFNI usage cited in P2-C §1 #9 is x86-only and breaks cross-arch primitive vocabulary if a generic bbnf primitive abstracts to it. |
| **x86 AVX-512 VBMI2 VPCOMPRESSB byte compress-store** | `SRC-X86-INTEL-INTRIN` `_mm512_mask_compressstoreu_epi8` → `VPCOMPRESSB zmm,k,m`; `SRC-LEMIRE-VBMI2-BLOG`; `SRC-VALIDARK-COMPRESS`. | grounded | source-backed background | `BulkEmitPositions64Vpcompressb` (one-instruction bulk-emit-positions: write only positions where mask bit is set) | CPU-ID AVX-512_VBMI2 + AVX-512BW/F/VL (Ice Lake+, Zen 4+); Rust `target_feature = "avx512vbmi2,avx512bw"`. | Cross-arch peer to aarch64 CTZ-loop / CSSC CTZ bulk-emit. **The x86 path is single-instruction**; aarch64 has no native equivalent (PEXT/VPCOMPRESSB pre-block per P2-C §4 "PEXT pre-block"). This asymmetry is the binding cross-arch primitive-vocabulary gap. |
| **x86 AVX-512 k-mask arithmetic** | `SRC-X86-INTEL-SDM` Vol 1 §14.2 + `SRC-X86-INTEL-INTRIN` `_kand_mask64`/`_kor_mask64`/`_kxnor_mask64`/`_kmov_mask64` → `KAND`/`KOR`/`KXNOR`/`KMOVQ` on `k0..k7`. | grounded | source-backed background | `MaskArithmetic64K` (dedicated 64-bit mask register arithmetic) | CPU-ID AVX-512F (k0..k7) + AVX-512BW for 64-bit ops; Rust `target_feature = "avx512f,avx512bw"`. | Cross-arch peer to aarch64 movemask-then-GP-register algebra. **k-mask registers are a parallel substrate** that simulates a sidecar at the ISA level; primitive-vocabulary abstraction must collapse k-mask operations to "ephemeral mask in local loop" (Lock 1: `local_temp_only`, `local_loop`). |
| **x86 AVX-IFMA integer FMA** | `SRC-X86-INTEL-INTRIN` `_mm256_madd52lo_epu64`/`_mm256_madd52hi_epu64` → `VPMADD52LUQ`/`VPMADD52HUQ`; AVX-IFMA Intel CPU-ID. | grounded | source-backed background | `IntegerFma52` (52-bit integer FMA for bignum / digest mixing) | CPU-ID AVX-IFMA (Tiger Lake+, Zen 4+); Rust `target_feature = "avxifma"`. | Cross-arch *no aarch64 equivalent*; NEON has scalar IMULL but no 52×52→104 FMA. Niche for digest mixing or bignum number parse; **not in C-P2C set**, surfaced as cross-arch asymmetry. |
| **x86 AVX-512 VNNI dot-product** | `SRC-X86-INTEL-INTRIN` `_mm512_dpbusd_epi32`/`_mm512_dpwssd_epi32` → `VPDPBUSD`/`VPDPWSSD`. | grounded | source-backed background | `DigitMac64Vnni` (byte/word dot-product MAC; x86 peer to UDOT) | CPU-ID AVX-512_VNNI (Cascade Lake+, Ice Lake client) or AVX-VNNI (Alder Lake+); Rust `target_feature = "avx512vnni"` or `"avxvnni"`. | Cross-arch peer to `A64-DOTPROD UDOT`. Same abstract primitive `DigitMacDotprod`, parameterized by lane width. **C-P2C-3 UDOT** has VNNI as its x86 counterpart. |
| **x86 AVX-512 BITALG VPOPCNTB / VPSHUFBITQMB** | `SRC-X86-INTEL-INTRIN` `_mm512_popcnt_epi8` → `VPOPCNTB`; `_mm512_bitshuffle_epi64_mask` → `VPSHUFBITQMB`. | grounded | source-backed background | `PopCount8Vpopcntb` / `BitShuffleSelect` (per-byte popcount / bit-extract-by-index) | CPU-ID AVX-512_BITALG (Ice Lake+, Zen 4+); Rust `target_feature = "avx512bitalg"`. | Cross-arch *no exact aarch64 equivalent*; NEON has `CNT` (per-byte popcount) since v8 baseline so popcount itself is portable, but VPSHUFBITQMB (per-byte bit-select) has no NEON peer. Niche for digest rank / bit-bulk operations. |

## Hardware Gates

| abstract primitive | primary hardware gate | local gate / proof | admissibility state |
|---|---|---|---|
| `ByteClassLookup64TBL` | `target_arch = "aarch64"` with Advanced SIMD; ACLE NEON `vqtbl4q_u8` | `dispatch.rs:89-112`; `classify_tbl4.rs:17-32`; checkasm classifier tests | `production_wired` (the NEON stripe at `scan.rs:200-275` consumes it). |
| `ByteClassLookup64TBX` | `target_arch = "aarch64"` Advanced SIMD; `vqtbx4q_u8` | No local body | `source_backed` (refinement for accumulator-preserving fallback). |
| `Interleave4Classify` (LD4) | `target_arch = "aarch64"` Advanced SIMD; `vld4q_u8` | Not consumed for de-interleave; `vld1q_u8_x4` consumed | `source_backed`; reserved for future SoA byte-stream paths. |
| `PrefixXor64Pmull` | `target_arch = "aarch64"` + FEAT_PMULL via `+aes` / `__ARM_FEATURE_AES` | REDRESS 88 disasm proof + measured reject; local aarch64 prefix-XOR scalar delegate at `bitmap_prefix_xor_64.rs:1-5` | `measured_rejected` (consumer); reopen only with material differential and row gate. **Cross-ref C-P2C-2**. |
| `NextSetBitCssc` | `target_arch = "aarch64"` + FEAT_CSSC via `+cssc` / `__ARM_FEATURE_CSSC` | REDRESS 89 disasm proof + measured reject; local next-bit scalar delegate at `bitmap_next_set_bit.rs:1-5` | `measured_rejected` (consumer); reopen only with different consumer, not the rejected bulk path. **Cross-ref C-P2C-2**. |
| `BulkEmitPositions64Ctz` | Paired with CTZ; no standalone ISA gate | Scalar delegate at `bulk_emit_positions_64.rs:1-5`; facade consumed by `lib.rs:208-223` | Not an ASM admission; wire or delete under D5. |
| `DigitMac4Udot` | `target_arch = "aarch64"` + FEAT_DotProd via `+dotprod` / `__ARM_FEATURE_DOTPROD` | `digit_mac.rs:25-71` uses `udot`/`sdot`; no parser consumer | `source_backed`; conditional until number parser consumes it and moves a row. **Cross-ref C-P2C-3**. |
| `TernaryXor3Eor3` | `target_arch = "aarch64"` + FEAT_SHA3 via `+sha3` / `__ARM_FEATURE_SHA3` | No local aarch64 body; scoping says no hot-leaf attribution | `source_backed`; conditional until row-local 3-input XOR fold exists. |
| `BicXor3Bcax` | `target_arch = "aarch64"` + FEAT_SHA3 via `+sha3` / `__ARM_FEATURE_SHA3` | No local aarch64 body | `source_backed`; **V6 NEW** — higher relevance than EOR3 because scanner uses AND-NOT chains. |
| `Match16ByteSetSve2` | `target_arch = "aarch64"` + FEAT_SVE2 via `+sve2` / `__ARM_FEATURE_SVE2` | No local SVE code; SK-V13/V14 fixed to NEON/M1-M4 | `architectural_block` for SK-V13/V14; future SVE2 / M5 SME generation only. |
| `AsciiSetRunSkip64` | `target_arch = "aarch64"` Advanced SIMD baseline | Microbench at `checkasm_ascii_set_member_find_64.rs:137-190`; REDRESS 126 | `micro_proven`; needs CSS production consumer (PRUNE-2 successor wave). |
| `ByteContextExt` | `target_arch = "aarch64"` Advanced SIMD baseline | `byte_context.rs`; no production caller | `source_backed`; folds into C-P2C-5. |
| `CacheHintPrefetchStore` | `target_arch = "aarch64"` baseline; placement uarch-sensitive | `cache_hints.rs:4-28`; no consumer | `architectural_block` as standalone; delete or wire with measured caller. |
| `Crc32CHash` | `target_arch = "aarch64"` + FEAT_CRC32 via `+crc` / `__ARM_FEATURE_CRC32` | No local consumer | `source_backed`; not in active candidate pool. |
| x86 AVX-512 classify/carry/digit/float family | `target_arch = "x86_64"` + per-feature gates: `avx512vbmi2`, `gfni`, `avx512bitalg`, `vpclmulqdq`, `avx512vnni`, `avxifma`, `avx512bw`, `avx512f`, `avx512vl` | Local x86 modules declare gates and scalar refs, but most bodies are `unimplemented!` | `source_backed` background; not SK-V13/V14 M5 Max admission. |

## S-P2 V3 P2-C Candidate Cross-Reference (V6 NEW)

Per dispatch §2: "Cross-reference S-P2 V3 P2-C 5 active arch esoterica
candidates (post-V2 demotion); 2E grounds *why* these admit per Lock 16
admissibility (and what other esoterica should be evaluated)."

| C-P2C ID | abstract primitive | aarch64 hardware gate | published citation | T-P2 2E V6 grounding verdict | further esoterica 2E surfaces |
|---|---|---|---|---|---|
| **C-P2C-2** `pmull_cssc_structural_union_emit64` | `PrefixXor64Pmull` + `NextSetBitCssc` composed | `+aes` + `+cssc` (FEAT_PMULL + FEAT_CSSC) | `SRC-INTEL-CLMUL-WP` (Intel CLMUL Whitepaper 323640: PCLMUL prefix-XOR algebra) + `SRC-LEMIRE-SIMDJSON-PAPER` §3.3 (Langdale + Lemire VLDBJ 2019 structural-scan construction) + `SRC-A64-ARM-ARM` §C7/§C7.2 + `SRC-LANGDALE-PCLMUL-PREFIX-BLOG` (Langdale 2019 branchfree code-fragment) | **GROUNDED at abstract-primitive level; PRE-BLOCKED at consumer level.** The published technique is well-established (Intel CLMUL Whitepaper 2014 + simdjson VLDBJ 2019 §3.3); the abstract primitive is admissible. The REDRESS 88+89 *consumer rejection* binds — only a SIMD-first union consumer that DELETES the scalar consume step opens this. Lock 16 manifest fields: substrate `existing_tape`/`direct_sink`/`admitted_fact_output`, retention `generated_function`, policy `generated_grammar`. **V2 fold note:** the V6 reference to a Lemire 2016 PCLMUL prefix-XOR blog is withdrawn (URL 404 at V2 fold time, no recoverable slug); citation chain now leans on Whitepaper + simdjson paper as the load-bearing pair, with Langdale's 2019 branchfree code-fragment as the named-technique secondary. | V6 surfaces: (a) **`BicXor3Bcax`** as an *adjacent* primitive (BCAX matches AND-NOT-XOR scanner algebra); (b) the *abstract-primitive lineage register* — `PrefixXorPmull` is parameterized by lane width (NEON 128b, x86 VPCLMULQDQ 256/512b); cross-arch primitive vocabulary is parameterized, not instruction-specialized. |
| **C-P2C-3** `udot_digit_span_x4` | `DigitMac4Udot` | `+dotprod` (FEAT_DotProd) | `SRC-A64-ACLE` `__ARM_FEATURE_DOTPROD` + `SRC-A64-NEON` `vdotq_u32 → UDOT` + `SRC-ARM-DOTPROD-BLOG` named-technique | **GROUNDED; conditional on F-V2-P1ABC-RERECORD.** Hardware gate clean (M-series advertises FEAT_DotProd). Abstract primitive is well-established. Re-evaluation gate is parse-attribution rebuild naming a numeric leaf rank-1; without it, UDOT remains proof-only inventory despite valid grounding. | V6 surfaces: (a) **x86 VNNI VPDPBUSD** as the cross-arch peer (`SRC-X86-INTEL-INTRIN`); (b) the abstract primitive `DigitMacDotprod` is parameterized by lane width and digit-count (4-lane UDOT vs 16-lane VNNI). |
| **C-P2C-4** `tbl_tbx_escape_decode_batch` | `ByteClassLookup64TBL` (JSON `\uXXXX` fixed-width) + `ByteClassLookup64TBX` (CSS variable-length escape, fallback) | Advanced SIMD baseline | `SRC-A64-NEON` `vqtbl1q_u8` → TBL + `vqtbx4q_u8` → TBX; local `unescape_uxxxx.rs:74-167` single + x4 variants | **GROUNDED + S-P3-eligible for JSON fixed-width route at SK-V14 V1.** TBX-for-CSS variant remains `NOT-S-P3-ELIGIBLE` at V1 until CSS L4 plane is rebuilt. Two of the three cleanest primitive-leaf attributions at SK-V14 (`unescape_string` 46.7% direct rank-1 on `unicode_escapes`, `read_hex_unit_scalar` 100% parse rank-1 on `y_string_unicode`) point here. | V6 surfaces: (a) **`Interleave4Classify` (LD4)** as a future de-interleave primitive for SoA hex-nibble decoding; (b) x86 **`VPERMB` / `VPERMI2B`** (`SRC-MULA-AVX512-VBMI`) as cross-arch peer — wider byte-table lookup. |
| **C-P2C-5** `string_special_64_context` | `ByteClassLookup64TBL` + `ByteContextExt` + movemask (SHRN/SRI per `SRC-LANGDALE-MOVMASK-NEON`) | Advanced SIMD baseline | `SRC-A64-NEON` `vextq_u8` → EXT + `SRC-LEMIRE-SIMDJSON-PAPER` §3.2 string scan + `SRC-LANGDALE-MOVMASK-NEON` | **GROUNDED; conditional as support primitive.** Folds into C-P2C-2 Union-C consumer (if landed) or C-P2C-4 64-byte oracle pre-step. Standalone admission requires measured string-mask consumer that does not exist at SK-V14 envelope-bound profile. | V6 surfaces: (a) **`BicXor3Bcax`** for the string-mask AND-NOT algebra; (b) cross-arch x86 **`VPMOVMSKB`** (`SRC-X86-INTEL-INTRIN`) is the single-instruction movemask vs aarch64's 5-instruction SHRN/SRI sequence. |
| **C-P2C-8** `parse_attribution_profile_rebuild_gate` | N/A — process gate, not instruction | N/A | N/A — measurement deliverable | **NOT an instruction candidate.** Process-gate prerequisite for every other C-P2C admission whose admission depends on envelope-cracked attribution. Tracks the F-V2-P1ABC-RERECORD deferred packet. | N/A. |

**Other esoterica 2E surfaces for future evaluation (V6 NEW):**

Each entry below carries explicit `state = source_backed; not_S-P3-eligible_at_V1; eligible only post-F-V2-P1ABC-RERECORD` per CH6 F7 fold — the V6 lineage strengthening does not by itself promote shortlist eligibility. None of these may be promoted to S-P3 admission on citation strength alone.

- **`Interleave4Classify` (LD4)** — `vld4q_u8` 4-way de-interleave load; abstract primitive for SoA byte-stream classification. Hardware gate: Advanced SIMD baseline. Citation: `SRC-A64-NEON` + `SRC-DOWNS-INTERLEAVED-LOADS`. **State: `source_backed; not_S-P3-eligible_at_V1`.** Re-evaluation only post-F-V2-P1ABC-RERECORD if UTF-8 / string deinterleave becomes measured hot leaf.
- **`BicXor3Bcax`** — `vbcaxq_u8` computes `a XOR (b AND NOT c)` in one cycle. Hardware gate: `+sha3` / FEAT_SHA3 (M-series advertises). Citation: `SRC-A64-NEON` + `SRC-A64-ARM-ARM` §C7. **State: `source_backed; not_S-P3-eligible_at_V1`.** Shape-matches SK-V14 scanner AND-NOT chains, but standalone shape-superiority over EOR3 is not shortlist-promoting — eligibility opens only post-F-V2-P1ABC-RERECORD when an AND-NOT-XOR algebra is named as a measured hot fan-in.
- **`Crc32CHash`** — hardware-accelerated CRC32C; cross-arch (aarch64 FEAT_CRC32 + x86 SSE 4.2 CRC32). Hardware gate: `+crc` / `__ARM_FEATURE_CRC32`. Citation: `SRC-A64-ACLE` + `SRC-A64-NEON`. **State: `source_backed; not_S-P3-eligible_at_V1`.** Re-evaluation only if future digest / fact-stream consumer profiles digest mix as hot and CRC32C beats current FxHash scalar mix in measured throughput.

## Lock 16 Hardware-Gate Manifest

Every T-P3 hardware route derived from this dossier must emit a gate-
consumable manifest row before redress. Architecture availability is
only `source_backed`; admission starts only after the row reaches
`production_wired` and then either `row_admitted`, `measured_rejected`,
or `architectural_block`. V6 retains all V3/V4 fields:

| field | V6 requirement |
|---|---|
| `candidate_id` | Stable primitive or route identifier, e.g. `pmull_cssc_structural_union_emit64`. |
| `source_paths_or_external_source` | Local source path or `SRC-*` authority; moving upstream sources must use the V2 pinned register. |
| `published_citation` (**V6 NEW**) | Explicit Intel Intrinsics Guide / ARM ACLE / ARM ARM section anchor or named-technique blog post URL. |
| `abstract_primitive_name` (**V6 NEW**) | Stable name independent of ISA-specific intrinsic; e.g. `PrefixXorPmull`, `DigitMacDotprod`, `BulkEmitPositions` (parameterized by lane width / hardware path). |
| `hardware_gate` | Exact `target_arch`, target feature, ACLE/Intel feature macro, and disasm expectation where relevant. |
| `scalar_reference` | bbnf-local scalar implementation or explicit scalar-delegate-non-ASM disposition. |
| `checkasm_or_parity_command` | Checkasm/parity command and seed set; microbench alone is insufficient. |
| `BBNF_SIMD_STRICT_status` | Strict feature-gated build status and fallback behavior. |
| `corpus_or_equality_oracle` | lightningcss/cssparser/sonic/Track 2 oracle, same-plane and strict. |
| `same_wave_consumer_path` | Generated/runtime caller path that consumes the primitive in production. |
| `expected_row_or_feature_gate` | CSS feature or JSON row expected to move; support-only rows are REVISE. |
| `loc_budget` / `risk_class` / `rollback_path` / `abrogate_threshold` | T-P3 planning envelope and fail-closed action. |
| `admissibility_state` | One of `source_backed`, `scalar_backed`, `checkasm_backed`, `micro_proven`, `production_wired`, `row_admitted`, `measured_rejected`, `architectural_block`. |
| `substrate_target` | One of `local_temp_only`, `existing_tape`, `direct_sink`, `admitted_fact_output`. |
| `retention_lifetime` | One of `local_loop`, `generated_function`, `output_row`; retained mask/class streams are blocked unless emitted as admitted row output. |
| `policy_owner` | One of `generated_grammar`, `caller_data`, `none`; shared primitive crates cannot own grammar policy. |

V6 fold note: the executable hardware ledger remains centralized in
`T-P2-V3-FOLD-ADDENDUM.md` plus the V4 delta at `T-P2-V4-FOLD-ADDENDUM.md`.
The V6 dossier adds `published_citation` and `abstract_primitive_name`
columns to every Lock 16 manifest row.

## Source-Present Primitive State

At close, every source-present SIMD/ASM primitive must be exactly one of:

```text
wired
deleted
scalar-delegate-non-ASM
architectural-block-with-REDRESS
```

`inventory_demoted_with_evidence` is historical REDRESS context only.

| source-present primitive | V6 state entering T-P3 | required disposition |
|---|---|---|
| `bitmap_prefix_xor_64` | scalar-delegate-non-ASM after REDRESS 88 | Keep scalar delegate or reopen only under PMULL material-differential row (C-P2C-2). |
| `bitmap_next_set_bit` | scalar-delegate-non-ASM after REDRESS 89 | Keep scalar delegate or reopen only under CSSC material-differential row (C-P2C-2). |
| `bulk_emit_positions_64` | scalar-delegate-non-ASM / source-present support | Wire with measured consumer or keep explicit non-ASM delegate; do not count as ASM admission. |
| `classify_tbl4` | production-wired | Continue; consumed by JSON NEON stripe at `scan.rs:200-275`. |
| `unescape_uxxxx` (single + x4) | source-present, parity-green | Wire to `parse_that_regex::unescape_string` / `read_hex_unit_scalar` consumer in same wave (C-P2C-4). |
| `digit_mac` | source-present conditional | Wire to generated number consumer (C-P2C-3) post-F-V2-P1ABC-RERECORD or leave non-admitting until architectural-block evidence exists. |
| `byte_context` | source-present conditional | Fold into C-P2C-5 64-byte oracle consumer or delete with REDRESS evidence. |
| `cache_hints` | source-present conditional | Wire with hot caller and measured placement or delete with REDRESS evidence. |
| `movemask` (SHRN/SRI) | production-wired | Continue; consumed in NEON stripe; documented per `SRC-LANGDALE-MOVMASK-NEON`. |
| x86 modules | background only | Keep out of SK-V13/V14 aarch64 close path; they cannot satisfy D5. |

## PMULL / CSSC / EOR3 / UDOT / TBL Material-Differential Gate

The labels `Union-C`, `SIMD-first`, `PMULL+CSSC`, `EOR3`, `BCAX`,
`UDOT`, or `TBL` are not shortlist-safe by themselves. A T-P3 route
must name:

1. prior REDRESS routes cited, including 88/89 and 96/97/98 for any
   PMULL/CSSC/union path;
2. the old scalar cost source that is deleted, bypassed, or intentionally
   retained;
3. the production consumer path and the row or CSS feature it moves;
4. `substrate_target`, `retention_lifetime`, `policy_owner`;
5. scalar reference, checkasm cell, strict feature gate, disasm expectation,
   and isolated microbench;
6. strict comparator/oracle, guard rows, rollback path, abort criteria;
7. **(V6 NEW)** `published_citation` + `abstract_primitive_name` (the V6
   fold-up requirement: every hardware route must trace its abstract
   primitive to a primary source).

Without those fields, the technique remains `source_backed` or
`micro_proven`, not S-P3-admissible.

## Architectural Assertions Defended

1. **Lock 16 must be a manifest, not a prose allowlist.** ACLE and local
   source show each primitive has a distinct hardware gate. The
   admissible unit is `{abstract primitive, published citation, feature
   gate, scalar reference, checkasm parity, same-wave consumer, measured
   row, substrate_target, retention_lifetime, policy_owner}`. A single
   "aarch64 SIMD supported" claim is too coarse.

2. **TBL is the current dispatchable classifier spine.** The local
   dispatcher selects `NeonTbl4` only when the low-6 alphabet table is
   admissible, and the classifier body uses `vqtbl4q_u8` over caller-
   provided tables. This is the grammar-neutral transfer pattern to
   preserve for CSS/Sheets/BBNF-self.

3. **PMULL/PCLMUL lineage is published and well-established.** The
   abstract primitive `PrefixXorPmull` is grounded by Lemire's 2016
   PCLMUL prefix-XOR blog + simdjson VLDB 2019 §3.3 + Intel CLMUL
   Whitepaper 2014. The published algebra `M ⊗ ALL_ONES` is the
   *primitive justification*; REDRESS 88's measured rejection of the
   consumer is the *binding limit*.

4. **CSSC CTZ is a real, native, GP-register CTZ on v8.9-A / v9.4-A.**
   ACLE `__ARM_FEATURE_CSSC` + ARM ARM §C7.2 confirm this. The SK-V7
   W10b probe verified emission with `+cssc`. The instruction is
   admissible; the REDRESS 89 consumer is not.

5. **W4's ASCII run-skip is a real candidate, but pre-production at
   SK-V14 V1.** It has caller-level scalar reference, parity, adversarial
   seeds, frozen CSS fixture coverage, and a 4.718x microbench. SK-V14
   V1 PRUNE-2 falsifies CSS L4 row movement; eligible only after S-P3
   PRUNE-2 successor wave.

6. **REDRESS 88/89 are route-specific, not category-wide.** PMULL and
   CSSC are real architecture features; the consumed prefix-XOR and CTZ
   bulk implementations regressed rows. C-P2C-2 can reopen the categories
   only by naming the material-differential fields above and measuring a
   row-moving consumer.

7. **x86 AVX-512 belongs in totality vocabulary but not in SK-V14
   closure.** Local x86 modules are useful for future cross-arch
   primitive vocabulary and checkasm process symmetry, but SK-V14's
   admission silicon is aarch64/M5 Max.

8. **(V6 NEW) BCAX outranks EOR3 for SK-V14 scanner algebra.** The
   scanner at `scan.rs:225-265` uses AND-NOT chains (`punctuation &
   !string_body`, `quotes & !escaped`), which match BCAX's `a XOR (b
   AND NOT c)` algebra directly. EOR3 collapses three XORs; BCAX
   collapses an AND-NOT and an XOR — a closer match to existing code.

9. **(V6 NEW) Cross-arch primitive vocabulary must be lane-parameterized,
   not instruction-specialized.** `PrefixXorPmull` is parameterized by
   lane width (NEON 128b PMULL, x86 256/512b VPCLMULQDQ);
   `DigitMacDotprod` is parameterized by lane width and digit-count
   (NEON 4-lane UDOT vs x86 16-lane VNNI VPDPBUSD). The abstract
   primitive is the lane-parameterized algebra; each ISA path is a
   width-specialized realization.

## Architectural Assertions Refuted

| assertion | refutation | consequence |
|---|---|---|
| "Instruction availability implies a primitive should land." | PMULL and CSSC implementations passed correctness/disasm checks but failed JSON row gates in REDRESS 88/89. | Every ISA row needs row-local measured consumption, not just a scalar/checkasm cell. |
| "`svmatch_u8` is a NEON primitive." | Arm documents `svmatch_u8` in the SVE2 MATCH path (ARM ARM §C2.2), not NEON; SK-V13/V14 is NEON-only on M1-M4 (M5 SVE2 availability requires verification). | Do not scope SVE2 MATCH as an SK-V13/V14 NEON/M5 Max route; the prompt phrase "NEON `svmatch_u8` port" is technically wrong. NEON port via `vqtbl1q_u8 + vceqq_u8 + OR-reduce` is the alternative but it is just the standard byte-set primitive. |
| "Final orphan count zero means the source tree has no future SIMD cleanup." | REDRESS 126 demotes five source-present primitives with evidence; scoping says several still delegate to scalar or have no caller. | SK-V14 D5 still needs wire-or-delete disposition if the active pin treats source-present support modules as orphans. |
| "PMULL prefix-XOR should replace scalar carry by default." | REDRESS 88 measured hard JSON regressions, including unicode and number rows, despite visible `pmull.1q`. | PMULL can appear only inside a named consumer shape that satisfies the V2 material-differential gate; `SIMD-first union C` is not sufficient by itself. |
| "Cache hints are harmless support code." | Local `cache_hints.rs` has `prfm`/`stnp` but no measured placement or caller. | Treat as support-only inventory; delete or wire with a hot caller. |
| **(V6 NEW)** "x86 AVX-512 GFNI/VBMI2 are cross-arch portable primitives." | GFNI (`VGF2P8AFFINEQB`) and VBMI2 `VPCOMPRESSB` have no aarch64 equivalent (`SRC-WIKICHIP-GFNI` + P2-C §4 PEXT pre-block). The "single-instruction byte-classifier" or "single-instruction bulk-emit" framing breaks the cross-arch primitive vocabulary if a generic bbnf primitive abstracts to it without a wider fallback. | Cross-arch primitive vocabulary must include an explicit `degrades_to` field: GFNI degrades to NEON TBL (lossy — GFNI is richer); VPCOMPRESSB degrades to CSSC CTZ loop on aarch64 (which REDRESS 89 already rejected). The asymmetry is the binding cross-arch gap. |

## Bbnf-Specific Transfer Notes

| primitive family | transfer condition | non-transfer condition |
|---|---|---|
| Byte-class set membership | Grammar supplies byte alphabet/table; checkasm covers collisions, duplicates, high-bit bytes, tails, source immutability; same-wave consumer is a generated scan-block. | Generic crate hardcodes JSON/CSS byte policy or the consumer is only a synthetic microbench. |
| Prefix/carry masks | Consumer is a string/escape/structural state machine whose row profile names carry propagation as hot; scalar reference handles cross-window carry. | Default replacement of scalar `prefix_xor_64` in the production dispatcher. |
| Next-bit extraction / compaction | Consumer writes positions or events in a way that avoids the REDRESS 89 bulk regression; row gate watches JSON guard floors and CSS parity. | `compact_mask` support primitive replacement without production row movement. |
| Digit dot product | Parser consumes fixed-width digit chunks and strict equality covers invalid/non-digit fallback; row gate targets number-heavy corpora. | Proof-only `parse_4_digits` helper with no generated number parser caller. |
| Ternary bitwise (EOR3) | Profile identifies 3-input XOR fan-in, and EOR3 replaces a measured XOR triple in string or digest code. | "Tap SHA3 surface" without a named XOR triple and hot-leaf consumer. |
| Ternary bitwise (BCAX, **V6 NEW**) | Profile identifies AND-NOT-XOR algebra `a XOR (b AND NOT c)`; BCAX replaces a measured chain in scanner mask algebra. | Speculative wiring to SHA3 surface without measuring the current AND-NOT-XOR cost. |
| LD4 interleaved classify | UTF-8 / SoA byte-stream classification consumer profiles 4-way de-interleave as hot. | Generic crate uses LD4 as decoration without a SoA byte-stream row. |
| SVE2 MATCH | Future M5-generation scalable-vector target with SVE2 feature gate and SVE checkasm. | SK-V13/V14 NEON/M1-M4 Max route. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| Does the M5 Max native target advertise CSSC to Rust without explicit `-C target-feature=+cssc`? | Capture `rustc --print cfg -C target-cpu=native` and disasm for any CSSC candidate before S-P3 wave-scoping. REDRESS 89 says SK-V7 native cfg did not advertise CSSC. |
| Which CSS generated scan-block function can consume `AsciiSetRunSkip64` without changing comment/string semantics? | Inspect CSS generated/template loops and build a caller-level scalar reference over the same fact stream before production wiring. Blocked at SK-V14 V1 by PRUNE-2. |
| Is C-P2C-2 PMULL+CSSC path actually a material differential from REDRESS 88/89, or just a composition of rejected bodies? | S-P3 must name the new consumer shape and microbench it in isolation before any source redress. |
| Do number-heavy rows still profile digit MAC as hot after F-V2-P1ABC-RERECORD? | Use fresh P1 TSVs only with `--features runtime/parse-attribution`; if digit parsing is not a hot leaf, keep UDOT inventory-only. |
| **(V6 NEW)** Does any SK-V14 scanner mask expression match BCAX `a XOR (b AND NOT c)` algebra? | Audit `runtime/src/grammars/json/scan.rs:225-265` mask algebra; if the existing `punctuation & !string_body` then XOR-fold is the hot chain, BCAX is a one-instruction replacement candidate post-F-V2-P1ABC-RERECORD. |
| **(V6 NEW)** Does the M5 Max generation introduce SVE2 / SME that enables `svmatch_u8` as a future track? | Verify via `sysctl hw.optional.arm.FEAT_SVE2` + `hw.optional.arm.FEAT_SME` on M5 Max hardware; gate any SVE2 candidate behind this check. |
| **(V6 NEW)** Should `Crc32CHash` be a candidate for `JsonDirectDigest` mix vs. current FxHash-style scalar? | Measure scalar mix throughput against `__crc32cd` per chunk on JSON direct rows; only candidate if measurement names mix as hot. Cross-arch (SSE 4.2 CRC32 on x86) makes it portable. |
| **(V6 NEW)** Are any x86 modules accidentally compiled or tested in the aarch64 close path? | `cargo metadata` and cfg audit should prove x86 modules are background only under SK-V13/V14. |

## LOCKS-AMENDMENTS-CANDIDATE

| Candidate | Type | Lock(s) | Proposed amendment candidate | Supporting evidence |
|---|---|---|---|---|
| LAC-2E-01 | addition | Lock 16 / Lock 1 | Add a hardware-gate manifest requirement: every intrinsic or `asm!` use maps to abstract primitive, ACLE/Intel source, target feature, scalar reference, checkasm test, same-wave consumer, measured row or deletion disposition, `substrate_target`, `retention_lifetime`, `policy_owner`. **(V6 fold-up:)** add `published_citation` and `abstract_primitive_name` as required manifest columns. | T-P1 1E marks Lock 16 traceability UNKNOWN; CH5 requires substrate and retention fields; local `digit_mac.rs`, `cache_hints.rs`, scalar delegates, and x86 modules show heterogeneous gates; V6 per-entry published-citation requirement. |
| LAC-2E-02 | refinement | Lock 16 / SK-V13 D5 | Define `inventory_demoted_with_evidence` as historical evidence, not a permanent zero-orphan source state. SK-V13/V14 close should require each source-present primitive to be exactly `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`. | SRC-SCOPE rows 16-23; REDRESS 126 final orphan wording; local bitmap/bulk files delegate to scalar. |
| LAC-2E-03 | refinement | Lock 16 / host-arch allowlist | State that `svmatch_u8` is SVE2-only and cannot satisfy a NEON/M1-M4 Max primitive gate. M5 SVE2/SME availability is gated on hardware-verify. | Arm SVE2 MATCH source (ARM ARM §C2.2) and absence of local SVE code; M5 generation introduces SME but SVE2 requires verification. |
| LAC-2E-04 | refinement | Lock 1 / Lock 16 | For PMULL/CSSC reopen attempts (C-P2C-2 Union-C), require material-differential text that distinguishes new union/consumer shape from REDRESS 88/89 and REDRESS 96/97/98, plus a micro-prove-first artifact before S-P3 wave-scoping. V6 fold-up (V2-refreshed): require explicit citation of Intel CLMUL Whitepaper 323640 + Langdale + Lemire VLDBJ 2019 §3.3 lineage as the abstract-primitive justification, distinct from the consumer-specific REDRESS rejection. (V2 fold withdraws the V6 reference to a Lemire 2016 PCLMUL prefix-XOR blog URL: 404 at V2 fold time, no recoverable slug.) | REDRESS 88/89 body-fill failures; REDRESS 96/97/98 union failures; SK-V13/V14 scoping Union-C risk table; Intel CLMUL Whitepaper 323640 + simdjson VLDBJ 2019 lineage. |
