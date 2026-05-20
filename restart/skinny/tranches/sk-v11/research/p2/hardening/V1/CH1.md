# SK-V11 S-P2 CH1 Correctness
Pass: S-P2 CHALLENGE. Cycle: V1.
Date: 2026-05-19.
Scope: correctness review of P2 candidate antecedents, citations, and strictness planes.
Output: this file.
Disposition: REVISE.
Accept rate contribution: 0.

## Findings
1. Major - P2-F admits a candidate on a non-S-P1 antecedent. CH1 requires every
   candidate primitive to trace to a named S-P1 hot leaf, and rejects candidates
   with no P1 antecedent (`restart/prompts/skinny/PASS-2-RESEARCH.md:95`).
   The converged S-P1 authority names exactly eight accepted research
   antecedents: `bounded_plain_string_scan`, `string_escape_decode`,
   `unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`,
   `container_dispatch`, `simd_movemask`, and `output_digest_hash`
   (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`).
   It separately says lazy-tape facts are diagnostic planning evidence only and
   do not admit rows
   (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:46`).
   P2-F adds `lazy-tape/direct consumer shape` to its antecedent header
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:7`)
   and then uses that non-leaf as C9's P1 antecedent
   (`restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md:40`).
   C9 must be REJECTED as a candidate primitive as written, or moved to a Lock 1
   constraint/benchmark-accounting section that is not counted as a hot-leaf
   primitive.

2. Major - P2-C over-traces cache hints to `output_digest_hash`. P2-C correctly
   observes that `cache_hints.rs` contains `PRFM`/`STNP` inventory and that Lock
   16 allows cache hints (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:22`).
   But its candidate turns that inventory into "cache placement, prefetch, or
   streaming store hints around output digest/hash or output-plane writeback"
   while citing `output_digest_hash` as the P1 antecedent
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:37`).
   S-P1's `output_digest_hash` leaf is hash/fold output work, not cache-placement
   or tape/writeback traffic; PMU/cycles and lazy-tape facts remain diagnostic
   only (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:46`,
   `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:113`).
   Fold by splitting this into: accepted scalar `output_digest_hash` research if
   tied to the current digest caller, and Lock-16 ISA inventory for PRFM/STNP
   until a fresh behavior wave proves output-digest/writeback memory traffic is
   the row-moving hot leaf.

3. Minor - Several external process/ISA citations are directionally right but
   not strict enough for CH1 source hygiene. P2-B cites the VideoLAN checkasm
   project page and FFmpeg Doxygen trunk for process claims
   (`restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:366`,
   `restart/skinny/tranches/sk-v11/research/p2/p2b-dav1d-process.md:368`),
   which are moving web surfaces rather than pinned source positions. P2-C's
   `CTZ`, `PRFM`, and `STNP` ISA claims cite non-Arm mirror/reference pages
   (`restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:78`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:79`,
   `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:80`).
   V2 should pin checkasm/FFmpeg claims to stable source commits or releases and
   cite official Arm ACLE/Arm ARM entries, or explicitly label any mirrors as
   mirrors of a named architecture revision.

4. Minor - P2-A's yyjson "no explicit SIMD" statement is not directly sourced.
   The strict/default and memory-model yyjson claims are source-backed
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:288`,
   `restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:290`), but
   the comparator row also claims "no explicit SIMD"
   (`restart/skinny/tranches/sk-v11/research/p2/p2a-sota-teardown.md:30`).
   Either cite a yyjson source/build authority for that architecture claim or
   soften it to the narrower sourced claim that the reviewed yyjson paths are
   portable C/FSM/unrolled-loop comparators.

## Required folds
1. In P2-F, remove `lazy-tape/direct consumer shape` from the P1 hot-leaf list
   and reclassify C9 as Lock 1 / output-plane accounting, not a candidate
   primitive. If C9 remains in a candidate table, it must be re-anchored to one
   of the eight accepted S-P1 hot leaves and must name a direct or typed
   consumer; otherwise REJECT it.
2. In P2-C, split scalar `output_digest_hash` work from cache-hint inventory.
   PRFM/STNP may stay as ISA inventory, but not as a hot-leaf-traced candidate
   without fresh behavior evidence.
3. Pin or replace the moving/non-official external process and ISA citations
   identified above.
4. Cite or soften P2-A's yyjson "no explicit SIMD" architecture claim.

## Accepted facts
None. V1 does not contribute S-P3 accepted facts until the required folds land.
