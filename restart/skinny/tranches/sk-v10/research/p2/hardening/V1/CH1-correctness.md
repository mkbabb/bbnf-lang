# SK-V10 S-P2 V1 CH1 Correctness

Pass: S-P2 Research CHALLENGE. Cycle: V1.
Date: 2026-05-19.
Scope: correctness lens over P2-A through P2-F.
Output: this file.
Verdict: REVISE.

## Basis

Read: `restart/skinny/tranches/sk-v10/research/p2/*.md`,
`restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`,
`restart/skinny/tranches/sk-v10/HANDOFF.md`, and
`restart/skinny/tranches/sk-v10/SYNTHESIS.md`.

The accepted S-P1 antecedent set is the P1-E product-plane map:
`string_tiny_scan`, `string_full_scan`, `string_escape`,
`unicode_escape_hex`, `number_digit_scan`, `number_scan`, `whitespace_skip`,
`array_walk`, `object_walk`, `direct_struct`, `simd_movemask`, `alloc`, and
`memcpy`
(`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:29-32`,
`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:38-47`).
P1 hardening accepted S-P1 for S-P2 and preserved the direct/typed PMU absence
as instrumentation-only fact
(`restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:44-48`).

External spot checks used primary source URLs for asmjson, sonic-rs, simdjson,
yyjson, FFmpeg checkasm, dav1d checkasm, and the dav1d checkasm migration MR.

## Findings

### F1 - Some candidates are not traced to accepted S-P1 hot leaves

Disposition: REVISE.

Most candidates trace cleanly to accepted P1 leaves. P2-A and P2-E are clean on
this axis. P2-B, P2-C, P2-D, and P2-F each contain at least one item that is
either gate-only/contract-only or keyed to a non-accepted observation but still
appears in the candidate/primitive pool.

Required fixes:

- In P2-C, demote `CSSC_CTZ_NEXT_SET_BIT` from candidate primitive to
  REDRESS-blocked ISA inventory unless it is re-anchored solely to an accepted
  hot leaf such as `simd_movemask` and a named product-plane consumer.
  `unicode_basic` `trailing_zeros` is visible in P1-E, but it is not an accepted
  hot-leaf class (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:67`).
- In P2-B, demote `mask_next_and_emit_positions_64` or rewrite its antecedent.
  Its current "trailing-zero visibility" basis has the same defect. It may be
  retained only as rejected/non-shortlist inventory unless traced to accepted
  `simd_movemask` plus a live direct/typed consumer.
- In P2-D, move C1 `Tape Capacity/Flag Economy Contract` out of the
  candidate-primitive pool or label it explicitly as a Lock 1 invariant /
  contract constraint. It is grounded in materialization ratios, not a named P1
  hot leaf, so it cannot be eligible for S-P3 primitive shortlisting without a
  fresh profile leaf.
- In P2-F, label `Comparator / telemetry refresh` as gate-only evidence schema,
  not a behavior/primitive candidate. Alpha-E supports it as gate-only work, but
  it has no P1 hot-leaf antecedent and must not be included in the behavior
  shortlist.

### F2 - P1 antecedent citations need correction or completion

Disposition: REVISE.

The substantive P1 posture is correct: direct is the primary JSON frontier,
typed product generalization is bounded, parse-only stays diagnostic, and W3 is
retired. Several local citations are nevertheless incomplete or mis-anchored.

Required fixes:

- P2-C finding 1 cites `p1e-hot-leaf-attribution.md:1` for the host, but the
  host triple is on line 10. Replace with
  `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:10`
  and keep the class-map citation at lines 40-47.
- P2-B finding 3 claims routes for `update_center`, `canada`, `mesh`, and
  `numbers` but omits their row anchors. Add P1-E lines 55, 58, 59, and 64
  alongside the existing lines 53, 57, 61, 63, 65, and 66.
- P2-B `whitespace_skip_mask_64` names `mesh` but cites no row anchor for
  `mesh` whitespace visibility. Cite either the P1-E class-map line 44 or the
  P1-B direct profile row at
  `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md:85`.
- P2-D C3 claims container-walk antecedents for `canada`, `mesh`,
  `marine_ik`, `citm_catalog`, and `instruments` but cites only P1-E lines 45
  and 60. Add row-specific P1-B anchors:
  `p1b-samply-mode-2.md:80-81`, `:85`, `:88`, and `:89`.

### F3 - External comparator claims are mostly faithful, but ISA/process citations are not precise enough

Disposition: REVISE.

P2-A's comparator teardown is source-faithful in the checked sample:
asmjson's SAX/DOM entry points, raw escaped-string events, and permissive
control-byte note are supported by its pinned source; sonic-rs UTF-8,
unchecked, raw-number, and borrowed-string claims are supported by its pinned
source; simdjson stage-1/tape/On-Demand claims are supported by its pinned
source; and yyjson strict/permissive flag and memory-model claims are supported
by its pinned source.

P2-B's FFmpeg/dav1d process claims are directionally supported, but its prose
uses broad references such as "master raw source" and "MR !1812" without stable
line/source-position anchors. P2-C's ISA inventory cites broad Arm and Intel
landing pages rather than instruction-specific manual/intrinsics entries for
the instruction claims.

Required fixes:

- In P2-B, pin dav1d `tests/checkasm/checkasm.c` to a commit or release URL and
  cite the exact lines for the DSP test matrix, CPU-flag matrix, and
  `checkasm_main` call. Do not rely on `master`.
- In P2-B, cite the dav1d MR rationale with a stable URL and source-position
  evidence for runtime scaling, outlier rejection, variance reporting, and
  maintainability. The current claim is supported by the MR description, but the
  document needs a citable anchor.
- In P2-C, replace broad ISA source bullets with instruction-specific citations
  for AArch64 `TBL`/`TBX`, `UDOT`, `PMULL`/`PMULL2`, FEAT_CSSC `CTZ`, and the
  x86 `VBMI2`, `GFNI`, `VPCLMULQDQ`, `VNNI`, `IFMA`, and `BITALG` families.
  Landing pages can remain in sources, but CH1 needs the instruction claims tied
  to the actual reference entries.

## Per-Artefact Disposition

| Artefact | CH1 disposition | Notes |
|---|---|---|
| P2-A SOTA comparator teardown | ACCEPT | Comparator strictness and candidate antecedents are correct. |
| P2-B DAV1D/FFmpeg ASM process | REVISE | Fix incomplete P1 anchors, demote or re-anchor `mask_next_and_emit_positions_64`, and pin external process citations. |
| P2-C host-arch ASM/SIMD esoterica | REVISE | Fix host-line citation, instruction-specific ISA citations, and CTZ/trailing-zeros antecedent handling. |
| P2-D substrate + tape design | REVISE | Reclassify C1 as invariant/contract-only and repair C3 row anchors. |
| P2-E parse-that primitive gaps | ACCEPT | Candidates trace to accepted hot leaves and preserve P1/P2 boundaries. |
| P2-F grammar-neutral abstraction | REVISE | Mark telemetry refresh as gate-only/non-primitive; do not count it as hot-leaf-traced behavior work. |

## Close Condition For CH1 ACCEPT

CH1 can accept after the next fold if:

1. Every behavior or primitive candidate either names an accepted S-P1 hot leaf
   or is explicitly classified as gate-only/contract-only/non-shortlist.
2. All P1 row claims use complete local file:line anchors.
3. Comparator and process claims cite pinned primary sources; ISA claims cite
   instruction-specific manual or intrinsics entries.

No source-code edits are authorized or required by this lens.
