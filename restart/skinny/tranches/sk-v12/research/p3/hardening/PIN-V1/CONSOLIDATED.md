# SK-V12 S-P3 PIN-V1 Challenge Consolidated

Pass: S-P3 Synthesis-Plan.
Cycle: PIN-V1.
Date: 2026-05-20.
Packet under review: commit `fa312b55`
(`docs(sk-v12-p3): draft pin-aware CSS synthesis plan`).

## Verdict

PIN-V1 does not converge. Five of six lenses return REVISE.

| Lens | Verdict | Confidence | Load-bearing finding |
|---|---|---:|---|
| CH1 correctness | REVISE | 92% | Exact CSS row/output plane and W2 topology drift across packet files. |
| CH2 generality / Lock 14 | REVISE | 94% | P3-B routes Sheets/BBNF fallback after W2 instead of W1b CSS redress. |
| CH3 regression / REDRESS | REVISE | 88% | W2 entry drift, W3/W4 local ADMIT wording, and FIXPOINT-credit gates need splitting. |
| CH4 cost / scope | REVISE | 84% | W1b and W4 are over-scoped for 30-minute redress without split/proof. |
| CH5 hidden coupling | REVISE | 94% | W1b can imply SIMD before W2, owner paths drift, and P3-D has a public-API escape hatch. |
| CH6 anti-paper-close | ACCEPT | 96% | No blocking paper-close issue; optional W3/W4 label cleanup. |

## Required PIN-V2 Fold Set

1. Bind the exact CSS admission row and output plane across P3-A, P3-C, P3-F,
   SPEC, and DISPATCH. The intended row is
   `css_l4/declaration_values/direct_to_struct/main`.
2. Normalize W2 topology. W2 is the `escape_mask_64` correctness prerequisite;
   either make it consistently after W1a and before any SIMD-backed W1b/W3/W4
   path, or consistently after W1b. PIN-V1 currently mixes both.
3. Fix P3-B wave-label drift: fallback follows W1b measured CSS redress, W1a is
   GrammarConfig legality, W1b is CSS emission, and W3 sidecar/union violations
   falsify W3.
4. Reserve ADMIT for CSS L4 Track 1 strictly greater than
   `lightningcss_mbps + 1`. Rename W3/W4 local primitive/guard success paths as
   behavior PASS or FIXPOINT-credit evidence unless they satisfy the CSS close
   bar.
5. Split or pre-prove W1b's CSS oracle/comparator/gate scaffold. If kept in one
   wave, the packet must name existing exact paths and commands; otherwise
   sub-wave the CSS generated baseline from comparator/gate integration.
6. Narrow W4 so one ASM-gen attempt does not hide five unrelated orphan source
   changes. Non-selected orphans may be inventory-demoted only with exact proof;
   source-consuming/removal work requires a separate wave or blocks close.
7. Canonicalize CSS runtime, fixture, bench, report, and gate owner paths across
   the packet.
8. Remove P3-D's public-substrate-API exception language. USER PIN D3/D4 reopen
   union and ASM-gen categories, not public substrate/API surfaces.

PIN-V2 must be challenged again. No SK-V12 wave dispatch authority exists until
two consecutive clean S-P3 challenge cycles converge under the user pin.
