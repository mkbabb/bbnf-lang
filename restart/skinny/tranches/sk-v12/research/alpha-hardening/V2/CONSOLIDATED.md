# SK-V12 Pass Alpha Hardening V2 - Consolidated

Date: 2026-05-20.
Cycle: V2 under `USER-PIN-W1-CSS-L4-SOTA.md`.

## Disposition

REVISE.

V2 folded the V1 contract defects around strict lightningcss admission, pass
order, CSS output-plane equality, JSON guard refresh at the global level,
zero-carried-orphan close, W1a/W1b split, wave caps, generated LOC ceilings,
and rollback paths. Five lenses accept V2. CH5 still finds hidden coupling in
Alpha-E that must be folded before G-Alpha can present the packet.

## Lens Results

| Lens | Disposition | V2 finding |
|---|---|---|
| CH1 correctness | PASS | Strict `generated_track1_mbps > lightningcss_mbps + 1`, same-plane CSS fact stream, pass order, citations, and rollback facts are folded. |
| CH2 generality / Lock 14 | PASS | CSS L4 is executable and first, Sheets/BBNF-self are fallback-only, `json_templates/generated.rs` is constrained, and public substrate expansion stays blocked. |
| CH3 regression / REDRESS | PASS | JSON guard rule, zero carried orphan, REDRESS adjacency, rejected patch paths, W0 revalidation, and SIMD/ASM preconditions are folded. |
| CH4 cost / tranche budget | PASS | W1a/W1b split, 20/15/30 caps, hand LOC caps, generated LOC ceilings, O(N) discipline, and failure actions are plausible. |
| CH5 hidden coupling | REVISE | Alpha-E E2 lacks the local JSON guard refresh rule despite touching generic/runtime/codegen paths, and Alpha-E still lists pre-pin SPEC/S-P artifacts without the required qualifier. |
| CH6 anti-paper-close | PASS | V2 prevents S-P3 jump, requires CSS redress before fallback, keeps strict lightningcss admission, consumes telemetry, and gives G-Alpha a sufficient intervention table. |

## Required V3 Folds

1. Add the full JSON guard refresh/demotion rule to Alpha-E E2/W1a. Because E2
   owns generic runtime, codegen, generated-output, benchmark/report, and
   Lock 14 paths, JSON generated-output parity is not enough: W1a must refresh
   direct/typed JSON guards or record measured REDRESS demotion unless no
   JSON-producing path moved and `skinny/RESULTS.md` is proven unchanged.
2. Qualify Alpha-E's authority list. `SPEC.md` and the pre-pin
   S-P1/S-P2/S-P3 converged artifacts are context only where they do not
   conflict with the user pin and only after measured revalidation. They must
   not re-import stale selected-baseline ordering, preflight fallback, or
   `ceil(baseline_mbps * 1.01)` threshold authority.

## Stable V2 Folds To Preserve

- CSS L4 remains authoritative; Sheets and BBNF-self are fallback-only after a
  measured CSS L4 redress attempt.
- CSS admission is strict `generated_track1_mbps > lightningcss_mbps + 1`;
  equality at `+1` is a miss.
- The output plane is one canonical CSS fact stream shared symmetrically by
  generated Track 1, independent Track 2/oracle, and lightningcss.
- G-Alpha precedes pin-aware S-P1, S-P2, and S-P3; pre-pin pass artifacts are
  not substitute convergence.
- ADMIT and FIXPOINT both require the carried aarch64 orphan set to be admitted,
  removed, or inventory-demoted with evidence.
- W1a legalizes `GrammarConfig`; W1b creates the CSS row and lightningcss
  comparator. They are not one over-budget redress slice.

## Next Step

Fold the two CH5 defects into Alpha-E, commit the V2 challenge record, then run
V3 six-lens hardening.
