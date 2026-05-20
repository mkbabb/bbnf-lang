# SK-V12 Pass Alpha Hardening V3 - Consolidated

Date: 2026-05-20.
Cycle: V3 under `USER-PIN-W1-CSS-L4-SOTA.md`.

## Disposition

REVISE.

V3 validates the V2 hidden-coupling folds in Alpha-E: W1a now locally inherits
the JSON direct/typed guard refresh or measured-demotion rule, and Alpha-E's
pre-pin SPEC/S-P authorities are qualified as context only under the user pin.
Five lenses pass. CH6 finds one remaining anti-paper-close blocker outside the
Alpha packet: the standalone `research/g-alpha/G-ALPHA-SK-V12.md` still presents
the stale pre-pin/V2 convergence surface.

## Lens Results

| Lens | Disposition | V3 finding |
|---|---|---|
| CH1 correctness | PASS | Strict lightningcss gate, CSS plane/equality, pass order, close conditions, citations, rollback, and V2 CH5 folds are correct. |
| CH2 generality / Lock 14 | PASS | CSS L4 generality is executable and first; Sheets/BBNF-self are fallback-only; generic JSON policy and public substrate expansion remain fenced. |
| CH3 regression / REDRESS | PASS | W1a local JSON guard refresh, zero carried orphan, REDRESS adjacency, rejected-patch paths, W0 revalidation, and SIMD/ASM preconditions pass. |
| CH4 cost / tranche budget | PASS | The V2 folds do not change the accepted W1a/W1b split, caps, generated LOC ceilings, O(N) discipline, or rollback actions. |
| CH5 hidden coupling | PASS | The prior W1a guard shortcut and stale Alpha-E authority coupling are resolved; no new plane, normalization, substrate, or orphan loophole remains. |
| CH6 anti-paper-close | REVISE | `research/g-alpha/G-ALPHA-SK-V12.md` still claims V2 convergence, treats CSS/Sheets/BBNF-self as selectable peers, omits the strict lightningcss gate/table, and keeps union routes category-blocked. |

## Required V4 Fold

Replace `restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md`
with a pin-aware presentation that:

1. Removes the stale V2 convergence claim and does not present `G-Alpha PASS`
   until V4 hardening converges.
2. Names CSS L4 as the authoritative first target and states Sheets/BBNF-self
   are fallbacks only after a measured CSS L4 redress attempt.
3. Presents the strict admission floor:
   `generated_track1_mbps > lightningcss_mbps + 1`, with same corpus, same
   output plane, same host, and strict equality.
4. Carries the S-P1/S-P2/S-P3 plus W0-W5 intervention table from the current
   `SYNTHESIS.md` / Alpha-F packet.
5. Carries the telemetry schema/provenance requirements and zero-carried-orphan
   close rule.
6. Treats union and ASM-gen categories as unblocked at category level under the
   user pin, while preserving REDRESS 96/97/98 and 88/89/90 as historical
   implementation evidence requiring material differential and CHALLENGE.

## Stable V3 Folds To Preserve

- Alpha-E's W1a gate requires JSON direct/typed guard refresh or measured
  demotion when generic/runtime/codegen/generated-output/report paths move.
- Alpha-E's pre-pin SPEC and S-P artifacts are context only where revalidated
  and non-conflicting with the user pin.
- All V2 strict-gate, CSS fact-stream, pass-order, zero-orphan, rollback, and
  W1a/W1b split constraints remain accepted.

## Next Step

Commit this V3 hardening record, fold the G-Alpha presentation, then run V4
six-lens hardening.
