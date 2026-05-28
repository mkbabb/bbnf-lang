# CH3 Regression — SK-V15 Alpha V1

Date: 2026-05-27.

## Verdict

REVISE, folded.

The packet preserved the honest SK-V14 JSON wins, but it needed stronger
deletion-before-rebuild controls before wave execution.

## Folded Fixes

- `alpha-C` now includes REDRESS-213 for the W6.0 destructive CSS L4 root
  runtime regeneration gap.
- `SYNTHESIS.md` now requires S-P3 to emit an artefact dependency table:
  retired/deleted artefact, delete/retire wave, rebuild provider wave, proof
  command, and evidence that the provider lands no later than the delete or
  retire wave.
- CSS parser retirement is now coupled to typed CSS value proof or blocked.
- `HANDOFF.md` now qualifies PRUNE-then-REBUILD order with the NEW-CH3
  dependency rule.

## Residual Risk

The detailed dependency table is an S-P3 output and remains a pass input for
the next phase.
