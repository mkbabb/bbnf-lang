# SK-V14 W5B-FRONTEND CHALLENGE V1 CH3 Regression

Date: 2026-05-26.
Lens: CH3 Regression.
Disposition: REVISE.

## Findings

The plan does not reopen REDRESS-209, REDRESS-210, or REDRESS-211, but it lacks
a live regression gate for SPEC's full-table maintain clause.

1. Missing full-table maintain proof. `SPEC.md:750` requires W5B-FRONTEND
   full-table maintain within +/-1.0% on all rows. `ORCHESTRATOR.md:85` makes
   silent row regression a CH3 blocker. The plan gates tests, regen, topology,
   and no ledger movement at `skv14-W5B-FRONTEND-plan.md:67` and
   `skv14-W5B-FRONTEND-plan.md:91`, but names no fresh full-table measurement.
   Byte-identical `RESULTS.md` / `ROLLING-SOTA-DELTA.md` proves no ledger
   movement, not no runtime regression.
2. REDRESS-209/210/211 are not reopened. The plan preserves provider-backed
   bytes and leaves provider-free generation/deletion downstream at
   `skv14-W5B-FRONTEND-plan.md:12`, `skv14-W5B-FRONTEND-plan.md:58`, and
   `skv14-W5B-FRONTEND-plan.md:144`.
3. NEW-CH3-V4/V6/V7 ordering holds. W5B is frontend/import/IR only, W5C owns the
   provider-free body, and W5D owns deletion per `SPEC.md:773` and
   `SPEC.md:838`.

## Required Fold

- Add an explicit W5B full-table maintain command/report against
  `SK-V14-open` within +/-1.0%, or cite the existing gate that enforces it.
- Route any maintain failure through the W5B revert/REDRESS protocol.

## Sources

- `restart/prompts/ORCHESTRATOR.md:85`
- `restart/skinny/tranches/sk-v14/SPEC.md:750`
- `restart/skinny/tranches/sk-v14/SPEC.md:773`
- `restart/skinny/tranches/sk-v14/SPEC.md:838`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:12`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:67`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:91`
