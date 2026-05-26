# SK-V14 W5B-FRONTEND CHALLENGE V1 CH6 Anti-Paper-Close

Date: 2026-05-26.
Lens: CH6 Anti-Paper-Close.
Disposition: REVISE.

## Findings

The plan has real same-wave consumer and revert sections, but three pieces of
the claim remain paper-only.

1. Owner-path mismatch creates a paper-close risk. `SPEC.md:719` includes
   `skinny/RESULTS.md` for W5B-FRONTEND row attribution, while the plan omits it
   from owner paths and instead requires byte-identical `RESULTS.md` /
   `ROLLING-SOTA-DELTA.md` at `skv14-W5B-FRONTEND-plan.md:98` and
   `skv14-W5B-FRONTEND-plan.md:104`. `DISPATCH-PROMPT.md:64` requires owner
   paths to match SPEC or return REVISE.
2. Full-table maintain is not evidenced before admit. `SPEC.md:750` requires
   +/-1.0% on all rows, but the plan's gate list only proves source/test checks
   plus no result-file diff at `skv14-W5B-FRONTEND-plan.md:69`. Byte-identical
   result files are not live performance evidence.
3. Import fail-closed coverage is incomplete. The plan requires missing imports
   and cycles to fail closed at `skv14-W5B-FRONTEND-plan.md:49`, but only names
   a positive import-resolution test at `skv14-W5B-FRONTEND-plan.md:73`.

## Required Fold

- Align W5B owner paths with SPEC or amend SPEC before accept.
- Add executable full-table maintain evidence to the W5B redress admit path.
- Add exact negative tests for missing import and import-cycle fail-closed
  behavior.

## Accepted Checks

- Same-wave consumers are named at `skv14-W5B-FRONTEND-plan.md:130`.
- Revert protocol exists at `skv14-W5B-FRONTEND-plan.md:117`.
- Not-yet-present W5B artefacts are treated in future tense rather than as
  already complete.

## Sources

- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:64`
- `restart/skinny/tranches/sk-v14/SPEC.md:719`
- `restart/skinny/tranches/sk-v14/SPEC.md:750`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:49`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:69`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:73`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:98`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:117`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:130`
