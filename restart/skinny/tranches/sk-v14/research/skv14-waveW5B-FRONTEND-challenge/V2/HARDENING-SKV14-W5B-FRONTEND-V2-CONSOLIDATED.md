# SK-V14 W5B-FRONTEND CHALLENGE V2 Consolidated

Date: 2026-05-26.
Wave: W5B-FRONTEND.
Cycle: V2.
Disposition: REVISE.
Acceptance: 2/7 lenses ACCEPT; five orphan REVISEs remain.

## Lens Results

| Lens | Disposition | Required folds |
|---|---:|---|
| CH1 Correctness | REVISE | Lowering-table owner file/type, exact fail-closed tests, exact Lock 14 tests, per-log nonzero proof, maintain/SPEC resolution |
| CH2 Generality | REVISE | Lock 14 leak census over all W5B generic owner paths; public-retirement tests for the full compatibility set |
| CH3 Regression | ACCEPT | NONE |
| CH4 Cost | REVISE | Aggregate W5B budget or SPEC formal sub-waves; narrower Lock14 slice; same-commit consumer wording |
| CH5 Hidden Coupling | REVISE | All-template guards; Lock14-only first checkpoint |
| CH6 Anti-Paper-Close | REVISE | Fresh full-table maintain evidence or SPEC amendment; per-test/per-log nonzero proof |
| CH7 Overfit-Prune | ACCEPT | NONE |

## Consolidated Finding

V2 does not converge. The folded plan improves V1 materially: owner paths are
reconciled, gates are cwd-explicit, missing-import and cycle tests are named,
provider reachability is visible, and the overfit-prune hazards remain blocked.
The open defects are still load-bearing.

The main unresolved issue is structural. V2 attempted to make W5B-FRONTEND
fit the 30-minute hard cap by introducing four W5B-internal sub-slices, but
SPEC still gives W5B one wave cap. Keeping this shape requires a SPEC-level
wave-graph amendment, likely through Pass Omega, or the plan must be narrowed
to a single cap-valid redress that does not claim full W5B closure. Because
W5C-GEN is gated on W5B-FRONTEND close, narrowing without formal sub-waves risks
turning the remaining frontend closure into a deferral. The honest governance
route is to treat this as a W5BR wave-graph correction candidate.

## Required Folds

1. Resolve the cap contradiction: either amend SPEC to formalize W5B-FRONTEND
   sub-waves with an aggregate cap, or narrow the plan to one 30-minute W5B
   redress and keep W5B open until the full closure is complete.
2. Split the Lock14 work into a hard Lock14-only checkpoint before any
   grammar/codegen/xtask frontend edits.
3. Broaden provider/template guards to all `_templates` paths, not only CSS
   template directories, and add tests for modified provider/template files.
4. Add a Lock 14 leak census over all W5B generic owner paths.
5. Add public-retirement tests for all compatibility constructs, not only
   `@ws`.
6. Add owner file/type to the lowering table and replace prose fail-closed cells
   with exact test names.
7. Replace wildcard nonzero proof with per-test/per-log assertions.
8. Resolve full-table maintain honestly: run fresh `SK-V14-open` maintain
   evidence, or route a SPEC amendment if W5B's non-admit maintain should be
   exact no-diff instead of +/-1.0%.
9. Include redress report and reject-only `skinny/REDRESS.md` edits in LOC
   accounting when touched.
10. Require same-commit consumer evidence; no sub-slice can land as accepted
    until final same-commit gates are present.

## No-Fold Items

- CH3 accepts REDRESS-209/210/211 ordering and no W5C/W5D borrowing.
- CH7 accepts no P-1..P-7 recurrence.
- Non-JSON proof carry remains named and useful.

## Next Action

Author a W5BR corrective packet. The packet should classify the V2 failure as a
SPEC-cap / wave-graph conflict, propose formal W5B-FRONTEND sub-waves or a
single-wave narrowing alternative, and route the change through the required
governance before any W5B source redress.
