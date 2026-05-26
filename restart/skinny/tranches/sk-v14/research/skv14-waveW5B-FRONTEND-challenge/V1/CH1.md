# SK-V14 W5B-FRONTEND CHALLENGE V1 CH1 Correctness

Date: 2026-05-26.
Lens: CH1 Correctness.
Disposition: REVISE.

## Findings

The W5B-FRONTEND plan is directionally aligned with SPEC Section 8B, but it is
not executable as written.

1. Owner paths do not match SPEC. `DISPATCH-PROMPT.md:64` requires plan owner
   paths to match the wave SPEC or return REVISE. `SPEC.md:714-720` includes
   `skinny/RESULTS.md` and conditional `skinny/REDRESS.md`; the plan omits
   `skinny/RESULTS.md` and adds the redress report artefact at
   `skv14-W5B-FRONTEND-plan.md:20-28`.
2. Gate commands mix working directories. The plan changes to
   `/Users/mkbabb/Programming/bbnf-lang/skinny` at
   `skv14-W5B-FRONTEND-plan.md:69-71`, then uses repo-root-relative
   `skinny/...` paths at `skv14-W5B-FRONTEND-plan.md:94-98` and root paths at
   `skv14-W5B-FRONTEND-plan.md:99`.
3. SPEC requires full-table maintain within +/-1.0% at `SPEC.md:750`, but the
   plan only proves tests, existing-results capture, and byte-identical ledger
   files at `skv14-W5B-FRONTEND-plan.md:80` and
   `skv14-W5B-FRONTEND-plan.md:98-105`. The current
   `--skv14-existing-results-capture` gate renders current `RESULTS.md`; it is
   not a fresh full-table measurement.
4. The lowering target is under-defined. The plan requires a request-scoped
   frontend IR/facts artefact and canonical lowering at
   `skv14-W5B-FRONTEND-plan.md:49-52`, while A1 records that current
   `ir::ExprKind` has no import graph, layout, projection, host capture, or
   fence node at `skv14-W5B-FRONTEND-A1-frontend-construct-gap.md:27-29`.

## Required Fold

- Reconcile owner paths with SPEC Section 8B or amend SPEC before accepting the
  plan; keep the redress report artefact distinct from source owner paths.
- Rewrite gates with explicit `git -C /Users/mkbabb/Programming/bbnf-lang` and
  `cargo --manifest-path /Users/mkbabb/Programming/bbnf-lang/skinny/Cargo.toml`
  style commands.
- Add a fresh measurable full-table maintain gate or route a SPEC amendment
  removing that requirement.
- Add a construct-by-construct lowering table naming target representation,
  owner path/type, and exact fail-closed tests for imports, `@ws`, `@pretty`,
  `?w`, `>>`, `<<`, span capture, and typed projections.

## Sources

- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md:64`
- `restart/skinny/tranches/sk-v14/SPEC.md:714`
- `restart/skinny/tranches/sk-v14/SPEC.md:750`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:20`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:49`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-plan.md:69`
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-A1-frontend-construct-gap.md:27`
