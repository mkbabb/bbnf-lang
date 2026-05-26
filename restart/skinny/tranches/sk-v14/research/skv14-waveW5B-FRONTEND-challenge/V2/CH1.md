# SK-V14 W5B-FRONTEND CHALLENGE V2 CH1 Correctness

Date: 2026-05-26.
Lens: CH1 Correctness.
Disposition: REVISE.
HEAD checked: `f31e827a2`.

## Findings

1. The lowering table exists, but it omits the V1-required owner file/type
   column. `skv14-W5B-FRONTEND-plan.md:89` starts the table, while V1
   consolidated required target representation, owner file/type, positive test,
   and fail-closed test at
   `skv14-waveW5B-FRONTEND-challenge/V1/HARDENING-SKV14-W5B-FRONTEND-V1-CONSOLIDATED.md:45`.
   Several fail-closed cells remain prose rather than exact gate names at
   `skv14-W5B-FRONTEND-plan.md:93`-`98`.
2. The Lock 14 gate is still a broad `lock14_baseline` filter at
   `skv14-W5B-FRONTEND-plan.md:127`, not exact W5B owner-path tests. SPEC
   requires a unit test proving the W5B roster admits only those paths before
   source redress at `SPEC.md:726`.
3. The nonzero test assertion is still wildcard aggregate. The `rg` over
   `/tmp/skv14-w5b-*.log` at `skv14-W5B-FRONTEND-plan.md:145` can pass if one
   log has nonzero tests while another exact-test command ran zero tests.
4. The plan substitutes byte-identical artefacts for SPEC's full-table maintain
   requirement at `skv14-W5B-FRONTEND-plan.md:116`-`121` without routing a SPEC
   amendment. SPEC still says `Full-table maintain: +/-1.0% on all rows` at
   `SPEC.md:750`, and the V1 consolidated packet allowed either an executable
   maintain gate or a SPEC amendment at
   `skv14-waveW5B-FRONTEND-challenge/V1/HARDENING-SKV14-W5B-FRONTEND-V1-CONSOLIDATED.md:43`.

## Accepted Folds

- Owner paths are reconciled with SPEC: `skv14-W5B-FRONTEND-plan.md:24`-`35`
  matches `SPEC.md:714`-`720`.
- Gates are cwd-explicit.
- Read-only topology and provider-reachability gates are safe to execute.

## Required Fold

- Add owner file/type per construct to the lowering table.
- Replace descriptive fail-closed cells with exact test names.
- Split Lock 14 into exact W5B test names for roster admit, W5C/W5D rejection,
  and provider/template modification rejection.
- Replace wildcard nonzero assertion with per-log checks for each exact test.
- Either add SPEC-required full-table maintain evidence or route a SPEC
  amendment before claiming the fold closed.
