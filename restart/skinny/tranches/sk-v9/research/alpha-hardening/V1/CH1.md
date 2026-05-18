# SK-V9 Alpha Hardening V1 - CH1 Correctness

Verdict: REVISE

Confidence: 94%

## Findings

1. Threshold arithmetic in `alpha-E` is not internally safe. The retained parse
   optional implementation gate sets `apache_builds/parse_only` at `>=15367`
   Mbps (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:182-186`),
   but the master formula is
   `max(ceil(SK-V8-open Track1 * 1.10), ceil(sonic_strict / 1.10))`
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:102-105`) and the row source is
   T1 12694 / sonic 16904 (`skinny/RESULTS.md:12`), so the floor is 15368, as
   the master table correctly renders (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:112`).
   The same file also gives current typed GO floors of 12405, 10865, 8459, and
   6319 Mbps (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:87-90`);
   if those are intended as GO slack floors they should be
   `ceil(sonic/1.10)` = 12406, 10866, 8460, and 6320 from
   `skinny/RESULTS.md:7`, `skinny/RESULTS.md:18`, `skinny/RESULTS.md:21`, and
   `skinny/RESULTS.md:28`. If they are intended as maintain floors, they must
   instead align to the stricter master maintain targets at
   `restart/skinny/tranches/sk-v9/SYNTHESIS.md:153-158`.

2. Citation coverage does not satisfy the CH1 contract. CH1 requires each claim
   to cite a file:line, commit SHA, RESULTS row, or resolving REDRESS entry
   (`restart/prompts/ORCHESTRATOR.md:81-84`;
   `restart/prompts/pass-contracts/PASS-ALPHA.md:35-38`). Several source-sensitive
   sections state facts without exact anchors: alpha-B evidence rules
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md:22-39`),
   alpha-C close authority and disposition rows
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md:25-55`),
   alpha-D current authority
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:24-39`),
   alpha-E baseline facts
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:21-33`),
   and alpha-F diagnosis
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md:28-43`).

3. Candidate scope is inconsistent across artifacts. The master contract and
   handoff say W6 routes exactly/only three SK-V9 Alpha planning candidates
   (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:36-42`;
   `restart/skinny/tranches/sk-v9/HANDOFF.md:40-46`), but alpha-E shortlists two
   additional interventions: comparator/sidecar same-run manifest
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:293-354`)
   and SK-V9-open telemetry/gate refresh
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:373-443`).
   These look like gate/telemetry prerequisites, not behavior dispatch, but the
   current wording conflicts with the "only W6 residual routes" boundary.

4. `alpha-D` says "HEAD for this ledger" is `32870fea`
   (`restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md:22-25`),
   while the alpha materialization commit is current repository HEAD `ba1bb23d`.
   If `32870fea` is meant as the SK-V8 close head, the wording should say that
   instead of "HEAD for this ledger".

## Required Folds

- Fix `alpha-E` threshold floors: raise `apache_builds/parse_only` to 15368 Mbps
  and align current typed GO floors either to exact `ceil(sonic/1.10)` values or
  to the master Section 4.1 maintain floors.
- Add exact source anchors to alpha-B through alpha-F source-sensitive claims,
  especially row-count, W6, REDRESS 91-93, strictness, and sidecar-freshness
  statements.
- Reconcile alpha-E candidates 4 and 5 with the "exactly/only three W6 residual
  routes" wording by demoting them to gate-only telemetry prerequisites or by
  explicitly adding them to SYNTHESIS/HANDOFF as non-behavior, non-dispatch
  prerequisites.
- Rename the `alpha-D` `HEAD` claim to `SK-V8 close head` or cite the current
  alpha commit if the ledger is meant to describe the committed alpha packet.

## Blockers To G-Alpha

G-Alpha should not be presented until the threshold arithmetic and citation
coverage folds land. I did not find a direct SK-V9 implementation dispatch:
`SYNTHESIS.md` and `HANDOFF.md` keep G-Alpha and downstream S-P3 as the required
boundaries (`restart/skinny/tranches/sk-v9/SYNTHESIS.md:53-65`;
`restart/skinny/tranches/sk-v9/HANDOFF.md:52-62`), but alpha-E's extra
gate/telemetry candidates must be scoped explicitly before convergence.
