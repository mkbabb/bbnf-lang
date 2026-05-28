# CH1 CORRECTNESS - T-P3 V4

Verdict: REVISE

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Context commit: `40528179e` (`docs(sk-v15-t-p3): open V4 confirmation hardening context`).

One CH1 defect is present. The required invariant checks pass, 3C covers all
live 1E and 2A-2F lock-amendment candidates, and the target packet touches only
the seven T-P3 proposal artifacts. The packet still needs a citation-range
repair before CH1 can accept it.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | `e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`; 7 files changed, 170 insertions, 123 deletions. |
| Full target-packet file list | `git show --name-only` and `git diff --name-only e6c1c2a84^ e6c1c2a84` list only `3A-architecture-synthesis.md`, `3B-master-plan-reconciliation.md`, `3C-locks-crystallisation.md`, `3C-locks-v+1-diff.md`, `3D-skinny-fold.md`, `3E-grammar-generalisation.md`, and `3F-migration-handoff.md`. A diff against `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`, `restart/HANDOFF.md`, and `restart/MIGRATION.md` is empty. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | Clean; no output. |
| Extract fenced diff from `3C-locks-v+1-diff.md` and run `git apply --check` | Clean; extracted diff has 36 lines and applies. I used a no-persistent-file pipeline equivalent to the required `/tmp/tp3-locks-v4.diff` check to preserve this CH1 single-output-file ownership. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`, matching the locked invariant. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`, matching the Pattern H runtime-file invariant. |
| Required stale-pattern `rg` scan | No matches, exit 1. |

## Citation And Path Audit

Checked 830 explicit repo `path:line` citations across the seven T-P3 proposal
artifacts. The scan found 45 unique cited paths, 829 in-bounds citations, and 1
out-of-bounds range. Path resolution otherwise succeeds.

Every proposal delta table still carries concrete evidence citations at the
expected surfaces: 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:59`-`74`),
3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:152`-`166`),
3C (`restart/audit/totality/p3/3C-locks-crystallisation.md:44`-`57`),
3D (`restart/audit/totality/p3/3D-skinny-fold.md:55`-`68`),
3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:64`-`78`),
and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:36`-`46`).

## 3C Live 1E/2X Coverage

3C covers every live 1E and 2A-2F lock-amendment candidate. Source candidate
rows total 42: 1E has 15 rows (`restart/audit/totality/p1/1E-locks-evidence.md:130`-`144`),
2A has 5 (`restart/audit/totality/p2/2A-sota-landscape.md:107`-`111`),
2B has 4 (`restart/audit/totality/p2/2B-primitive-vocabulary.md:201`-`204`),
2C has 6 (`restart/audit/totality/p2/2C-grammar-neutrality.md:144`-`149`),
2D has 5 (`restart/audit/totality/p2/2D-cost-model.md:114`-`118`),
2E has 3 (`restart/audit/totality/p2/2E-host-arch-esoterica.md:139`-`141`),
and 2F has 4 (`restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`122`).

The 3C disposition matrix has the same 42 unique ids, with no missing or extra
ids, and counts 23 `ACCEPT`, 19 `MODIFY`, 0 `REJECT`, and 0 `DEFER`
(`restart/audit/totality/p3/3C-locks-crystallisation.md:31`-`40`,
`restart/audit/totality/p3/3C-locks-crystallisation.md:78`-`123`).

## Finding

| id | severity | owner | target artifact | conflicting evidence | finding | repair directive |
|---|---|---|---|---|---|---|
| CH1-V4-001 | High | 3A | `restart/audit/totality/p3/3A-architecture-synthesis.md:56` | `restart/audit/totality/p3/hardening/V2/CH4.md:36`, `restart/audit/totality/p3/hardening/V2/CH4.md:41` | The 3A V3 delta summary cites `restart/audit/totality/p3/hardening/V2/CH4.md:38`-`47`, but `V2/CH4.md` has only 41 lines. The cited range is out of bounds and misses the direct `CH4-V2-001` row at line 36. | Replace the bad range with valid evidence, for example `restart/audit/totality/p3/hardening/V2/CH4.md:36` for the V2 defect and, if V3 closure evidence is intended, `restart/audit/totality/p3/hardening/V3/CH4.md:35`-`42`. Rerun the path-line citation scan before the next confirmation cycle. |

## Residual Risk

No REJECT condition is present from the required invariants: lock count is 16,
Pattern H runtime file count is 67, the stale-pattern scan is clean, the
extractable `LOCKS.md` diff applies, and the target packet edits no live
governance/spec files outside the seven T-P3 proposal artifacts. The current
worktree has unrelated dirty runtime/research/xtask files from other agents;
they were not used to explain any mismatch and do not affect this CH1 verdict.
