# CH1 CORRECTNESS - T-P3 V5

Verdict: ACCEPT

Target packet: `77b6e9fd7` (`docs(sk-v15-t-p3): repair V4 citation finding`).
Context commit: `6f1dd8aae` (`docs(sk-v15-t-p3): open V5 final hardening context`).

CH1-V4-001 is closed. The V5 packet is limited to the citation repair in
`3A-architecture-synthesis.md`, the required local checks pass, the proposed
LOCKS diff is extractable and applies, 3C covers every live 1E/2A-2F
lock-amendment candidate, and no target-packet scope fault is present.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline 77b6e9fd7 -- restart/audit/totality/p3` | `77b6e9fd7 docs(sk-v15-t-p3): repair V4 citation finding`; 1 file changed, 1 insertion, 1 deletion, only `restart/audit/totality/p3/3A-architecture-synthesis.md`. |
| Target-packet file list | `git show --name-only` and `git diff --name-only 77b6e9fd7^ 77b6e9fd7 -- restart/audit/totality/p3` both list only `restart/audit/totality/p3/3A-architecture-synthesis.md`. A diff against `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`, `restart/HANDOFF.md`, and `restart/MIGRATION.md` is empty. |
| `git diff --check 77b6e9fd7^ 77b6e9fd7 -- restart/audit/totality/p3` | Clean; no output. |
| Extract fenced diff from `3C-locks-v+1-diff.md` to `/tmp/tp3-locks-v5.diff`; `git apply --check /tmp/tp3-locks-v5.diff` | Clean; no output. Extracted diff has 36 lines and adds the proposed addendum before the v+1 governance boundary (`restart/audit/totality/p3/3C-locks-v+1-diff.md:33`-`70`). |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`, matching the locked invariant. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`, matching the Pattern H runtime-file invariant. |
| Required stale-pattern `rg` scan | No matches; exit 1. |

## CH1-V4-001 Closure

V4 found one out-of-range citation: `3A-architecture-synthesis.md:56` cited
`restart/audit/totality/p3/hardening/V2/CH4.md:38`-`47`, but the V2 CH4 file
ended at line 41 and the direct `CH4-V2-001` row was line 36
(`restart/audit/totality/p3/hardening/V4/CH1.md:55`-`59`;
`restart/audit/totality/p3/hardening/HARDENING-T-P3-V4-CONSOLIDATED.md:36`-`40`).

Current 3A now cites the in-range row:
`restart/audit/totality/p3/3A-architecture-synthesis.md:56` points to
`restart/audit/totality/p3/hardening/V2/CH4.md:36`, and that line is the
`CH4-V2-001` finding row naming 3A plus the required per-delta CH4 fields
(`restart/audit/totality/p3/hardening/V2/CH4.md:36`). The V3 CH4 closure row
also cites that same V2 evidence and the added coverage matrices
(`restart/audit/totality/p3/hardening/V3/CH4.md:35`-`47`).

## Citation And Path Audit

I scanned explicit repo `path:line` citations across the seven T-P3 proposal
artifacts, including `grammar/` citations and citations inside the extractable
diff. Result: 830 citations, 45 unique cited paths, 0 missing paths, and 0
out-of-range line or range references.

Per-artifact citation counts: 3A 172, 3B 114, 3C 109, 3C diff 44, 3D 148, 3E
184, and 3F 59. The repaired 3A citation is included in this scan.

Proposal evidence surfaces remain line-backed: 3A delta and CH4 coverage at
`restart/audit/totality/p3/3A-architecture-synthesis.md:52`-`56` and
`restart/audit/totality/p3/3A-architecture-synthesis.md:98`-`111`; 3B at
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:152`-`166` and
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:170`-`182`; 3C at
`restart/audit/totality/p3/3C-locks-crystallisation.md:44`-`57` and
`restart/audit/totality/p3/3C-locks-crystallisation.md:82`-`123`; 3D at
`restart/audit/totality/p3/3D-skinny-fold.md:55`-`68` and
`restart/audit/totality/p3/3D-skinny-fold.md:72`-`83`; 3E at
`restart/audit/totality/p3/3E-grammar-generalisation.md:64`-`78` and
`restart/audit/totality/p3/3E-grammar-generalisation.md:138`-`150`; and 3F at
`restart/audit/totality/p3/3F-migration-handoff.md:36`-`46` and
`restart/audit/totality/p3/3F-migration-handoff.md:115`-`123`.

## 3C Live 1E/2X Coverage

The live source candidate set totals 42 rows: 15 from 1E
(`restart/audit/totality/p1/1E-locks-evidence.md:130`-`144`), 5 from 2A
(`restart/audit/totality/p2/2A-sota-landscape.md:107`-`111`), 4 from 2B
(`restart/audit/totality/p2/2B-primitive-vocabulary.md:201`-`204`), 6 from 2C
(`restart/audit/totality/p2/2C-grammar-neutrality.md:144`-`149`), 5 from 2D
(`restart/audit/totality/p2/2D-cost-model.md:114`-`118`), 3 from 2E
(`restart/audit/totality/p2/2E-host-arch-esoterica.md:139`-`141`), and 4 from
2F (`restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`122`).

3C states and implements the same 42-row total
(`restart/audit/totality/p3/3C-locks-crystallisation.md:31`,
`restart/audit/totality/p3/3C-locks-crystallisation.md:37`-`40`). The
disposition matrix has 42 unique candidate ids after normalizing the descriptive
suffixes on the 2C source rows, with no missing or extra ids
(`restart/audit/totality/p3/3C-locks-crystallisation.md:82`-`123`). Counts are
23 `ACCEPT`, 19 `MODIFY`, 0 `REJECT`, and 0 `DEFER`, matching the 3C summary
(`restart/audit/totality/p3/3C-locks-crystallisation.md:31`). The proposed
delta table folds those rows into `D-L01` through `D-L16`
(`restart/audit/totality/p3/3C-locks-crystallisation.md:46`-`57`), and the
per-clause matrix covers cost, propagation, hard-cap fit, and fail action
(`restart/audit/totality/p3/3C-locks-crystallisation.md:63`-`76`).

## LOCKS Diff And Target Scope

The fenced diff is extractable from
`restart/audit/totality/p3/3C-locks-v+1-diff.md:33`-`70` and applies cleanly to
the current `restart/locks/LOCKS.md`. The diff is proposal-only: the target
packet does not edit live `ARCHITECTURE.md`, `MASTER-PLAN.md`, `LOCKS.md`,
`HANDOFF.md`, or `MIGRATION.md`, and the live lock count remains 16.

The target packet scope is citation-only. It changes one line in 3A, replacing
the invalid V2 CH4 range with `restart/audit/totality/p3/hardening/V2/CH4.md:36`;
no other T-P3 proposal artifact or live governance/spec file is modified by
`77b6e9fd7`.

## Defects

None.

## Residual Risk

The current worktree contains unrelated dirty runtime, research, bench, docs, and
xtask files from other agents. None are T-P3 target artifacts or live governance
files used to explain a mismatch. CH1 does not rely on dirty state for acceptance.
