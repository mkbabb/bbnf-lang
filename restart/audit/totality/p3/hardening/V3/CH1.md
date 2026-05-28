# CH1 CORRECTNESS - T-P3 V3

Verdict: ACCEPT

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Context commit: `5b85f7d5d` (`docs(sk-v15-t-p3): open V3 hardening context`).

No CH1 defects found.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | Target packet is the V3 synthesis fold and changes only the seven T-P3 proposal artifacts: 170 insertions, 123 deletions. `git show --name-status --oneline e6c1c2a84 --` also listed only `3A` through `3F` under `restart/audit/totality/p3/`, and a live-surface diff against `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/locks/LOCKS.md`, `restart/HANDOFF.md`, and `restart/MIGRATION.md` was empty. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | Clean; no whitespace/path errors emitted. |
| Extract `3C-locks-v+1-diff.md` diff and run `git apply --check /tmp/tp3-locks-v3.diff` | Clean; extracted diff applies to current `restart/locks/LOCKS.md`. The diff states it remains proposal-only, anchors before the governance boundary, and preserves the 16 locks / five-shape canon (`restart/audit/totality/p3/3C-locks-v+1-diff.md:29`-`40`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:72`-`76`). |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`, matching the locked invariant. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`, matching the Pattern H runtime-file invariant. |
| Required stale-pattern `rg` scan | No matches for stale prompt paths, stale `2F-parse-that-gaps.md:518`, active legacy regex-owner wording, docs-only cleanup, unresolved receiver/blocker/gate phrases, or missing runtime-regex Lock 1 wording in the seven V3 artifacts. |

## Citation And Evidence Validation

- Checked 830 explicit repo `path:line` citations across `3A` through `3F`; every cited path resolved and every cited line or range was in bounds.
- Every proposed delta table carries concrete evidence citations: 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:59`-`74`), 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:152`-`166`), 3C (`restart/audit/totality/p3/3C-locks-crystallisation.md:42`-`57`), 3D (`restart/audit/totality/p3/3D-skinny-fold.md:55`-`68`), 3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:64`-`78`), and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:36`-`46`).
- Spot checks confirmed the cited evidence supports the proposed routing rather than just resolving syntactically: T-P1/T-P2 governance is carried from the hardening locks (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md:21`-`28`; `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md:15`-`19`), the CSS broadcast and provider blockers are grounded in PASS-IMPL/T-P2 evidence (`restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:21`-`33`; `restart/audit/totality/p2/2C-grammar-neutrality.md:144`-`149`), and the Decision/lowerer/five-shape rows cite the 2D cost-model candidate lines (`restart/audit/totality/p2/2D-cost-model.md:114`-`118`).

## 3C Coverage

3C covers every live 1E and 2A-2F lock-amendment candidate. Source candidate rows total 42: 1E has 15 rows (`restart/audit/totality/p1/1E-locks-evidence.md:130`-`144`), 2A has 5 (`restart/audit/totality/p2/2A-sota-landscape.md:107`-`111`), 2B has 4 (`restart/audit/totality/p2/2B-primitive-vocabulary.md:201`-`204`), 2C has 6 (`restart/audit/totality/p2/2C-grammar-neutrality.md:144`-`149`), 2D has 5 (`restart/audit/totality/p2/2D-cost-model.md:114`-`118`), 2E has 3 (`restart/audit/totality/p2/2E-host-arch-esoterica.md:139`-`141`), and 2F has 4 (`restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`122`). The 3C disposition matrix has the same 42 unique ids, with no missing or extra ids (`restart/audit/totality/p3/3C-locks-crystallisation.md:78`-`123`), matching the V3 summary counts (`restart/audit/totality/p3/3C-locks-crystallisation.md:29`-`40`).

## V2 CH4 Repair Closure

| V2 finding | closure evidence |
|---|---|
| `CH4-V2-001`: every carried delta in 3A, 3B, 3D, 3E, and 3F must name LOC, numeric propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action (`restart/audit/totality/p3/hardening/V2/CH4.md:34`-`37`; consolidated at `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:35`-`40`). | Closed. V3 adds row-level CH4 matrices for 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:93`-`111`), 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:168`-`182`), 3D (`restart/audit/totality/p3/3D-skinny-fold.md:70`-`83`), 3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:131`-`150`), and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:113`-`123`). |
| `CH4-V2-002`: the 3C `D-L*` matrix must add hard-cap fit and fail-action columns, with Pass Omega doc-only status, wave consumption, exact non-fit route, no W12, and no challenge-time implementation overflow (`restart/audit/totality/p3/hardening/V2/CH4.md:37`; consolidated at `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:39`-`40`). | Closed. V3 records the repair in 3C frontmatter (`restart/audit/totality/p3/3C-locks-crystallisation.md:19`-`24`) and the per-clause cost matrix now includes `hard-cap fit` and `fail action` for every `D-L*` row (`restart/audit/totality/p3/3C-locks-crystallisation.md:59`-`76`). The companion diff remains extractable and apply-clean (`restart/audit/totality/p3/3C-locks-v+1-diff.md:18`-`29`). |

## Residual Risk

The worktree has unrelated dirty runtime/research files from other agents. They do not affect the CH1 checks above: target packet scope is limited to the seven T-P3 proposal artifacts, current `restart/locks/LOCKS.md` still has 16 numbered locks, the extracted 3C diff applies to it, the Pattern H census is 67, and this report writes only `restart/audit/totality/p3/hardening/V3/CH1.md`.
