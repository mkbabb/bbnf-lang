# CH1 CORRECTNESS - T-P3 V2

Verdict: ACCEPT

Target packet: `7885b29ab` (`docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`).
Context commit: `d1d073a50` (`docs(sk-v15-t-p3): open V2 hardening context`).

No CH1 defects found.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline 7885b29ab -- restart/audit/totality/p3` | Target packet is the V2 synthesis fold and changes only the seven T-P3 artifacts: 287 insertions, 206 deletions. A full `git diff --name-only 7885b29ab^ 7885b29ab` also listed only `3A` through `3F` under `restart/audit/totality/p3/`, so no live V1 spec surface was edited. |
| `git diff --check 7885b29ab^ 7885b29ab -- restart/audit/totality/p3` | Clean; no whitespace/path errors emitted. |
| Extract `3C-locks-v+1-diff.md` diff and run `git apply --check /tmp/tp3-locks-v2.diff` | Clean; extracted diff applies to current `restart/locks/LOCKS.md`. The V2 hunk states it anchors on the governance boundary and avoids inherited stale citation context (`restart/audit/totality/p3/3C-locks-v+1-diff.md:28`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:36`). |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`, matching the locked invariant. The proposed diff adds no numbered lock heading. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`, matching the Pattern H runtime-file invariant. |
| Required stale-pattern `rg` scan | No matches for `ORCHESTRATOR-PROMPT`, stale `2F-parse-that-gaps.md:518`, active legacy regex-owner wording, docs-only cleanup, unresolved receiver/blocker/gate phrases, or missing runtime-regex Lock 1 wording in the seven V2 artifacts. |

## Citation And Evidence Validation

- Checked 855 explicit repo `path:line` citations across `3A` through `3F`; every cited path resolved and every cited line or range was in bounds.
- The proposed delta tables all carry path:line evidence to T-P1, T-P2, PASS-IMPL V1, SK-V15, V1 spec surfaces, or V2 hardening: 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:58`-`71`), 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:151`-`165`), 3C (`restart/audit/totality/p3/3C-locks-crystallisation.md:43`-`56`), 3D (`restart/audit/totality/p3/3D-skinny-fold.md:55`-`66`), 3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:64`-`76`), and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:38`-`46`).
- The 3C proposed diff preserves the locked boundaries in the proposed text: 16 locks, exactly `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, no new directive, BIR variant, substrate, public substrate API, retained sidecar, lock, retirement, or sixth shape (`restart/audit/totality/p3/3C-locks-v+1-diff.md:39`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:55`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:59`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:63`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:73`-`75`).

## 3C Coverage

3C covers every live 1E and 2A-2F lock-amendment candidate. Source candidate rows total 42: 1E has 15 rows (`restart/audit/totality/p1/1E-locks-evidence.md:130`-`144`), 2A has 5 (`restart/audit/totality/p2/2A-sota-landscape.md:107`-`111`), 2B has 4 (`restart/audit/totality/p2/2B-primitive-vocabulary.md:201`-`204`), 2C has 6 (`restart/audit/totality/p2/2C-grammar-neutrality.md:144`-`149`), 2D has 5 (`restart/audit/totality/p2/2D-cost-model.md:114`-`118`), 2E has 3 (`restart/audit/totality/p2/2E-host-arch-esoterica.md:139`-`141`), and 2F has 4 (`restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`122`). The 3C disposition matrix has the same 42 unique ids, with no missing or extra ids (`restart/audit/totality/p3/3C-locks-crystallisation.md:81`-`122`), matching its V2 summary (`restart/audit/totality/p3/3C-locks-crystallisation.md:30`, `restart/audit/totality/p3/3C-locks-crystallisation.md:38`).

## V1 CH1 Repair Closure

| V1 finding | closure evidence |
|---|---|
| `CH1-V1-001` stale out-of-range `2F-parse-that-gaps.md:518` in the 3C diff context (`restart/audit/totality/p3/hardening/V1/CH1.md:115`; consolidated repair at `restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:39`). | Closed. V2 records the repair in 3C frontmatter (`restart/audit/totality/p3/3C-locks-crystallisation.md:19`; `restart/audit/totality/p3/3C-locks-v+1-diff.md:19`), the diff now anchors at `## v+1 Governance Boundary` without restating the stale citation (`restart/audit/totality/p3/3C-locks-v+1-diff.md:28`, `restart/audit/totality/p3/3C-locks-v+1-diff.md:36`), the required stale-pattern scan has no `2F-parse-that-gaps.md:518` match, and `git apply --check` succeeds. |
| `CH1-V1-002` unresolved missing SK-V15 companion prompt route (`restart/audit/totality/p3/hardening/V1/CH1.md:116`; consolidated repair at `restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:40`). | Closed. V2 3F routes current SK-V15 authority to the extant `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` and says not to cite a missing companion prompt unless a separate owner creates it first (`restart/audit/totality/p3/3F-migration-handoff.md:25`, `restart/audit/totality/p3/3F-migration-handoff.md:33`, `restart/audit/totality/p3/3F-migration-handoff.md:44`, `restart/audit/totality/p3/3F-migration-handoff.md:86`-`89`). Local check confirmed `DISPATCH-PROMPT.md` exists and `ORCHESTRATOR-PROMPT.md` remains absent, so the repair is a real route change, not a hidden file assumption. |

## Residual Risk

The worktree has unrelated dirty runtime/research files and untracked sibling V2 challenge files from other agents. They do not affect the CH1 checks above: current `restart/locks/LOCKS.md` is unchanged, the extracted 3C diff applies to it, target packet edits are limited to the seven T-P3 artifacts, and this report writes only `restart/audit/totality/p3/hardening/V2/CH1.md`.
