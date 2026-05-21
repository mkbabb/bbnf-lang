---
lens: CH1
name: CORRECTNESS
pass: T-P1-excavation
cycle: V1
disposition: REVISE
reviewed_artifacts:
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
mechanical_citation_check: "665 backticked file:line citations extracted; 664 resolved; 1 missing path"
---

## Verdict

REVISE. Most V1 inventory citations resolve and the major RESULTS / REDRESS rows in 1D resolve to real entries, but CH1 found one broken path citation and several correctness issues where a resolving citation is too narrow for the verdict stated.

REJECT: none. No artifact is wholly unusable under CH1.

ACCEPT: 1C runtime evidence, 1D REDRESS / RESULTS row mapping, and the 1F coherence / past-corpora drift findings are broadly supportable after spot checks.

## Findings

| Disposition | Finding | Evidence |
|---|---|---|
| REVISE | 1B has a non-resolving implementation citation in the codegen table. The row claims `LayoutFacts.backend_shape` is wired through `codegen/src/lib.rs:95-100`, but that path does not exist from repo root. The same lines do exist at the skinny path. | Broken citation is in `restart/audit/totality/p1/1B-codegen-evidence.md:35`. Resolving source is `skinny/crates/codegen/src/lib.rs:95-100`. |
| REVISE | 1A under-states the live `LayoutFacts.backend_shape` implementation. 1A says no named side table appears in audited `ir/src` and catalogs the side table as missing, but the spec claim is about `LayoutFacts` produced by passes, and live `passes` has `backend_shape: HashMap<RuleId, BackendShape>` plus compile-time population from the derived shape plan. The correct verdict is not "named side table missing"; it should distinguish "side table implemented in passes" from "cost facts / algorithm still thin or order-drifted." | 1A row and gap: `restart/audit/totality/p1/1A-substrate-evidence.md:35`, `restart/audit/totality/p1/1A-substrate-evidence.md:65`. Spec says `LayoutFacts` carries `backend_shape` at `restart/ARCHITECTURE.md:1047` and defines the eight-step algorithm at `restart/ARCHITECTURE.md:1090-1098`. Live implementation has the fields at `skinny/crates/passes/src/lib.rs:84-92` and population at `skinny/crates/passes/src/lib.rs:44-55`. 1B already cites this correctly at `restart/audit/totality/p1/1B-codegen-evidence.md:35`. |
| REVISE | Several negative-search and command-output claims are not reproducible from a path:line, commit SHA, RESULTS row, or REDRESS entry. The findings may be true, but CH1 cannot validate them as written because the command output is not captured in an artifact and no commit SHA is supplied. | Examples: 1A claims an `rg` negative search for old substrate names at `restart/audit/totality/p1/1A-substrate-evidence.md:43`; 1E cites `rg __EAGER_EMPTY_PATH` returning none at `restart/audit/totality/p1/1E-locks-evidence.md:54`; 1F cites `wc -l` and child-count scans at `restart/audit/totality/p1/1F-anti-pattern.md:30-31`. These should either cite a committed command-output artifact or be rewritten as verify actions / UNKNOWNs. |
| REVISE | 1F AP-002 overstates the Lock 13 directory violation unless it proves the "mixing concerns" half of the lock. Lock 13 forbids directories with more than 10 children mixing concerns, not every directory with more than 10 children. The row itself admits some listed directories may be cohesive ISA partitions, so the verdict should be narrowed or moved to UNKNOWN pending a child inventory. | Lock text is at `restart/locks/LOCKS.md:76`. The AP-002 claim and caveat are at `restart/audit/totality/p1/1F-anti-pattern.md:31` and `restart/audit/totality/p1/1F-anti-pattern.md:43`. |
| ACCEPT | 1D's RESULTS-row and REDRESS-entry citations resolve for the high-load claims checked: parse-only NO-GO rows, CSS L4 strict same-plane admission, direct row movement predicate, union-substrate rejections, and SK-V13 reopening of all JSON rows. | Parse rows resolve in `skinny/RESULTS.md:5-45`; CSS admission resolves at `skinny/RESULTS.md:94-95` and `skinny/REDRESS.md:3824-3840`; direct contract rows resolve at `skinny/REDRESS.md:2980-3022`; union rejection resolves at `skinny/REDRESS.md:2795-2934`; SK-V13 reopen resolves at `restart/skinny/tranches/sk-v13/SYNTHESIS.md:95-110` and `restart/skinny/tranches/sk-v13/SYNTHESIS.md:239-257`. |
| ACCEPT | The coherence drift finding that HANDOFF / skinny INDEX lag later SK-V12 result authority is accurate. | 1F coherence claims drift at `restart/audit/totality/p1/1F-coherence-scan.md:32` and `restart/audit/totality/p1/1F-past-corpora.md:36`. Stale HANDOFF / INDEX authority is at `restart/HANDOFF.md:3-4` and `restart/skinny/INDEX.md:42-56`; later authority is `skinny/RESULTS.md:145-148`. |

## Required Revisions

1. Fix the 1B bad path citation to `skinny/crates/codegen/src/lib.rs:95-100`.
2. Revise 1A-SUB-006 and the matching gap so `LayoutFacts.backend_shape` is accepted as live in `passes`, while preserving the separate finding that the objective cost model and priority algorithm are incomplete / drifted.
3. Convert uncaptured command-output claims into cited artifacts, commit-SHA-backed evidence, or UNKNOWN verify actions.
4. Narrow 1F AP-002 to directories whose child inventory proves mixed concerns, or mark the cohesive ISA/test directories as needing CH4/CH5 follow-up rather than CH1 correctness closure.
