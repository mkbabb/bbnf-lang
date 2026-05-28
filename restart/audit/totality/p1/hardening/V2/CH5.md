---
lens: CH5
name: HIDDEN COUPLING
pass: T-P1-excavation
cycle: V2
disposition: ACCEPT
input_commit: 2fcbc1dc8
generated_at: 2026-05-28
files_audited:
  - restart/prompts/ORCHESTRATOR.md
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/skinny/PASS-IMPL-OVERFIT-AUDIT.md
  - restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# CH5 Hidden Coupling - V2

## Verdict

ACCEPT.

The SK-V15 V2 packet folds the V1 CH5 blockers into current inventory rows.
It does not claim CSS or the generic gates are clean. Instead it marks CSS L4
as audit-demoted, keeps fact streams out of the value API, reports the
24-row broadcast, identifies the string-literal CSS generator, carries the
root `OnceCell<StructuralIndex>` sidecar, and adds Lock 14/16 exclusion
reporting obligations. No CH5 hidden-coupling REVISE remains.

## Findings

| id | disposition | check | evidence | required action |
|---|---|---|---|---|
| CH5-V2-01 | ACCEPT | CSS 24-row broadcast is detected and not treated as 24 independent admits. | `1D-skinny-lessons.md:102`, `:147`, `:171`, `:179`; `1E-locks-evidence.md:92`, `:106`, `:131`, `:151`; `1F-coherence-scan.md:98`, `:131`. | None for CH5. |
| CH5-V2-02 | ACCEPT | CSS string-literal tokenizer / generator provenance failure is carried as audit-demoted evidence. | `1D-skinny-lessons.md:103`, `:148`; `1E-locks-evidence.md:98`; `1F-coherence-scan.md:77`, `:112`; `1B-codegen-evidence.md:49`, `:95`; `1C-runtime-evidence.md:57`. | None for CH5. |
| CH5-V2-03 | ACCEPT | CSS fact stream is fenced as an output plane, not a CSS value API or sixth `BackendShape`. | `1A-substrate-evidence.md:76`, `:136`-`:143`; `1C-runtime-evidence.md:63`, `:110`, `:129`; `1D-skinny-lessons.md:105`, `:178`; `1E-locks-evidence.md:125`, `:184`. | None for CH5. |
| CH5-V2-04 | ACCEPT | Hidden-coupling surfaces are catalogued instead of paper-closed: transient scanner, EventTape fences, root structural-index sidecar, and CSS comparator sidecar. | `1A-substrate-evidence.md:72`-`:77`, `:89`, `:145`-`:156`; `1C-runtime-evidence.md:98`-`:100`; `1F-coherence-scan.md:82`-`:83`, `:121`-`:122`. | None for CH5. |
| CH5-V2-05 | ACCEPT | Track 1 / Track 2 and retained-sidecar caveats are preserved as substrate-union constraints, not hidden independence claims. | `1D-skinny-lessons.md:106`-`:107`; `1F-coherence-scan.md:121`-`:122`; `1A-substrate-evidence.md:126`-`:134`, `:154`-`:156`. | None for CH5. |
| CH5-V2-06 | ACCEPT | Lock 14 / Lock 16 self-exempting gate and scan-exclusion detection is present. | `1D-skinny-lessons.md:109`, `:180`, `:195`; `1E-locks-evidence.md:135`, `:161`-`:166`; `1F-coherence-scan.md:72`-`:73`, `:92`-`:99`, `:107`, `:124`, `:142`, `:152`. | None for CH5. |
| CH5-V2-07 | ACCEPT | NEW-CH5-V5-02 and NEW-CH7-V5-03 coverage is folded into inventory/gate carriers. | Broadcast: `1D-skinny-lessons.md:171`, `:179`; `1E-locks-evidence.md:131`; `1F-coherence-scan.md:98`, `:130`-`:131`. Gate exclusions: `1E-locks-evidence.md:135`, `:161`-`:166`; `1F-coherence-scan.md:92`-`:99`, `:133`. | None for CH5. |
| CH5-V2-08 | ACCEPT | Historical 1F auxiliaries are explicitly demoted, so stale SK-V14 CH5 surfaces are not live V2 authority. | `1F-coherence-scan.md:85`-`:90`; `1F-anti-pattern.md:12`-`:23`; `1F-past-corpora.md:12`-`:28`. | None for CH5. |

## Notes

- Static audit only. No source edits, builds, tests, staging, or commits.
- Residual implementation work remains for PRUNE/REBUILD waves, but the V2
  excavation packet detects and routes the CH5 coupling risks correctly.
