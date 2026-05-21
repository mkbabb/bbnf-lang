---
agent: CH6
pass: T-P1-excavation
cycle: V2
lens: ANTI-PAPER-CLOSE
disposition: REVISE
generated_at: 2026-05-21T00:00:00-04:00
inputs_audited:
  - restart/prompts/totality/PASS-1-EXCAVATION.md §3
  - restart/prompts/ORCHESTRATOR.md §3W
  - restart/prompts/ORCHESTRATOR.md §3Z
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

## Lens Contract

CH6 checks that V2 does not close on prose. The governing contract requires CHALLENGE dispositions to be explicit ACCEPT / REVISE / REJECT, rejects self-reported "complete" / "wired" / "verified" style claims without live evidence, forbids deferral to a later phase as closure, and requires every UNKNOWN to carry a verify_action.

## Findings

| disposition | target | finding | required revision |
|---|---|---|---|
| ACCEPT | `restart/audit/totality/p1/1A-substrate-evidence.md:72-77` | All 1A UNKNOWN rows carry concrete verify_action commands. V2 also weakens the prior uncaptured-search claims: EventTape/runtime absence is routed to UNKNOWN at `restart/audit/totality/p1/1A-substrate-evidence.md:37`, old-name census is UNKNOWN at `restart/audit/totality/p1/1A-substrate-evidence.md:44`, and `StructuralIndex` is classified as transient rather than a closed no-sidecar proof at `restart/audit/totality/p1/1A-substrate-evidence.md:42`. | None. |
| ACCEPT | `restart/audit/totality/p1/1B-codegen-evidence.md:96-102` | 1B fixed the V1 VM replay paper-close issue: the lowerer hierarchy row now says VM replay is `UNKNOWN inline` and points to `UNKNOWN-1`, whose verify_action is to list and line-read VM crate files. | None. |
| ACCEPT | `restart/audit/totality/p1/1B-codegen-evidence.md:36` | The `LayoutFacts.backend_shape` claim is backed by live side-table and codegen handoff citations instead of symbol-free "wired" wording. Objective cost content is separately kept thin at `restart/audit/totality/p1/1B-codegen-evidence.md:37` and in divergence `P1-1B-D2` at `restart/audit/totality/p1/1B-codegen-evidence.md:75`. | None. |
| ACCEPT | `restart/audit/totality/p1/1C-runtime-evidence.md:113-119` | 1C UNKNOWN rows all carry verify_action text. EventTape is not closed by witness scaffolding: it is explicitly `Unimplemented` at `restart/audit/totality/p1/1C-runtime-evidence.md:54` and `restart/audit/totality/p1/1C-runtime-evidence.md:70`, with the witness-vs-runtime question left open at `restart/audit/totality/p1/1C-runtime-evidence.md:118`. | None. |
| REVISE | `restart/audit/totality/p1/1D-skinny-lessons.md:41` | The row marks the "single substrate" lesson as `proved` and then calls it a `Grammar-neutral substrate lesson`, but the cited evidence is JSON REDRESS evidence. The planning metadata names future substrate/runtime/codegen waves, so the row can be read as closing grammar-neutral substrate policy from JSON-only evidence plus future routing. | Change the verdict/note to "proved for JSON; grammar-neutral rule candidate" or add exact non-JSON live substrate evidence in the same row. |
| ACCEPT | `restart/audit/totality/p1/1D-skinny-lessons.md:69-71` | 1D fixed the V1 grammar-generalization closure wording. `SKINNY-GEN-002` is now `proved as negative rule`, `SKINNY-GEN-003` is `proved as direction; partial row-level repair`, and CSS L4 positive evidence is separately backed by REDRESS/RESULTS rows. | None. |
| ACCEPT | `restart/audit/totality/p1/1D-skinny-lessons.md:111-122` | 1D UNKNOWN rows and open questions all carry verify_action text. The SK-V13 table also distinguishes "unblocked with fresh evidence" from acceptance at `restart/audit/totality/p1/1D-skinny-lessons.md:90-94`. | None. |
| REVISE | `restart/audit/totality/p1/1E-locks-evidence.md:54` | Lock 1 is still labelled `honoured, with amendment candidate` while the same row depends on CH5 caveats and routes closure to a T-P3 substrate consumer or explicit exclusion. That is too close to future-phase deferral as closure for the full lock, even though the JSON tape evidence is live. | Change the verdict to "partial / honoured for scoped JSON lazy-offset evidence" and keep the T-P3 consumer as a required future verification, not as part of closure. |
| ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:69` | 1E fixed the V1 Lock 16 issue. The row now says `partial; allowlist traceability UNKNOWN` and names the missing manifest/traceability proof instead of calling the lock honoured. | None. |
| ACCEPT | `restart/audit/totality/p1/1E-locks-evidence.md:126-127` | 1E UNKNOWN rows carry verify_action text for both cursor elision and Lock 16 allowlist coverage. | None. |
| ACCEPT | `restart/audit/totality/p1/1F-anti-pattern.md:31-34` | 1F anti-pattern weakened closure wording around uncaptured scans. Lock 13 child-count status is `UNKNOWN mixed-concern status`, EventCursor no-match status is scan-derived and uncaptured, and AP-005 is `partial / residue` rather than V1's over-closed wording. | None. |
| ACCEPT | `restart/audit/totality/p1/1F-anti-pattern.md:80-84` | 1F anti-pattern UNKNOWN rows all carry verify_action text, including exact EventCursor/stale-sidecar scan terms before any absence claim is promoted. | None. |
| ACCEPT | `restart/audit/totality/p1/1F-coherence-scan.md:40-43` | 1F coherence no longer presents prior-corpus pre-blocks as current no-match closure. The EventCursor row says current retained implementation is not cited and residue remains; historical rows are `accepted historical pre-block`, and line `restart/audit/totality/p1/1F-coherence-scan.md:58` states historical pre-blocks are ledger constraints, not implementation closure. | None. |
| ACCEPT | `restart/audit/totality/p1/1F-coherence-scan.md:82-86` | 1F coherence UNKNOWN rows carry verify_action text. | None. |
| ACCEPT | `restart/audit/totality/p1/1F-past-corpora.md:30-35` | 1F past-corpora fixed the V1 "implemented pre-block" wording. PC-001 through PC-004 are accepted historical pre-blocks; PC-003 explicitly says current absence is UNKNOWN; PC-005 and PC-006 use `implemented` only where live generated/codegen evidence is cited. | None. |
| ACCEPT | `restart/audit/totality/p1/1F-past-corpora.md:88-92` | 1F past-corpora UNKNOWN rows all carry verify_action text, including targeted `rg` terms for old Lock 14 and historical no-match claims. | None. |

## Cycle Disposition

REVISE. V2 materially improved the anti-paper-close posture: UNKNOWN rows consistently carry verify_action entries, V1's VM/EventCursor/Lock16 over-closure was weakened, and historical pre-blocks are now separated from live absence claims. The remaining CH6 defects are narrow but real: a JSON-only substrate lesson in 1D and Lock 1 wording in 1E still read as broader closure than the cited live evidence proves, with future consumer routing adjacent to the closure label.
