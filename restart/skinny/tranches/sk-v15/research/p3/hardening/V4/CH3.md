# SK-V15 S-P3 V4 CH3 REGRESSION

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V4. Lens: CH3.
Date: 2026-05-28.
HEAD: `21ae60663`.
Owned output: `restart/skinny/tranches/sk-v15/research/p3/hardening/V4/CH3.md`.

## Verdict

ACCEPT.

The active S-P3 packet blocks the SK-V14 W2R/W4R regression class. Delete,
retirement, diagnostic demotion, and neutralization actions must bind to
named dependency rows before redress; provider/runtime/template deletion is
blocked until replacement proof lands no later than the delete wave; the
normalized REDRESS pre-block ledger is present in the final dispatch surfaces;
and W11 cannot close with orphan dependency rows or route unresolved SK-V15
misses to SK-V16.

## Evidence Table

| check | disposition | evidence | residual risk |
|---|---|---|---|
| Wave graph topology | ACCEPT | P3-B fixes the order as `W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11` (`p3b-wave-sequencing.md:17`). SPEC preserves the provider-before-retire chain: W5 dispatches after W1-W4, W6 after the W5 typed CSS provider, and W11 only after W1-W10 are resolved (`SPEC.md:40`-`:43`). | None for CH3. |
| No W12 or challenge-overflow escape | ACCEPT | P3-B, SPEC, and DISPATCH all state that W0-W11 consumes the 12-wave ceiling and over-budget work cannot spawn W12 or use CHALLENGE time as implementation overflow; the legal routes are row-level intrinsic block, revert/REDRESS, or gate-routed wave-graph amendment (`p3b-wave-sequencing.md:30`-`:32`; `SPEC.md:166`-`:169`; `DISPATCH-PROMPT.md:60`-`:63`). | None. |
| W2R/W4R delete-before-provider recurrence blocked | ACCEPT | SPEC makes `NEW-CH3-V5-01` non-negotiable: no delete or retirement before rebuild proof (`SPEC.md:140`). P3-E pre-blocks REDRESS 183/184/209-213 by forbidding provider/runtime/template retirement before replacement proof and treats static centralization of committed generated text as the same rejected route (`p3e-preblocked-ledger.md:51`). SPEC repeats that final block in Section 15 (`SPEC.md:481`). | None. |
| Named dependency schema | ACCEPT | SPEC Section 2.1 requires every delete, retirement, diagnostic demotion, or neutralization to match a row before redress and carries the full schema: `row_id`, `retired_or_deleted_artifact`, `delete_or_retire_wave`, `rebuild_provider_wave`, `proof_command`, `provider_lands_no_later`, `conditional_status`, `consuming_exit_gates`, and `preblock_cluster` (`SPEC.md:187`-`:192`). DISPATCH requires compact plans to bind back to those same fields (`DISPATCH-PROMPT.md:70`-`:76`). | None. |
| Dependency row coverage | ACCEPT | SPEC rows cover CSS broadcast demotion, old CSS proof retirement, provider/template fanout, Pattern H provenance, CSS legacy runtime shim, Decision scaffold, W8/W9 lowerers, FNV quarantine, and W11 no-orphans (`SPEC.md:194`-`:204`). DISPATCH mirrors all active `DEP-*` rows and their consuming waves (`DISPATCH-PROMPT.md:80`-`:90`). | None. |
| Per-wave consumption | ACCEPT | SPEC consumes dependency rows at W1, W3, W4, W5, W6, W7, W8, W9, W10, and W11 (`SPEC.md:281`, `:316`, `:333`-`:334`, `:352`-`:355`, `:373`-`:376`, `:392`, `:410`, `:428`, `:445`, `:465`). DISPATCH mirrors those per-wave consumers (`DISPATCH-PROMPT.md:141`, `:168`, `:183`-`:184`, `:198`-`:200`, `:214`-`:216`, `:228`, `:249`, `:270`, `:297`, `:316`). | None. |
| Normalized pre-block ledger | ACCEPT | The normalized list appears in P3-B, P3-C, P3-E, P3-F, SPEC, and DISPATCH: `28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration` (`p3b-wave-sequencing.md:108`-`:111`; `p3c-falsifiability-gates.md:352`; `p3e-preblocked-ledger.md:35`; `p3f-spec-draft.md:69`-`:70`; `SPEC.md:471`; `DISPATCH-PROMPT.md:335`). | None. |
| W11 no-orphan anti-deferral | ACCEPT | P3-C requires PASS-IMPL V2 to accept every axis or record row-level intrinsic-block proof at HEAD, and states SK-V16 routing is not close evidence (`p3c-falsifiability-gates.md:337`-`:345`). SPEC W11 requires no dependency-table row to lack proof or intrinsic-block evidence and rejects SK-V16 routing as substitute proof (`SPEC.md:457`-`:463`). DISPATCH W11 aborts close on unresolved implementation fixes, measurement reruns, or dependency rows and consumes `DEP-W11-CLOSE-NO-ORPHANS` (`DISPATCH-PROMPT.md:309`-`:316`). | None. |
| Stale regression labels | ACCEPT | Active-surface grep for `Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|W1 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|209\\.\\.213|96/97/98|930\\.281` over P3-A through P3-F, SPEC, and DISPATCH returned no matches. | None. |

## Verification

Commands run:

```sh
git rev-parse --short=9 HEAD
git status --short
sed -n '1,240p' restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md
sed -n '1,520p' restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
sed -n '1,240p' restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md
sed -n '1,280p' restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
sed -n '1,320p' restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md
sed -n '1,620p' restart/skinny/tranches/sk-v15/SPEC.md
sed -n '1,620p' restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
sed -n '1,300p' restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md
sed -n '1,260p' restart/skinny/tranches/sk-v15/research/p3/hardening/V3/CH3.md
rg -n "DEP-W1-CSS-BROADCAST|DEP-W6-CSS-GENERATED-RS|DEP-W6-CSS-SUMMARY-FACT-STREAM|DEP-W3-W6-CSS-PROVIDER-TEMPLATE|DEP-W4-PATTERN-H-PROVENANCE|DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM|DEP-W7-DECISION-SPINE|DEP-W8-LOWERERS-A|DEP-W9-LOWERERS-B|DEP-W10-FNV-QUARANTINE|DEP-W11-CLOSE-NO-ORPHANS|provider_lands_no_later|consuming_exit_gates" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "28\\+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration|provider/runtime/template delete before replacement proof|SK-V16 routing|orphan|PASS-IMPL V2|row-level intrinsic block" restart/skinny/tranches/sk-v15/research/p3/*.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11|W5 dispatches after W1-W4|W6 dispatches after W5|W11 dispatches after W1-W10|W0-W11 consumes|cannot spawn W12|CHALLENGE time" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "Cycle: V1|S-P3 V1|W0-W9|W1-W9|W0 through W9|W1 through W9|P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|209\\.\\.213|96/97/98|930\\.281" restart/skinny/tranches/sk-v15/research/p3/p3a-candidate-shortlist.md restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
```

Observed:

- HEAD was `21ae60663`.
- The unrelated dirty implementation files were present before the audit and were not touched.
- The stale regression-label grep returned no matches in active S-P3 packet surfaces.
- The dependency-row, pre-block, topology, and W11 close-route searches resolved to active SPEC/DISPATCH gates, dependency rows, or explicit rejection clauses.

## Required Edits If REVISE

None. Verdict is ACCEPT.
