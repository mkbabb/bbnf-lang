# SK-V15 S-P3 V3 CH3 REGRESSION

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V3. Lens: CH3.
Date: 2026-05-28.
HEAD: `efe1e4b01`.
Scope: audit the active S-P3 packet for regression against
NEW-CH3-V5-01, SK-V14 W2R/W4R delete-before-provider precedent,
normalized REDRESS pre-blocks, W11 anti-deferral, and no-orphan close
routing.

## Verdict

ACCEPT.

The V3 packet folds the V2 CH3 findings into executable final dispatch
surfaces. `SPEC.md` now carries named dependency rows with the required
schema, `DISPATCH-PROMPT.md` binds wave plans back to those schema fields
before redress, per-wave envelopes consume the relevant `DEP-*` rows, and
W11 blocks close on unresolved dependency rows instead of routing misses to
SK-V16.

## Evidence Table

| check | disposition | evidence | residual risk |
|---|---|---|---|
| Wave graph topology | ACCEPT | P3-B fixes the final order as `W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11` and marks the 12-wave ceiling (`p3b-wave-sequencing.md:17`-`:33`). SPEC dispatch lock preserves the same topological chain: W5 after W1-W4, W6 after W5, and W11 after W1-W10 are resolved (`SPEC.md:38`-`:43`). | None for CH3. |
| W2R/W4R recurrence guard | ACCEPT | SK-V14 W2R showed root-runtime generation could not be required before its W6 owner (`astral/V3/ΩA-coherence-audit.md:12`-`:15`, `:48`-`:57`). SK-V14 W4R showed provider/template deletion must move into the same wave as replacement (`astral/V4/ΩA-coherence-audit.md:13`-`:17`, `:46`-`:58`). SK-V15 carries the same rule: no delete/retirement before rebuild proof (`SPEC.md:140`), provider/template deletion remains blocked until W6 proof (`SPEC.md:315`), and provider/runtime/template delete before replacement proof is pre-blocked (`SPEC.md:481`). | None. |
| Named dependency rows and required schema | ACCEPT | SPEC Section 2.1 requires every delete, retirement, diagnostic demotion, or neutralization to match a row before redress (`SPEC.md:187`-`:190`) and lists the required columns `row_id`, `retired_or_deleted_artifact`, `delete_or_retire_wave`, `rebuild_provider_wave`, `proof_command`, `provider_lands_no_later`, `conditional_status`, `consuming_exit_gates`, and `preblock_cluster` (`SPEC.md:192`). The row set covers CSS broadcast, CSS old proof, provider/template, Pattern H, CSS legacy shim, Decision, W8/W9 lowerers, FNV, and W11 no-orphans (`SPEC.md:194`-`:204`). | None. |
| DISPATCH executability | ACCEPT | DISPATCH requires any delete/retire/demotion/neutralization plan to cite a SPEC dependency row, names the same authoritative schema fields, and rejects compact plans unless they bind back to those fields (`DISPATCH-PROMPT.md:70`-`:76`). Its compact dispatch table mirrors the active rows (`DISPATCH-PROMPT.md:80`-`:90`). | None. |
| Per-wave DEP consumption | ACCEPT | SPEC consumes rows at W1, W3, W4, W5, W6, W7, W8, W9, W10, and W11 (`SPEC.md:281`, `:316`, `:333`-`:334`, `:352`-`:355`, `:373`-`:376`, `:392`, `:410`, `:428`, `:445`, `:464`-`:465`). DISPATCH mirrors the same per-wave row consumption (`DISPATCH-PROMPT.md:141`, `:168`, `:183`-`:184`, `:198`-`:200`, `:214`-`:216`, `:228`, `:249`, `:270`, `:297`, `:315`-`:316`). | None. |
| Pre-block normalization | ACCEPT | The normalized list appears in P3-B/P3-C/P3-E/P3-F, SPEC, and DISPATCH: `28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration` (`p3b-wave-sequencing.md:108`-`:111`; `p3c-falsifiability-gates.md:352`; `p3e-preblocked-ledger.md:35`; `p3f-spec-draft.md:69`-`:70`; `SPEC.md:469`-`:484`; `DISPATCH-PROMPT.md:333`-`:335`). | P3-A's shortlist carries a shorter candidate-local pre-block summary, but it does not contradict the authoritative normalized lists and is not the CH3 final dispatch surface. |
| W11 anti-deferral and no-orphan route | ACCEPT | P3-E blocks closing from future-wave promises and leaving dependency rows orphaned (`p3e-preblocked-ledger.md:247`-`:253`). SPEC W11 requires no dependency-table row to lack proof or intrinsic-block evidence, requires HEAD evidence, rejects SK-V16 routing as repair proof, and consumes every `DEP-*` row including `DEP-W11-CLOSE-NO-ORPHANS` (`SPEC.md:455`-`:465`). DISPATCH W11 aborts close on implementation fixes, measurement reruns, or unresolved dependency rows and refuses deferral to SK-V16 (`DISPATCH-PROMPT.md:309`-`:316`). | None. |

## Verification

Commands run:

```sh
git rev-parse --short=9 HEAD
git status --short
rg -n "DEP-W1-CSS-BROADCAST|DEP-W6-CSS-GENERATED-RS|DEP-W6-CSS-SUMMARY-FACT-STREAM|DEP-W3-W6-CSS-PROVIDER-TEMPLATE|DEP-W4-PATTERN-H-PROVENANCE|DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM|DEP-W7-DECISION-SPINE|DEP-W8-LOWERERS-A|DEP-W9-LOWERERS-B|DEP-W10-FNV-QUARANTINE|DEP-W11-CLOSE-NO-ORPHANS" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "row_id|retired_or_deleted_artifact|delete_or_retire_wave|rebuild_provider_wave|proof_command|provider_lands_no_later|conditional_status|consuming_exit_gates|preblock_cluster" restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "28\\+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration|REDRESS 183/184/209-213|FNV closed enum|242-247|No provider/runtime/template delete before replacement proof|SK-V16 routing|No dependency-table row lacks proof|orphaned" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md restart/skinny/tranches/sk-v15/research/p3/p3f-spec-draft.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "W0 -> W1 -> W2 -> W3 -> W4 -> W5 -> W6 -> W7 -> W8 -> W9 -> W10 -> W11|W1 dispatches after W0|W5 dispatches after W1-W4|W6 dispatches after W5|W11 dispatches after W1-W10|provider.*deletion.*forbidden|deletion remains blocked|delete before replacement|replacement proof" restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
```

Result: HEAD was `efe1e4b01`; unrelated dirty implementation files were
present and not touched. The greps confirmed the named dependency rows,
schema fields, per-wave consumption, normalized pre-block list, provider
ordering, and W11 no-orphan close route in the active packet.
