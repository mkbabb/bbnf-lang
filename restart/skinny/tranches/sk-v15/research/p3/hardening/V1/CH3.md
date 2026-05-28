# SK-V15 S-P3 V1 CH3 REGRESSION

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Agent: CH3 REGRESSION.
Input packet: commit 4fe37c042.
Scope: SPEC, DISPATCH-PROMPT, P3-A through P3-F, PASS-3-SYNTHESIS-PLAN CH3, ORCHESTRATOR 3W/3Z, SK-V15 SYNTHESIS/HANDOFF, P3-E, and REDRESS provider-deletion / historical blocked-route clusters.
Disposition vocabulary: ACCEPT / REVISE.

## Verdict

Overall CH3 verdict: REVISE.

The packet has the right regression posture in several places: P3-B and P3-E both state the no-delete-before-provider rule, P3-B includes an initial NEW-CH3-V5-01 dependency table, and the W1/W3/W4/W5 prose generally avoids the SK-V14 W2R/W4R delete-before-rebuild cycle. V1 still cannot pass CH3 because the fold is not complete. P3-C still binds an older expected wave set while P3-B and SPEC dispatch W0-W9, the dependency table is not folded into SPEC or DISPATCH-PROMPT as a load-bearing dispatch surface, and the pre-blocked REDRESS coverage is inconsistent across P3-E, P3-B, P3-C, and SPEC.

## Findings

| ID | Surface | Disposition | Finding |
|---|---|---|---|
| CH3-V1-1 | P3-C vs P3-B wave set | REVISE | P3-C says P3-B did not exist at authoring time and binds W0, PRUNE-WAVE-A through PRUNE-WAVE-D, and REBUILD-WAVE-E through REBUILD-WAVE-G only. P3-B and SPEC define W0-W9, split Decision Engine into W6 REBUILD-F.1 and W7 REBUILD-F.2, move FNV quarantine to W8, and add W9 close reconciliation. P3-F claims this mapping was adopted, but P3-C itself was not revised. That leaves W7/W8/W9 gate and pre-block ownership ambiguous before dispatch. |
| CH3-V1-2 | NEW-CH3-V5-01 dependency table | REVISE | P3-B emits the required table shape and initial rows, but SPEC and DISPATCH-PROMPT do not carry the table or its columns. SPEC only states the rule globally and checks for no orphan row at W9; DISPATCH-PROMPT only asks the wave to verify a dependency-table row. The canonical dispatch contract therefore lacks the actual table Synthesis 0.5 requires S-P3 to emit. |
| CH3-V1-3 | Pre-blocked REDRESS coverage | REVISE | P3-E covers the main global clusters 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, and FNV quarantine. Coverage is not consistent enough for CH3: P3-E omits the recent 242-247 decoded/string/structural-stream cluster that SPEC and P3-F include, P3-C lists 210-213 but not 183/184 in its provider-deletion row, and P3-B's pre-block table does not explicitly carry 242-247. |
| CH3-V1-4 | W2R/W4R-style provider cycles | ACCEPT | No wave directly authorizes the old cycle. W1 demotes CSS admission without provider deletion, W3 blocks CSS provider/template deletion until W5 typed proof, W4 requires non-writing check or delete-plus-regen proof before destructive root runtime deletion, and W5 retires CSS parser/provider artifacts only with same-wave typed provider proof. This protection must be made load-bearing by the table and P3-C folds above. |
| CH3-V1-5 | Delete before provider proof | REVISE | The intended rule is present, but it is not enforceable from every dispatch surface. A wave agent reading SPEC plus DISPATCH-PROMPT can see that a dependency-table row is required, but cannot see the authoritative retired artifact, delete wave, provider wave, proof command, and provider-no-later evidence rows unless it also reads P3-B. CH3 requires the final packet to prevent this silent regression route without relying on side knowledge. |

## Required Folds

| Fold | Required change | Target surfaces |
|---|---|---|
| F-V2-CH3-1 | Revise P3-C to the actual P3-B/SPEC W0-W9 wave set. Remove the stale "P3-B does not exist" note. Split current REBUILD-WAVE-F into W6 Decision Engine spine and W7 BackendShape lowerers, move FNV quarantine to W8, and add W9 close reconciliation / PASS-IMPL V2 gates. | P3-C, P3-F, SPEC if any references shift |
| F-V2-CH3-2 | Promote the P3-B NEW-CH3-V5-01 table into the final dispatch contract. The table must retain the required columns: retired/deleted artifact, delete/retire wave, rebuild provider wave, proof command, and evidence provider lands no later than delete/retire. | SPEC and DISPATCH-PROMPT; P3-B remains the source ledger |
| F-V2-CH3-3 | Make P3-C fail any delete/retire action without a matching dependency-table row and same-wave or prior-wave provider proof. Include this in each relevant W1/W3/W4/W5/W9 exit gate, not only in prose. | P3-C, SPEC, DISPATCH-PROMPT |
| F-V2-CH3-4 | Normalize the pre-blocked REDRESS cluster list across P3-E, P3-B, P3-C, P3-F, SPEC, and DISPATCH-PROMPT. The shared list must include 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96-98, 183/184/209-213, 215, 242-247, and FNV closed-enum production migration. | P3-E, P3-B, P3-C, P3-F, SPEC, DISPATCH-PROMPT |
| F-V2-CH3-5 | Preserve the W2R/W4R anti-cycle language as an explicit dependency-row invariant: W1 may demote but not delete CSS providers; W3 may neutralize generic fanout but not delete live CSS providers/templates before W5; W4 destructive root runtime deletion requires generator/check proof first; W5 may retire old CSS proof only with typed provider proof in the same wave. | P3-B table, P3-C gates, SPEC wave sections, DISPATCH-PROMPT pre-dispatch checks |

## CH3 Close Condition For V2

V2 can move CH3 to ACCEPT only if the packet has one canonical W0-W9 wave set, one canonical NEW-CH3-V5-01 dependency table visible from the final dispatch surfaces, and one consistent REDRESS pre-block list. The current V1 packet is directionally aligned with the no-regression rule, but the incomplete fold leaves enough ambiguity to reopen the historical provider-deletion route under wave sequencing or gate mismatch.
