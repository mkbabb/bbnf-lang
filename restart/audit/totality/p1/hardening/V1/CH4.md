---
agent: CH4
pass: T-P1-excavation
cycle: V1
lens: COST
generated_at: 2026-05-28
disposition: REVISE
audited_artifacts:
  - restart/prompts/ORCHESTRATOR.md
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/skinny/tranches/sk-v15/SYNTHESIS.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-past-corpora.md
---

# T-P1 V1 CH4 Cost Audit

## Verdict

REVISE. The packet has useful cost framing in 1A, 1B, 1C, and 1F, but it does
not satisfy T-P1 CH4 uniformly. PASS-1 CH4 requires every divergence to carry
a realistic LOC-delta estimate and risk class, requires 1E amendment candidates
to state a wave-alignment hint, and marks amendment candidates without
supporting path:line evidence as REVISE
(`restart/prompts/totality/PASS-1-EXCAVATION.md:121`-`123`). The universal
CH4 lens also requires LOC budget, risk class, wave alignment, and hard cap to
be stated and realistic (`restart/prompts/ORCHESTRATOR.md:81`-`87`), and
ORCHESTRATOR §3Z requires hardening findings to fold into the next cycle before
advance (`restart/prompts/ORCHESTRATOR.md:112`-`123`).

This is not a REJECT: the missing fields are repairable table-shape defects.
V1 explicitly expects honest REVISE rather than paper-close
(`restart/prompts/totality/PASS-1-EXCAVATION.md:93`-`100`).

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH4-V1-001 | ACCEPT | 1A, 1B, and 1C divergence tables carry LOC and risk fields. | 1A table has `loc_delta_estimate` and `risk` columns with populated rows at `restart/audit/totality/p1/1A-substrate-evidence.md:80`-`90`. 1B table has `loc_budget` and `risk` columns at `restart/audit/totality/p1/1B-codegen-evidence.md:74`-`88`. 1C table has `loc_budget`, `risk`, and `wave hint` at `restart/audit/totality/p1/1C-runtime-evidence.md:102`-`111`. |
| CH4-V1-002 | REVISE | 1D's divergence table lacks LOC-delta and risk fields, so its four divergence buckets cannot pass the "every divergence" CH4 test. | The table header is only `divergence | count bucket | evidence | V1 impact` at `restart/audit/totality/p1/1D-skinny-lessons.md:150`-`153`; rows at `:154`-`:157` describe implemented, unimplemented, impl-exceeds-spec, and unknown buckets without LOC/risk. |
| CH4-V1-003 | REVISE | 1E's divergence table lacks LOC-delta and risk fields for all 14 lock divergences. | The 1E divergence table header is only `id | locks | divergence | evidence` at `restart/audit/totality/p1/1E-locks-evidence.md:99`-`102`; rows D-1E-V1-01 through D-1E-V1-14 at `:103`-`:116` do not carry LOC budget, risk class, wave hint, or hard cap. |
| CH4-V1-004 | REVISE | 1E amendment candidates do not state wave-alignment hints. | The LOCKS-AMENDMENTS-CANDIDATE table header has `candidate | type | target locks | proposed candidate text | supporting path:line evidence` at `restart/audit/totality/p1/1E-locks-evidence.md:118`-`121`; LAC-1E-V1-01 through LAC-1E-V1-14 at `:122`-`:135` have no wave-alignment column or inline wave receiver. This misses PASS-1 CH4's explicit 1E requirement. |
| CH4-V1-005 | REVISE | 1E mostly cites path:line evidence, but absence/transcript-backed candidates need stricter evidence anchoring before ACCEPT. | LAC-1E-V1-03 relies on "no live `__EAGER_EMPTY_PATH` grep match" without a path:line or captured transcript anchor at `restart/audit/totality/p1/1E-locks-evidence.md:124`. LAC-1E-V1-06 relies partly on "live `find`/`test -d archive` transcript" at `:127`, and LAC-1E-V1-12 relies partly on "live count transcript in this artifact" at `:133`. Each row has some path:line evidence, but the absence/transcript claim must be anchored to an artifact line or replaced with direct path:line evidence. |
| CH4-V1-006 | ACCEPT | 1F cost framing is adequate for CH4 at the divergence-table level. | 1F coherence has `LOC / risk` populated for COH rows at `restart/audit/totality/p1/1F-coherence-scan.md:79`-`94`; 1F anti-pattern has `LOC / risk` at `restart/audit/totality/p1/1F-anti-pattern.md:59`-`80` plus a fuller planning table at `:84`-`:105`; 1F past-corpora has `LOC / risk` at `restart/audit/totality/p1/1F-past-corpora.md:100`-`120` and planning fields at `:124`-`:142`. |
| CH4-V1-007 | REVISE | SK-V15 procedural addenda make the missing 1E wave routing materially risky rather than cosmetic. | SK-V15 adds wave-graph dependency proof, broadcast-admission detection, and gate-exclusion reporting at `restart/skinny/tranches/sk-v15/SYNTHESIS.md:98`-`110`. 1E proposes candidates directly touching broadcast detection, Pattern H, and Lock 14/16 gate exclusions at `restart/audit/totality/p1/1E-locks-evidence.md:128`, `:132`, and `:133`, but without wave hints those candidates are not aligned to the SK-V15 prune/rebuild receivers. |

## Fold Directives

1. Add `loc_delta_estimate` and `risk` columns to `1D-skinny-lessons.md`'s
   `Divergences Catalogued` table. Populate all rows at
   `restart/audit/totality/p1/1D-skinny-lessons.md:154`-`157`; use `0 LOC`
   only for pure pre-block or ledger rows, and make the risk class explicit
   even when reopened-risk rather than implementation-risk is the relevant cost.

2. Add `loc_delta_estimate`, `risk`, `wave_hint`, and `hard_cap` columns to
   `1E-locks-evidence.md`'s `Divergences Catalogued` table. Populate every row
   D-1E-V1-01 through D-1E-V1-14 at
   `restart/audit/totality/p1/1E-locks-evidence.md:103`-`116`. The LOC budget
   may reference an existing 1A/1B/1C/1F estimate only if the row cites that
   artifact path:line.

3. Add a `wave_alignment_hint` column to the 1E
   `LOCKS-AMENDMENTS-CANDIDATE` table and populate every LAC row at
   `restart/audit/totality/p1/1E-locks-evidence.md:122`-`135`. Candidate
   routing should name the receiving totality fold or SK-V15 receiver, for
   example T-P3 3C, PRUNE-WAVE-B, PRUNE-WAVE-D, REBUILD-WAVE-E, or
   REBUILD-WAVE-F where applicable.

4. Normalize 1E amendment evidence. For rows whose support depends on absence
   or command output, either cite a captured transcript line inside 1E or cite
   a concrete path:line plus a verify action. At minimum repair
   LAC-1E-V1-03 (`:124`), LAC-1E-V1-06 (`:127`), and LAC-1E-V1-12 (`:133`).

5. Do not advance T-P1 V1 as CH4-accepted until the above fold lands in the
   next cycle. Per ORCHESTRATOR §3Z, this hardening must be folded into V2
   before convergence can be claimed (`restart/prompts/ORCHESTRATOR.md:112`-`123`).

## Aggregator Note

CH4 disposition is REVISE. The packet is close on 1A/1B/1C/1F, but 1D and 1E
fail the uniform divergence cost requirement, and 1E fails the amendment
wave-alignment requirement. No CH4 REJECT is warranted because the fixes are
explicit table folds, not evidence collapse.
