---
agent: CH4
pass: T-P1-excavation
cycle: V2
lens: COST
generated_at: 2026-05-21T00:00:00-04:00
disposition: REVISE
audited_artifacts:
  - restart/prompts/totality/PASS-1-EXCAVATION.md
  - restart/prompts/ORCHESTRATOR.md
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

## Lens Basis

`restart/prompts/totality/PASS-1-EXCAVATION.md:121` requires every divergence to carry realistic LOC-delta and risk. `restart/prompts/ORCHESTRATOR.md:86` adds wave alignment, hard cap, and same-wave consumer for each kernel/primitive. V1 consolidation made the required V2 schema explicit: every divergence and amendment candidate must carry `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis` (`restart/audit/totality/p1/hardening/HARDENING-T-P1-V1-CONSOLIDATED.md:57`).

## Findings

| disposition | finding | evidence | required action |
|---|---|---|---|
| ACCEPT | 1A folds CH4 schema into all divergence rows and its Lock 1 amendment candidate. | The 1A divergence table includes `loc_budget`, `risk`, `wave`, `hard_cap`, `same_wave_consumer`, and `evidence_basis` at `restart/audit/totality/p1/1A-substrate-evidence.md:49`; rows `1A-DIV-001` through `1A-DIV-007` carry those fields at `restart/audit/totality/p1/1A-substrate-evidence.md:51-57`. The Lock amendment candidate carries the same schema at `restart/audit/totality/p1/1A-substrate-evidence.md:81-83`. | None for CH4. |
| ACCEPT | 1B folds CH4 schema into all codegen divergences and amendment candidates. | The 1B divergence table has the full schema plus `verify_action` at `restart/audit/totality/p1/1B-codegen-evidence.md:72`; rows `P1-1B-D1` through `P1-1B-D10` carry concrete budgets, caps, consumers, and evidence at `restart/audit/totality/p1/1B-codegen-evidence.md:74-83`. Amendment candidates carry the same CH4 fields at `restart/audit/totality/p1/1B-codegen-evidence.md:106-109`. | None for CH4. |
| ACCEPT | 1C runtime divergences now have realistic numeric budgets, wave boundaries, hard caps, same-wave consumers, and evidence basis. | The 1C divergence table header carries the full CH4 schema at `restart/audit/totality/p1/1C-runtime-evidence.md:94`, and `1C-D1` through `1C-D7` populate it at `restart/audit/totality/p1/1C-runtime-evidence.md:96-102`. The multi-grammar row is bounded by a per-wave cap rather than the open-ended V1 bucket at `restart/audit/totality/p1/1C-runtime-evidence.md:99`. | None for CH4. |
| ACCEPT | 1D converts route lessons into budgeted divergence rows and explicitly handles SIMD/ASM same-wave consumer risk. | The 1D divergence table carries the full CH4 schema at `restart/audit/totality/p1/1D-skinny-lessons.md:98`. The SIMD/ASM row names `CSS/JSON production scanner row, measured rejection, or deletion` as the consumer and caps the work at 1000 LOC at `restart/audit/totality/p1/1D-skinny-lessons.md:104`. Sheets/BBNF-self generality work is capped and consumer-routed at `restart/audit/totality/p1/1D-skinny-lessons.md:105`. | None for CH4. |
| ACCEPT | 1E remains the strongest cost template: every lock divergence and amendment candidate has the required schema. | Lock divergences include the full schema at `restart/audit/totality/p1/1E-locks-evidence.md:73`; rows `D-1E-01` through `D-1E-11` populate it at `restart/audit/totality/p1/1E-locks-evidence.md:75-85`. Lock amendment candidates include and populate the same fields at `restart/audit/totality/p1/1E-locks-evidence.md:89-101`, including Lock 16 same-wave production-row routing at `restart/audit/totality/p1/1E-locks-evidence.md:100`. | None for CH4. |
| REVISE | 1F has complete ID-keyed cost metadata, but the three `Divergences Catalogued` tables still do not carry the schema inline. | `1F-anti-pattern.md` lists divergences with only `LOC / risk` at `restart/audit/totality/p1/1F-anti-pattern.md:43-53`, then supplies the full schema in `V2 Planning Metadata` for AP-001 through AP-010 at `restart/audit/totality/p1/1F-anti-pattern.md:57-68`. `1F-coherence-scan.md` repeats that pattern at `restart/audit/totality/p1/1F-coherence-scan.md:47-54` and `restart/audit/totality/p1/1F-coherence-scan.md:60-69`. `1F-past-corpora.md` repeats it at `restart/audit/totality/p1/1F-past-corpora.md:56-63` and `restart/audit/totality/p1/1F-past-corpora.md:67-74`. | Either merge the V2 metadata columns into each 1F divergence row or state directly above each divergence table that the ID-keyed `V2 Planning Metadata` table is the authoritative CH4 carrier. |
| ACCEPT | Historical pre-block and closed-authority rows have justified non-implementation dispositions rather than fake implementation budgets. | 1F past-corpora treats rejected alternates and closed diagnosis rows as 0-LOC ledger constraints with evidence and consumers, e.g. PC-001 at `restart/audit/totality/p1/1F-past-corpora.md:69`, PC-003 at `restart/audit/totality/p1/1F-past-corpora.md:71`, and SKV13-PB-008 at `restart/audit/totality/p1/1F-past-corpora.md:76`. 1F anti-pattern also marks closed authority rows AP-006/AP-007 as 0 LOC with `none` consumers at `restart/audit/totality/p1/1F-anti-pattern.md:63-64`. | Preserve this pattern. It prevents pre-block ledger rows from becoming bogus implementation work. |
| ACCEPT | No amendment candidate without path:line evidence remains in the folded V2 set. | 1A's amendment cites lock, REDRESS, RESULTS, and live runtime paths at `restart/audit/totality/p1/1A-substrate-evidence.md:83`. 1B's amendment candidates cite ARCH, passes, codegen, and diagnostics paths at `restart/audit/totality/p1/1B-codegen-evidence.md:108-109`. 1E's amendment candidates all carry supporting evidence and `evidence_basis` at `restart/audit/totality/p1/1E-locks-evidence.md:91-101`. | None for CH4. |
| REJECT | No CH4 rejection. | The required metadata is present for every divergence/amendment ID either inline or via ID-keyed V2 planning metadata, and non-implementation rows are explicitly budgeted as 0 LOC or audit-only constraints. | Not applicable. |

## Coverage Check

| artifact | divergence rows | amendment rows | CH4 status |
|---|---:|---:|---|
| `1A-substrate-evidence.md` | 7 | 1 | ACCEPT |
| `1B-codegen-evidence.md` | 10 | 2 | ACCEPT |
| `1C-runtime-evidence.md` | 7 | 0 | ACCEPT |
| `1D-skinny-lessons.md` | 6 | 0 | ACCEPT |
| `1E-locks-evidence.md` | 11 | 11 | ACCEPT |
| `1F-anti-pattern.md` | 9 divergence IDs plus 10 planning IDs | 0 | REVISE for schema indirection only |
| `1F-coherence-scan.md` | 6 divergence IDs plus 9 planning IDs | 0 | REVISE for schema indirection only |
| `1F-past-corpora.md` | 6 divergence IDs plus 8 planning IDs | 0 | REVISE for schema indirection only |

## Cycle Verdict

REVISE. V2 materially fixes the V1 CH4 failure: the required cost fields now exist for all audited divergence/amendment IDs, and producer/SIMD/pre-block rows are routed to same-wave consumers or explicit non-implementation dispositions. The only remaining CH4 defect is structural: the 1F files make downstream readers join `Divergences Catalogued` rows to separate `V2 Planning Metadata` tables by ID. That is usable and not a rejection, but it should be made explicit or normalized inline before convergence.
