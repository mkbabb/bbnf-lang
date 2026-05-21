---
agent: CH4
pass: T-P3-synthesis
cycle: V1
lens: COST
generated_at: 2026-05-21T15:29:59-04:00
disposition: REVISE
audited_artifacts:
  - restart/prompts/totality/PASS-3-SYNTHESIS.md
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p1/1A-substrate-evidence.md
  - restart/audit/totality/p1/1B-codegen-evidence.md
  - restart/audit/totality/p1/1C-runtime-evidence.md
  - restart/audit/totality/p1/1D-skinny-lessons.md
  - restart/audit/totality/p1/1E-locks-evidence.md
  - restart/audit/totality/p1/1F-anti-pattern.md
  - restart/audit/totality/p1/1F-coherence-scan.md
  - restart/audit/totality/p1/1F-past-corpora.md
  - restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md
  - restart/audit/totality/p2/2A-sota-landscape.md
  - restart/audit/totality/p2/2B-primitive-vocabulary.md
  - restart/audit/totality/p2/2C-grammar-neutrality.md
  - restart/audit/totality/p2/2D-cost-model.md
  - restart/audit/totality/p2/2E-host-arch-esoterica.md
  - restart/audit/totality/p2/2F-parse-that-gaps.md
  - restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md
  - restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md
  - restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md
  - restart/audit/totality/p2/hardening/HARDENING-T-P2-V5-CONVERGED.md
---

## Lens Basis

PASS-3 requires every artifact to include consequences with cost and propagation,
including how many surfaces a delta touches (`restart/prompts/totality/PASS-3-SYNTHESIS.md:81`-`90`).
CH4 is stricter: every delta must state a LOC budget, propagation cost, risk
class, and wave alignment; 3B NEW waves must carry a same-wave consumer; 3C
dispositions must be realistic (`restart/prompts/totality/PASS-3-SYNTHESIS.md:118`-`120`).

The T-P1/T-P2 records provide the usable cost schema. T-P1 1B uses
`loc_budget`, `risk`, `wave`, `hard_cap`, and `same_wave_consumer` for codegen
divergences (`restart/audit/totality/p1/1B-codegen-evidence.md:73`-`84`). T-P2
then formalizes a per-technique admission ledger with `same_wave_consumer_path`,
`loc_budget`, `risk_class`, rollback, abrogate threshold, substrate target,
retention lifetime, and policy owner (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:159`-`180`).
The V3/V4 addenda make this concrete with candidate-level budgets and first
consumers (`restart/audit/totality/p2/T-P2-V3-FOLD-ADDENDUM.md:89`-`101`;
`restart/audit/totality/p2/T-P2-V4-FOLD-ADDENDUM.md:47`-`58`).

## Verdict

REVISE.

The V1 T-P3 packet is directionally useful and not a cost paper-close in 3B/3E,
but it is not CH4-acceptable. Four of six substantive artifacts do not attach
per-delta LOC budgets, propagation counts, risk classes, and wave alignments.
3B's NEW waves mostly satisfy the same-wave-consumer rule, and 3C's lock
dispositions are plausible as lock-text constraints rather than implementation
admissions. The packet still needs a V2 cost fold before it can be accepted.

## Findings

| disposition | finding | evidence | required V2 correction |
|---|---|---|---|
| REVISE | 3A has ten architecture deltas but no per-delta LOC budget, risk class, or wave alignment. | The proposed delta table lists ARCH-3A-D01 through D10 with rationale only (`restart/audit/totality/p3/3A-architecture-synthesis.md:31`-`42`). The consequence table has propagation counts, but its cost column is prose such as "requires downstream lock/status docs" or "adds schema and freshness burden", not numeric LOC/risk/wave metadata (`restart/audit/totality/p3/3A-architecture-synthesis.md:46`-`57`). | Add `loc_budget`, `propagation_surfaces`, `risk_class`, and `wave_alignment` for every ARCH-3A-Dxx row. D09/D10 also need explicit consumer/receiver gates because primitives and parse-that imports inherit the T-P2 same-wave consumer discipline. |
| ACCEPT | 3B NEW waves carry same-wave consumers and mostly usable cost ranges. | MP.NW0 through MP.NW11 each have LOC/risk/propagation text and a same-wave consumer column (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:109`-`122`). Examples include CSS feature rows, the 51-row strict sonic report, generated non-JSON rows, bounded resolver reports, and row movement or architectural-block evidence. | Preserve this structure. It is the closest V1 artifact to CH4 acceptance. |
| REVISE | 3B's proposed delta table still lacks explicit propagation cost per delta. | MP-3B-D1 through D9 include LOC/risk/wave alignment in one column (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:126`-`136`), but propagation is not consistently numeric there. Some consumers are also broad process receivers, e.g. "implementation dispatch and G-Omega sign-off" (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:129`). | Add a numeric `propagation_surfaces` cell to MP-3B-D1 through D9, and keep same-wave consumers concrete where a NEW wave can move implementation or gate state. |
| REVISE | 3C dispositions are plausible, but the disposition matrix is undercosted. | 3C dispositions all land as ACCEPT or MODIFY (`restart/audit/totality/p3/3C-locks-crystallisation.md:42`-`50`), and the matrix routes all 41 candidates without silent drops (`restart/audit/totality/p3/3C-locks-crystallisation.md:54`-`96`). However, neither the disposition matrix nor the proposed delta table carries per-candidate LOC budget, propagation count, risk class, or wave alignment (`restart/audit/totality/p3/3C-locks-crystallisation.md:54`-`107`). The consequences section gives only aggregate costs for Lock 14, Lock 16, and cost-model fold work (`restart/audit/totality/p3/3C-locks-crystallisation.md:109`-`115`). | Add a 3C cost/disposition ledger per hunk or candidate with `lock_hunk`, `candidate_ids`, `loc_budget`, `propagation_surfaces`, `risk_class`, `wave_alignment`, and `same_wave_consumer_or_gate`. Keep ACCEPT/MODIFY only where the text is clearly "lock constraint accepted", not "implementation admitted". |
| ACCEPT | 3C's Lock 16 text remains realistic as a constraint because it refuses prerequisite-only and orphan primitive closure. | The proposed diff requires primitive manifest rows with same-wave production consumer, expected row/feature gate, LOC/risk, rollback, abrogate threshold, and final disposition (`restart/audit/totality/p3/3C-locks-v+1-diff.md:336`-`345`). It states `escape_mask_64` admits a row only when a JSON/CSS string or escape consumer wires it in the same wave (`restart/audit/totality/p3/3C-locks-v+1-diff.md:353`-`360`), and requires source-present primitives to be wired, deleted, scalar-delegated, or architecturally blocked (`restart/audit/totality/p3/3C-locks-v+1-diff.md:362`-`369`). | Preserve the non-admission wording, but tie each accepted/modified Lock 16 candidate back to the concrete T-P2 V3/V4 ledger rows and budgets. |
| REVISE | 3D has no numeric cost model for its ten folds. | FOLD-3D-001 through FOLD-3D-010 are listed with rationale and impact, not LOC/risk/wave/propagation fields (`restart/audit/totality/p3/3D-skinny-fold.md:64`-`77`). Its consequences table groups deltas and uses qualitative phrases such as "documentation delta is small", "high verification cost", and "medium-to-high implementation risk" (`restart/audit/totality/p3/3D-skinny-fold.md:91`-`100`). | Split the grouped consequences into one row per FOLD-3D-xxx with numeric LOC budget, propagation surface count, risk class, wave alignment, and receiver gate. |
| ACCEPT | 3E mostly satisfies CH4 at the per-delta consequence level. | 3E-D01 through D08 have per-delta consequence rows with LOC ranges, risk classes, wave hints, and propagation counts (`restart/audit/totality/p3/3E-grammar-generalisation.md:147`-`156`). | Use 3E's consequence table as a template for 3A/3D/3F, but tighten vague cells. |
| REVISE | 3E still leaves one cost tail too vague for CH4 acceptance. | 3E-D06 says "120-260 doc/test LOC now; higher generated-fixture cost later" without a later cap or wave alignment (`restart/audit/totality/p3/3E-grammar-generalisation.md:154`). Several rows say implementation LOC lands "later" without a hard receiver budget (`restart/audit/totality/p3/3E-grammar-generalisation.md:151`-`156`). | Add a hard cap or explicit "not budgeted in this T-P3 delta" receiver for later generated-fixture work, and name the exact Omega/S-P3 wave alignment for 3E-D06. |
| REVISE | 3F has eleven migration/handoff deltas but no per-delta cost framing. | 3F-MIG-001 through 006 and 3F-HANDOFF-001 through 005 are listed with source and rationale only (`restart/audit/totality/p3/3F-migration-handoff.md:73`-`87`). The consequences table is category-level and omits LOC budgets, numeric propagation, risk classes, and wave alignment per delta (`restart/audit/totality/p3/3F-migration-handoff.md:157`-`165`). | Add cost columns to each 3F delta. Handoff-only deltas should be doc LOC with G3/Omega CRUD-4 alignment; migration rows touching generated providers, decision engine, and primitives need implementation receiver waves and same-wave consumer/gate fields. |
| REVISE | V1 does not consistently carry propagation as "how many surfaces", as PASS-3 requires. | PASS-3 defines propagation as how many surfaces a delta touches (`restart/prompts/totality/PASS-3-SYNTHESIS.md:81`-`85`). 3A and 3E provide surface counts (`restart/audit/totality/p3/3A-architecture-synthesis.md:48`-`57`; `restart/audit/totality/p3/3E-grammar-generalisation.md:149`-`156`), while 3C and 3F give only aggregate propagation (`restart/audit/totality/p3/3C-locks-crystallisation.md:115`; `restart/audit/totality/p3/3F-migration-handoff.md:159`-`165`). | Normalize every consequence table to `propagation_surfaces: <number> (<surface names>)`, not only prose. |

## Required Repairs

1. Add this shared cost schema to every 3X proposed delta or consequence row:
   `loc_budget`, `propagation_surfaces`, `risk_class`, `wave_alignment`,
   `same_wave_consumer_or_receiver`, and `hard_cap_or_abrogate_gate` where an
   implementation/gate path is implied.

2. Backfill 3A, 3D, and 3F from aggregate prose into per-delta rows. Qualitative
   terms such as "small", "medium", "later", "high burden", and "future wave"
   are not CH4-acceptable unless paired with numeric budgets and a named receiver.

3. Preserve 3B's NEW-wave consumer column, but add propagation counts to MP-3B-D1
   through D9 and make any process-level consumer concrete enough to be checked
   in the same wave.

4. Add a 3C cost/disposition ledger. For each ACCEPT/MODIFY hunk, state whether
   the cost is doc-only, gate/report, generated-runtime, source, or bench work;
   state the LOC budget and risk class; state how many surfaces propagate; and
   bind source-present primitives to the T-P2 V3/V4 state machine rather than
   treating ACCEPT as implementation admission.

5. Tighten 3E-D06 and any "later" implementation tails with a capped receiver
   wave or an explicit non-budgeted handoff gate.

## Cycle Verdict

REVISE. V1 has enough cost discipline to fold forward, especially in 3B NEW
waves and 3E consequences, but it fails the CH4 requirement that every delta
carry LOC budget, propagation cost, risk class, and wave alignment.
