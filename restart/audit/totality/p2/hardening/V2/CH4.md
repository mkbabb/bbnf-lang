# T-P2 V2 CH4 Cost / Implementation Realism

Pass: T-P2 Research.
Cycle: V2.
Lens: CH4 COST.
Date: 2026-05-21.

## Verdict

REVISE.

V2 is a real improvement over V1: it adds the shared admission schema, forbids
proof-only/support-only closure, records the source-present SIMD/ASM close-state
enum, and makes the decision-engine route fail closed on several concrete
conditions. It is not yet fully gate-consumable for T-P3 because the owner
dossiers do not consistently instantiate the full per-technique ledger fields
they require, and two abrogate gates still defer their numeric threshold to an
unnamed cap or future SPEC budget.

## Findings

1. The shared ledger schema now has the right CH4 fields, but several
   per-technique rows are summaries rather than executable cost rows.

   `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` requires
   `scalar_reference`, `checkasm_or_parity_command`, `same_wave_consumer_path`,
   `loc_budget`, `risk_class`, `rollback_path`, `abrogate_threshold`,
   `admissibility_state`, `substrate_target`, `retention_lifetime`, and
   `policy_owner` (`Per-Technique Admission Ledger`, lines 159-180). 2D
   largely instantiates this shape for the decision-engine rows
   (`restart/audit/totality/p2/2D-cost-model.md` lines 77-87). By contrast,
   2B's primitive ledger
   (`restart/audit/totality/p2/2B-primitive-vocabulary.md` lines 136-148),
   2E's hardware-candidate table
   (`restart/audit/totality/p2/2E-host-arch-esoterica.md` lines 129-140), and
   2F's parse-that ledger
   (`restart/audit/totality/p2/2F-parse-that-gaps.md` lines 183-218) still omit
   row-local LOC/risk/rollback/abrogate values and often leave scalar/checkasm
   details as prose prerequisites.

2. The admissibility state vocabulary is not yet normalized.

   The addendum defines the state machine
   `source_backed -> scalar_backed -> checkasm_backed -> micro_proven ->
   production_wired -> row_admitted | measured_rejected | architectural_block`
   (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` lines 152-157). The
   dossier rows also use `conditional`, `conditional-high-risk`, `inventory`,
   `partial`, `ADMITTED-EVIDENCE`, and `NOT-VALIDATED`
   (`restart/audit/totality/p2/2B-primitive-vocabulary.md` lines 142-148;
   `restart/audit/totality/p2/2C-grammar-neutrality.md` lines 111-124;
   `restart/audit/totality/p2/2F-parse-that-gaps.md` lines 210-218). Those are
   useful dispositions, but they should not occupy the `admissibility_state`
   field without a mapping.

3. Source-present SIMD/ASM orphan handling is acceptable.

   The addendum's close enum is explicit: `wired`, `deleted`,
   `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`, and
   `inventory_demoted_with_evidence` is historical only
   (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` lines 194-206). 2B
   and 2E repeat this and name the current source-present candidates and
   dispositions (`restart/audit/totality/p2/2B-primitive-vocabulary.md` lines
   150-171; `restart/audit/totality/p2/2E-host-arch-esoterica.md` lines
   142-164). This satisfies the V1 CH4 orphan-kernel redress.

4. No support-only or proof-only route is admitted.

   V2 consistently treats parity, correctness fixes, microbench wins, and
   source presence as prerequisites or architecture pressure, not admission.
   The strongest examples are 2A's REDRESS 121-127 taxonomy and candidate-state
   table (`restart/audit/totality/p2/2A-sota-landscape.md` lines 103-125),
   2B's statement that pre-`production_wired` states are non-admitting
   (`restart/audit/totality/p2/2B-primitive-vocabulary.md` lines 121-134), and
   2F's rejection of proof-only SIMD/ASM primitives for parse-that gaps
   (`restart/audit/totality/p2/2F-parse-that-gaps.md` lines 145-155). This part
   is CH4-acceptable.

5. Decision-engine abrogate gates are partly concrete, but not complete.

   2D gives concrete fail-closed thresholds for CSP (`>1s per grammar`), stale
   cost evidence (`>30%`), any row regression, and any parity/checkasm/equality
   failure (`restart/audit/totality/p2/2D-cost-model.md` lines 136-146). The
   e-graph gate still says only "node or iteration cap exceeded" without the
   cap value, and generated LOC growth still says "exceeds SPEC wave budget"
   without naming the budget or resolving it to each row's LOC ceiling
   (`restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md` lines 212-219;
   `restart/audit/totality/p2/2D-cost-model.md` lines 140-143). Those two gates
   remain too elastic for CH4 acceptance.

## Required Redress If Any

1. Populate the minimum ledger rows in 2B, 2E, and 2F, or move them into one
   shared table, with the full V2 columns and row-local values for scalar
   reference, strict checkasm/parity command, same-wave consumer, expected row
   gate, LOC budget, risk class, rollback path, abrogate threshold,
   admissibility state, substrate target, retention lifetime, and policy owner.

2. Reserve `admissibility_state` for the addendum's state-machine values. Put
   labels such as `conditional`, `inventory`, `NOT-VALIDATED`, and
   `conditional-high-risk` in a separate disposition/blocker field, or define an
   explicit mapping to the state enum.

3. Make the remaining decision-engine abrogate thresholds numeric or
   dereferenceable: e-graph node/iteration/memory caps, and generated LOC growth
   as either the row's upper `loc_budget` bound or a named SPEC budget with file
   and section reference.

## Evidence Checked

- `restart/prompts/totality/PASS-2-RESEARCH.md`, especially CH4 scope and
  pass-convergence requirements.
- `restart/audit/totality/p2/hardening/V1/CH4.md` and
  `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`.
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- `restart/audit/totality/p2/2A-sota-landscape.md` through
  `restart/audit/totality/p2/2F-parse-that-gaps.md`.
- Skinny scoping cross-checks in `restart/skinny/tranches/sk-v13/HANDOFF.md`
  lines 145-166 and `restart/skinny/tranches/sk-v13/SYNTHESIS.md` lines 81-93.
