# CH4 COST

## Lens

CH4 COST: every grounded primitive or route must carry scalar reference,
checkasm/parity, hardware gate, same-wave consumer, row movement, realistic
LOC/risk, and wave owner. Orphan kernels, citation-only primitives, and
uncosted rewrites are not admissible.

## Disposition

REVISE.

The packet has the right admission posture in several places: 2B defines the
scalar/differential/hardware/consumer/row-movement cell set, and 2E treats ISA
features as gates rather than admission. But the V1 dossiers do not carry a
complete cost/admission manifest across the grounded primitive and rebuild
routes. LOC, risk class, wave owner, and hard-cap feasibility are missing or
implicit for too many rows.

## Critical Findings

| id | severity | finding | evidence |
|---|---:|---|---|
| CH4-V1-01 | critical | The primitive manifest omits required LOC/risk/wave-owner cost fields. 2B correctly requires scalar oracle, strict differential, hardware gate, same-wave consumer, and row movement, but its manifest stops there; 2E's table likewise has abstract primitive, hardware gate, scalar/checkasm plan, and same-wave consumer, but no adoption LOC/risk/wave owner. | `restart/audit/totality/p2/2B-primitive-vocabulary.md:90`-`100`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:171`-`174`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:67`-`78` |
| CH4-V1-02 | critical | The Decision Engine / five-lowerer rebuild is a large route with no realistic cost budget. 2D asks for active e-graph rewrites, non-tautological CSP, real lowerer output for all five shapes, and aarch64 close evidence, but does not state LOC, risk class, hard-cap feasibility, or wave owner per lowerer. | `restart/audit/totality/p2/2D-cost-model.md:44`-`50`, `restart/audit/totality/p2/2D-cost-model.md:63`-`64`, `restart/audit/totality/p2/2D-cost-model.md:92`-`96`, `restart/audit/totality/p2/2D-cost-model.md:102`-`106` |
| CH4-V1-03 | critical | Grammar-neutral rebuild routes are under-costed. 2C proposes generated provider manifests, CSS typed value/document generation, Pattern H provenance across the 67-file runtime census, and full-surface Lock 14 scans, but does not attach LOC/risk/wave owner or same-wave row movement per route. | `restart/audit/totality/p2/2C-grammar-neutrality.md:44`-`48`, `restart/audit/totality/p2/2C-grammar-neutrality.md:127`-`131`, `restart/audit/totality/p2/2C-grammar-neutrality.md:137`-`141` |
| CH4-V1-04 | high | parse-that gap routes name upstream/vendor direction without complete admission cost. Regex/HIR, SIMD scan, string/UTF-8, float, CSS typed parsing, and generator integration are classified, but the per-gap rows lack LOC/risk/wave owner and do not consistently name row-local movement before adoption. | `restart/audit/totality/p2/2F-parse-that-gaps.md:33`-`40`, `restart/audit/totality/p2/2F-parse-that-gaps.md:71`-`78`, `restart/audit/totality/p2/2F-parse-that-gaps.md:107`-`111`, `restart/audit/totality/p2/2F-parse-that-gaps.md:117`-`120` |
| CH4-V1-05 | high | 2A grounds targeted parser primitive candidates but defers their admission cost to later dossiers/gates. The sonic-rs leaf transfer is named, and the Lock 16 amendment lists scalar/checkasm/hardware/consumer/row movement, but no per-primitive scalar reference, gate, LOC/risk, wave owner, or named row is carried in 2A. | `restart/audit/totality/p2/2A-sota-landscape.md:68`, `restart/audit/totality/p2/2A-sota-landscape.md:94`-`98`, `restart/audit/totality/p2/2A-sota-landscape.md:104`-`108` |
| CH4-V1-06 | medium | Positive control: the packet does reject several orphan/citation-only primitives, which keeps this at REVISE rather than REJECT. 2B rejects LD4 admission by citation and FSM/frame-stack source-only contracts; 2E blocks PMULL/CSSC/SVE2 feature-bit promotion without consumer and row-local movement. | `restart/audit/totality/p2/2B-primitive-vocabulary.md:65`-`67`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:136`-`144`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:71`-`78`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:117`-`120` |

## Evidence Inspected

- `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md:47`-`55`
- `restart/prompts/totality/PASS-2-RESEARCH.md:117`-`120`
- `restart/prompts/ORCHESTRATOR.md:81`-`88`, `restart/prompts/ORCHESTRATOR.md:204`-`212`
- `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:95`-`97`, `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md:107`-`118`
- `restart/audit/totality/p2/2A-sota-landscape.md`
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`
- `restart/audit/totality/p2/2C-grammar-neutrality.md`
- `restart/audit/totality/p2/2D-cost-model.md`
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`
- `restart/audit/totality/p2/2F-parse-that-gaps.md`

## Fold Requirements

1. Add a uniform cost/admission manifest row for every grounded primitive and
   rebuild route: scalar reference, parity/checkasm command or equivalent,
   hardware gate, same-wave consumer, row movement target, LOC estimate, risk
   class, wave owner, and hard-cap fit.
2. For 2D, split the Decision Engine and five lowerers into independently
   costed W7/W8/W9 units. Each unit must state whether it is implemented,
   gate-consumed rejected, scalar-delegated, blocked, or deleted.
3. For 2C and 2F, cost the generated provider, Pattern H provenance, CSS typed
   provider, regex/HIR, SIMD, string/UTF-8, float, and CSS value routes
   separately. Do not let "generated provider" or "vendor in bbnf-simd" stand
   as a bulk rewrite without owner and LOC/risk.
4. For 2A, either remove primitive-admission language from SOTA rows or attach
   the full manifest fields to each transferred parser leaf.
5. Preserve the current rejection of citation-only PMULL/CSSC/LD4/SVE2 and
   source-only FSM/frame-stack routes unless a same-wave consumer and row-local
   movement are supplied.

## Blocks T-P2 V1 Convergence

Yes. CH4 blocks V1 convergence until the packet carries complete cost and
admission details for the grounded primitives and rebuild routes. The blockers
are foldable in V2 because most dossiers already identify the right admission
cells and reject paper-close primitives; the missing piece is the concrete
cost/wave-owner layer.
