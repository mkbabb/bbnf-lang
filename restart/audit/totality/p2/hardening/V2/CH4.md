# CH4 COST

Lens name: CH4 COST.

Disposition: `ACCEPT`.

## Critical Findings

| id | severity | finding | evidence | required action |
|---|---|---|---|---|
| CH4-V2-00 | none | No blocking CH4 cost defect found. The V2 packet supplies LOC estimate, risk class, wave owner, hard-cap fit, admission gate, verification action, and close status for grounded primitive and rebuild routes. SIMD/ASM/primitive rows also carry scalar reference, parity/checkasm, hardware gate, same-wave consumer, and row-movement target where applicable. | Required fields are defined by `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:27`-`64` and the CH4 V2 check is scoped at `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md:48`-`50`. Dossier evidence: `2B-primitive-vocabulary.md:101`-`115`, `2B-primitive-vocabulary.md:144`-`155`; `2C-grammar-neutrality.md:59`-`75`, `2C-grammar-neutrality.md:144`-`149`; `2D-cost-model.md:57`-`68`, `2D-cost-model.md:70`-`76`; `2E-host-arch-esoterica.md:67`-`82`; `2F-parse-that-gaps.md:71`-`80`. | None. |

## Evidence Inspected

- Challenge and fold authorities: `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md:24`-`38`, `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md:48`-`50`, and `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md:17`-`24`, `:27`-`:68`.
- V1 CH4 obligation: `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md:45`-`49`, which required scalar reference, parity/checkasm, hardware gate, same-wave consumer, row movement, LOC estimate, risk class, wave owner, and hard-cap fit.
- `2A-sota-landscape.md`: transferred parser leaf row now carries cost and owner fields at `:53`; diagnostic CSS comparator rows route provider costs to 2C/2F rather than claiming adoption in 2A at `:56`-`:58`.
- `2B-primitive-vocabulary.md`: required primitive manifest cells are enumerated at `:101`-`:115`; primitive and macro-family route rows carry scalar/checkasm/hardware/consumer/movement plus LOC/risk/owner/cap/status at `:144`-`:155`.
- `2C-grammar-neutrality.md`: generated-provider, CSS typed-provider, Pattern H, Sheets, BBNF-self, full-surface Lock 14 scan, and onboarding routes carry the V2 row shape at `:59`-`:75` and LAC route costs at `:144`-`:149`.
- `2D-cost-model.md`: grounded Decision Engine and BackendShape rows carry the required cost/admission fields at `:57`-`:68`; the prior bulk lowerer route is split into W7 Decision Engine, W8 EagerTape/OffsetTape, and W9 EventTape/SinkOnly/CollapsedStage/all-five guard work units at `:70`-`:76`.
- `2E-host-arch-esoterica.md`: every host primitive row states the anti-paper-close manifest, including scalar reference, parity/checkasm, hardware gate, same-wave consumer, row movement, LOC, risk, owner, hard-cap fit, and close status at `:67`-`:82`.
- `2F-parse-that-gaps.md`: each parse-that gap row states owner, scalar oracle, parity/checkasm command, hardware gate, same-wave consumer, row movement target, verification action, close status, LOC, risk, wave owner, and hard-cap fit at `:71`-`:80`.

## Cost-Lens Assessment

The V2 fold addresses the V1 CH4 defects.

1. Primitive manifests are now explicit. 2B carries the schema and route table for scalar-backed primitives, source-present macro families, LD4, and PMULL/CSSC/SVE2 candidates. 2E repeats the same discipline for host-architecture primitives and keeps ISA bits as gates, not admission.
2. Rebuild routes are no longer bulk-costed. 2C separates generated provider manifest, CSS typed provider, Pattern H provenance, full-surface Lock 14 scan, and future grammar onboarding. 2F separates regex/HIR, runtime regex, SIMD scan, string/UTF-8, float, CSS value parsing, generator integration, and CSS broadcast gates.
3. Decision Engine and lowerer ownership is split cleanly. W7 owns e-graph/cost/CSP activation; W8 owns EagerTape and OffsetTape; W9 owns EventTape, SinkOnly, CollapsedStage, and the exact five-shape guard. Each unit carries LOC estimate, risk, owner, hard-cap fit, admission gate, verification action, and close status.
4. Hard-cap language is realistic enough for a research dossier. Large routes are marked conditional, blocked, or scoped to one receiver or lowerer slice; no row claims a broad generated-provider rewrite, full CSSOM rewrite, or all-lowerer implementation fits as a single uncapped action.

## Fold Requirements

None. CH4 has no REVISE or REJECT fold requirement for T-P2 V2.

## Convergence Impact

CH4 does not block T-P2 V2 convergence. If the other V2 challenge lenses also accept, this can count as the first clean T-P2 hardening cycle only; `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md:37`-`38` still requires a second consecutive clean challenge cycle before normal §3Z convergence.
