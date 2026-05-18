# SK-V9 Alpha Hardening V2 Consolidated

Date: 2026-05-18.

Scope: six-lens challenge over the folded SK-V9 Pass Alpha packet after commit
`e3ebe0b4` (`docs(sk-v9-alpha): fold V1 hardening revisions`).

## Verdict

V2 outcome: REVISE.

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 Correctness | REVISE | 96% | Folded. One citation-range defect remained: Alpha-B through Alpha-F used `skinny/RESULTS.md:3-40` for complete-table claims while the main table ends at line 42. |
| CH2 Generality | ACCEPT | 96% | No fold required. Lock 14, grammar-aware telemetry, sidecar evidence-only scope, and candidate-local strictness boundaries verified. |
| CH3 Regression | ACCEPT | 96% | No fold required. REDRESS 73 and Alpha-C historical pre-block carry-forward verified. |
| CH4 Cost | ACCEPT | 96% | No fold required. Candidate status, LOC budgets, <=90 min hard caps, and proof-only retained route verified. |
| CH5 Hidden Coupling | ACCEPT | 97% | No fold required. Typed/source rows, direct/real-typed separation, Track 1/Track 2 independence, and no proxy-performance claims verified. |
| CH6 Next-Tranche Impact | ACCEPT | 96% | No fold required. G-Alpha boundary, Alpha-depth-only planning, doc-link integrity, and no SK-V9 dispatch verified. |

Nominal ACCEPT rate: 5/6. Minimum confidence: 96%. This fails convergence
because CH1 remains REVISE. V3 re-challenge is required after the citation fold.

## Required Fold Applied

Complete-table citations in Alpha-B through Alpha-F now cite
`skinny/RESULTS.md:3-42` instead of `skinny/RESULTS.md:3-40`.

The fold applies to:

- `restart/skinny/tranches/sk-v9/research/alpha/alpha-B-competitor-deltas.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-F-contract-draft.md`

## V3 Re-Challenge Target

V3 must verify the folded citation range plus the V1 folds. The target is
acceptable only if all six lenses return ACCEPT with minimum confidence >=95%,
zero open critical defects, no orphan REVISE, and no SK-V9 `SPEC.md`,
`DISPATCH-PROMPT.md`, or implementation dispatch before G-Alpha.
