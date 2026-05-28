# SK-V15 S-P1 Hardening V2 - Consolidated

Disposition: ACCEPT 6/6.

| Lens group | Result | Notes |
|---|---|---|
| CH1 + CH2 | ACCEPT | Coverage, artifact existence, PMU c/B, and normalized primitive attribution accepted. |
| CH3 + CH4 | ACCEPT | Delta/routing and reproducibility accepted. |
| CH5 + CH6 | ACCEPT | Hidden-coupling and anti-paper-close checks accepted. |

S-P1 §3Z result: LOCKED.

Evidence:

- V1 folded to ACCEPT 6/6 in `hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`.
- V2 confirmed ACCEPT 6/6 with no defects.
- Two consecutive ACCEPT cycles are now present, with zero orphan REVISEs.

S-P2 may consume `evidence/p1e-normalized-attribution.tsv` as the binding empirical floor, plus `evidence/pmu-cpb-summary.tsv` for c/B deltas.
