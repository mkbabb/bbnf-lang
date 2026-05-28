# Alpha-D - Validated / Invalidated Ledger - SK-V16 V1

Pass: Pass Alpha. Cycle: SK-V15 -> SK-V16.
Date: 2026-05-28.
Scope: durable wins, invalidated claims, and still-open implementation gaps.
Output: this file.

## Validated

| Surface | Validated state |
|---|---|
| JSON | 51 / 51 strict measured rows sustained. |
| Lock gates | default `(cd skinny && cargo xtask gate-json --check-results)` passes after W11 Lock 14 accounting repair. |
| Pattern H provenance | 67 runtime files and line-1 provenance scan pass. |
| Decision Engine | W7 e-graph/CSP/generated-selection proof admitted. |
| BackendShape lowerers | all five lowerer proofs admitted. |
| FNV quarantine | bench-only quarantine admitted; production migration blocked. |

Evidence:
`restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md:13-20` and
`restart/skinny/tranches/sk-v15/research/w11/skv15-W11-redress.md:35-57`.

## Invalidated Or Non-Admitted

| Surface | Current state |
|---|---|
| CSS L4 SOTA | non-admitted; W11 retime returns `admitted_rows=0`. |
| CSS legacy generated proof | retired from live admission. |
| CSS dirty generated runtime files | pre-existing dirty state, not close proof. |
| Full Pattern H collapse | not proven; only provenance discipline closed. |
| Grammar-driven inflection | not reached. |

## Still Open

| Gap | Receiver constraint |
|---|---|
| CSS grammar-derived provider | derive from `grammar/css/l4/*.bbnf`; no string-literal generated parser. |
| CSS typed equality | Track 1 typed document/value summary must equal cssparser same-workload summary before speed counts. |
| CSS >SOTA | beat cssparser after typed equality on Apple M5 Max / aarch64. |
| Dirty generated state | retire or regenerate cleanly before broad codegen gates close. |
| Pattern H collapse | replace provenance-only state with generator-owned grammar-id template. |
| Deep native SIMD | conditional only after S-P1 profiles a fresh hot leaf; scalar-reference, checkasm/parity, same-wave consumer. |

## Ledger Text

SK-V16 starts from JSON validated, CSS open, and infrastructure partially
validated. It is the grammar-derived CSS and Pattern H collapse tranche unless
S-P0 finds a new contrivance that must be pruned first.
