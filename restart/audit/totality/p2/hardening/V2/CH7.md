# T-P2 V2 CH7 — Overfit-Prune

Lens: CH7 overfit-prune.

Disposition: `ACCEPT`.

## Critical Findings

| id | severity | finding | evidence | convergence impact |
|---|---|---|---|---|
| CH7-V2-01 | none | V2 preserves the CSS broadcast refutation instead of laundering the W8R tuple into new admission evidence. | `2A-sota-landscape.md` refutes the 24-row aggregate tuple and requires independent command/input/equality/timing; `2C-grammar-neutrality.md` and `2F-parse-that-gaps.md` route duplicate-signature detection and broadcast demotion to W1 before CSS retime. | No block. |
| CH7-V2-02 | none | V2 preserves the CSS fake-parity fence: fact streams, `CssFullParseSummary`, brace counters, and `CSS_GENERATED_RS` are diagnostic/refuted until a generated typed CSS provider exists. | `2A-sota-landscape.md` refutes fact-stream and four-counter CSS parity; `2C-grammar-neutrality.md` marks the current CSS generator sidecar as refuted and requires typed value/document/view/visitor output; `2F-parse-that-gaps.md` keeps CSS value parsing owned by a generated typed provider with `cssparser` as same-workload oracle. | No block. |
| CH7-V2-03 | none | V2 catches self-excluding gates instead of depending on them. | `2C-grammar-neutrality.md` has a full-surface Lock 14 scan LAC that fails on omitted leak roots and reports included/excluded roots, owners, reasons, self-scan status, consumers, rows, and disposition. | No block. |
| CH7-V2-04 | none | V2 does not admit source inventory, ISA bits, checkasm-only rows, x86/AVX-512 evidence, or FNV-style bench contrivance as close evidence. | `2B-primitive-vocabulary.md` and `2E-host-arch-esoterica.md` keep x86 diagnostic-only, PMULL/CSSC scalar-delegated or blocked without same-wave consumer and row movement, and NEON `svmatch_u8` refuted; the target packet does not route the W11 FNV closed-enum scheme into runtime admission. | No block. |
| CH7-V2-05 | none | V2 preserves provider-before-deletion ordering and sidecar pre-blocks. | `2C-grammar-neutrality.md` blocks `CSS_GENERATED_RS` deletion until same-wave generated typed provider proof; `2A-sota-landscape.md` and `2B-primitive-vocabulary.md` reject retained sidecar/cursor/list/class-column routes without a new Alpha/P1/SPEC contract. | No block. |

## Evidence Inspected

- `restart/audit/totality/p2/hardening/V2/CHALLENGE-CONTEXT.md`.
- `restart/audit/totality/p2/T-P2-V2-FOLD-ADDENDUM.md`.
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V1-CONSOLIDATED.md`.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
- `restart/audit/totality/p2/2A-sota-landscape.md`.
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`.
- `restart/audit/totality/p2/2C-grammar-neutrality.md`.
- `restart/audit/totality/p2/2D-cost-model.md`.
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`.
- `restart/audit/totality/p2/2F-parse-that-gaps.md`.
- Local grep over the six V2 dossiers for broadcast, fact-stream,
  brace-counter, `CssFullParseSummary`, self-excluding gates, FNV, x86,
  AVX-512, sidecar, BackendShape, provider-before-consumer, admission gates,
  verification actions, and close statuses.

## Fold Requirements

None.

## Convergence Impact

CH7 does not block T-P2 V2 convergence. It confirms that the V2 packet folds
the overfit-prune guards from PASS-IMPL V1 and preserves the SK-V15 refusal to
close by broadcast measurement, fake CSS parity, self-excluding gates,
source-only primitive inventory, wrong-host SIMD evidence, or delete-before-
provider sequencing.
