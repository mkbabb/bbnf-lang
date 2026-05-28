# T-P2 V3 CH7 — Overfit-Prune

Lens: CH7 overfit-prune.

Disposition: `ACCEPT`.

## Critical Findings

| id | severity | finding | evidence | convergence impact |
|---|---|---|---|---|
| CH7-V3-01 | none | Broadcast admission remains blocked. No V3 target-packet row turns the old W8R CSS aggregate tuple into row-local evidence. | `2A-sota-landscape.md` refutes the 24-row aggregate tuple; `2C-grammar-neutrality.md` and `2F-parse-that-gaps.md` keep duplicate-signature detection and broadcast demotion before CSS retime. | No block. |
| CH7-V3-02 | none | Fake CSS parity remains blocked. Fact streams, `CssFullParseSummary`, brace counters, and `CSS_GENERATED_RS` are still diagnostic/refuted until a generated typed CSS provider exists. | `2A-sota-landscape.md` refutes fact-stream and four-counter CSS parity; `2C-grammar-neutrality.md` refutes the CSS generator sidecar; `2F-parse-that-gaps.md` assigns CSS value parsing to a generated typed provider with `cssparser` as same-workload oracle. | No block. |
| CH7-V3-03 | none | Self-excluding gates remain blocked. The packet does not treat current Lock 14 / Lock 16 scans as clean while their leak roots are excluded. | `2C-grammar-neutrality.md` requires full-surface Lock 14 inclusion/exclusion reporting and fails same-change leak-path omission. | No block. |
| CH7-V3-04 | none | Source-only primitive inventory, ISA bits, checkasm-only rows, x86/AVX-512 evidence, and absent-SVE2 paths remain non-admitting. | `2B-primitive-vocabulary.md` separates source inventory from admission; `2E-host-arch-esoterica.md` keeps Apple M5 Max/aarch64 as the only close route and refutes NEON `svmatch_u8`; PMULL/CSSC require same-wave consumer and row movement. | No block. |
| CH7-V3-05 | none | Bench-only FNV closed-enum technique does not leak into runtime admission, and delete-before-provider paths remain blocked. | V3 target dossiers do not route the W11 FNV closed-enum scheme into runtime. `2C-grammar-neutrality.md` blocks `CSS_GENERATED_RS` deletion until same-wave generated typed provider proof; `2A`/`2B` keep sidecar-like routes pre-blocked. | No block. |

## Evidence Inspected

- `restart/audit/totality/p2/hardening/V3/CHALLENGE-CONTEXT.md`.
- `restart/audit/totality/p2/hardening/HARDENING-T-P2-V2-CONSOLIDATED.md`.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
- `restart/audit/totality/p2/2A-sota-landscape.md`.
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`.
- `restart/audit/totality/p2/2C-grammar-neutrality.md`.
- `restart/audit/totality/p2/2D-cost-model.md`.
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`.
- `restart/audit/totality/p2/2F-parse-that-gaps.md`.
- Local grep over the six V3 target dossiers for broadcast, fact-stream,
  brace-counter, `CssFullParseSummary`, self-excluding gates, FNV, x86,
  AVX-512, sidecar, BackendShape, source inventory, provider-before-consumer,
  admission gates, verification actions, and close statuses.

## Fold Requirements

None.

## Convergence Impact

CH7 does not block T-P2 V3 convergence. If CH1-CH6 also return `ACCEPT`, this
lens contributes the second consecutive clean hardening cycle needed for T-P2
§3Z convergence.
