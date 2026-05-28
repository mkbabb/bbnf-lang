# T-P2 V1 CH7 — OVERFIT-PRUNE

Disposition: **ACCEPT**.

## Critical Findings

| id | severity | finding | evidence | fold requirement |
|---|---|---|---|---|
| CH7-V1-OK-01 | none | The packet does not launder the CSS 24-row broadcast into research authority. It names the broadcast as refuted/diagnostic across 2A, 2B, 2C, 2E, and 2F. | `restart/audit/totality/p2/2A-sota-landscape.md:35`-`40`, `:74`-`83`; `2B-primitive-vocabulary.md:153`-`154`; `2C-grammar-neutrality.md:127`-`130`; `2E-host-arch-esoterica.md:113`-`122`; `2F-parse-that-gaps.md:99`-`101`. | None. Keep this as a V2 guard if other CH lenses request citation repair. |
| CH7-V1-OK-02 | none | The packet rejects brace-counter, fact-stream, and `CssFullParseSummary` evidence as CSS Value/API or CSSOM parity. | `restart/audit/totality/p2/2A-sota-landscape.md:82`-`89`; `2C-grammar-neutrality.md:120`-`126`; `2F-parse-that-gaps.md:99`-`100`. | None. |
| CH7-V1-OK-03 | none | The packet detects self-excluding Lock 14 gates instead of treating current gate output as proof. | `restart/audit/totality/p2/2C-grammar-neutrality.md:134`-`141`; `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md:45`-`47`. | None. |
| CH7-V1-OK-04 | none | The packet preserves the Apple M5 Max / aarch64 close route and demotes x86 / AVX-512 to diagnostics. | `restart/audit/totality/p2/2B-primitive-vocabulary.md:127`-`144`, `:152`; `2E-host-arch-esoterica.md:23`-`39`, `:113`-`120`; `restart/skinny/tranches/sk-v15/SPEC.md:133`-`137`. | None. |
| CH7-V1-OK-05 | none | The packet carries delete-before-provider protection: CSS provider retirement, Pattern H deletion/provenance, and parse-that/CSS primitive promotion all require replacement provider or same-wave consumer proof first. | `restart/audit/totality/p2/2C-grammar-neutrality.md:138`-`141`; `2F-parse-that-gaps.md:88`-`90`, `:110`-`111`; `restart/skinny/tranches/sk-v15/SPEC.md:193`-`196`. | None. |
| CH7-V1-OK-06 | none | Deep SIMD entries are source-grounded but not admitted by citation, ISA feature, or checkasm alone. The packet requires scalar oracle, strict parity/checkasm, hardware gate, same-wave consumer, and row movement. | `restart/audit/totality/p2/2B-primitive-vocabulary.md:138`-`144`, `:153`; `2E-host-arch-esoterica.md:29`-`39`; `2F-parse-that-gaps.md:83`-`84`, `:117`-`120`. | None. |

## Evidence Inspected

- `restart/audit/totality/p2/hardening/V1/CHALLENGE-CONTEXT.md`.
- `restart/audit/totality/p2/T-P2-DISPATCH-CONTEXT.md`.
- `restart/audit/totality/p2/2A-sota-landscape.md`.
- `restart/audit/totality/p2/2B-primitive-vocabulary.md`.
- `restart/audit/totality/p2/2C-grammar-neutrality.md`.
- `restart/audit/totality/p2/2D-cost-model.md`.
- `restart/audit/totality/p2/2E-host-arch-esoterica.md`.
- `restart/audit/totality/p2/2F-parse-that-gaps.md`.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md`.
- `restart/skinny/tranches/sk-v15/SPEC.md`.

Local grep scan:

```text
rg -n "broadcast|brace|fact-stream|CssFullParseSummary|CSS_GENERATED_RS|byte-identical|AVX-512|x86|aarch64|M5|gate|exclude|delete|provider|consumer|overfit|contriv" restart/audit/totality/p2/2{A,B,C,D,E,F}-*.md
```

The scan found the expected overfit vocabulary, but the matching rows use it
to reject or bound evidence rather than to admit rows.

## Fold Requirements

None for CH7. If CH1-CH6 require V2 citation or cost folds, preserve the
following CH7 invariants while editing:

1. repeated CSS timings remain diagnostic unless row-local commands, inputs,
   equality, timing, and `broadcast_group_id` prove otherwise;
2. Lock 14 / Lock 16 gates must disclose included roots and exclusions;
3. `CSS_GENERATED_RS`, fact-stream strings, brace counters, and
   `CssFullParseSummary` do not count as typed CSS Value/API or CSSOM parity;
4. x86 / AVX-512 cannot close SK-V15 M5 Max / aarch64 rows;
5. deletion/retirement routes require replacement provider or same-wave
   consumer proof first.

## Convergence Impact

CH7 does not block T-P2 V1 convergence. It returns ACCEPT and adds no orphan
REVISE.
